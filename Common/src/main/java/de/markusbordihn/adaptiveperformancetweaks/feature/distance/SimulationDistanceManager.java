/*
 * Copyright 2024 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweaks.feature.distance;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.util.Map;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SimulationDistanceManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_DISTANCE);
  private static final double MEDIUM_MAX_REDUCTION_RATIO = 0.75D;
  private static final double HIGH_MAX_REDUCTION_RATIO = 0.50D;

  private static int currentDistance = -1;
  private static int currentLoadBaselineDistance = -1;
  private static int currentMovementReduction = 0;
  private static int activeExplorerCount = 0;
  private static int configuredDistanceMax = -1;
  private static int recoveryStartTick = -1;
  private static int nextRecoveryTick = -1;
  private static int lastChangeTick = Integer.MIN_VALUE;
  private static ServerLoadLevel currentLoadLevel = ServerLoadLevel.NORMAL;

  private SimulationDistanceManager() {
  }

  public static void handleServerStarting(MinecraftServer server) {
    currentDistance = -1;
    currentLoadBaselineDistance = -1;
    currentMovementReduction = 0;
    activeExplorerCount = 0;
    configuredDistanceMax = server.getPlayerList().getSimulationDistance();
    recoveryStartTick = -1;
    nextRecoveryTick = -1;
    lastChangeTick = Integer.MIN_VALUE;
    currentLoadLevel = ServerLoadLevel.NORMAL;
    if (FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled()) {
      log.info("Adaptive simulation distance enabled (range {}-{})",
        SimulationDistanceConfig.simDistanceMin, SimulationDistanceConfig.simDistanceMax);
    }
  }

  public static void handleFeatureEnabled(MinecraftServer server) {
    handleServerStarting(server);
    evaluateAndApply(false);
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled()) {
      return;
    }

    currentLoadLevel = event.getServerLoadLevel();
    evaluateAndApply(false);
  }

  public static void handleServerTick() {
    int currentTick = PlayerPositionManager.getCurrentServerTick();
    if (currentDistance == -1
      || currentMovementReduction > 0
      || currentTick % SimulationDistanceConfig.movementThrottleSampleTicks == 0) {
      evaluateAndApply(currentTick % SimulationDistanceConfig.movementThrottleSampleTicks == 0);
    }
  }

  public static void handlePlayerLoggedIn(ServerPlayer player) {
    if (!FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled()) {
      return;
    }

    PlayerPositionManager.handlePlayerLoggedIn(player);
    markPlayerWarmup(player, "login");
    evaluateAndApply(false);
  }

  public static void handlePlayerTeleported(ServerPlayer player) {
    if (!FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled()) {
      return;
    }

    PlayerPositionManager.handlePlayerTeleported(player);
    markPlayerWarmup(player, "teleport");
    evaluateAndApply(false);
  }

  public static void handlePlayerLoggedOut() {
    if (!FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled()) {
      return;
    }

    evaluateAndApply(false);
  }

  public static void handleFeatureDisabled() {
    MinecraftServer server = ServerManager.getMinecraftServer();
    clearMovementThrottle();
    currentLoadBaselineDistance = -1;
    lastChangeTick = Integer.MIN_VALUE;
    currentLoadLevel = ServerLoadLevel.NORMAL;
    if (server == null) {
      currentDistance = -1;
      return;
    }

    int targetDistance = getConfiguredDistanceMax();
    if (targetDistance <= 0) {
      currentDistance = -1;
      return;
    }
    if (currentDistance == targetDistance) {
      return;
    }

    currentDistance = targetDistance;
    server.getPlayerList().setSimulationDistance(targetDistance);
  }

  public static int getCurrentLoadBaselineDistance() {
    return currentLoadBaselineDistance;
  }

  public static int getCurrentSimulationDistance() {
    return currentDistance;
  }

  public static boolean isMovementThrottleActive() {
    return currentMovementReduction > 0;
  }

  public static int getActiveExplorerCount() {
    return activeExplorerCount;
  }

  public static int getCurrentMovementReduction() {
    return currentMovementReduction;
  }

  static int calculateMovementReduction(
    ServerLoadLevel loadLevel, int trackedPlayers, int activeExplorers) {
    if (trackedPlayers <= 0 || activeExplorers <= 0) {
      return 0;
    }

    int reduction = SimulationDistanceConfig.movementThrottleMinReduction;
    if (SimulationDistanceConfig.movementThrottleMaxReduction
      <= SimulationDistanceConfig.movementThrottleMinReduction) {
      return reduction;
    }

    double activeRatio = activeExplorers / (double) trackedPlayers;
    boolean useMaxReduction = switch (loadLevel) {
      case MEDIUM -> activeRatio >= MEDIUM_MAX_REDUCTION_RATIO;
      case HIGH -> activeRatio >= HIGH_MAX_REDUCTION_RATIO;
      case VERY_HIGH -> true;
      default -> false;
    };

    return useMaxReduction
      ? SimulationDistanceConfig.movementThrottleMaxReduction
      : reduction;
  }

  static int resolveNextLoadBaselineDistance(
    ServerLoadLevel loadLevel, int currentBaselineDistance) {
    int targetBaselineDistance = targetDistanceForLevel(loadLevel);
    if (currentBaselineDistance < 0) {
      return targetBaselineDistance;
    }
    if (targetBaselineDistance < currentBaselineDistance) {
      return Math.max(targetBaselineDistance, currentBaselineDistance - 1);
    }
    if (targetBaselineDistance > currentBaselineDistance
      && !loadLevel.isAtLeast(ServerLoadLevel.NORMAL)) {
      return targetBaselineDistance;
    }

    return currentBaselineDistance;
  }

  private static void evaluateAndApply(boolean recordMovementSample) {
    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null) {
      return;
    }

    int previousLoadBaselineDistance = currentLoadBaselineDistance;
    currentLoadBaselineDistance = resolveNextLoadBaselineDistance(currentLoadLevel,
      currentLoadBaselineDistance);
    if (currentLoadBaselineDistance != previousLoadBaselineDistance) {
      log.debug("Simulation distance load baseline: {} -> {} (load={})",
        previousLoadBaselineDistance, currentLoadBaselineDistance, currentLoadLevel);
    }

    updateMovementThrottle(recordMovementSample);

    int targetDistance = Math.max(
      SimulationDistanceConfig.simDistanceMin,
      Math.min(getConfiguredDistanceMax(), currentLoadBaselineDistance) - currentMovementReduction);
    if (targetDistance == currentDistance) {
      return;
    }

    int currentTick = PlayerPositionManager.getCurrentServerTick();
    boolean isReduction = currentDistance < 0 || targetDistance < currentDistance;
    if (!isReduction) {
      if (currentTick - lastChangeTick
        < SimulationDistanceConfig.movementThrottleRecoveryStepTicks) {
        return;
      }
      targetDistance = Math.min(targetDistance, currentDistance + 1);
      if (targetDistance == currentDistance) {
        return;
      }
    }

    if (recordMovementSample && currentMovementReduction > 0 && isReduction) {
      PerformanceStats.simulationDistanceMovementAdjustments++;
    }

    log.debug(
      "Simulation distance {} -> {} (baseline={} movementReduction={} activeExplorers={} load={})",
      currentDistance, targetDistance, currentLoadBaselineDistance, currentMovementReduction,
      activeExplorerCount, currentLoadLevel);
    currentDistance = targetDistance;
    lastChangeTick = currentTick;
    PerformanceStats.simulationDistanceChanges++;
    server.getPlayerList().setSimulationDistance(currentDistance);
  }

  private static void updateMovementThrottle(boolean recordMovementSample) {
    int currentTick = PlayerPositionManager.getCurrentServerTick();
    Map<String, PlayerPosition> playerPositions = PlayerPositionManager.getPlayerPositionMap();
    int trackedPlayers = playerPositions.size();
    int newActiveExplorerCount = 0;
    int warmupPlayerCount = 0;
    int previousReduction = currentMovementReduction;
    boolean allPlayersStable = trackedPlayers == 0;
    for (PlayerPosition playerPosition : playerPositions.values()) {
      boolean warmupActive = playerPosition.isLoginWarmupActive(currentTick);
      boolean hasRecentMovement = playerPosition.hasRecentMovementDistance(
        SimulationDistanceConfig.movementThrottleDistanceThresholdBlocks);
      if (warmupActive || hasRecentMovement) {
        newActiveExplorerCount++;
      }
      if (warmupActive) {
        warmupPlayerCount++;
      }

      if (!playerPosition.isStableForTicks(PlayerPositionManager.getMovementUpdateTick())) {
        allPlayersStable = false;
      }
    }

    activeExplorerCount = newActiveExplorerCount;
    int targetReduction = 0;
    boolean loginWarmupActive = warmupPlayerCount > 0;
    if (loginWarmupActive) {
      targetReduction = getWarmupReduction();
    }
    boolean movementWarmupActive = SimulationDistanceConfig.movementThrottleEnabled
      && activeExplorerCount > 0;
    if (movementWarmupActive) {
      targetReduction = Math.max(targetReduction,
        calculateMovementReduction(currentLoadLevel, trackedPlayers, activeExplorerCount));
    }

    if (targetReduction > 0) {
      recoveryStartTick = -1;
      nextRecoveryTick = -1;
      currentMovementReduction = Math.max(currentMovementReduction, targetReduction);
      if (recordMovementSample) {
        PerformanceStats.simulationDistanceMovementThrottleSamples++;
        PerformanceStats.simulationDistanceMovementMaxReduction =
          Math.max(PerformanceStats.simulationDistanceMovementMaxReduction,
            currentMovementReduction);
      }
      if (!loginWarmupActive && currentMovementReduction != previousReduction) {
        log.debug(
          "Simulation distance movement warmup: reduction {} -> {} (activeExplorers={} load={})",
          previousReduction, currentMovementReduction, activeExplorerCount, currentLoadLevel);
      }
      return;
    }

    if (currentMovementReduction <= 0) {
      clearMovementThrottle();
      return;
    }

    if (currentLoadLevel.isAtLeast(ServerLoadLevel.NORMAL)) {
      return;
    }

    if (SimulationDistanceConfig.movementThrottleRecoverOnlyWhenStable && !allPlayersStable) {
      recoveryStartTick = -1;
      nextRecoveryTick = -1;
      return;
    }

    if (recoveryStartTick < 0) {
      recoveryStartTick = currentTick + currentRecoveryDelayTicks();
      nextRecoveryTick = recoveryStartTick;
    }

    if (currentTick < nextRecoveryTick) {
      return;
    }

    currentMovementReduction = Math.max(0, currentMovementReduction - 1);
    if (currentMovementReduction != previousReduction) {
      log.debug("Simulation distance warmup recovery: reduction {} -> {} (load={})",
        previousReduction, currentMovementReduction, currentLoadLevel);
    }
    if (currentMovementReduction == 0) {
      recoveryStartTick = -1;
      nextRecoveryTick = -1;
    } else {
      nextRecoveryTick = currentTick + SimulationDistanceConfig.movementThrottleRecoveryStepTicks;
    }
  }

  private static void clearMovementThrottle() {
    activeExplorerCount = 0;
    currentMovementReduction = 0;
    recoveryStartTick = -1;
    nextRecoveryTick = -1;
  }

  private static int currentRecoveryDelayTicks() {
    return Math.max(
      SimulationDistanceConfig.movementThrottleRecoveryDelayTicks,
      SimulationDistanceConfig.movementThrottleRecoveryMinDelayTicks);
  }

  private static int getConfiguredDistanceMax() {
    return configuredDistanceMax > 0
      ? Math.min(SimulationDistanceConfig.simDistanceMax, configuredDistanceMax)
      : SimulationDistanceConfig.simDistanceMax;
  }

  private static int getWarmupReduction() {
    return Math.max(0, getConfiguredDistanceMax() - SimulationDistanceConfig.simDistanceMin);
  }

  private static void markPlayerWarmup(ServerPlayer player, String triggerSource) {
    if (player == null || !SimulationDistanceConfig.loginWarmupEnabled) {
      return;
    }

    PlayerPosition playerPosition = PlayerPositionManager.getPlayerPositionMap().get(
      player.getStringUUID());
    if (playerPosition != null) {
      playerPosition.setLoginWarmup(PlayerPositionManager.getCurrentServerTick(),
        SimulationDistanceConfig.movementThrottleLoginTicks);
      log.debug("Simulation distance {} warmup triggered for {} ({} ticks)",
        triggerSource, player.getName().getString(),
        SimulationDistanceConfig.movementThrottleLoginTicks);
    }
  }

  private static int targetDistanceForLevel(ServerLoadLevel level) {
    int raw = switch (level) {
      case VERY_LOW -> SimulationDistanceConfig.simDistanceVeryLow;
      case LOW -> SimulationDistanceConfig.simDistanceLow;
      case NORMAL -> SimulationDistanceConfig.simDistanceNormal;
      case MEDIUM -> SimulationDistanceConfig.simDistanceMedium;
      case HIGH -> SimulationDistanceConfig.simDistanceHigh;
      case VERY_HIGH -> SimulationDistanceConfig.simDistanceVeryHigh;
    };

    return Math.max(SimulationDistanceConfig.simDistanceMin,
      Math.min(SimulationDistanceConfig.simDistanceMax, raw));
  }
}
