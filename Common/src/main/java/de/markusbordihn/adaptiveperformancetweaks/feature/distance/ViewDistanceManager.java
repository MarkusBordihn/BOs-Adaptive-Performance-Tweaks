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

public final class ViewDistanceManager {

  private static final double MEDIUM_MAX_REDUCTION_RATIO = 0.75D;
  private static final double HIGH_MAX_REDUCTION_RATIO = 0.50D;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_DISTANCE);

  private static int currentDistance = -1;
  private static int configuredDistanceMax = -1;
  private static int currentLoadBaselineDistance = -1;
  private static int currentWarmupReduction = 0;
  private static int activeExplorerCount = 0;
  private static int fastExplorerCount = 0;
  private static int recoveryStartTick = -1;
  private static int nextRecoveryTick = -1;
  private static int lastChangeTick = Integer.MIN_VALUE;
  private static ServerLoadLevel currentLoadLevel = ServerLoadLevel.NORMAL;

  private ViewDistanceManager() {
  }

  public static void handleServerStarting(MinecraftServer server) {
    currentDistance = -1;
    configuredDistanceMax = server.getPlayerList().getViewDistance();
    currentLoadBaselineDistance = -1;
    currentWarmupReduction = 0;
    activeExplorerCount = 0;
    fastExplorerCount = 0;
    recoveryStartTick = -1;
    nextRecoveryTick = -1;
    lastChangeTick = Integer.MIN_VALUE;
    currentLoadLevel = ServerLoadLevel.NORMAL;
    if (FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled()) {
      log.info("Adaptive view distance enabled (range {}-{})",
        ViewDistanceConfig.viewDistanceMin, ViewDistanceConfig.viewDistanceMax);
    }
  }

  public static void handleFeatureEnabled(MinecraftServer server) {
    handleServerStarting(server);
    evaluateAndApply();
  }

  public static void handleFeatureDisabled() {
    MinecraftServer server = ServerManager.getMinecraftServer();
    currentLoadBaselineDistance = -1;
    currentWarmupReduction = 0;
    activeExplorerCount = 0;
    fastExplorerCount = 0;
    recoveryStartTick = -1;
    nextRecoveryTick = -1;
    currentLoadLevel = ServerLoadLevel.NORMAL;
    if (server == null || configuredDistanceMax <= 0) {
      currentDistance = -1;
      return;
    }

    int targetDistance = configuredDistanceMax;
    if (currentDistance == targetDistance) {
      return;
    }

    currentDistance = targetDistance;
    server.getPlayerList().setViewDistance(targetDistance);
  }

  public static void handlePlayerLoggedIn(ServerPlayer player) {
    if (!FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled()) {
      return;
    }

    PlayerPositionManager.handlePlayerLoggedIn(player);
    markPlayerWarmup(player, "login");
    evaluateAndApply();
  }

  public static void handlePlayerTeleported(ServerPlayer player) {
    if (!FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled()) {
      return;
    }

    PlayerPositionManager.handlePlayerTeleported(player);
    markPlayerWarmup(player, "teleport");
    evaluateAndApply();
  }

  public static void handleServerTick() {
    if (!FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled()) {
      return;
    }

    int currentTick = PlayerPositionManager.getCurrentServerTick();
    if (currentDistance == -1
      || currentWarmupReduction > 0
      || currentTick % ViewDistanceConfig.evaluationIntervalTicks == 0) {
      evaluateAndApply();
    }
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled()) {
      return;
    }

    currentLoadLevel = event.getServerLoadLevel();
    evaluateAndApply();
  }

  public static int getCurrentViewDistance() {
    return currentDistance;
  }

  public static int getCurrentLoadBaselineDistance() {
    return currentLoadBaselineDistance;
  }

  public static int getCurrentWarmupReduction() {
    return currentWarmupReduction;
  }

  public static int getActiveExplorerCount() {
    return activeExplorerCount;
  }

  public static boolean isWarmupActive() {
    return currentWarmupReduction > 0;
  }

  private static void markPlayerWarmup(ServerPlayer player, String triggerSource) {
    if (player == null || !ViewDistanceConfig.loginWarmupEnabled) {
      return;
    }

    PlayerPosition playerPosition = PlayerPositionManager.getPlayerPositionMap().get(
      player.getStringUUID());
    if (playerPosition != null) {
      playerPosition.setLoginWarmup(PlayerPositionManager.getCurrentServerTick(),
        ViewDistanceConfig.loginWarmupTicks);
      log.debug("View distance {} warmup triggered for {} ({} ticks)",
        triggerSource, player.getName().getString(), ViewDistanceConfig.loginWarmupTicks);
    }
  }

  private static void evaluateAndApply() {
    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null) {
      return;
    }

    currentLoadBaselineDistance = resolveNextLoadBaselineDistance(currentLoadLevel,
      currentLoadBaselineDistance);
    updateWarmupReduction();

    int targetDistance = Math.max(
      ViewDistanceConfig.viewDistanceMin,
      Math.min(getConfiguredDistanceMax(), currentLoadBaselineDistance) - currentWarmupReduction);
    applyDistance(server, targetDistance);
  }

  private static void updateWarmupReduction() {
    int currentTick = PlayerPositionManager.getCurrentServerTick();
    Map<String, PlayerPosition> playerPositions = PlayerPositionManager.getPlayerPositionMap();
    int trackedPlayers = playerPositions.size();
    int newActiveExplorerCount = 0;
    int newFastExplorerCount = 0;
    int warmupPlayerCount = 0;
    int previousReduction = currentWarmupReduction;
    for (PlayerPosition playerPosition : playerPositions.values()) {
      boolean warmupActive = playerPosition.isLoginWarmupActive(currentTick);
      if (warmupActive) {
        warmupPlayerCount++;
      }
      if (warmupActive || playerPosition.hasRecentMovementSpeed(
        ViewDistanceConfig.movementSpeedBlocksPerSecond)) {
        newActiveExplorerCount++;
      }
      if (playerPosition.hasRecentMovementSpeed(
        ViewDistanceConfig.movementFastSpeedBlocksPerSecond)) {
        newFastExplorerCount++;
      }
    }

    activeExplorerCount = newActiveExplorerCount;
    fastExplorerCount = newFastExplorerCount;
    int targetReduction = 0;
    if (ViewDistanceConfig.loginWarmupEnabled && warmupPlayerCount > 0) {
      targetReduction = getWarmupReduction();
    }
    if (ViewDistanceConfig.movementWarmupEnabled && activeExplorerCount > 0) {
      targetReduction = Math.max(targetReduction,
        calculateMovementReduction(trackedPlayers, activeExplorerCount, fastExplorerCount));
    }

    if (targetReduction >= currentWarmupReduction) {
      currentWarmupReduction = targetReduction;
      recoveryStartTick = -1;
      nextRecoveryTick = -1;
    } else if (targetReduction <= 0 && activeExplorerCount > 0) {
      recoveryStartTick = -1;
      nextRecoveryTick = -1;
    } else {
      if (recoveryStartTick < 0) {
        recoveryStartTick = currentTick + currentRecoveryDelayTicks();
        nextRecoveryTick = recoveryStartTick;
      }
      if (currentTick >= nextRecoveryTick) {
        currentWarmupReduction = Math.max(targetReduction, currentWarmupReduction - 1);
        nextRecoveryTick = currentTick + currentRecoveryStepTicks();
      }
      if (currentWarmupReduction <= targetReduction) {
        recoveryStartTick = -1;
        nextRecoveryTick = -1;
      }
    }

    if (currentWarmupReduction != previousReduction) {
      log.debug("View distance warmup: reduction {} -> {} (target={} explorers={} fast={} load={})",
        previousReduction, currentWarmupReduction, targetReduction, activeExplorerCount,
        fastExplorerCount, currentLoadLevel);
    }
  }

  private static int calculateMovementReduction(
    int trackedPlayers, int activeExplorers, int fastExplorers) {
    if (trackedPlayers <= 0 || activeExplorers <= 0
      || ViewDistanceConfig.movementReductionMax <= 0) {
      return 0;
    }
    if (ViewDistanceConfig.movementReductionMax <= 1) {
      return ViewDistanceConfig.movementReductionMax;
    }

    double activeRatio = activeExplorers / (double) trackedPlayers;
    boolean useMaxReduction = fastExplorers > 0 || switch (currentLoadLevel) {
      case MEDIUM -> activeRatio >= MEDIUM_MAX_REDUCTION_RATIO;
      case HIGH -> activeRatio >= HIGH_MAX_REDUCTION_RATIO;
      case VERY_HIGH -> true;
      default -> false;
    };
    return useMaxReduction ? ViewDistanceConfig.movementReductionMax : 1;
  }

  private static int resolveNextLoadBaselineDistance(
    ServerLoadLevel loadLevel, int currentBaselineDistance) {
    int targetBaselineDistance = targetDistanceForLevel(loadLevel);
    if (currentBaselineDistance < 0) {
      return targetBaselineDistance;
    }
    if (targetBaselineDistance < currentBaselineDistance) {
      return Math.max(targetBaselineDistance, currentBaselineDistance - 1);
    }
    if (targetBaselineDistance > currentBaselineDistance) {
      return Math.min(targetBaselineDistance, currentBaselineDistance + 1);
    }
    return currentBaselineDistance;
  }

  private static void applyDistance(MinecraftServer server, int targetDistance) {
    if (targetDistance == currentDistance) {
      return;
    }

    int currentTick = PlayerPositionManager.getCurrentServerTick();
    boolean isReduction = currentDistance < 0 || targetDistance < currentDistance;
    if (!isReduction) {
      if (currentTick - lastChangeTick < currentRecoveryStepTicks()) {
        return;
      }
      targetDistance = Math.min(targetDistance, currentDistance + 1);
      if (targetDistance == currentDistance) {
        return;
      }
    }

    log.debug("View distance {} -> {} (baseline={} warmupReduction={} activeExplorers={} load={})",
      currentDistance, targetDistance, currentLoadBaselineDistance, currentWarmupReduction,
      activeExplorerCount, currentLoadLevel);
    currentDistance = targetDistance;
    lastChangeTick = currentTick;
    PerformanceStats.viewDistanceChanges++;
    server.getPlayerList().setViewDistance(currentDistance);
  }

  private static int getWarmupReduction() {
    return Math.max(0, getConfiguredDistanceMax() - ViewDistanceConfig.viewDistanceMin);
  }

  private static boolean isLowLoadForRecovery() {
    return !currentLoadLevel.isAtLeast(ViewDistanceConfig.minOptimizationLoadLevel);
  }

  private static int currentRecoveryDelayTicks() {
    int baseRecoveryDelayTicks = isLowLoadForRecovery()
      ? ViewDistanceConfig.recoveryFastDelayTicks
      : ViewDistanceConfig.recoveryDelayTicks;
    return Math.max(baseRecoveryDelayTicks, ViewDistanceConfig.recoveryMinDelayTicks);
  }

  private static int currentRecoveryStepTicks() {
    return isLowLoadForRecovery()
      ? ViewDistanceConfig.recoveryFastStepTicks
      : ViewDistanceConfig.recoveryStepTicks;
  }

  private static int getConfiguredDistanceMax() {
    return configuredDistanceMax > 0
      ? Math.min(ViewDistanceConfig.viewDistanceMax, configuredDistanceMax)
      : ViewDistanceConfig.viewDistanceMax;
  }

  private static int targetDistanceForLevel(ServerLoadLevel level) {
    if (!level.isAtLeast(ViewDistanceConfig.minOptimizationLoadLevel)) {
      return getConfiguredDistanceMax();
    }

    int raw = switch (level) {
      case VERY_LOW -> ViewDistanceConfig.viewDistanceVeryLow;
      case LOW -> ViewDistanceConfig.viewDistanceLow;
      case NORMAL -> ViewDistanceConfig.viewDistanceNormal;
      case MEDIUM -> ViewDistanceConfig.viewDistanceMedium;
      case HIGH -> ViewDistanceConfig.viewDistanceHigh;
      case VERY_HIGH -> ViewDistanceConfig.viewDistanceVeryHigh;
    };

    return Math.max(ViewDistanceConfig.viewDistanceMin,
      Math.min(getConfiguredDistanceMax(), raw));
  }
}
