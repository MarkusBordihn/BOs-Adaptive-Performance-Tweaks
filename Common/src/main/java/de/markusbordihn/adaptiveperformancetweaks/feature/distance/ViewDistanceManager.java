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
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ViewDistanceManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_DISTANCE);

  private static int currentDistance = -1;
  private static int configuredDistanceMax = -1;
  private static long warmupUntilTime = 0L;
  private static long lastRecoveryTime = 0L;
  private static ServerLoadLevel currentLoadLevel = ServerLoadLevel.NORMAL;

  private ViewDistanceManager() {
  }

  public static void handleServerStarting(MinecraftServer server) {
    currentDistance = -1;
    configuredDistanceMax = server.getPlayerList().getViewDistance();
    warmupUntilTime = 0L;
    lastRecoveryTime = System.currentTimeMillis();
    currentLoadLevel = ServerLoadLevel.NORMAL;
    if (FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled()) {
      log.info("Adaptive view distance enabled (range {}-{})",
        ViewDistanceConfig.viewDistanceMin, ViewDistanceConfig.viewDistanceMax);
    }
  }

  public static void handleFeatureEnabled(MinecraftServer server) {
    handleServerStarting(server);
    applyDistance(server, resolveTargetDistance());
  }

  public static void handleFeatureDisabled() {
    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null || configuredDistanceMax <= 0) {
      currentDistance = -1;
      warmupUntilTime = 0L;
      lastRecoveryTime = System.currentTimeMillis();
      currentLoadLevel = ServerLoadLevel.NORMAL;
      return;
    }

    int targetDistance = configuredDistanceMax;
    warmupUntilTime = 0L;
    lastRecoveryTime = System.currentTimeMillis();
    currentLoadLevel = ServerLoadLevel.NORMAL;
    if (currentDistance == targetDistance) {
      return;
    }

    currentDistance = targetDistance;
    server.getPlayerList().setViewDistance(targetDistance);
  }

  public static void handlePlayerLoggedIn(ServerPlayer player) {
    applyPlayerWarmup();
  }

  public static void handlePlayerTeleported(ServerPlayer player) {
    applyPlayerWarmup();
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled()) {
      return;
    }

    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null) {
      return;
    }

    currentLoadLevel = event.getServerLoadLevel();
    int targetDistance = resolveTargetDistance();
    if (!event.hasChanged() && currentDistance != -1 && targetDistance == currentDistance) {
      return;
    }
    if (targetDistance == currentDistance) {
      return;
    }

    log.debug("View distance {} -> {} (load: {})",
      currentDistance, targetDistance, currentLoadLevel);
    currentDistance = targetDistance;
    PerformanceStats.viewDistanceChanges++;
    server.getPlayerList().setViewDistance(currentDistance);
  }

  private static void applyPlayerWarmup() {
    if (!FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled()) {
      return;
    }

    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null) {
      return;
    }

    warmupUntilTime = Math.max(warmupUntilTime,
      System.currentTimeMillis() + SimulationDistanceConfig.movementThrottleLoginTicks * 50L);
    lastRecoveryTime = System.currentTimeMillis();
    applyDistance(server, ViewDistanceConfig.viewDistanceMin);
  }

  private static void applyDistance(MinecraftServer server, int targetDistance) {
    if (targetDistance == currentDistance) {
      return;
    }

    currentDistance = targetDistance;
    PerformanceStats.viewDistanceChanges++;
    server.getPlayerList().setViewDistance(currentDistance);
  }

  private static int resolveTargetDistance() {
    int configuredMax = getConfiguredDistanceMax();
    if (isWarmupActive()) {
      return Math.max(ViewDistanceConfig.viewDistanceMin,
        Math.min(configuredMax, ViewDistanceConfig.viewDistanceMin));
    }

    int baselineTarget = Math.max(ViewDistanceConfig.viewDistanceMin,
      Math.min(configuredMax, targetDistanceForLevel(currentLoadLevel)));
    if (currentDistance < 0 || baselineTarget <= currentDistance) {
      return baselineTarget;
    }
    if (currentLoadLevel.isAtLeast(ServerLoadLevel.NORMAL)) {
      return currentDistance;
    }
    if (System.currentTimeMillis() - lastRecoveryTime < 10_000L) {
      return currentDistance;
    }

    lastRecoveryTime = System.currentTimeMillis();
    return Math.min(baselineTarget, currentDistance + 1);
  }

  private static int getConfiguredDistanceMax() {
    return configuredDistanceMax > 0
      ? Math.min(ViewDistanceConfig.viewDistanceMax, configuredDistanceMax)
      : ViewDistanceConfig.viewDistanceMax;
  }

  private static boolean isWarmupActive() {
    return System.currentTimeMillis() < warmupUntilTime;
  }

  private static int targetDistanceForLevel(ServerLoadLevel level) {
    int raw = switch (level) {
      case VERY_LOW -> ViewDistanceConfig.viewDistanceVeryLow;
      case LOW -> ViewDistanceConfig.viewDistanceLow;
      case NORMAL -> ViewDistanceConfig.viewDistanceNormal;
      case MEDIUM -> ViewDistanceConfig.viewDistanceMedium;
      case HIGH -> ViewDistanceConfig.viewDistanceHigh;
      case VERY_HIGH -> ViewDistanceConfig.viewDistanceVeryHigh;
    };

    return Math.max(ViewDistanceConfig.viewDistanceMin,
      Math.min(ViewDistanceConfig.viewDistanceMax, raw));
  }
}
