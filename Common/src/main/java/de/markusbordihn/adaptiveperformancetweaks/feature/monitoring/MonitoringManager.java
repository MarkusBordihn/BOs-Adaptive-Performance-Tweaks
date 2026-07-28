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

package de.markusbordihn.adaptiveperformancetweaks.feature.monitoring;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ExperienceOrbManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemEntityManager;
import net.minecraft.server.MinecraftServer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class MonitoringManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static long lastLogTime = 0;
  private static ServerLoadLevel lastLoggedLoadLevel = null;

  private MonitoringManager() {
  }

  public static void reset() {
    lastLogTime = 0;
    lastLoggedLoadLevel = null;
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!FeatureToggle.MONITORING.isEnabled()) {
      return;
    }

    long currentTime = System.currentTimeMillis();
    if (!shouldLog(currentTime, event.getServerLoadLevel())) {
      return;
    }

    lastLogTime = currentTime;
    lastLoggedLoadLevel = event.getServerLoadLevel();
    logStatus(event);
  }

  private static boolean shouldLog(long currentTime, ServerLoadLevel loadLevel) {
    int intervalMillis = MonitoringConfig.monitoringIntervalSeconds * 1000;
    if (intervalMillis <= 0) {
      return loadLevel != lastLoggedLoadLevel;
    }

    return currentTime - lastLogTime >= intervalMillis;
  }

  private static void logStatus(ServerLoadEvent event) {
    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null) {
      return;
    }

    StringBuilder status = new StringBuilder("[Monitor]");

    if (MonitoringConfig.monitoringLogTps) {
      double ticksPerSecond = Math.min(20.0, 1000.0 / Math.max(1.0, event.getAvgTickTime()));
      status.append(String.format(" TPS=%.1f (%.1fms)", ticksPerSecond, event.getAvgTickTime()));
    }

    if (MonitoringConfig.monitoringLogLoadLevel) {
      status.append(" Load=").append(event.getServerLoadLevel());
    }

    if (MonitoringConfig.monitoringLogPlayers) {
      status.append(" Players=").append(server.getPlayerList().getPlayerCount());
    }

    if (MonitoringConfig.monitoringLogEntities) {
      int total =
        CoreEntityManager.getTotalTrackedEntityCount()
          + ItemEntityManager.getTrackedItemEntityCount()
          + ExperienceOrbManager.getTrackedExperienceOrbCount();
      status.append(" Entities=").append(total);
    }

    log.info("{}", status);
  }
}
