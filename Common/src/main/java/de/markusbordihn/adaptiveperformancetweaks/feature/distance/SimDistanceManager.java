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
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import net.minecraft.server.MinecraftServer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SimDistanceManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_DISTANCE);

  private static int currentDistance = -1;

  private SimDistanceManager() {
  }

  public static void handleServerStarting(MinecraftServer server) {
    currentDistance = -1;
    log.info("Adaptive simulation distance enabled (range {}-{})",
      SimDistanceConfig.simDistanceMin, SimDistanceConfig.simDistanceMax);
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!event.hasChanged() && currentDistance != -1) {
      return;
    }

    int targetDistance = targetDistanceForLevel(event.getServerLoadLevel());
    if (targetDistance == currentDistance) {
      return;
    }

    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null) {
      return;
    }

    log.debug("Sim distance {} → {} (load: {})",
      currentDistance, targetDistance, event.getServerLoadLevel());
    currentDistance = targetDistance;
    server.getPlayerList().setSimulationDistance(currentDistance);
  }

  private static int targetDistanceForLevel(ServerLoadLevel level) {
    int raw = switch (level) {
      case VERY_LOW -> SimDistanceConfig.simDistanceVeryLow;
      case LOW -> SimDistanceConfig.simDistanceLow;
      case NORMAL -> SimDistanceConfig.simDistanceNormal;
      case MEDIUM -> SimDistanceConfig.simDistanceMedium;
      case HIGH -> SimDistanceConfig.simDistanceHigh;
      case VERY_HIGH -> SimDistanceConfig.simDistanceVeryHigh;
    };

    return Math.max(SimDistanceConfig.simDistanceMin,
      Math.min(SimDistanceConfig.simDistanceMax, raw));
  }
}
