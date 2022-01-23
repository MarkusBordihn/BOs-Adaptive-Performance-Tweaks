/**
 * Copyright 2021 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweakscore.server;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.config.CommonConfig;

@EventBusSubscriber
public class ServerLoad {

  private static final Logger log = LogManager.getLogger(CoreConstants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static boolean logServerLoad = CommonConfig.COMMON.logServerLoad.get();
  private static int timeBetweenUpdates = COMMON.timeBetweenUpdates.get() * 1000;
  private static long lastUpdateTime = System.currentTimeMillis();

  private static ServerLoadLevel currentServerLoad = ServerLoadLevel.NORMAL;
  private static ServerLoadLevel lastServerLoad = ServerLoadLevel.NORMAL;
  private static double avgTickTime = 50.0;
  private static double lastAvgTickTime = 45.0;

  public enum ServerLoadLevel {
    VERY_LOW, LOW, NORMAL, MEDIUM, HIGH, VERY_HIGH
  }

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    logServerLoad = CommonConfig.COMMON.logServerLoad.get();
    timeBetweenUpdates = COMMON.timeBetweenUpdates.get() * 1000;
  }

  public static void measureLoadAndPost(Dist dist) {
    double currentAverangeTickTime = ServerManager.getAverageTickTime();

    // Restrict and smoother high to low server load updates
    if (lastAvgTickTime >= currentAverangeTickTime
        && System.currentTimeMillis() - lastUpdateTime < timeBetweenUpdates) {
      return;
    }

    // Cache former tick time and load and calculate current load.
    lastAvgTickTime = avgTickTime;
    avgTickTime = ServerManager.getAverageTickTime();
    lastServerLoad = currentServerLoad;
    currentServerLoad = getServerLoadLevelFromTickTime(avgTickTime);

    // Report change to server log, if enabled.
    if (currentServerLoad != lastServerLoad && logServerLoad) {
      String loadIndicator = lastAvgTickTime > avgTickTime ? "↓" : "↑";
      log.info("{} Server load changed from {} (avg. {}) to {} (avg. {})", loadIndicator,
          lastServerLoad, lastAvgTickTime, currentServerLoad, avgTickTime);
    }

    // Post result to the event bus.
    MinecraftForge.EVENT_BUS.post(
        new ServerLoadEvent(currentServerLoad, lastServerLoad, avgTickTime, lastAvgTickTime, dist));

    // Update the last update time
    lastUpdateTime = System.currentTimeMillis();
  }

  public static ServerLoadLevel getServerLoadLevelFromTickTime(double tickTime) {
    if (tickTime <= 20.0) {
      return ServerLoadLevel.VERY_LOW;
    } else if (tickTime <= 40.0) {
      return ServerLoadLevel.LOW;
    } else if (tickTime <= 46.0) {
      return ServerLoadLevel.NORMAL;
    } else if (tickTime <= 49.0) {
      return ServerLoadLevel.MEDIUM;
    } else if (tickTime <= 55.0) {
      return ServerLoadLevel.HIGH;
    } else if (tickTime > 55.0) {
      return ServerLoadLevel.VERY_HIGH;
    }
    return ServerLoadLevel.NORMAL;
  }

  public static ServerLoadLevel getServerLoad() {
    return currentServerLoad;
  }

  public static ServerLoadLevel getLastServerLoad() {
    return lastServerLoad;
  }

  public static boolean hasVeryHighServerLoad() {
    return currentServerLoad == ServerLoadLevel.VERY_HIGH;
  }

  public static boolean hasHighServerLoad() {
    return currentServerLoad == ServerLoadLevel.MEDIUM || currentServerLoad == ServerLoadLevel.HIGH
        || currentServerLoad == ServerLoadLevel.VERY_HIGH;
  }

  public static boolean hasNormalServerLoad() {
    return currentServerLoad == ServerLoadLevel.NORMAL;
  }

  public static boolean hasLowServerLoad() {
    return currentServerLoad == ServerLoadLevel.VERY_LOW
        || currentServerLoad == ServerLoadLevel.LOW;
  }

  public static double getAvgTickTime() {
    return avgTickTime;
  }

}
