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

import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.level.ServerLevel;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;
import de.markusbordihn.adaptiveperformancetweakscore.config.CommonConfig;

@EventBusSubscriber
public class ServerLevelLoad {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static boolean logServerLevelLoad = COMMON.logServerLevelLoad.get();
  private static int timeBetweenUpdates = COMMON.timeBetweenUpdates.get() * 1000;
  private static long lastUpdateTime = System.currentTimeMillis();

  private static Map<ServerLevel, Double> levelLoad = new ConcurrentHashMap<>();
  private static Map<ServerLevel, ServerLevelLoadLevel> levelLoadLevel = new ConcurrentHashMap<>();

  public enum ServerLevelLoadLevel {
    VERY_LOW, LOW, NORMAL, MEDIUM, HIGH, VERY_HIGH
  }

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    levelLoad = new ConcurrentHashMap<>();
    levelLoadLevel = new ConcurrentHashMap<>();
    logServerLevelLoad = COMMON.logServerLevelLoad.get();
    timeBetweenUpdates = COMMON.timeBetweenUpdates.get() * 1000;
  }

  public static void measureLoadAndPost(Dist dist) {
    for (ServerLevel ServerLevel : ServerManager.getAllLevels()) {
      // Calculate current averange tick times.
      long[] tickTimes = ServerManager.getMinecraftServer().getTickTime(ServerLevel.dimension());
      if (tickTimes != null) {
        double avgTickTime = Arrays.stream(tickTimes).average().orElse(Double.NaN) / 1000000;

        // Restrict and smoother high to low server load updates
        double lastAvgTickTime = levelLoad.getOrDefault(ServerLevel, 45.0);
        if (lastAvgTickTime >= avgTickTime
            && System.currentTimeMillis() - lastUpdateTime < timeBetweenUpdates) {
          continue;
        }

        // Cache avg. tick time
        levelLoad.put(ServerLevel, avgTickTime);

        // Cache former load level and calculate current load level.
        ServerLevelLoadLevel lastLoadLevel =
            levelLoadLevel.getOrDefault(ServerLevel, ServerLevelLoadLevel.NORMAL);
        ServerLevelLoadLevel loadLevel = getServerLevelLoadLevelFromTickTime(avgTickTime);
        levelLoadLevel.put(ServerLevel, loadLevel);

        // Report change to server log, if enabled.
        if (loadLevel != lastLoadLevel && logServerLevelLoad) {
          String loadIndicator = lastAvgTickTime > avgTickTime ? "↓" : "↑";
          log.info("{} {} Level load for {} changed from {} (avg. {}) to {} (avg. {})",
              Constants.LOG_PREFIX, loadIndicator, ServerLevel.dimension().location(),
              lastLoadLevel, lastAvgTickTime, loadLevel, avgTickTime);
        }

        // Post result to the event bus.
        MinecraftForge.EVENT_BUS.post(new ServerLevelLoadEvent(ServerLevel, loadLevel,
            lastLoadLevel, avgTickTime, lastAvgTickTime, dist));
      }
    }

    // Update the last update time
    lastUpdateTime = System.currentTimeMillis();
  }

  public static ServerLevelLoadLevel getServerLevelLoadLevelFromTickTime(double tickTime) {
    if (tickTime <= 15.0) {
      return ServerLevelLoadLevel.VERY_LOW;
    } else if (tickTime <= 35.0) {
      return ServerLevelLoadLevel.LOW;
    } else if (tickTime <= 41.0) {
      return ServerLevelLoadLevel.NORMAL;
    } else if (tickTime <= 46.0) {
      return ServerLevelLoadLevel.MEDIUM;
    } else if (tickTime <= 50.0) {
      return ServerLevelLoadLevel.HIGH;
    } else if (tickTime > 50.0) {
      return ServerLevelLoadLevel.VERY_HIGH;
    }
    return ServerLevelLoadLevel.NORMAL;
  }

  public static Map<ServerLevel, Double> getLevelLoad() {
    return levelLoad;
  }

  public static Map<ServerLevel, ServerLevelLoadLevel> getLevelLoadLevel() {
    return levelLoadLevel;
  }

}
