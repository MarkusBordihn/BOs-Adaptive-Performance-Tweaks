/*
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

import de.markusbordihn.adaptiveperformancetweakscore.Constants;
import de.markusbordihn.adaptiveperformancetweakscore.config.CommonConfig;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.server.level.ServerLevel;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber
public class ServerLevelLoad {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  private static int timeBetweenUpdates = 10 * 1000;
  private static long lastUpdateTime = System.currentTimeMillis();

  private static Map<ServerLevel, Double> levelLoad = new ConcurrentHashMap<>();
  private static Map<ServerLevel, ServerLevelLoadLevel> levelLoadLevel = new ConcurrentHashMap<>();
  private static Map<String, ServerLevelLoadLevel> levelNameLoadLevel = new ConcurrentHashMap<>();

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    levelLoad = new ConcurrentHashMap<>();
    levelLoadLevel = new ConcurrentHashMap<>();
    levelNameLoadLevel = new ConcurrentHashMap<>();
    timeBetweenUpdates = COMMON.timeBetweenUpdates.get() * 1000;
  }

  public static void measureLoadAndPost(Dist dist) {
    // Precalculate of specific values.
    long currentTime = System.currentTimeMillis();

    for (ServerLevel serverLevel : ServerManager.getAllLevels()) {
      // Get average tick times.
      double avgTickTime = ServerManager.getAverageTickTime(serverLevel);

      if (avgTickTime <= 0) {
        continue;
      }

      // Restrict and smoother high to low server load updates
      double lastAvgTickTime = levelLoad.getOrDefault(serverLevel, 45.0);
      if (lastAvgTickTime >= avgTickTime && currentTime - lastUpdateTime < timeBetweenUpdates) {
        continue;
      }

      // Cache avg. tick time
      levelLoad.put(serverLevel, avgTickTime);

      // Cache former load level and calculate current load level.
      String serverLevelName = serverLevel.dimension().location().toString();
      ServerLevelLoadLevel lastLoadLevel =
          levelLoadLevel.getOrDefault(serverLevel, ServerLevelLoadLevel.NORMAL);
      ServerLevelLoadLevel loadLevel = getServerLevelLoadLevelFromTickTime(avgTickTime);
      levelLoadLevel.put(serverLevel, loadLevel);
      levelNameLoadLevel.put(serverLevelName, loadLevel);

      // Report change to server log, if enabled.
      if (loadLevel != lastLoadLevel && Boolean.TRUE.equals(COMMON.logServerLevelLoad.get())) {
        String loadIndicator = lastAvgTickTime > avgTickTime ? "↓" : "↑";
        log.info(
            "{} {} Level load for {} changed from {} (avg. {}) to {} (avg. {})",
            Constants.LOG_PREFIX,
            loadIndicator,
            serverLevelName,
            lastLoadLevel,
            lastAvgTickTime,
            loadLevel,
            avgTickTime);
      }

      // Post result to the event bus.
      MinecraftForge.EVENT_BUS.post(
          new ServerLevelLoadEvent(
              serverLevel, loadLevel, lastLoadLevel, avgTickTime, lastAvgTickTime, dist));
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

  public static Map<String, ServerLevelLoadLevel> getLevelNameLoadLevel() {
    return levelNameLoadLevel;
  }

  public static boolean hasHighLevelLoad(ServerLevelLoadLevel serverLevelLoadLevel) {
    if (serverLevelLoadLevel == null) {
      return false;
    }
    return switch (serverLevelLoadLevel) {
      case MEDIUM, HIGH, VERY_HIGH -> true;
      default -> false;
    };
  }

  public enum ServerLevelLoadLevel {
    VERY_LOW,
    LOW,
    NORMAL,
    MEDIUM,
    HIGH,
    VERY_HIGH
  }
}
