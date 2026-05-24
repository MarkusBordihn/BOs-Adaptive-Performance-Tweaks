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

package de.markusbordihn.adaptiveperformancetweaks.core.server;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.server.level.ServerLevel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ServerLevelLoad {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final double SMOOTHING_FACTOR = 0.2d;
  private static final Map<ServerLevel, Long> levelTickStartTimes = new ConcurrentHashMap<>();
  private static final Map<ServerLevel, Double> levelTickTimes = new ConcurrentHashMap<>();
  private static final Map<ServerLevel, Double> levelReportedTickTimes = new ConcurrentHashMap<>();
  private static final Map<ServerLevel, ServerLoadLevel> levelLoadLevels = new ConcurrentHashMap<>();
  private static long lastUpdateTime = System.currentTimeMillis();

  private ServerLevelLoad() {
  }

  public static void reset() {
    lastUpdateTime = System.currentTimeMillis();
    levelTickStartTimes.clear();
    levelTickTimes.clear();
    levelReportedTickTimes.clear();
    levelLoadLevels.clear();
  }

  public static void handleServerLevelTickStart(ServerLevel serverLevel) {
    levelTickStartTimes.put(serverLevel, System.nanoTime());
  }

  public static void handleServerLevelTickEnd(ServerLevel serverLevel) {
    Long startTime = levelTickStartTimes.remove(serverLevel);
    if (startTime == null) {
      return;
    }

    double currentTickTime = (System.nanoTime() - startTime) / 1_000_000.0d;
    levelTickTimes.compute(serverLevel, (level, previousTickTime) -> previousTickTime == null
      ? currentTickTime
      : previousTickTime + (currentTickTime - previousTickTime) * SMOOTHING_FACTOR);
  }

  public static void measureLoadAndPost() {
    measureLoadAndPost(ServerManager.getAllLevels());
  }

  static void measureLoadAndPost(Iterable<ServerLevel> serverLevels) {
    long currentTime = System.currentTimeMillis();
    Set<ServerLevel> activeLevels = new HashSet<>();
    for (ServerLevel serverLevel : serverLevels) {
      activeLevels.add(serverLevel);
    }

    levelTickStartTimes.keySet().retainAll(activeLevels);
    levelTickTimes.keySet().retainAll(activeLevels);
    levelReportedTickTimes.keySet().retainAll(activeLevels);
    levelLoadLevels.keySet().retainAll(activeLevels);

    for (ServerLevel serverLevel : activeLevels) {
      double currentAvgTickTime = levelTickTimes.getOrDefault(serverLevel, 0.0d);
      if (currentAvgTickTime <= 0.0d) {
        continue;
      }

      double lastTickTime = levelReportedTickTimes.getOrDefault(serverLevel, 45.0d);
      if (lastTickTime >= currentAvgTickTime
        && currentTime - lastUpdateTime < (long) CoreConfig.timeBetweenUpdates * 1000L) {
        continue;
      }

      levelReportedTickTimes.put(serverLevel, currentAvgTickTime);

      ServerLoadLevel lastLoadLevel =
        levelLoadLevels.getOrDefault(serverLevel, ServerLoadLevel.NORMAL);
      ServerLoadLevel loadLevel = ServerLoadLevel.fromAverageTickTime(currentAvgTickTime);
      levelLoadLevels.put(serverLevel, loadLevel);

      if (loadLevel != lastLoadLevel && CoreConfig.logServerLoad) {
        String indicator = lastTickTime > currentAvgTickTime ? "down" : "up";
        log.info("{} Level load for {} changed from {} (avg. {}ms) to {} (avg. {}ms)",
          indicator,
          serverLevel.dimension().location(),
          lastLoadLevel, String.format("%.1f", lastTickTime),
          loadLevel, String.format("%.1f", currentAvgTickTime));
      }
    }

    lastUpdateTime = currentTime;
  }

  public static ServerLoadLevel getLevelLoad(ServerLevel serverLevel) {
    return levelLoadLevels.getOrDefault(serverLevel, ServerLoadLevel.NORMAL);
  }

  public static boolean hasMeasuredLoad(ServerLevel serverLevel) {
    return levelTickTimes.containsKey(serverLevel);
  }

  public static double getAverageTickTime(ServerLevel serverLevel) {
    return levelTickTimes.getOrDefault(serverLevel, 0.0d);
  }

  public static Map<ServerLevel, ServerLoadLevel> getAllLevelLoads() {
    return levelLoadLevels;
  }
}
