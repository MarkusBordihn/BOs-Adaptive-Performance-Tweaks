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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.server.level.ServerLevel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ServerLevelLoad {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static long lastUpdateTime = System.currentTimeMillis();
  private static Map<ServerLevel, Double> levelTickTimes = new ConcurrentHashMap<>();
  private static Map<ServerLevel, ServerLoadLevel> levelLoadLevels = new ConcurrentHashMap<>();

  private ServerLevelLoad() {
  }

  public static void reset() {
    lastUpdateTime = System.currentTimeMillis();
    levelTickTimes = new ConcurrentHashMap<>();
    levelLoadLevels = new ConcurrentHashMap<>();
  }

  public static void measureLoadAndPost() {
    long currentTime = System.currentTimeMillis();

    for (ServerLevel serverLevel : ServerManager.getAllLevels()) {
      double currentAvgTickTime = ServerManager.getAverageTickTime(serverLevel);
      if (currentAvgTickTime <= 0) {
        continue;
      }

      double lastTickTime = levelTickTimes.getOrDefault(serverLevel, 45.0);
      if (lastTickTime >= currentAvgTickTime
        && currentTime - lastUpdateTime < (long) CoreConfig.timeBetweenUpdates * 1000L) {
        continue;
      }

      levelTickTimes.put(serverLevel, currentAvgTickTime);

      ServerLoadLevel lastLoadLevel =
        levelLoadLevels.getOrDefault(serverLevel, ServerLoadLevel.NORMAL);
      ServerLoadLevel loadLevel = ServerLoadLevel.fromAverageTickTime(currentAvgTickTime);
      levelLoadLevels.put(serverLevel, loadLevel);

      if (loadLevel != lastLoadLevel && CoreConfig.logServerLoad) {
        String indicator = lastTickTime > currentAvgTickTime ? "↓" : "↑";
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

  public static Map<ServerLevel, ServerLoadLevel> getAllLevelLoads() {
    return levelLoadLevels;
  }
}
