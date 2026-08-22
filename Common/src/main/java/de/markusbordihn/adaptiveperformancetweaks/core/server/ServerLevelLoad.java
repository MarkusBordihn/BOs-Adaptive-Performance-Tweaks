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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.minecraft.server.level.ServerLevel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ServerLevelLoad {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final double SMOOTHING_FACTOR = 0.2d;
  private static final Map<ServerLevel, Long> levelTickStartTimes = new HashMap<>();
  private static final Map<ServerLevel, Double> levelTickTimes = new HashMap<>();
  private static final Map<ServerLevel, Double> levelReportedTickTimes = new HashMap<>();
  private static final Map<ServerLevel, ServerLoadLevel> levelLoadLevels = new HashMap<>();
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
    Double previousTickTime = levelTickTimes.get(serverLevel);
    if (previousTickTime == null) {
      levelTickTimes.put(serverLevel, currentTickTime);
      return;
    }

    levelTickTimes.put(serverLevel,
      previousTickTime + (currentTickTime - previousTickTime) * SMOOTHING_FACTOR);
  }

  public static void measureLoadAndPost() {
    measureLoadAndPost(ServerManager.getAllLevels());
  }

  static void measureLoadAndPost(Iterable<ServerLevel> serverLevels) {
    long currentTime = System.currentTimeMillis();
    long updateIntervalMs = (long) CoreConfig.timeBetweenUpdates * 1000L;
    Set<ServerLevel> activeLevels = new HashSet<>();
    for (ServerLevel serverLevel : serverLevels) {
      activeLevels.add(serverLevel);
    }

    if (!levelTickStartTimes.isEmpty() || !levelTickTimes.isEmpty()
      || !levelReportedTickTimes.isEmpty() || !levelLoadLevels.isEmpty()) {
      levelTickStartTimes.keySet().retainAll(activeLevels);
      levelTickTimes.keySet().retainAll(activeLevels);
      levelReportedTickTimes.keySet().retainAll(activeLevels);
      levelLoadLevels.keySet().retainAll(activeLevels);
    }

    for (ServerLevel serverLevel : activeLevels) {
      double currentAvgTickTime = levelTickTimes.getOrDefault(serverLevel, 0.0d);
      if (currentAvgTickTime <= 0.0d) {
        continue;
      }

      double lastTickTime = levelReportedTickTimes.getOrDefault(serverLevel, 45.0d);
      if (lastTickTime >= currentAvgTickTime && currentTime - lastUpdateTime < updateIntervalMs) {
        continue;
      }

      levelReportedTickTimes.put(serverLevel, currentAvgTickTime);

      ServerLoadLevel lastLoadLevel =
        levelLoadLevels.getOrDefault(serverLevel, ServerLoadLevel.NORMAL);
      ServerLoadLevel loadLevel = ServerLoadLevel.fromAverageTickTime(currentAvgTickTime);
      levelLoadLevels.put(serverLevel, loadLevel);

      if (loadLevel != lastLoadLevel && CoreConfig.logServerLevelLoadChanges
        && log.isDebugEnabled()) {
        String indicator = ServerLoad.getLoadChangeIndicator(lastTickTime, currentAvgTickTime);
        log.debug("{} Level load for {} changed from {} (avg. {}ms) to {} (avg. {}ms)",
          indicator,
          serverLevel.dimension().identifier(),
          lastLoadLevel, String.format("%.1f", lastTickTime),
          loadLevel, String.format("%.1f", currentAvgTickTime));
      }
    }

    lastUpdateTime = currentTime;
  }

  public static ServerLoadLevel getLevelLoad(ServerLevel serverLevel) {
    ServerLoadLevel loadLevelOverride = ServerLoad.getLoadLevelOverride();
    if (loadLevelOverride != null) {
      return loadLevelOverride;
    }

    return levelLoadLevels.getOrDefault(serverLevel, ServerLoadLevel.NORMAL);
  }

  public static boolean hasMeasuredLoad(ServerLevel serverLevel) {
    return levelTickTimes.containsKey(serverLevel);
  }

  public static double getAverageTickTime(ServerLevel serverLevel) {
    return levelTickTimes.getOrDefault(serverLevel, 0.0d);
  }

  public static Map<ServerLevel, ServerLoadLevel> getAllLevelLoads() {
    return Collections.unmodifiableMap(levelLoadLevels);
  }

  public static List<LevelLoadSnapshot> getTopLoadedLevels(int limit) {
    if (limit <= 0 || levelTickTimes.isEmpty()) {
      return List.of();
    }

    List<LevelLoadSnapshot> snapshots = new ArrayList<>();
    for (Map.Entry<ServerLevel, Double> entry : levelTickTimes.entrySet()) {
      ServerLevel serverLevel = entry.getKey();
      double averageTickTime = entry.getValue();
      if (serverLevel == null || averageTickTime <= 0.0d) {
        continue;
      }

      snapshots.add(new LevelLoadSnapshot(
        serverLevel.dimension().identifier().toString(),
        ServerLoadLevel.fromAverageTickTime(averageTickTime),
        averageTickTime));
    }

    snapshots.sort(Comparator.comparingDouble(LevelLoadSnapshot::averageTickTime).reversed());
    return snapshots.size() <= limit ? snapshots : snapshots.subList(0, limit);
  }

  public record LevelLoadSnapshot(
    String dimensionId,
    ServerLoadLevel loadLevel,
    double averageTickTime) {

  }
}
