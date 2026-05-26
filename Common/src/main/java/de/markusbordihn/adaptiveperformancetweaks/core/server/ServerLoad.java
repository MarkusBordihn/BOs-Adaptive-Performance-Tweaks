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
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ServerLoad {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static long lastUpdateTime = System.currentTimeMillis();
  private static long lastLogTime = 0L;
  private static ServerLoadLevel currentServerLoad = ServerLoadLevel.NORMAL;
  private static ServerLoadLevel lastServerLoad = ServerLoadLevel.NORMAL;
  private static double avgTickTime = 50.0;
  private static double lastAvgTickTime = 45.0;

  private ServerLoad() {
  }

  public static void reset() {
    lastUpdateTime = System.currentTimeMillis();
    lastLogTime = 0L;
    currentServerLoad = ServerLoadLevel.NORMAL;
    lastServerLoad = ServerLoadLevel.NORMAL;
    avgTickTime = 50.0;
    lastAvgTickTime = 45.0;
  }

  public static void measureLoadAndPost() {
    double currentAvgTickTime = ServerManager.getAverageTickTime();
    long currentTime = System.currentTimeMillis();
    if (lastAvgTickTime >= currentAvgTickTime
      && currentTime - lastUpdateTime < (long) CoreConfig.timeBetweenUpdates * 1000L) {
      return;
    }

    lastAvgTickTime = avgTickTime;
    avgTickTime = currentAvgTickTime;
    lastServerLoad = currentServerLoad;
    currentServerLoad = ServerLoadLevel.fromAverageTickTime(avgTickTime);

    if (shouldLogServerLoadChange(currentServerLoad, lastServerLoad, currentTime, lastLogTime)) {
      String indicator = getLoadChangeIndicator(lastAvgTickTime, avgTickTime);
      log.info("{} Server load changed from {} (avg. {}ms) to {} (avg. {}ms)",
        indicator,
        lastServerLoad, String.format("%.1f", lastAvgTickTime),
        currentServerLoad, String.format("%.1f", avgTickTime));
      logTopLoadedLevelsDebug();
      lastLogTime = currentTime;
    }

    ServerLoadDispatcher.dispatch(
      new ServerLoadEvent(currentServerLoad, lastServerLoad, avgTickTime, lastAvgTickTime));
    lastUpdateTime = currentTime;
  }

  public static ServerLoadLevel getCurrentServerLoad() {
    return currentServerLoad;
  }

  public static double getAvgTickTime() {
    return avgTickTime;
  }

  static String getLoadChangeIndicator(double previousAvgTickTime, double currentAvgTickTime) {
    return previousAvgTickTime > currentAvgTickTime ? "↓" : "↑";
  }

  static boolean shouldLogServerLoadChange(
    ServerLoadLevel currentLoad, ServerLoadLevel previousLoad, long currentTime,
    long previousLogTime) {
    if (!CoreConfig.logServerLoad || currentLoad == previousLoad) {
      return false;
    }

    return isSignificantLoadChange(currentLoad, previousLoad)
      || currentTime - previousLogTime >= (long) CoreConfig.serverLoadLogIntervalSeconds * 1000L;
  }

  static boolean isSignificantLoadChange(
    ServerLoadLevel currentLoad, ServerLoadLevel previousLoad) {
    return Math.abs(currentLoad.ordinal() - previousLoad.ordinal())
      >= CoreConfig.serverLoadLogSignificantChangeSteps;
  }

  private static void logTopLoadedLevelsDebug() {
    if (!log.isDebugEnabled()) {
      return;
    }

    String topLoadedLevelsSummary = formatTopLoadedLevelsSummary();
    if (!topLoadedLevelsSummary.isBlank()) {
      log.debug("{}", topLoadedLevelsSummary);
    }
  }

  private static String formatTopLoadedLevelsSummary() {
    List<ServerLevelLoad.LevelLoadSnapshot> topLoadedLevels =
      ServerLevelLoad.getTopLoadedLevels(CoreConfig.serverLoadLogTopWorldCount);
    if (topLoadedLevels.isEmpty()) {
      return "";
    }

    StringBuilder summary = new StringBuilder("Top loaded worlds: ");
    for (int index = 0; index < topLoadedLevels.size(); index++) {
      ServerLevelLoad.LevelLoadSnapshot snapshot = topLoadedLevels.get(index);
      if (index > 0) {
        summary.append(", ");
      }
      summary.append(snapshot.dimensionId())
        .append('=')
        .append(String.format("%.1fms", snapshot.averageTickTime()))
        .append(" (")
        .append(snapshot.loadLevel())
        .append(')');
    }
    return summary.toString();
  }
}
