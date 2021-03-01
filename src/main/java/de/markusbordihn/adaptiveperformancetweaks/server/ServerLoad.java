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

package de.markusbordihn.adaptiveperformancetweaks.server;

import java.util.Arrays;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import net.minecraft.server.MinecraftServer;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import de.markusbordihn.adaptiveperformancetweaks.Constants;

public class ServerLoad {
  public static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static ServerLoadLevel currentServerLoad = ServerLoadLevel.NORMAL;
  private static ServerLoadLevel lastServerLoad = ServerLoadLevel.NORMAL;
  private static double avgTickTime;

  public enum ServerLoadLevel {
    VERY_LOW, LOW, NORMAL, MEDIUM, HIGH, VERY_HIGH
  }

  public static void measureLoad() {
    MinecraftServer currentServer =  ServerLifecycleHooks.getCurrentServer();
    if (currentServer == null) {
      return;
    }
    long[] tickTimes = currentServer.tickTimeArray;
    if (tickTimes == null) {
      return;
    }
    avgTickTime = Arrays.stream(tickTimes).average().orElse(Double.NaN) / 1000000;
    lastServerLoad = currentServerLoad;
    currentServerLoad = getServerLoadLevelFromTickTime(avgTickTime);
    if (currentServerLoad != lastServerLoad) {
      log.info("Server load changed from {} to {} (avg. {})", lastServerLoad, currentServerLoad,
          avgTickTime);
    }
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

  public static void postServerLoadEvent() {
    MinecraftForge.EVENT_BUS.post(new ServerLoadEvent(currentServerLoad, lastServerLoad));
  }
}
