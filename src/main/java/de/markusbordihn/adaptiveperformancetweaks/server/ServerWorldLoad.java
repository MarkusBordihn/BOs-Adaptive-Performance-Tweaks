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
import java.util.HashMap;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.MinecraftForge;

import de.markusbordihn.adaptiveperformancetweaks.Constants;

public class ServerWorldLoad {
  public static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<ServerWorld, Double> worldLoad = new HashMap<>();
  private static final Map<ServerWorld, ServerWorldLoadLevel> worldLoadLevel = new HashMap<>();
  private static final Map<ServerWorld, ServerWorldLoadLevel> lastWorldLoadLevel = new HashMap<>();

  public enum ServerWorldLoadLevel {
    VERY_LOW, LOW, NORMAL, MEDIUM, HIGH, VERY_HIGH
  }

  public static void measureLoad() {
    java.lang.Iterable<ServerWorld> serverWorlds = ServerManager.getWorlds();
    for (ServerWorld serverWorld : serverWorlds) {
      long[] tickTimes =
          ServerManager.getCurrentServer().getTickTime(serverWorld.getDimensionKey());
      if (tickTimes == null) {
        return;
      }
      double avgTickTime = Arrays.stream(tickTimes).average().orElse(Double.NaN) / 1000000;
      ServerWorldLoadLevel loadLevel = getServerWorldLoadLevelFromTickTime(avgTickTime);
      ServerWorldLoadLevel lastLoadLevel =
          worldLoadLevel.getOrDefault(serverWorld, ServerWorldLoadLevel.NORMAL);
      lastWorldLoadLevel.put(serverWorld, lastLoadLevel);
      worldLoad.put(serverWorld, avgTickTime);
      worldLoadLevel.put(serverWorld, loadLevel);
      if (loadLevel != lastLoadLevel) {
        log.info("World load for {} changed from {} to {} (avg. {})",
            serverWorld.getDimensionKey().getLocation(), lastLoadLevel, loadLevel, avgTickTime);
      }
    }
  }

  public static ServerWorldLoadLevel getServerWorldLoadLevelFromTickTime(double tickTime) {
    if (tickTime <= 15.0) {
      return ServerWorldLoadLevel.VERY_LOW;
    } else if (tickTime <= 35.0) {
      return ServerWorldLoadLevel.LOW;
    } else if (tickTime <= 41.0) {
      return ServerWorldLoadLevel.NORMAL;
    } else if (tickTime <= 46.0) {
      return ServerWorldLoadLevel.MEDIUM;
    } else if (tickTime <= 50.0) {
      return ServerWorldLoadLevel.HIGH;
    } else if (tickTime > 50.0) {
      return ServerWorldLoadLevel.VERY_HIGH;
    }
    return ServerWorldLoadLevel.NORMAL;
  }

  public static Map<ServerWorld, Double> getWorldLoad() {
    return worldLoad;
  }

  public static Map<ServerWorld, ServerWorldLoadLevel> getWorldLoadLevel() {
    return worldLoadLevel;
  }

  public static void postServerWorldLoadEvent() {
    worldLoadLevel.forEach((ServerWorld serverWorld, ServerWorldLoadLevel serverWorldLoadLevel) -> {
      if (serverWorld == null) {
        return;
      }
      ServerWorldLoadLevel lastServerWorldLoadLevel =
          lastWorldLoadLevel.getOrDefault(serverWorld, ServerWorldLoadLevel.NORMAL);
      MinecraftForge.EVENT_BUS.post(
          new ServerWorldLoadEvent(serverWorld, serverWorldLoadLevel, lastServerWorldLoadLevel));
    });
  }

}
