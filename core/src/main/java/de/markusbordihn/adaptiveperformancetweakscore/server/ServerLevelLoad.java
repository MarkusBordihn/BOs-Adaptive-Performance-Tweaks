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

import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.server.ServerLifecycleHooks;
import net.minecraftforge.event.server.ServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.config.CommonConfig;

@EventBusSubscriber
public class ServerLevelLoad {

  private static final Logger log = LogManager.getLogger(CoreConstants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static boolean logServerLoad = COMMON.logServerLoad.get();
  private static Map<ServerLevel, Double> levelLoad = new ConcurrentHashMap<>();
  private static Map<ServerLevel, ServerLevelLoadLevel> lastLevelLoadLevel = new ConcurrentHashMap<>();
  private static Map<ServerLevel, ServerLevelLoadLevel> levelLoadLevel = new ConcurrentHashMap<>();

  public enum ServerLevelLoadLevel {
    VERY_LOW, LOW, NORMAL, MEDIUM, HIGH, VERY_HIGH
  }

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    levelLoad = new ConcurrentHashMap<>();
    lastLevelLoadLevel = new ConcurrentHashMap<>();
    levelLoadLevel = new ConcurrentHashMap<>();
    logServerLoad = COMMON.logServerLoad.get();
  }

  public static void measureLoadAndPost() {
    MinecraftServer minecraftServer = ServerLifecycleHooks.getCurrentServer();
    java.lang.Iterable<ServerLevel> serverLevels = minecraftServer.getAllLevels();
    for (ServerLevel ServerLevel : serverLevels) {
      long[] tickTimes = minecraftServer.getTickTime(ServerLevel.dimension());
      if (tickTimes == null) {
        return;
      }
      double avgTickTime = Arrays.stream(tickTimes).average().orElse(Double.NaN) / 1000000;
      ServerLevelLoadLevel loadLevel = getServerLevelLoadLevelFromTickTime(avgTickTime);
      ServerLevelLoadLevel lastLoadLevel =
          levelLoadLevel.getOrDefault(ServerLevel, ServerLevelLoadLevel.NORMAL);
      lastLevelLoadLevel.put(ServerLevel, lastLoadLevel);
      levelLoad.put(ServerLevel, avgTickTime);
      levelLoadLevel.put(ServerLevel, loadLevel);

      if (loadLevel != lastLoadLevel && logServerLoad) {
        log.info("Level load for {} changed from {} to {} (avg. {})",
            ServerLevel.dimension().location(), lastLoadLevel, loadLevel, avgTickTime);
      }
      MinecraftForge.EVENT_BUS
          .post(new ServerLevelLoadEvent(ServerLevel, loadLevel, lastLoadLevel));
    }
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
