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

import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Difficulty;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.DifficultyChangeEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.server.ServerLifecycleHooks;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartingEvent;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;
import de.markusbordihn.adaptiveperformancetweakscore.config.CommonConfig;

@EventBusSubscriber
public class ServerManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static double gameDifficultyFactorEasy = COMMON.gameDifficultyFactorEasy.get();
  private static double gameDifficultyFactorNormal = COMMON.gameDifficultyFactorNormal.get();
  private static double gameDifficultyFactorPeaceful = COMMON.gameDifficultyFactorPeaceful.get();
  private static double gameDifficultyFactorHard = COMMON.gameDifficultyFactorHard.get();

  private static short ticks = 0;
  private static final short SERVER_LOAD_TICK = 1 * 20;
  private static final short WORLD_LOAD_TICK = 2 * 20;
  private static final short OPTIMIZATION_TICK = 3 * 20;
  private static final short RESET_TICK = 6 * 20;

  private static MinecraftServer minecraftServer = null;
  private static java.lang.Iterable<ServerLevel> serverLevels = null;
  private static int maxNumberOfPlayers = 0;
  private static int numberOfPlayers = 0;

  private static Difficulty gameDifficulty = Difficulty.NORMAL;
  private static double gameDifficultyFactor = gameDifficultyFactorNormal;

  protected ServerManager() {

  }

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    gameDifficultyFactorEasy = COMMON.gameDifficultyFactorEasy.get();
    gameDifficultyFactorNormal = COMMON.gameDifficultyFactorNormal.get();
    gameDifficultyFactorPeaceful = COMMON.gameDifficultyFactorPeaceful.get();
    gameDifficultyFactorHard = COMMON.gameDifficultyFactorHard.get();
    log.info("{} Game difficult factors EASY: {}, NORMAL: {}, PEACEFUL: {} and HARD: {}",
        Constants.LOG_PREFIX, gameDifficultyFactorEasy, gameDifficultyFactorNormal,
        gameDifficultyFactorPeaceful, gameDifficultyFactorHard);
  }

  @SubscribeEvent
  public static void handleServerStartingEvent(ServerStartingEvent event) {
    maxNumberOfPlayers = getMinecraftServer().getPlayerList().getMaxPlayers();
    numberOfPlayers = getMinecraftServer().getPlayerList().getPlayerCount();
    updateGameDifficulty(getMinecraftServer().getWorldData().getDifficulty());
    log.info("{} Max number of remote players is set to {}", Constants.LOG_PREFIX,
        maxNumberOfPlayers);
  }

  @SubscribeEvent
  @OnlyIn(Dist.CLIENT)
  public static void handleClientServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.START) {
      handleServerTickEvent(Dist.CLIENT);
    } else {
      ticks++;
    }
  }

  @SubscribeEvent
  @OnlyIn(Dist.DEDICATED_SERVER)
  public static void handleDedicatedServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.START) {
      handleServerTickEvent(Dist.DEDICATED_SERVER);
    } else {
      ticks++;
    }
  }

  @SubscribeEvent
  public static void handleDifficultyChangeEvent(DifficultyChangeEvent event) {
    updateGameDifficulty(event.getDifficulty());
  }

  @SubscribeEvent
  public static void handlePlayerLoggedInEvent(PlayerEvent.PlayerLoggedInEvent event) {
    numberOfPlayers = getMinecraftServer().getPlayerList().getPlayerCount();
  }

  @SubscribeEvent
  public static void handlePlayerLoggedOutEvent(PlayerEvent.PlayerLoggedOutEvent event) {
    numberOfPlayers = getMinecraftServer().getPlayerList().getPlayerCount();
  }

  @SubscribeEvent
  public static void handlePlayerRespawnEvent(PlayerEvent.PlayerRespawnEvent event) {
    numberOfPlayers = getMinecraftServer().getPlayerList().getPlayerCount();
  }

  public static void handleServerTickEvent(Dist dist) {
    if (ticks == SERVER_LOAD_TICK) {
      ServerLoad.measureLoadAndPost(dist);
    } else if (ticks == WORLD_LOAD_TICK) {
      ServerLevelLoad.measureLoadAndPost(dist);
    } else if (ticks == OPTIMIZATION_TICK) {
      MinecraftForge.EVENT_BUS.post(new OptimizationEvent(dist));
    } else if (ticks >= RESET_TICK) {
      ticks = 0;
    }
  }

  public static MinecraftServer getMinecraftServer() {
    if (minecraftServer == null) {
      minecraftServer = ServerLifecycleHooks.getCurrentServer();
    }
    return minecraftServer;
  }

  public static float getAverageTickTime() {
    return minecraftServer != null ? minecraftServer.getAverageTickTime() : 50F;
  }

  public static Iterable<ServerLevel> getAllLevels() {
    if (serverLevels == null) {
      serverLevels = getMinecraftServer().getAllLevels();
    }
    return serverLevels;
  }

  public static int getMaxNumberOfPlayers() {
    return maxNumberOfPlayers;
  }

  public static int getNumberOfPlayers() {
    return numberOfPlayers;
  }

  public static Difficulty getGameDifficulty() {
    return gameDifficulty;
  }

  public static double getGameDifficultyFactor() {
    return gameDifficultyFactor;
  }

  private static void updateGameDifficulty(Difficulty difficulty) {
    if (difficulty == gameDifficulty) {
      return;
    }
    gameDifficulty = difficulty;
    switch (difficulty) {
      case EASY:
        gameDifficultyFactor = gameDifficultyFactorEasy;
        break;
      case NORMAL:
        gameDifficultyFactor = gameDifficultyFactorNormal;
        break;
      case PEACEFUL:
        gameDifficultyFactor = gameDifficultyFactorPeaceful;
        break;
      case HARD:
        gameDifficultyFactor = gameDifficultyFactorHard;
        break;
      default:
        gameDifficultyFactor = gameDifficultyFactorNormal;
    }
    log.info("{} Game difficulty is set to {} with a {} factor.", Constants.LOG_PREFIX,
        gameDifficulty, gameDifficultyFactor);
  }

}
