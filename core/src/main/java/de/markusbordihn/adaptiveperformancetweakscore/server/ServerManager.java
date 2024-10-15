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
import java.util.Arrays;
import java.util.Random;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Difficulty;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.DifficultyChangeEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.server.ServerLifecycleHooks;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber
public class ServerManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static final int BASE_TICK = 25;
  private static final int OPTIMIZATION_TICK = 4 * BASE_TICK;
  private static final int RESET_TICK = 6 * BASE_TICK;
  private static final int SERVER_LOAD_TICK = 1 * BASE_TICK;
  private static final int WORLD_LOAD_TICK = 2 * BASE_TICK;
  private static final Random random = new Random();
  private static int ticks = random.nextInt(15);

  private static MinecraftServer minecraftServer = null;
  private static java.lang.Iterable<ServerLevel> serverLevels = null;
  private static int numberOfPlayers = 0;

  private static Difficulty gameDifficulty = Difficulty.NORMAL;
  private static double gameDifficultyFactor = 1;

  protected ServerManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    log.info(
        "{} Game difficult factors EASY: {}, NORMAL: {}, PEACEFUL: {} and HARD: {}",
        Constants.LOG_PREFIX,
        COMMON.gameDifficultyFactorEasy.get(),
        COMMON.gameDifficultyFactorNormal.get(),
        COMMON.gameDifficultyFactorPeaceful.get(),
        COMMON.gameDifficultyFactorHard.get());
  }

  @SubscribeEvent
  public static void handleServerStartingEvent(ServerStartingEvent event) {
    int maxNumberOfPlayers = getMinecraftServer().getPlayerList().getMaxPlayers();
    numberOfPlayers = getMinecraftServer().getPlayerList().getPlayerCount();
    updateGameDifficulty(getMinecraftServer().getWorldData().getDifficulty());
    log.info(
        "{} Max number of remote players is set to {}", Constants.LOG_PREFIX, maxNumberOfPlayers);
  }

  public static void handleServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.START) {
      handleServerTickEvent();
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

  public static void handleServerTickEvent() {
    if (ticks == SERVER_LOAD_TICK) {
      ServerLoad.measureLoadAndPost();
    } else if (ticks == WORLD_LOAD_TICK) {
      ServerLevelLoad.measureLoadAndPost();
    } else if (ticks == OPTIMIZATION_TICK) {
      MinecraftForge.EVENT_BUS.post(new OptimizationEvent());
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

  public static long[] getTickTime(ResourceKey<Level> level) {
    return minecraftServer != null ? minecraftServer.getTickTime(level) : null;
  }

  public static double getAverageTickTime(ServerLevel serverLevel) {
    long[] tickTimes = getTickTime(serverLevel.dimension());
    return (tickTimes != null)
        ? Arrays.stream(tickTimes).average().orElse(Double.NaN) / 1000000
        : 0;
  }

  public static float getAverageTickTime() {
    return minecraftServer != null ? (minecraftServer.getAverageTickTimeNanos() / 1000000f) : 50F;
  }

  public static Iterable<ServerLevel> getAllLevels() {
    if (serverLevels == null) {
      serverLevels = getMinecraftServer().getAllLevels();
    }
    return serverLevels;
  }

  public static int getNumberOfPlayers() {
    return numberOfPlayers;
  }

  public static double getGameDifficultyFactor() {
    return gameDifficultyFactor;
  }

  private static void updateGameDifficulty(Difficulty difficulty) {
    if (difficulty == gameDifficulty) {
      return;
    }
    gameDifficulty = difficulty;
    gameDifficultyFactor =
        switch (difficulty) {
          case EASY -> COMMON.gameDifficultyFactorEasy.get();
          case PEACEFUL -> COMMON.gameDifficultyFactorPeaceful.get();
          case HARD -> COMMON.gameDifficultyFactorHard.get();
          default -> COMMON.gameDifficultyFactorNormal.get();
        };
    log.info(
        "{} Game difficulty is set to {} with a {} factor.",
        Constants.LOG_PREFIX,
        gameDifficulty,
        gameDifficultyFactor);
  }
}
