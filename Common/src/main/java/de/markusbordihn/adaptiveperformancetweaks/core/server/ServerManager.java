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
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import java.util.List;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ServerManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final int BASE_TICK = 25;
  private static final int SERVER_LOAD_TICK = BASE_TICK;
  private static final int WORLD_LOAD_TICK = 2 * BASE_TICK;
  private static final int PLAYER_COUNT_TICK = 4 * BASE_TICK;
  private static final int RESET_TICK = 6 * BASE_TICK;

  private static MinecraftServer minecraftServer;
  private static int numberOfPlayers;
  private static int ticks;

  private ServerManager() {
  }

  public static void handleServerAboutToStart(MinecraftServer server) {
    log.info("Server starting ...");
    minecraftServer = server;
    numberOfPlayers = 0;
    ticks = 0;
    ServerLoad.reset();
    ServerLevelLoad.reset();
    PlayerPositionManager.reset();
    CoreEntityManager.reset();
  }

  public static void handleServerStarting(MinecraftServer server) {
    minecraftServer = server;
    numberOfPlayers = server.getPlayerList().getPlayerCount();
    log.debug("Max number of remote players is set to {}", server.getPlayerList().getMaxPlayers());
  }

  public static void handleServerStopping(MinecraftServer server) {
    log.info("Server stopping ...");
    minecraftServer = null;
    numberOfPlayers = 0;
    ticks = 0;
  }

  public static void handleServerTick() {
    if (ticks == SERVER_LOAD_TICK) {
      ServerLoad.measureLoadAndPost();
    } else if (ticks == WORLD_LOAD_TICK) {
      ServerLevelLoad.measureLoadAndPost();
    } else if (ticks == PLAYER_COUNT_TICK && minecraftServer != null) {
      numberOfPlayers = minecraftServer.getPlayerList().getPlayerCount();
    }
    CoreEntityManager.handleServerTick();
    PlayerPositionManager.handleServerTick();
    if (++ticks >= RESET_TICK) {
      ticks = 0;
    }
  }

  public static void handlePlayerCountChange() {
    if (minecraftServer != null) {
      numberOfPlayers = minecraftServer.getPlayerList().getPlayerCount();
    }
  }

  public static MinecraftServer getMinecraftServer() {
    return minecraftServer;
  }

  public static float getAverageTickTime() {
    return minecraftServer != null ? minecraftServer.getAverageTickTime() : 50f;
  }

  public static double getAverageTickTime(ServerLevel serverLevel) {
    return getAverageTickTime();
  }

  public static Iterable<ServerLevel> getAllLevels() {
    return minecraftServer != null ? minecraftServer.getAllLevels() : List.of();
  }

  public static int getNumberOfPlayers() {
    return numberOfPlayers;
  }
}
