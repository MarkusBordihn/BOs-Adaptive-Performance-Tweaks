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

package de.markusbordihn.adaptiveperformancetweaks.core.player;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.players.PlayerList;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class PlayerPositionManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final int DEFAULT_PLAYER_MOVEMENT_UPDATE_TICK = 20;
  private static final int PLAYER_POSITION_UPDATE_TICK = 60;

  private static Map<String, PlayerPosition> playerPositionMap = new ConcurrentHashMap<>();
  private static int playerMovementUpdateTick = DEFAULT_PLAYER_MOVEMENT_UPDATE_TICK;
  private static int playerMovementWindowSamples = 5;
  private static int ticks = 0;
  private static int nextMovementUpdateTick = DEFAULT_PLAYER_MOVEMENT_UPDATE_TICK;
  private static int nextFullUpdateTick = PLAYER_POSITION_UPDATE_TICK;

  private PlayerPositionManager() {
  }

  public static void configureMovementTracking(int updateTick, int movementWindowSamples) {
    playerMovementUpdateTick = Math.max(1, updateTick);
    playerMovementWindowSamples = Math.max(1, movementWindowSamples);
    nextMovementUpdateTick = ticks + playerMovementUpdateTick;
  }

  public static void reset() {
    playerPositionMap = new ConcurrentHashMap<>();
    playerMovementUpdateTick = DEFAULT_PLAYER_MOVEMENT_UPDATE_TICK;
    playerMovementWindowSamples = 5;
    ticks = 0;
    nextMovementUpdateTick = DEFAULT_PLAYER_MOVEMENT_UPDATE_TICK;
    nextFullUpdateTick = PLAYER_POSITION_UPDATE_TICK;
  }

  public static void handleServerTick() {
    int currentTick = ++ticks;
    boolean movementUpdate = currentTick >= nextMovementUpdateTick;
    boolean fullUpdate = currentTick >= nextFullUpdateTick;
    if (!movementUpdate && !fullUpdate) {
      return;
    }

    if (movementUpdate) {
      nextMovementUpdateTick = currentTick + playerMovementUpdateTick;
    }
    if (fullUpdate) {
      nextFullUpdateTick = currentTick + PLAYER_POSITION_UPDATE_TICK;
    }

    updatePlayerPositions(currentTick, movementUpdate, fullUpdate);
  }

  public static int getCurrentServerTick() {
    return ticks;
  }

  public static int getMovementUpdateTick() {
    return playerMovementUpdateTick;
  }

  public static void handlePlayerLoggedIn(ServerPlayer player) {
    trackPlayer(player);
  }

  public static void handlePlayerTeleported(ServerPlayer player) {
    trackPlayer(player);
  }

  private static void trackPlayer(ServerPlayer player) {
    if (player == null) {
      return;
    }

    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null) {
      return;
    }

    PlayerList playerList = minecraftServer.getPlayerList();
    if (playerList == null) {
      return;
    }

    updatePlayerPosition(player, playerList.getViewDistance(), playerList.getSimulationDistance(),
      ticks, true, true);
  }

  public static void handlePlayerLoggedOut(String playerUUID) {
    if (playerUUID != null) {
      log.debug("[Player Position] Removing player {} from position tracking.", playerUUID);
      playerPositionMap.remove(playerUUID);
    }
  }

  public static Map<String, PlayerPosition> getPlayerPositionMap() {
    return playerPositionMap;
  }

  private static void updatePlayerPositions(int currentTick, boolean movementUpdate,
    boolean fullUpdate) {
    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null) {
      return;
    }

    PlayerList playerList = minecraftServer.getPlayerList();
    if (playerList == null || playerList.getPlayerCount() == 0) {
      playerPositionMap.clear();
      return;
    }

    int viewDistance = playerList.getViewDistance();
    int simulationDistance = playerList.getSimulationDistance();
    for (ServerPlayer player : playerList.getPlayers()) {
      if (player.isAlive() && !player.hasDisconnected()) {
        updatePlayerPosition(player, viewDistance, simulationDistance, currentTick,
          movementUpdate, fullUpdate);
      }
    }
  }

  private static void updatePlayerPosition(
    ServerPlayer player, int viewDistance, int simulationDistance, int currentTick,
    boolean movementUpdate, boolean fullUpdate) {
    PlayerPosition playerPosition =
      playerPositionMap.computeIfAbsent(
        player.getStringUUID(),
        key -> new PlayerPosition(player, viewDistance, simulationDistance));
    String levelName = player.level().dimension().location().toString();
    if (movementUpdate) {
      playerPosition.updateMovement(player, levelName, currentTick, playerMovementWindowSamples,
        playerMovementUpdateTick);
    }

    if (fullUpdate && playerPosition.updateViewArea(
      player, levelName, viewDistance, simulationDistance)) {
      log.debug("[Player Position] Updated position for {} to {}", player.getName().getString(),
        playerPosition);
    }
  }
}
