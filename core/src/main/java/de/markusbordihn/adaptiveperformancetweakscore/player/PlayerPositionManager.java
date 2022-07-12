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

package de.markusbordihn.adaptiveperformancetweakscore.player;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.players.PlayerList;

import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.server.ServerLifecycleHooks;
import net.minecraftforge.event.server.ServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;
import de.markusbordihn.adaptiveperformancetweakscore.dimension.DimensionManager;

@Mod.EventBusSubscriber
public class PlayerPositionManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static Map<String, PlayerPosition> playerPositionMap = new ConcurrentHashMap<>();
  private static int ticks = 0;
  private static int numberOfPlayers = 0;

  protected PlayerPositionManager() {}

  @SubscribeEvent
  public static void onServerAboutToStartEvent(ServerAboutToStartEvent event) {
    playerPositionMap = new ConcurrentHashMap<>();
  }

  @SubscribeEvent
  public static void handlePlayerLoggedOutEvent(PlayerEvent.PlayerLoggedOutEvent event) {
    String playerUUID = event.getEntity().getStringUUID();
    if (playerUUID != null) {
      log.debug("Removing player {} from position tracking.", event.getEntity());
      playerPositionMap.remove(playerUUID);
    }
  }

  @SubscribeEvent
  public static void handleServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.END) {
      ticks++;
      return;
    }
    if (ticks >= 50) {
      MinecraftServer minecraftServer = ServerLifecycleHooks.getCurrentServer();
      if (minecraftServer != null) {
        PlayerList playerList = minecraftServer.getPlayerList();
        if (playerList != null) {
          numberOfPlayers = playerList.getPlayerCount();
          if (numberOfPlayers == 0) {
            if (!playerPositionMap.isEmpty()) {
              playerPositionMap = new ConcurrentHashMap<>();
            }
          } else {
            List<ServerPlayer> serverPlayerList = playerList.getPlayers();
            int viewDistance = playerList.getViewDistance();
            int simulationDistance = playerList.getSimulationDistance();
            for (ServerPlayer player : serverPlayerList) {
              if (player.isAlive() && !player.hasDisconnected()) {
                updatePlayerPosition(player, viewDistance, simulationDistance);
              }
            }
          }
        }
      }
      ticks = 0;
    }
  }

  public static int getNumberOfPlayers() {
    return numberOfPlayers;
  }

  public static List<PlayerPosition> getPlayerPositionsInsideViewArea(String world, int x, int y,
      int z) {
    List<PlayerPosition> playerPositions = new ArrayList<>();
    for (PlayerPosition playerPosition : playerPositionMap.values()) {
      if (playerPosition.isInsidePlayerViewArea(world, x, y, z)) {
        playerPositions.add(playerPosition);
      }
    }
    return playerPositions;
  }

  public static List<PlayerPosition> getPlayerPositions(String world) {
    List<PlayerPosition> playerPositions = new ArrayList<>();
    for (PlayerPosition playerPosition : playerPositionMap.values()) {
      if (playerPosition.isInsidePlayerViewArea(world)) {
        playerPositions.add(playerPosition);
      }
    }
    return playerPositions;
  }

  public static ConcurrentMap<String, List<PlayerPosition>> getPlayerPositions() {
    ConcurrentHashMap<String, List<PlayerPosition>> playerPositions = new ConcurrentHashMap<>();
    List<String> dimensionList = DimensionManager.getDimensionList();
    for (String dimension : dimensionList) {
      List<PlayerPosition> playerPositionsForDimension = getPlayerPositions(dimension);
      if (!playerPositionsForDimension.isEmpty()) {
        playerPositions.put(dimension, playerPositionsForDimension);
      }
    }
    return playerPositions;
  }

  public static boolean isInsidePlayersViewArea(String world, int x, int y, int z) {
    for (PlayerPosition playerPosition : playerPositionMap.values()) {
      if (playerPosition.isInsidePlayerViewArea(world, x, y, z)) {
        return true;
      }
    }
    return false;
  }

  public static Map<String, PlayerPosition> getPlayerPositionMap() {
    return playerPositionMap;
  }

  private static void updatePlayerPosition(ServerPlayer player, int viewDistance,
      int simulationDistance) {
    PlayerPosition playerPosition = playerPositionMap.computeIfAbsent(player.getStringUUID(),
        key -> new PlayerPosition(player, viewDistance, simulationDistance));
    playerPosition.update(player, viewDistance, simulationDistance);
  }
}
