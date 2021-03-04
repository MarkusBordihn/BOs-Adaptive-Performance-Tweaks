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

package de.markusbordihn.adaptiveperformancetweaks.player;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerManager;

@Mod.EventBusSubscriber
public class PlayerManager extends Manager {
  private static Map<String, ServerPlayerEntity> usernamePlayerMap = new HashMap<>();
  private static Set<ServerPlayerEntity> playerList = new HashSet<>();
  private static Set<PlayerValidation> playerValidationList = new HashSet<>();
  private static boolean hasPlayers = false;
  private static int playerCount = 0;
  private static short ticks = 0;
  private static int validationTimeout = 60;

  @SubscribeEvent
  public static void handleServerStartingEvent(FMLServerStartingEvent event) {
    for (ServerPlayerEntity player : ServerManager.getPlayers()) {
      playerValidationList.add(new PlayerValidation(player));
    }
  }

  @SubscribeEvent
  public static void handlePlayerLoggedInEvent(PlayerEvent.PlayerLoggedInEvent event) {
    String username = event.getPlayer().getName().getString();
    if (!username.isEmpty()) {
      log.info("Player {} {} logged in.", username, event.getEntity());
      ServerPlayerEntity player = ServerManager.getPlayerByUsername(username);

      // Add protection during login process and give 1 heal
      if (Boolean.TRUE.equals(COMMON.optimizePlayerLogin.get())) {
        player.setInvisible(true);
        player.setInvulnerable(true);
        player.heal(1);
      }
      playerValidationList.add(new PlayerValidation(player));
    }
  }

  @SubscribeEvent
  public static void handlePlayerLoggedOutEvent(PlayerEvent.PlayerLoggedOutEvent event) {
    String username = event.getPlayer().getName().getString();
    if (!username.isEmpty()) {
      log.info("Player {} logged out.", event.getEntity());
      removePlayer(username);
    }
  }

  @SubscribeEvent
  public static void handleServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.END) {
      ticks++;
      return;
    }

    if (ticks == 80) {
      for (PlayerValidation playerValidation : playerValidationList) {
        String username = playerValidation.getUsername();
        if (playerValidation.hasPlayerMoved()) {
          log.info("User {} was successful validated ...", username);
          addPlayer(username);
        } else if (playerValidation.getValidationTimeElapsed() / 1000 >= validationTimeout) {
          log.warn("User validation for {} timed out after {} sec", username, validationTimeout);
          addPlayer(username);
        }
      }
      ticks = 0;
    }
  }

  private static void addPlayer(String username) {
    ServerPlayerEntity player = ServerManager.getPlayerByUsername(username);
    usernamePlayerMap.put(username, player);
    for (PlayerValidation playerValidation : playerValidationList) {
      if (username.equals(playerValidation.getUsername())) {

        // Remove protection from login process
        if (Boolean.TRUE.equals(COMMON.optimizePlayerLogin.get())) {
          if (player.isInvisible()) {
            player.setInvisible(false);
          }
          if (player.isInvulnerable()) {
            player.setInvulnerable(false);
          }
        }
        playerValidationList.remove(playerValidation);
      }
    }
    playerList.add(player);
    playerCount = ServerManager.getCurrentServer().getCurrentPlayerCount();
    hasPlayers = true;
    log.info("Added {} to PlayerMap: {}", username, usernamePlayerMap);
  }

  private static void removePlayer(String username) {
    usernamePlayerMap.remove(username);
    for (PlayerValidation playerValidation : playerValidationList) {
      if (username.equals(playerValidation.getUsername())) {
        playerValidationList.remove(playerValidation);
      }
    }
    ServerPlayerEntity player = ServerManager.getPlayerByUsername(username);
    if (playerList.contains(player)) {
      playerList.remove(player);
    }
    playerCount--;
    hasPlayers = playerCount > 0;
    log.info("Remove {} from PlayerMap: {}", username, usernamePlayerMap);
  }


  public static Set<ServerPlayerEntity> getPlayers() {
    return playerList;
  }

  public static Set<PlayerValidation> getPlayerValidationList() {
    return playerValidationList;
  }

  public static String getUserName(ServerPlayerEntity player) {
    return player.getName().getString();
  }

  public static boolean hasPlayers() {
    return hasPlayers;
  }

  public static int getNumberOfPlayers() {
    return playerCount;
  }

}
