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

package de.markusbordihn.adaptiveperformancetweaksplayer.player;

import java.util.ConcurrentModificationException;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.level.ServerPlayer;

import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.server.ServerLifecycleHooks;

import de.markusbordihn.adaptiveperformancetweaksplayer.Constants;
import de.markusbordihn.adaptiveperformancetweaksplayer.config.CommonConfig;

@Mod.EventBusSubscriber
public class PlayerProtection {

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static Set<PlayerValidation> playerValidationList = new HashSet<>();
  private static short ticker = 0;

  private static boolean protectPlayerDuringLogin = COMMON.protectPlayerDuringLogin.get();
  private static boolean protectPlayerDuringLoginLogging =
      COMMON.protectPlayerDuringLoginLogging.get();
  private static int playerLoginValidationTimeout = COMMON.playerLoginValidationTimeout.get();

  protected PlayerProtection() {}

  @SubscribeEvent
  public static void onServerAboutToStartEvent(ServerAboutToStartEvent event) {
    protectPlayerDuringLogin = COMMON.protectPlayerDuringLogin.get();
    protectPlayerDuringLoginLogging = COMMON.protectPlayerDuringLoginLogging.get();
    playerLoginValidationTimeout = COMMON.playerLoginValidationTimeout.get();
  }

  @SubscribeEvent
  public static void handleServerStartingEvent(ServerStartingEvent event) {
    if (protectPlayerDuringLogin) {
      log.info("Player will be protected during login for max. of {} secs.",
          playerLoginValidationTimeout);
    }
  }

  @SubscribeEvent
  public static void handlePlayerLoggedInEvent(PlayerEvent.PlayerLoggedInEvent event) {
    if (!protectPlayerDuringLogin) {
      return;
    }
    String username = event.getPlayer().getName().getString();
    if (!username.isEmpty()) {
      if (protectPlayerDuringLoginLogging) {
        log.info("Player {} {} logged in and will be protected for {} secs.", username,
            event.getEntity(), playerLoginValidationTimeout);
      } else {
        log.debug("Player {} {} logged in.", username, event.getEntity());
      }
      ServerPlayer player =
          ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(username);

      // Protect player during login
      player.setInvisible(true);
      player.setInvulnerable(true);
      player.heal(1);

      playerValidationList.add(new PlayerValidation(player));
    }
  }

  @SubscribeEvent
  public static void handlePlayerLoggedOutEvent(PlayerEvent.PlayerLoggedOutEvent event) {
    if (!protectPlayerDuringLogin) {
      return;
    }
    String username = event.getPlayer().getName().getString();
    if (!username.isEmpty()) {
      log.debug("Player {} logged out.", event.getEntity());
      removePlayer(username);
    }
  }

  @SubscribeEvent
  public static void handleServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.END) {
      return;
    }

    if (ticker++ == 40) {
      if (!playerValidationList.isEmpty()) {
        // Check for any un-validated players and try to detect if they logged-in.
        for (PlayerValidation playerValidation : playerValidationList) {
          String username = playerValidation.getUsername();
          if (playerValidation.hasPlayerMoved()) {
            long validationTimeInSecs =
                TimeUnit.MILLISECONDS.toSeconds(playerValidation.getValidationTimeElapsed());
            if (protectPlayerDuringLoginLogging) {
              log.info("Player {} was successful validated after {} secs.", username,
                  validationTimeInSecs);
            } else {
              log.debug("User {} was successful validated after {} secs.", username,
                  validationTimeInSecs);
            }
            addPlayer(username);
          } else if (playerValidation.getValidationTimeElapsed()
              / 1000 >= playerLoginValidationTimeout) {
            log.warn("User validation for {} timed out after {} secs.", username,
                playerLoginValidationTimeout);
            addPlayer(username);
          }
        }
      }
      ticker = 0;
    }
  }

  private static void addPlayer(String username) {
    ServerPlayer player =
        ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(username);
    try {
      for (PlayerValidation playerValidation : playerValidationList) {
        if (username.equals(playerValidation.getUsername())) {
          log.debug("Found player {} with player validation {}", player, playerValidation);
          // Remove protection from login process
          if (player.isInvisible()) {
            player.setInvisible(false);
          }
          if (player.isInvulnerable()) {
            player.setInvulnerable(false);
          }
          playerValidationList.remove(playerValidation);
          break;
        }
      }
    } catch (ConcurrentModificationException error) {
      log.error(
          "Unexpected error during adding player. Please report the following error under {} .\n{}",
          Constants.ISSUE_REPORT, error);
    }
    log.debug("Added player {}", username);
  }

  private static void removePlayer(String username) {
    try {
      for (PlayerValidation playerValidation : playerValidationList) {
        if (username.equals(playerValidation.getUsername())) {
          playerValidationList.remove(playerValidation);
          break;
        }
      }
    } catch (ConcurrentModificationException error) {
      log.error(
          "Unexpected error during removing player. Please report the following error under {} .\n{}",
          Constants.ISSUE_REPORT, error);
    }
    log.debug("Remove player {}", username);
  }

}
