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
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
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

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.message.WarnMessages;
import de.markusbordihn.adaptiveperformancetweaksplayer.Constants;
import de.markusbordihn.adaptiveperformancetweaksplayer.config.CommonConfig;

@Mod.EventBusSubscriber
public class PlayerProtection {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  private static Set<PlayerValidation> playerValidationList = ConcurrentHashMap.newKeySet();
  private static short ticker = 0;

  protected PlayerProtection() {}

  @SubscribeEvent
  public static void onServerAboutToStartEvent(ServerAboutToStartEvent event) {
    playerValidationList = ConcurrentHashMap.newKeySet();

    // Additional checks for conflicting mods.
    if (CoreConstants.LOGIN_PROTECTION_LOADED) {
      log.warn(() -> WarnMessages.conflictingFeaturesModWarning(CoreConstants.LOGIN_PROTECTION_NAME,
          "protects the user during the login"));
    }
  }

  @SubscribeEvent
  public static void handleServerStartingEvent(ServerStartingEvent event) {
    if (Boolean.TRUE.equals(COMMON.protectPlayerDuringLogin.get())) {
      log.info("Player will be protected during login for max. of {} secs.",
          COMMON.playerLoginValidationTimeout.get());
    }
  }

  @SubscribeEvent
  public static void handlePlayerLoggedInEvent(PlayerEvent.PlayerLoggedInEvent event) {
    if (Boolean.FALSE.equals(COMMON.protectPlayerDuringLogin.get())) {
      return;
    }
    String username = event.getEntity().getName().getString();
    if (!username.isEmpty()) {
      ServerPlayer player =
          ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(username);

      // Player Protection
      if (Boolean.TRUE.equals(COMMON.protectPlayerDuringLoginLogging.get())) {
        log.info("Player {} {} logged in and will be protected for {} secs.", username,
            event.getEntity(), COMMON.playerLoginValidationTimeout.get());
        player.setInvisible(true);
        player.setInvulnerable(true);
        player.heal(1);
      } else {
        log.debug("Player {} {} logged in.", username, event.getEntity());
      }

      // Child Player Protection
      if (Boolean.TRUE.equals(COMMON.enableChildPlayerProtection.get())
          && COMMON.childPlayerProtectionList.get().contains(username)) {
        log.info(
            "Child Player {} logged-in and game settings adjusted for a better player experience.",
            username);
        if (player.experienceLevel < 50) {
          player.setExperienceLevels(++player.experienceLevel);
        }
        player.heal(10);
        player.setInvisible(true);
        player.setInvulnerable(true);
      }

      playerValidationList.add(new PlayerValidation(player));
    }
  }

  @SubscribeEvent
  public static void handlePlayerLoggedOutEvent(PlayerEvent.PlayerLoggedOutEvent event) {
    if (Boolean.FALSE.equals(COMMON.protectPlayerDuringLogin.get())) {
      return;
    }
    String username = event.getEntity().getName().getString();
    if (!username.isEmpty()) {
      log.debug("Player {} logged out.", event.getEntity());
      removePlayer(username);
    }
  }

  @SubscribeEvent
  public static void handleServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.END || ticker++ < 40) {
      return;
    }

    if (!playerValidationList.isEmpty()) {
      try {
        // Check for any un-validated players and try to detect if they logged-in.
        for (PlayerValidation playerValidation : playerValidationList) {
          String username = playerValidation.getUsername();
          if (playerValidation.hasPlayerMoved()) {
            long validationTimeInSecs =
                TimeUnit.MILLISECONDS.toSeconds(playerValidation.getValidationTimeElapsed());
            log.info("{} {} was successful validated after {} secs.",
                Boolean.TRUE.equals(COMMON.protectPlayerDuringLoginLogging.get())
                    ? "Protected Player"
                    : "Player",
                username, validationTimeInSecs);
            addPlayer(username);
          } else if (playerValidation.getValidationTimeElapsed() >= TimeUnit.SECONDS
              .toMillis(COMMON.playerLoginValidationTimeout.get())) {
            log.warn("User validation for {} timed out after {} secs.", username,
                COMMON.playerLoginValidationTimeout.get());
            addPlayer(username);
          }
        }
      } catch (ConcurrentModificationException error) {
        log.error(
            "Unexpected error during user validation. Please report the following error under {} .\n{}",
            CoreConstants.ISSUE_REPORT, error);
      }
    }
    ticker = 0;
  }

  private static void addPlayer(String username) {
    try {
      for (PlayerValidation playerValidation : playerValidationList) {
        if (username.equals(playerValidation.getUsername())) {
          ServerPlayer player =
              ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(username);
          log.debug("Found player {} with player validation {}", player, playerValidation);
          boolean isChildPlayerAccount = (COMMON.enableChildPlayerProtection.get()
              && COMMON.childPlayerProtectionList.get().contains(username));
          if (Boolean.TRUE.equals(COMMON.protectPlayerDuringLoginLogging.get())
              && (player.isInvisible() || player.isInvulnerable())) {
            log.info("Removing player protection from player {}!", username);
            if (player.isInvisible() && !isChildPlayerAccount) {
              player.setInvisible(false);
            }
            if (player.isInvulnerable() && !isChildPlayerAccount) {
              player.setInvulnerable(false);
            }
          }
          playerValidationList.remove(playerValidation);
          break;
        }
      }
    } catch (ConcurrentModificationException error) {
      log.error(
          "Unexpected error during adding player. Please report the following error under {} .\n{}",
          CoreConstants.ISSUE_REPORT, error);
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
          CoreConstants.ISSUE_REPORT, error);
    }
    log.debug("Remove player {}", username);
  }

}
