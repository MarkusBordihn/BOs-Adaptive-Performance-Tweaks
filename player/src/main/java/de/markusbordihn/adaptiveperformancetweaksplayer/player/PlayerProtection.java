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

package de.markusbordihn.adaptiveperformancetweaksplayer.player;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.message.WarnMessages;
import de.markusbordihn.adaptiveperformancetweaksplayer.Constants;
import de.markusbordihn.adaptiveperformancetweaksplayer.config.CommonConfig;
import java.util.ConcurrentModificationException;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.server.ServerLifecycleHooks;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod.EventBusSubscriber
public class PlayerProtection {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static Set<PlayerValidation> playerValidationList = ConcurrentHashMap.newKeySet();
  private static short ticker = 0;

  private static List<String> childPlayerProtectionList = COMMON.childPlayerProtectionList.get();
  private static boolean enableChildPlayerProtection = COMMON.enableChildPlayerProtection.get();
  private static boolean protectPlayerDuringLogin = COMMON.protectPlayerDuringLogin.get();
  private static boolean protectPlayerDuringLoginLogging =
      COMMON.protectPlayerDuringLoginLogging.get();
  private static int playerLoginValidationTimeout = COMMON.playerLoginValidationTimeout.get();
  private static long playerLoginValidationTimeoutMilli =
      TimeUnit.SECONDS.toMillis(playerLoginValidationTimeout);

  protected PlayerProtection() {}

  @SubscribeEvent
  public static void onServerAboutToStartEvent(ServerAboutToStartEvent event) {
    playerValidationList = ConcurrentHashMap.newKeySet();
    childPlayerProtectionList = COMMON.childPlayerProtectionList.get();
    enableChildPlayerProtection = COMMON.enableChildPlayerProtection.get();
    playerLoginValidationTimeout = COMMON.playerLoginValidationTimeout.get();
    protectPlayerDuringLogin = COMMON.protectPlayerDuringLogin.get();
    protectPlayerDuringLoginLogging = COMMON.protectPlayerDuringLoginLogging.get();
    playerLoginValidationTimeoutMilli = TimeUnit.SECONDS.toMillis(playerLoginValidationTimeout);

    // Additional checks for conflicting mods.
    if (CoreConstants.LOGIN_PROTECTION_LOADED) {
      log.warn(
          () ->
              WarnMessages.conflictingFeaturesModWarning(
                  CoreConstants.LOGIN_PROTECTION_NAME, "protects the user during the login"));
    }
  }

  @SubscribeEvent
  public static void handleServerStartingEvent(ServerStartingEvent event) {
    if (protectPlayerDuringLogin) {
      log.info(
          "Player will be protected during login for max. of {} secs.",
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
      ServerPlayer player =
          ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(username);

      // Warn if we are unable to find server player by username.
      if (player == null) {
        log.info("Unable to match username {} to server player!", username);
        return;
      }

      // Player Protection
      if (protectPlayerDuringLoginLogging) {
        log.info(
            "Player {} {} logged in and will be protected for {} secs.",
            username,
            event.getEntity(),
            playerLoginValidationTimeout);
        player.setInvisible(true);
        player.setInvulnerable(true);
        player.heal(1);
      } else {
        log.debug("Player {} {} logged in.", username, event.getEntity());
      }

      // Child Player Protection
      if (enableChildPlayerProtection && childPlayerProtectionList.contains(username)) {
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
            log.info(
                "{} {} was successful validated after {} secs.",
                protectPlayerDuringLoginLogging ? "Protected Player" : "Player",
                username,
                validationTimeInSecs);
            addPlayer(username);
          } else if (playerValidation.getValidationTimeElapsed()
              >= playerLoginValidationTimeoutMilli) {
            log.warn(
                "User validation for {} timed out after {} secs.",
                username,
                playerLoginValidationTimeout);
            addPlayer(username);
          }
        }
      } catch (ConcurrentModificationException error) {
        log.error(
            "Unexpected error during user validation. Please report the following error under {} .\n{}",
            CoreConstants.ISSUE_REPORT,
            error);
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

          // Warn if we are unable to find server player by username.
          if (player == null) {
            log.info("Unable to match username {} to server player!", username);
            return;
          }

          log.debug("Found player {} with player validation {}", player, playerValidation);
          if (protectPlayerDuringLoginLogging
              && (player.isInvisible() || player.isInvulnerable())) {

            if (enableChildPlayerProtection && childPlayerProtectionList.contains(username)) {
              // Handle child player accounts.
              if (player.isInvisible() && Boolean.FALSE.equals(COMMON.childPlayerInvisible.get())) {
                log.info("Removing player protection invisible from child player {}!", username);
                player.setInvisible(false);
              }
              if (player.isInvulnerable()
                  && Boolean.FALSE.equals(COMMON.childPlayerInvulnerable.get())) {
                log.info("Removing player protection invulnerable from child player {}!", username);
                player.setInvulnerable(false);
              }
            } else {
              // Handle normal player accounts.
              if (player.isInvisible()) {
                log.info("Removing player protection invisible from player {}!", username);
                player.setInvisible(false);
              }
              if (player.isInvulnerable()) {
                log.info("Removing player protection invulnerable from player {}!", username);
                player.setInvulnerable(false);
              }
            }
          }
          playerValidationList.remove(playerValidation);
          break;
        }
      }
    } catch (ConcurrentModificationException error) {
      log.error(
          "Unexpected error during adding player. Please report the following error under {} .\n{}",
          CoreConstants.ISSUE_REPORT,
          error);
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
          CoreConstants.ISSUE_REPORT,
          error);
    }
    log.debug("Remove player {}", username);
  }
}
