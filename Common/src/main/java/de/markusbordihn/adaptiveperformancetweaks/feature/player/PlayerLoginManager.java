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

package de.markusbordihn.adaptiveperformancetweaks.feature.player;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class PlayerLoginManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_PLAYERS);
  private static final String LOG_PREFIX = "[Login Protection]";
  private static final short TICK_INTERVAL = 40;

  private static Set<PlayerValidation> playerValidationList = ConcurrentHashMap.newKeySet();
  private static short ticker = 0;

  private PlayerLoginManager() {
  }

  public static void handleServerAboutToStart() {
    playerValidationList = ConcurrentHashMap.newKeySet();
    ticker = 0;
  }

  public static void handleServerStopping() {
    playerValidationList = ConcurrentHashMap.newKeySet();
    ticker = 0;
  }

  public static void handlePlayerLoggedIn(ServerPlayer player) {
    if (!PlayerLoginProtectionConfig.protectPlayerDuringLogin) {
      return;
    }

    String username = player.getName().getString();

    if (PlayerLoginProtectionConfig.protectPlayerDuringLoginLogging) {
      log.info(
        "{} {}: Logged in, protecting for {} secs.",
        LOG_PREFIX,
        username,
        PlayerLoginProtectionConfig.playerLoginValidationTimeout);
    } else {
      log.debug("{} {}: Logged in.", LOG_PREFIX, username);
    }

    player.setInvisible(true);
    player.setInvulnerable(true);
    player.heal(1);

    playerValidationList.add(new PlayerValidation(player));
  }

  public static void handlePlayerLoggedOut(String username) {
    if (!PlayerLoginProtectionConfig.protectPlayerDuringLogin) {
      return;
    }

    log.debug("{} {}: Logged out.", LOG_PREFIX, username);
    playerValidationList.removeIf(v -> username.equals(v.getUsername()));
  }

  public static void handleServerTick() {
    if (playerValidationList.isEmpty() || ticker++ < TICK_INTERVAL) {
      return;
    }

    ticker = 0;

    Iterator<PlayerValidation> iterator = playerValidationList.iterator();
    while (iterator.hasNext()) {
      PlayerValidation validation = iterator.next();
      String username = validation.getUsername();

      boolean timedOut =
        validation.getValidationTimeElapsed()
          >= TimeUnit.SECONDS.toMillis(
          PlayerLoginProtectionConfig.playerLoginValidationTimeout);

      if (validation.hasPlayerMoved() || timedOut) {
        if (timedOut) {
          log.warn(
            "{} {}: Validation timed out after {} secs.",
            LOG_PREFIX,
            username,
            PlayerLoginProtectionConfig.playerLoginValidationTimeout);
        } else {
          log.info(
            "{} {}: Validated after {} secs.",
            LOG_PREFIX,
            username,
            TimeUnit.MILLISECONDS.toSeconds(validation.getValidationTimeElapsed()));
        }
        restorePlayer(username);
        iterator.remove();
      }
    }
  }

  private static void restorePlayer(String username) {
    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null) {
      return;
    }

    ServerPlayer player = server.getPlayerList().getPlayerByName(username);
    if (player == null) {
      log.warn("{} {}: Cannot restore: not found on server.", LOG_PREFIX, username);
      return;
    }

    if (player.isInvisible()) {
      log.debug("{} {}: Remove invisibility", LOG_PREFIX, username);
      player.setInvisible(false);
    }
    if (player.isInvulnerable()) {
      log.debug("{} {}: Remove invulnerability", LOG_PREFIX, username);
      player.setInvulnerable(false);
    }
  }
}
