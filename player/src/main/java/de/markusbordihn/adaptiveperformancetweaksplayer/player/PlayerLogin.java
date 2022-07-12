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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.level.ServerPlayer;

import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.server.ServerLifecycleHooks;

import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLoad;
import de.markusbordihn.adaptiveperformancetweaksplayer.Constants;
import de.markusbordihn.adaptiveperformancetweaksplayer.config.CommonConfig;

@EventBusSubscriber
public class PlayerLogin {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  protected PlayerLogin() {}

  @SubscribeEvent
  public static void handlePlayerLoggedInEvent(PlayerEvent.PlayerLoggedInEvent event) {
    String username = event.getEntity().getName().getString();

    // Do nothing if player login optimization is disabled or user has no name.
    if (Boolean.TRUE.equals(!COMMON.optimizePlayerLogin.get()) || username.isEmpty()) {
      return;
    }

    // Skip optimization if there is no other player on the server and the server load is low.
    int numOfPlayers = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerCount();
    if (numOfPlayers == 0 && (ServerLoad.hasLowServerLoad())) {
      return;
    }

    // Optimize Player login based on the available options.
    ServerPlayer player =
        ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByName(username);

    log.debug("Optimize Player Login for {} {} and {} players", username, player, numOfPlayers);
  }
}
