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

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import de.markusbordihn.adaptiveperformancetweaks.Optimization;
import de.markusbordihn.adaptiveperformancetweaks.gamerules.GameRuleManager;
import de.markusbordihn.adaptiveperformancetweaks.world.WorldViewManager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoad;

@EventBusSubscriber
public class PlayerOptimization extends Optimization {

  @SubscribeEvent
  public static void handlePlayerLoggedInEvent(PlayerEvent.PlayerLoggedInEvent event) {
    String username = event.getPlayer().getName().getString();
    if (!username.isEmpty() && Boolean.TRUE.equals(COMMON.optimizePlayerLogin.get())) {
      ServerPlayerEntity player = ServerLifecycleHooks.getCurrentServer().getPlayerList().getPlayerByUsername(username);
      log.info("Optimize Player Login for {} {}", username, player);

      if (PlayerManager.getNumberOfPlayers() == 0
          && (ServerLoad.hasLowServerLoad() || ServerLoad.hasNormalServerLoad())) {
        WorldViewManager.setAvgViewDistance(player.getServerWorld());
      } else {

        // Decrease initial view distance to for a faster login process.
        // This will be increased with the next view distance optimization after 10-20secs.
        if (Boolean.TRUE.equals(COMMON.optimizeViewDistance.get())) {
          WorldViewManager.decreaseViewDistance(player.getServerWorld());
        }

        // Decrease random ticks during the login process
        GameRuleManager.decreaseRandomTickSpeed();

        // Decrease entity cramming during the login process
        GameRuleManager.decreaseMaxEntityCramming();
      }
    }
  }

}
