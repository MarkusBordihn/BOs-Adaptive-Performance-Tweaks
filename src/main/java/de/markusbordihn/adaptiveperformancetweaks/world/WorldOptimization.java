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

package de.markusbordihn.adaptiveperformancetweaks.world;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweaks.Optimization;
import de.markusbordihn.adaptiveperformancetweaks.player.PlayerManager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoad;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerWorldLoadEvent;

@EventBusSubscriber
public class WorldOptimization extends Optimization {

  public static final int VIEW_DISTANCE_EMPTY_WORLD = 3;

  @SubscribeEvent
  public static void handleServerWorldLoadEvent(ServerWorldLoadEvent event) {
    ServerWorld serverWorld = event.getServerWorld();
    java.util.List<ServerPlayerEntity> players = serverWorld.getPlayers();

    if (Boolean.FALSE.equals(COMMON.optimizeViewDistance.get())) {
      return;
    }
    if (players.isEmpty()) {
      WorldViewManager.setViewDistance(serverWorld, VIEW_DISTANCE_EMPTY_WORLD);
      return;
    }
    if (ServerLoad.hasHighServerLoad() && event.hasHighServerWorldLoad()) {
      WorldViewManager.decreaseViewDistance(serverWorld);
    } else if (ServerLoad.hasVeryHighServerLoad() && event.hasNormalServerWorldLoad()) {
      WorldViewManager.decreaseViewDistance(serverWorld);
    } else if (ServerLoad.hasLowServerLoad() && event.hasLowServerWorldLoad()
        && PlayerManager.getNumberOfPlayers() > 0) {
      WorldViewManager.increaseViewDistance(serverWorld);
    }
  }
}
