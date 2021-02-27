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

import java.util.HashMap;
import java.util.Map;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.network.play.server.SUpdateViewDistancePacket;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerManager;

@Mod.EventBusSubscriber
public class WorldViewManager extends Manager {
  private static Map<String, Integer> viewDistancePerWorld = new HashMap<>();
  private static final int MIN_VIEW_DISTANCE = 2;
  private static final int MAX_VIEW_DISTANCE = 16;
  private static int defaultViewDistance = 10;

  @SubscribeEvent
  public static void onServerStarting(FMLServerStartingEvent event) {
    defaultViewDistance = ServerManager.getPlayerList().getViewDistance();
    log.info("Default view distance is {}", defaultViewDistance);
  }

  public static void setViewDistance(ServerWorld serverWorld, Integer viewDistance) {
    if (viewDistance > MAX_VIEW_DISTANCE) {
      viewDistance = MAX_VIEW_DISTANCE;
    } else if (viewDistance < MIN_VIEW_DISTANCE) {
      viewDistance = MIN_VIEW_DISTANCE;
    }
    String serverWorldName = serverWorld.getDimensionKey().getLocation().toString();
    Integer currentViewDistance = viewDistancePerWorld.get(serverWorldName);
    if (currentViewDistance != null && currentViewDistance.equals(viewDistance)) {
      return;
    }
    log.info("Changing world view distance for {} from {} to {}", serverWorldName,
        currentViewDistance, viewDistance);
    java.util.List<ServerPlayerEntity> players = serverWorld.getPlayers();
    if (!players.isEmpty()) {
      SUpdateViewDistancePacket viewDistancePacket = new SUpdateViewDistancePacket(viewDistance);
      for (ServerPlayerEntity player : players) {
        player.connection.sendPacket(viewDistancePacket);
      }
    }
    serverWorld.getChunkProvider().setViewDistance(viewDistance);
    viewDistancePerWorld.put(serverWorldName, viewDistance);
  }

  public static void increaseViewDistance(ServerWorld serverWorld) {
    Integer viewDistance = getViewDistance(serverWorld);
    setViewDistance(serverWorld, viewDistance + 1);
  }

  public static void decreaseViewDistance(ServerWorld serverWorld) {
    Integer viewDistance = getViewDistance(serverWorld);
    setViewDistance(serverWorld, viewDistance - 1);
  }

  public static int getViewDistance(ServerWorld serverWorld) {
    String serverWorldName = serverWorld.getDimensionKey().getLocation().toString();
    return getViewDistance(serverWorldName);
  }

  public static int getViewDistance(String serverWorldName) {
    return viewDistancePerWorld.getOrDefault(serverWorldName, defaultViewDistance);
  }
}
