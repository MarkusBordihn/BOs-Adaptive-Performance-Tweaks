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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.network.play.server.SUpdateViewDistancePacket;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;

@EventBusSubscriber
public class WorldViewManager extends Manager {
  private static Map<String, Integer> viewDistancePerWorld = new ConcurrentHashMap<>();
  private static int viewDistanceMin = COMMON.viewDistanceMin.get();
  private static int viewDistanceMax = COMMON.viewDistanceMax.get();
  private static int viewDistanceDefault = COMMON.viewDistanceDefault.get();

  @SubscribeEvent
  public static void onServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    viewDistanceMin = COMMON.viewDistanceMin.get();
    viewDistanceMax = COMMON.viewDistanceMax.get();
    viewDistanceDefault = COMMON.viewDistanceDefault.get();
  }

  @SubscribeEvent
  public static void onServerStarting(FMLServerStartingEvent event) {
    log.info("ViewDistance will be optimized between {} and {} with {} as default", viewDistanceMin,
        viewDistanceMax, viewDistanceDefault);
  }

  public static void setViewDistance(ServerWorld serverWorld, Integer viewDistance) {
    if (viewDistance > viewDistanceMax) {
      viewDistance = viewDistanceMax;
    } else if (viewDistance < viewDistanceMin) {
      viewDistance = viewDistanceMin;
    }
    String serverWorldName = serverWorld.getLevel().dimension().location().toString();
    Integer currentViewDistance = viewDistancePerWorld.get(serverWorldName);
    if (currentViewDistance != null && currentViewDistance.equals(viewDistance)) {
      return;
    }
    log.info("Changing world view distance for {} from {} to {}", serverWorldName,
        currentViewDistance, viewDistance);
    java.util.List<ServerPlayerEntity> players = serverWorld.players();
    if (!players.isEmpty()) {
      SUpdateViewDistancePacket viewDistancePacket = new SUpdateViewDistancePacket(viewDistance);
      for (ServerPlayerEntity player : players) {
        player.connection.send(viewDistancePacket);
      }
    }
    serverWorld.getChunkSource().setViewDistance(viewDistance);
    viewDistancePerWorld.put(serverWorldName, viewDistance);
  }

  public static void setDefaultViewDistance(ServerWorld serverWorld) {
    setViewDistance(serverWorld, viewDistanceDefault);
  }

  public static void setAvgViewDistance(ServerWorld serverWorld) {
    setViewDistance(serverWorld, (int) Math.round((viewDistanceDefault + viewDistanceMin) / 2.0));
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
    String serverWorldName = serverWorld.getLevel().dimension().location().toString();
    return getViewDistance(serverWorldName);
  }

  public static int getViewDistance(String serverWorldName) {
    return viewDistancePerWorld.getOrDefault(serverWorldName, viewDistanceDefault);
  }
}
