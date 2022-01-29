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

package de.markusbordihn.adaptiveperformancetweakscore.server;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.server.ServerLifecycleHooks;
import net.minecraftforge.event.server.ServerStartingEvent;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;

@EventBusSubscriber
public class ServerManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static short ticks = 0;
  private static final short SERVER_LOAD_TICK = 1 * 20;
  private static final short WORLD_LOAD_TICK = 2 * 20;
  private static final short OPTIMIZATION_TICK = 3 * 20;
  private static final short RESET_TICK = 6 * 20;

  private static MinecraftServer minecraftServer = null;
  private static java.lang.Iterable<ServerLevel> serverLevels = null;

  protected ServerManager() {

  }

  @SubscribeEvent
  @OnlyIn(Dist.CLIENT)
  public static void handleClientServerStartingEvent(ServerStartingEvent event) {
    log.info("Client Server game difficulty is set to {}",
        getMinecraftServer().getWorldData().getDifficulty());
    log.info("Max number of local players is set to {}",
        getMinecraftServer().getPlayerList().getMaxPlayers());
  }

  @SubscribeEvent
  @OnlyIn(Dist.DEDICATED_SERVER)
  public static void handleDedicatedServerStartingEvent(ServerStartingEvent event) {
    log.info("Dedicated Server game difficulty is set to {}",
        getMinecraftServer().getWorldData().getDifficulty());
    log.info("Max number of remote players is set to {}",
        getMinecraftServer().getPlayerList().getMaxPlayers());
  }

  @SubscribeEvent
  @OnlyIn(Dist.CLIENT)
  public static void handleClientServerTickEvent(TickEvent.ServerTickEvent event) {
    handleServerTickEvent(event, Dist.CLIENT);
  }

  @SubscribeEvent
  @OnlyIn(Dist.DEDICATED_SERVER)
  public static void handleDedicatedServerTickEvent(TickEvent.ServerTickEvent event) {
    handleServerTickEvent(event, Dist.DEDICATED_SERVER);
  }

  public static void handleServerTickEvent(TickEvent.ServerTickEvent event, Dist dist) {
    if (event.phase == TickEvent.Phase.END) {
      ticks++;
      return;
    }
    if (ticks == SERVER_LOAD_TICK) {
      ServerLoad.measureLoadAndPost(dist);
    } else if (ticks == WORLD_LOAD_TICK) {
      ServerLevelLoad.measureLoadAndPost(dist);
    } else if (ticks == OPTIMIZATION_TICK) {
      MinecraftForge.EVENT_BUS.post(new OptimizationEvent(dist));
    } else if (ticks >= RESET_TICK) {
      ticks = 0;
    }
  }

  public static MinecraftServer getMinecraftServer() {
    if (minecraftServer == null) {
      minecraftServer = ServerLifecycleHooks.getCurrentServer();
    }
    return minecraftServer;
  }

  public static float getAverageTickTime() {
    return minecraftServer != null ? minecraftServer.getAverageTickTime() : 50F;
  }

  public static Iterable<ServerLevel> getAllLevels() {
    if (serverLevels == null) {
      serverLevels = getMinecraftServer().getAllLevels();
    }
    return serverLevels;
  }

}
