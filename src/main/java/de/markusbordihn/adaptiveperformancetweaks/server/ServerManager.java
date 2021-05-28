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

package de.markusbordihn.adaptiveperformancetweaks.server;

import org.apache.logging.log4j.Logger;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import de.markusbordihn.adaptiveperformancetweaks.Manager;

@EventBusSubscriber
public class ServerManager extends Manager {

  private static short ticks = 0;
  private static final short SERVER_LOAD_TICK = (short) 0.5 * 20;
  private static final short WORLD_LOAD_TICK = 2 * 20;
  private static final short OPTIMIZATION_TICK = 3 * 20;
  private static final short RESET_TICK = 6 * 20;
  private static final Logger log = getLogger(ServerManager.class.getSimpleName());

  @SubscribeEvent
  public static void handleServerStartingEvent(FMLServerStartingEvent event) {
    log.info("Game Difficulty is set to {}",
        ServerLifecycleHooks.getCurrentServer().getWorldData().getDifficulty());
    log.info("Max number of players is set to {}",
        ServerLifecycleHooks.getCurrentServer().getPlayerList().getMaxPlayers());
    ServerLoad.measureLoadAndPost();
    ServerWorldLoad.measureLoadAndPost();
  }

  @SubscribeEvent
  public static void handleServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.END) {
      ticks++;
      return;
    }

    if (ticks == SERVER_LOAD_TICK) {
      ServerLoad.measureLoadAndPost();
    } else if (ticks == WORLD_LOAD_TICK) {
      ServerWorldLoad.measureLoadAndPost();
    } else if (ticks == OPTIMIZATION_TICK) {
      MinecraftForge.EVENT_BUS.post(new OptimizationEvent());
    } else if (ticks >= RESET_TICK) {
      ticks = 0;
    }
  }

}
