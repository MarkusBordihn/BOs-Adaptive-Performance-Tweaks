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

import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import de.markusbordihn.adaptiveperformancetweaks.Manager;

@EventBusSubscriber
public class WorldManager extends Manager {

  private static Map<String, ServerWorld> worlds = new HashMap<>();

  @SubscribeEvent
  public static void handleServerStartingEvent(FMLServerStartingEvent event) {
    java.lang.Iterable<ServerWorld> serverWorlds =
        ServerLifecycleHooks.getCurrentServer().getAllLevels();
    for (ServerWorld serverWorld : serverWorlds) {
      String worldName = serverWorld.getLevel().dimension().location().toString();
      worlds.put(worldName, serverWorld);
    }
  }

  public static ServerWorld getWorldByName(String worldName) {
    return worlds.get(worldName);
  }

}
