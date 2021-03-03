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

package de.markusbordihn.adaptiveperformancetweaks.config;

import java.util.HashMap;
import java.util.Map;

import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.config.mods.*;

@Mod.EventBusSubscriber
public class SpawnConfigManager extends Manager {

  private static Map<String, Integer> spawnConfigPerPlayer = new HashMap<>();
  private static Map<String, Integer> spawnConfigPerWorld = new HashMap<>();
  private static Map<String, Integer> spawnConfigSpecial = new HashMap<>();
  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    calculateSpawnRates();
  }

  public static void calculateSpawnRates() {
    log.info("Pre-calculate entity spawns rates for Players and World");
    AquacultureConfig.addSpawnRates(spawnConfigPerPlayer, spawnConfigPerWorld);
    IceAndFireConfig.addSpawnRates(spawnConfigPerPlayer, spawnConfigPerWorld);
    MekanismAdditions.addSpawnRates(spawnConfigPerPlayer, spawnConfigPerWorld);
    MinecraftConfig.addSpawnRates(spawnConfigPerPlayer, spawnConfigPerWorld, spawnConfigSpecial);
    QuarkConfig.addSpawnRates(spawnConfigPerPlayer, spawnConfigPerWorld);
    SavageAndRavage.addSpawnRates(spawnConfigPerPlayer, spawnConfigPerWorld);
    TheAbyssConfig.addSpawnRates(spawnConfigPerPlayer, spawnConfigPerWorld);

    log.info("Added {} player spawn rules and {} world spawn rules.", spawnConfigPerPlayer.size(),
        spawnConfigPerWorld.size());
  }

  public static int getSpawnLimitPerPlayer(String entityName) {
    return spawnConfigPerPlayer.getOrDefault(entityName, COMMON.maxEntityPerPlayer.get());
  }

  public static int getSpawnLimitPerWorld(String entityName) {
    return spawnConfigPerWorld.getOrDefault(entityName, COMMON.maxEntityPerWorld.get());
  }

  public static int getSpawnerListSpecial(String entityName) {
    return spawnConfigPerWorld.get(entityName);
  }

  public static Map<String, Integer> getSpawnConfigPerPlayer() {
    return spawnConfigPerPlayer;
  }

  public static Map<String, Integer> getSpawnConfigPerWorld() {
    return spawnConfigPerWorld;
  }

  public static Map<String, Integer> getSpawnConfigSpecial() {
    return spawnConfigSpecial;
  }

}
