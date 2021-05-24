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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.config.mods.*;

@Mod.EventBusSubscriber
public class SpawnConfigManager extends Manager {

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  public static Map<String, Integer> spawnConfigPerPlayer = new HashMap<>();
  public static Map<String, Integer> spawnConfigPerWorld = new HashMap<>();
  public static Map<String, Integer> spawnConfigSpecial = new HashMap<>();
  public static Set<String> spawnConfigEntity = new HashSet<>();

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    log.info("Optimize Passive Mobs: {}", Boolean.TRUE.equals(COMMON.optimizePassiveMobs.get()));
    log.info("Optimize Neutral Mobs: {}", Boolean.TRUE.equals(COMMON.optimizeNeutralMobs.get()));
    log.info("Optimize Hostile Mobs: {}", Boolean.TRUE.equals(COMMON.optimizeHostileMobs.get()));
    log.info("Optimize Boss Mobs: {}", Boolean.TRUE.equals(COMMON.optimizeBossMobs.get()));
    calculateSpawnRates();
  }

  public static void calculateSpawnRates() {
    log.info("Pre-calculate entity spawns rates for Players and World");
    AlexMobsConfig.addSpawnRates();
    AquacultureConfig.addSpawnRates();
    ArtifactsConfig.addSpawnRates();
    DungeonsmodConfig.addSpawnRates();
    IceAndFireConfig.addSpawnRates();
    MekanismAdditions.addSpawnRates();
    MinecraftConfig.addSpawnRates();
    MowziesMobsConfig.addSpawnRates();
    MutantBeastsConfig.addSpawnRates();
    QuarkConfig.addSpawnRates();
    RatsConfig.addSpawnRates();
    SavageAndRavageConfig.addSpawnRates();
    StatuesConfig.addSpawnRates();
    SupplementariesConfig.addSpawnRates();
    TheAbyssConfig.addSpawnRates();
    TinkersConstructConfig.addSpawnRates();

    log.info("Added {} player spawn rules, {} world spawn rules and {} special spawn rules.",
        spawnConfigPerPlayer.size(), spawnConfigPerWorld.size(), spawnConfigSpecial.size());
  }

  public static int getSpawnLimitPerPlayer(String entityName) {
    return spawnConfigPerPlayer.get(entityName);
  }

  public static int getSpawnLimitPerWorld(String entityName) {
    return spawnConfigPerWorld.get(entityName);
  }

  public static int getSpawnerListSpecial(String entityName) {
    return spawnConfigPerWorld.get(entityName);
  }

  public static boolean hasSpawnLimit(String entityName) {
    return spawnConfigEntity.contains(entityName);
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

  public static Set<String> getSpawnConfigEntity() {
    return spawnConfigEntity;
  }

}
