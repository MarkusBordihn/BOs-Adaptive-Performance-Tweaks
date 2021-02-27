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

import net.minecraft.entity.EntityClassification;
import net.minecraft.entity.EntityType;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;
import net.minecraftforge.registries.ForgeRegistries;

import de.markusbordihn.adaptiveperformancetweaks.Manager;

@Mod.EventBusSubscriber
public class SpawnConfigManager extends Manager {

  private static Map<String, Integer> spawnConfigPerPlayer = new HashMap<>();
  private static Map<String, Integer> spawnConfigPerWorld = new HashMap<>();
  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  @SubscribeEvent
  public static void onServerStarting(FMLServerAboutToStartEvent event) {
    calculateSpawnRates();
  }

  public static void calculateSpawnRates() {
    log.info("Pre-calculate entity spawns rates for Players and World");

    // Minecraft
    if (Boolean.TRUE.equals(COMMON.minecraftEnabled.get())) {
      log.info("✓ Enable spawn rate control for Minecraft");
      calculateMinecraftSpawnRates();
    }

    // Aquaculture 2
    if (Boolean.TRUE.equals(COMMON.modAquacultureEnabled.get())
        && ModList.get().isLoaded("aquaculture")) {
      log.info("✓ Enable spawn rate control for Aquaculture");
      calculateAquacultureSpawnRates();
    }

    // Ice and Fire
    if (Boolean.TRUE.equals(COMMON.modIceandfireEnabled.get())
        && ModList.get().isLoaded("iceandfire")) {
      log.info("✓ Enable spawn rate control for Iceandfire");
      calculateIceandfireSpawnRates();
    }

    // Mekanism Additions
    if (Boolean.TRUE.equals(COMMON.modMekanismadditionsEnabled.get())
        && ModList.get().isLoaded("mekanismadditions")) {
      log.info("✓ Enable spawn rate control for Mekanismadditions");
      calculateMekanismadditionsSpawnRates();
    }

    // Quark
    if (Boolean.TRUE.equals(COMMON.modQuarkEnabled.get()) && ModList.get().isLoaded("quark")) {
      log.info("✓ Enable spawn rate control for Quark");
      calculateQuarkSpawnRates();
    }

    // Savage and Ravage
    if (Boolean.TRUE.equals(COMMON.modSavageandravageEnabled.get())
        && ModList.get().isLoaded("savageandravage")) {
      log.info("✓ Enable spawn rate control for Savageandravage");
      calculateSavageandravageSpawnRates();
    }

    // The Abyss
    if (Boolean.TRUE.equals(COMMON.modTheabyssEnabled.get())
        && ModList.get().isLoaded("theabyss")) {
      log.info("✓ Enable spawn rate control for Theabyss");
      calculateTheabyssSpawnRates();
    }

    log.info("Added {} player spawn rules and {} world spawn rules.", spawnConfigPerPlayer.size(),
        spawnConfigPerWorld.size());
  }

  public static void calculateMinecraftSpawnRates() {
    for (ResourceLocation registryName : ForgeRegistries.ENTITIES.getKeys()) {
      EntityType<?> entityType = ForgeRegistries.ENTITIES.getValue(registryName);
      EntityClassification entityClassification =  entityType.getClassification();
      if (entityClassification == EntityClassification.AMBIENT) {
        spawnConfigPerPlayer.put(registryName.toString(), COMMON.minecraftMaxPassiveMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(), COMMON.minecraftMaxPassiveMobsPerWorld.get());
      } else if (entityClassification == EntityClassification.CREATURE) {
        spawnConfigPerPlayer.put(registryName.toString(), COMMON.minecraftMaxNeutralMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(), COMMON.minecraftMaxNeutralMobsPerWorld.get());
      } else if (entityClassification == EntityClassification.MONSTER) {
        spawnConfigPerPlayer.put(registryName.toString(), COMMON.minecraftMaxHostileMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(), COMMON.minecraftMaxHostileMobsPerWorld.get());
      } else if (entityClassification == EntityClassification.WATER_AMBIENT) {
        spawnConfigPerPlayer.put(registryName.toString(), COMMON.minecraftMaxWaterPassiveMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(), COMMON.minecraftMaxWaterPassiveMobsPerWorld.get());
      } else if (entityClassification == EntityClassification.WATER_CREATURE) {
        spawnConfigPerPlayer.put(registryName.toString(), COMMON.minecraftMaxWaterNeutralMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(), COMMON.minecraftMaxWaterNeutralMobsPerWorld.get());
      }
    }
  }

  public static void calculateAquacultureSpawnRates() {
    for (String fish : SpawnConfigModSupport.getAquacultureFishList()) {
      spawnConfigPerPlayer.put(fish, COMMON.modAquacultureMaxFishPerPlayer.get());
      spawnConfigPerWorld.put(fish, COMMON.modAquacultureMaxFishPerWorld.get());
    }
  }

  public static void calculateIceandfireSpawnRates() {
    for (String entity : SpawnConfigModSupport.getIceandfirePassiveMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modIceandfireMaxPassiveMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modIceandfireMaxPassiveMobsPerWorld.get());
    }
    for (String entity : SpawnConfigModSupport.getIceandfireNeutralMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modIceandfireMaxNeutralMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modIceandfireMaxNeutralMobsPerWorld.get());
    }
    for (String entity : SpawnConfigModSupport.getIceandfireHostileMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modIceandfireMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modIceandfireMaxHostileMobsPerWorld.get());
    }
    for (String entity : SpawnConfigModSupport.getIceandfireBossList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modIceandfireMaxBossesPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modIceandfireMaxBossesPerWorld.get());
    }
  }

  public static void calculateMekanismadditionsSpawnRates() {
    for (String entity : SpawnConfigModSupport.getMekanismadditionsHostileMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modMekanismadditionsMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modMekanismadditionsMaxHostileMobsPerWorld.get());
    }
  }

  public static void calculateQuarkSpawnRates() {
    for (String entity : SpawnConfigModSupport.getQuarkPassiveMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modQuarkMaxPassiveMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modQuarkMaxPassiveMobsPerWorld.get());
    }
    for (String entity : SpawnConfigModSupport.getQuarkNeutralMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modQuarkMaxNeutralMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modQuarkMaxNeutralMobsPerWorld.get());
    }
    for (String entity : SpawnConfigModSupport.getQuarkHostileMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modQuarkMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modQuarkMaxHostileMobsPerWorld.get());
    }
  }

  public static void calculateSavageandravageSpawnRates() {
    for (String entity : SpawnConfigModSupport.getSavageandravageHostileMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modSavageandravageMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modSavageandravageMaxHostileMobsPerWorld.get());
    }
  }

  public static void calculateTheabyssSpawnRates() {
    for (String entity : SpawnConfigModSupport.getTheabyssPassiveMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modTheabyssMaxPassiveMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modTheabyssMaxPassiveMobsPerWorld.get());
    }
    for (String entity : SpawnConfigModSupport.getIceandfireHostileMobList()) {
      spawnConfigPerPlayer.put(entity, COMMON.modTheabyssMaxHostileMobsPerPlayer.get());
      spawnConfigPerWorld.put(entity, COMMON.modTheabyssMaxHostileMobsPerWorld.get());
    }
  }

  public static int getSpawnLimitPerPlayer(String entityName) {
    return spawnConfigPerPlayer.getOrDefault(entityName, COMMON.maxEntityPerPlayer.get());
  }

  public static int getSpawnLimitPerWorld(String entityName) {
    return spawnConfigPerWorld.getOrDefault(entityName, COMMON.maxEntityPerWorld.get());
  }

}
