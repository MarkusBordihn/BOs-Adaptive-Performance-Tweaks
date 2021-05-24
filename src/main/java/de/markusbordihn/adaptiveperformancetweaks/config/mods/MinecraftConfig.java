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

package de.markusbordihn.adaptiveperformancetweaks.config.mods;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.minecraft.entity.EntityClassification;
import net.minecraft.entity.EntityType;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.registries.ForgeRegistries;

import de.markusbordihn.adaptiveperformancetweaks.Constants;

public class MinecraftConfig extends SpawnConfigModSupport {

  private static final String NAME = Constants.MINECRAFT_NAME;
  private static final String MOD_ID = Constants.MINECRAFT_MOD;

  private static Set<String> passiveMobList = new HashSet<>();

  private static Set<String> neutralMobList = new HashSet<>();

  private static Set<String> hostileMobList = new HashSet<>();

  private static Set<String> passiveWaterMobList = new HashSet<>();

  private static Set<String> neutralWaterMobList = new HashSet<>();

  private static Map<String, Integer> specialMobList = new HashMap<>();

  static {
    // Special mob definitions to make sure we have enough enderman in the end ...
    specialMobList.put("minecraft:the_end:minecraft:enderman", 40);
    specialMobList.put("minecraft:the_nether:minecraft:piglin", 20);
    specialMobList.put("minecraft:the_nether:minecraft:piglin_brute", 20);
    specialMobList.put("minecraft:the_nether:minecraft:skeleton", 20);
    specialMobList.put("minecraft:the_nether:minecraft:wither_skeleton", 10);
    specialMobList.put("minecraft:the_nether:minecraft:zombified_piglin", 20);
  }

  public static void addSpawnRates() {
    if (Boolean.FALSE.equals(COMMON.minecraftEnabled.get()) || MOD_ID.isEmpty()) {
      return;
    }

    // Add special spawn rates
    addSpecialSpawnRates(NAME, specialMobList);

    // Get minecraft entities from registry and categorize them according their classification.
    for (ResourceLocation registryName : ForgeRegistries.ENTITIES.getKeys()) {
      if (registryName.toString().startsWith("minecraft:")) {
        EntityType<?> entityType = ForgeRegistries.ENTITIES.getValue(registryName);
        EntityClassification entityClassification = entityType.getCategory();
        if (entityClassification == EntityClassification.AMBIENT) {
          passiveMobList.add(registryName.toString());
        } else if (entityClassification == EntityClassification.CREATURE) {
          neutralMobList.add(registryName.toString());
        } else if (entityClassification == EntityClassification.MONSTER) {
          hostileMobList.add(registryName.toString());
        } else if (entityClassification == EntityClassification.WATER_AMBIENT) {
          passiveWaterMobList.add(registryName.toString());
        } else if (entityClassification == EntityClassification.WATER_CREATURE) {
          neutralWaterMobList.add(registryName.toString());
        }
      }
    }
    addSpawnRatesForPassiveMobs(NAME, passiveMobList, COMMON.minecraftMaxPassiveMobsPerPlayer.get(),
        COMMON.minecraftMaxPassiveMobsPerWorld.get());
    addSpawnRatesForNeutralMobs(NAME, neutralMobList, COMMON.minecraftMaxNeutralMobsPerPlayer.get(),
        COMMON.minecraftMaxNeutralMobsPerWorld.get());
    addSpawnRatesForHostileMobs(NAME, hostileMobList, COMMON.minecraftMaxHostileMobsPerPlayer.get(),
        COMMON.minecraftMaxHostileMobsPerWorld.get());
    addSpawnRatesForPassiveMobs(NAME + " (water)", passiveWaterMobList,
        COMMON.minecraftMaxWaterPassiveMobsPerPlayer.get(),
        COMMON.minecraftMaxWaterPassiveMobsPerWorld.get());
    addSpawnRatesForNeutralMobs(NAME + " (water)", neutralWaterMobList,
        COMMON.minecraftMaxWaterNeutralMobsPerPlayer.get(),
        COMMON.minecraftMaxWaterNeutralMobsPerWorld.get());
  }

}
