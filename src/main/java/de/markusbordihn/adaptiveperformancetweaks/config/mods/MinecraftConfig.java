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

import java.util.Map;

import net.minecraft.entity.EntityClassification;
import net.minecraft.entity.EntityType;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.registries.ForgeRegistries;

public class MinecraftConfig extends SpawnConfigModSupport {

  public static void addSpawnRates(Map<String, Integer> spawnConfigPerPlayer,
      Map<String, Integer> spawnConfigPerWorld, Map<String, Integer> spawnConfigSpecial) {
    if (Boolean.FALSE.equals(COMMON.minecraftEnabled.get())) {
      return;
    }
    log.info("\u2713 Enable spawn rate control for Minecraft");
    addSpecialSpawnRates(spawnConfigSpecial);
    for (ResourceLocation registryName : ForgeRegistries.ENTITIES.getKeys()) {
      EntityType<?> entityType = ForgeRegistries.ENTITIES.getValue(registryName);
      EntityClassification entityClassification = entityType.getClassification();
      if (entityClassification == EntityClassification.AMBIENT) {
        spawnConfigPerPlayer.put(registryName.toString(),
            COMMON.minecraftMaxPassiveMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(),
            COMMON.minecraftMaxPassiveMobsPerWorld.get());
      } else if (entityClassification == EntityClassification.CREATURE) {
        spawnConfigPerPlayer.put(registryName.toString(),
            COMMON.minecraftMaxNeutralMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(),
            COMMON.minecraftMaxNeutralMobsPerWorld.get());
      } else if (entityClassification == EntityClassification.MONSTER) {
        spawnConfigPerPlayer.put(registryName.toString(),
            COMMON.minecraftMaxHostileMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(),
            COMMON.minecraftMaxHostileMobsPerWorld.get());
      } else if (entityClassification == EntityClassification.WATER_AMBIENT) {
        spawnConfigPerPlayer.put(registryName.toString(),
            COMMON.minecraftMaxWaterPassiveMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(),
            COMMON.minecraftMaxWaterPassiveMobsPerWorld.get());
      } else if (entityClassification == EntityClassification.WATER_CREATURE) {
        spawnConfigPerPlayer.put(registryName.toString(),
            COMMON.minecraftMaxWaterNeutralMobsPerPlayer.get());
        spawnConfigPerWorld.put(registryName.toString(),
            COMMON.minecraftMaxWaterNeutralMobsPerWorld.get());
      }
    }
  }

  public static void addSpecialSpawnRates(Map<String, Integer> spawnConfigSpecial) {
    spawnConfigSpecial.put("minecraft:the_end:minecraft:enderman", 40);
    spawnConfigSpecial.put("minecraft:the_nether:minecraft:piglin", 20);
    spawnConfigSpecial.put("minecraft:the_nether:minecraft:piglin_brute", 20);
    spawnConfigSpecial.put("minecraft:the_nether:minecraft:skeleton", 20);
    spawnConfigSpecial.put("minecraft:the_nether:wither_skeleton", 10);
    spawnConfigSpecial.put("minecraft:the_nether:zombified_piglin", 20);
  }
}
