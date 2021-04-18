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
import java.util.Set;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.config.CommonConfig;
import de.markusbordihn.adaptiveperformancetweaks.config.SpawnConfigManager;

public abstract class SpawnConfigModSupport {

  protected SpawnConfigModSupport() {}

  public static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  public static void addSpawnRatesForPassiveMobs(String name, Set<String> passiveMobList,
      int maxPassiveMobsPerPlayer, int maxPassiveMobsPerWorld) {
    if (Boolean.FALSE.equals(COMMON.optimizePassiveMobs.get()) || passiveMobList.isEmpty()) {
      return;
    }
    log.info(
        "\u2713 Enable passive mobs spawn rate control for {} and {} mobs with maxPerPlayer:{} and maxPerWorld:{} ...",
        name, passiveMobList.size(), maxPassiveMobsPerPlayer, maxPassiveMobsPerWorld);
    addSpawnRatesForPassiveMobs(passiveMobList, maxPassiveMobsPerPlayer, maxPassiveMobsPerWorld);
  }

  public static void addSpawnRatesForNeutralMobs(String name, Set<String> neutralMobList,
      int maxNeutralMobsPerPlayer, int maxNeutralMobsPerWorld) {
    if (Boolean.FALSE.equals(COMMON.optimizeNeutralMobs.get()) || neutralMobList.isEmpty()) {
      return;
    }
    log.info(
        "\u2713 Enable neutral mobs spawn rate control for {} and {} mobs with maxPerPlayer:{} and maxPerWorld:{} ...",
        name, neutralMobList.size(), maxNeutralMobsPerPlayer, maxNeutralMobsPerWorld);
    addSpawnRatesForNeutralMobs(neutralMobList, maxNeutralMobsPerPlayer, maxNeutralMobsPerWorld);
  }

  public static void addSpawnRatesForHostileMobs(String name, Set<String> hostileMobList,
      int maxHostileMobsPerPlayer, int maxHostileMobsPerWorld) {
    if (Boolean.FALSE.equals(COMMON.optimizeHostileMobs.get()) || hostileMobList.isEmpty()) {
      return;
    }
    log.info(
        "\u2713 Enable hostile mobs spawn rate control for {} and {} mobs with maxPerPlayer:{} and maxPerWorld:{} ...",
        name, hostileMobList.size(), maxHostileMobsPerPlayer, maxHostileMobsPerWorld);
    addSpawnRatesForHostileMobs(hostileMobList, maxHostileMobsPerPlayer, maxHostileMobsPerWorld);
  }

  public static void addSpawnRatesForBossMobs(String name, Set<String> bossMobList,
      int maxBossMobsPerPlayer, int maxBossMobsPerWorld) {
    if (Boolean.FALSE.equals(COMMON.optimizeBossMobs.get()) || bossMobList.isEmpty()) {
      return;
    }
    log.info(
        "\u2713 Enable boss mobs spawn rate control for {} and {} mobs with maxPerPlayer:{} and maxPerWorld:{} ...",
        name, bossMobList.size(), maxBossMobsPerPlayer, maxBossMobsPerWorld);
    addSpawnRatesForBossMobs(bossMobList, maxBossMobsPerPlayer, maxBossMobsPerWorld);
  }

  public static void addSpecialSpawnRates(String name, Map<String, Integer> specialMobList) {
    if (specialMobList.isEmpty()) {
      return;
    }
    log.info("\u2713 Enable special mobs spawn rate control for {} and {} mobs with: {}", name,
        specialMobList.size(), specialMobList);
    addSpecialSpawnRates(specialMobList);
  }

  public static void addSpawnRatesForPassiveMobs(Set<String> passiveMobList,
      int maxPassiveMobsPerPlayer, int maxPassiveMobsPerWorld) {
    for (String entity : passiveMobList) {
      SpawnConfigManager.spawnConfigPerPlayer.put(entity, maxPassiveMobsPerPlayer);
      SpawnConfigManager.spawnConfigPerWorld.put(entity, maxPassiveMobsPerWorld);
    }
  }

  public static void addSpawnRatesForNeutralMobs(Set<String> neutralMobList,
      int maxNeutralMobsPerPlayer, int maxNeutralMobsPerWorld) {
    for (String entity : neutralMobList) {
      SpawnConfigManager.spawnConfigPerPlayer.put(entity, maxNeutralMobsPerPlayer);
      SpawnConfigManager.spawnConfigPerWorld.put(entity, maxNeutralMobsPerWorld);
    }
  }

  public static void addSpawnRatesForHostileMobs(Set<String> hostileMobList,
      int maxHostileMobsPerPlayer, int maxHostileMobsPerWorld) {
    for (String entity : hostileMobList) {
      SpawnConfigManager.spawnConfigPerPlayer.put(entity, maxHostileMobsPerPlayer);
      SpawnConfigManager.spawnConfigPerWorld.put(entity, maxHostileMobsPerWorld);
    }
  }

  public static void addSpawnRatesForBossMobs(Set<String> bossMobList, int maxBossMobsPerPlayer,
      int maxBossMobsPerWorld) {
    for (String entity : bossMobList) {
      SpawnConfigManager.spawnConfigPerPlayer.put(entity, maxBossMobsPerPlayer);
      SpawnConfigManager.spawnConfigPerWorld.put(entity, maxBossMobsPerWorld);
    }
  }

  public static void addSpecialSpawnRates(Map<String, Integer> specialMobList) {
    for (Map.Entry<String, Integer> entry : specialMobList.entrySet()) {
      SpawnConfigManager.spawnConfigSpecial.put(entry.getKey(), entry.getValue());
    }
  }

  public static void addSpawnRateForMob(String entity, int maxMobsPerPlayer, int maxMobsPerWorld) {
    SpawnConfigManager.spawnConfigPerPlayer.put(entity, maxMobsPerPlayer);
    SpawnConfigManager.spawnConfigPerWorld.put(entity, maxMobsPerWorld);
  }

}
