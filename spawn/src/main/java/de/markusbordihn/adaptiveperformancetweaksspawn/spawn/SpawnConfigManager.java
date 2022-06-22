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

package de.markusbordihn.adaptiveperformancetweaksspawn.spawn;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod;

import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.CommonConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.AlexsMobsSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.AquacultureSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.CustomSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.MekanismAdditionsSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.MinecraftSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.QuarkSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.TinkersConstructConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.UntamedWildsSpawnConfig;

@Mod.EventBusSubscriber
public class SpawnConfigManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  private static final AlexsMobsSpawnConfig.Config ALEXS_MOBS_CONFIG = AlexsMobsSpawnConfig.COMMON;
  private static final AquacultureSpawnConfig.Config AQUACULTURE_CONFIG =
      AquacultureSpawnConfig.COMMON;
  private static final CustomSpawnConfig.Config CUSTOM_CONFIG = CustomSpawnConfig.COMMON;
  private static final MekanismAdditionsSpawnConfig.Config MEKANISM_ADDITIONS_CONFIG =
      MekanismAdditionsSpawnConfig.COMMON;
  private static final MinecraftSpawnConfig.Config MINECRAFT_CONFIG = MinecraftSpawnConfig.COMMON;
  private static final QuarkSpawnConfig.Config QUARK_CONFIG = QuarkSpawnConfig.COMMON;
  private static final TinkersConstructConfig.Config TINKERS_CONSTRUCT_CONFIG =
      TinkersConstructConfig.COMMON;
  private static final UntamedWildsSpawnConfig.Config UNTAMED_WILDS_CONFIG =
      UntamedWildsSpawnConfig.COMMON;

  private static Map<String, Integer> spawnConfigPerPlayer = new HashMap<>();
  private static Map<String, Integer> spawnConfigPerWorld = new HashMap<>();
  private static Map<String, Integer> spawnConfigSpecial = new HashMap<>();

  protected SpawnConfigManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    clearSpawnRates();
    calculateSpawnRates();
  }

  public static void calculateSpawnRates() {

    ModList modList = ModList.get();

    // Minecraft Vanilla Mobs
    if (Boolean.TRUE.equals(MINECRAFT_CONFIG.minecraftEnabled.get())) {
      addSpawnRatesForPassiveMobs(MINECRAFT_CONFIG.minecraftId.get(),
          new HashSet<>(MINECRAFT_CONFIG.minecraftPassiveMobsList.get()),
          MINECRAFT_CONFIG.minecraftMaxPassiveMobsPerPlayer.get(),
          MINECRAFT_CONFIG.minecraftMaxPassiveMobsPerWorld.get());
      addSpawnRatesForNeutralMobs(MINECRAFT_CONFIG.minecraftId.get(),
          new HashSet<>(MINECRAFT_CONFIG.minecraftNeutralMobsList.get()),
          MINECRAFT_CONFIG.minecraftMaxNeutralMobsPerPlayer.get(),
          MINECRAFT_CONFIG.minecraftMaxNeutralMobsPerWorld.get());
      addSpawnRatesForHostileMobs(MINECRAFT_CONFIG.minecraftId.get(),
          new HashSet<>(MINECRAFT_CONFIG.minecraftHostileMobsList.get()),
          MINECRAFT_CONFIG.minecraftMaxHostileMobsPerPlayer.get(),
          MINECRAFT_CONFIG.minecraftMaxHostileMobsPerWorld.get());

      addSpawnRatesForPassiveMobs(MINECRAFT_CONFIG.minecraftId.get(),
          new HashSet<>(MINECRAFT_CONFIG.minecraftWaterPassiveMobsList.get()),
          MINECRAFT_CONFIG.minecraftMaxWaterPassiveMobsPerPlayer.get(),
          MINECRAFT_CONFIG.minecraftMaxWaterPassiveMobsPerWorld.get());
      addSpawnRatesForNeutralMobs(MINECRAFT_CONFIG.minecraftId.get(),
          new HashSet<>(MINECRAFT_CONFIG.minecraftWaterNeutralMobsList.get()),
          MINECRAFT_CONFIG.minecraftMaxWaterNeutralMobsPerPlayer.get(),
          MINECRAFT_CONFIG.minecraftMaxWaterNeutralMobsPerWorld.get());
      addSpawnRatesForHostileMobs(MINECRAFT_CONFIG.minecraftId.get(),
          new HashSet<>(MINECRAFT_CONFIG.minecraftWaterHostileMobsList.get()),
          MINECRAFT_CONFIG.minecraftMaxWaterHostileMobsPerPlayer.get(),
          MINECRAFT_CONFIG.minecraftMaxWaterHostileMobsPerWorld.get());
    }

    // Aquaculture Mobs
    if (Boolean.TRUE.equals(AQUACULTURE_CONFIG.modAquacultureEnabled.get())
        && modList.isLoaded(AQUACULTURE_CONFIG.modAquacultureId.get())) {
      addSpawnRatesForPassiveMobs(AQUACULTURE_CONFIG.modAquacultureId.get(),
          new HashSet<>(AQUACULTURE_CONFIG.modAquacultureFishList.get()),
          AQUACULTURE_CONFIG.modAquacultureMaxFishPerPlayer.get(),
          AQUACULTURE_CONFIG.modAquacultureMaxFishPerWorld.get());
    }

    // Alex's Mobs
    if (Boolean.TRUE.equals(ALEXS_MOBS_CONFIG.alexsMobsEnabled.get())
        && modList.isLoaded(ALEXS_MOBS_CONFIG.alexsMobsId.get())) {
      addSpawnRatesForPassiveMobs(ALEXS_MOBS_CONFIG.alexsMobsId.get(),
          new HashSet<>(ALEXS_MOBS_CONFIG.alexsMobsPassiveMobsList.get()),
          ALEXS_MOBS_CONFIG.alexsMobsMaxPassiveMobsPerPlayer.get(),
          ALEXS_MOBS_CONFIG.alexsMobsMaxPassiveMobsPerWorld.get());
      addSpawnRatesForNeutralMobs(ALEXS_MOBS_CONFIG.alexsMobsId.get(),
          new HashSet<>(ALEXS_MOBS_CONFIG.alexsMobsNeutralMobsList.get()),
          ALEXS_MOBS_CONFIG.alexsMobsMaxNeutralMobsPerPlayer.get(),
          ALEXS_MOBS_CONFIG.alexsMobsMaxNeutralMobsPerWorld.get());
      addSpawnRatesForHostileMobs(ALEXS_MOBS_CONFIG.alexsMobsId.get(),
          new HashSet<>(ALEXS_MOBS_CONFIG.alexsMobsHostileMobsList.get()),
          ALEXS_MOBS_CONFIG.alexsMobsMaxHostileMobsPerPlayer.get(),
          ALEXS_MOBS_CONFIG.alexsMobsMaxHostileMobsPerWorld.get());
    }

    // Mekanism Additions
    if (Boolean.TRUE.equals(MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsEnabled.get())
        && modList.isLoaded(MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsId.get())) {
      addSpawnRatesForPassiveMobs(MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsId.get(),
          new HashSet<>(MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsPassiveMobsList.get()),
          MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsMaxPassiveMobsPerPlayer.get(),
          MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsMaxPassiveMobsPerWorld.get());
      addSpawnRatesForNeutralMobs(MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsId.get(),
          new HashSet<>(MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsNeutralMobsList.get()),
          MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsMaxNeutralMobsPerPlayer.get(),
          MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsMaxNeutralMobsPerWorld.get());
      addSpawnRatesForHostileMobs(MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsId.get(),
          new HashSet<>(MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsHostileMobsList.get()),
          MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsMaxHostileMobsPerPlayer.get(),
          MEKANISM_ADDITIONS_CONFIG.mekanismAdditionsMaxHostileMobsPerWorld.get());
    }

    // Quark
    if (Boolean.TRUE.equals(QUARK_CONFIG.quarkEnabled.get())
        && modList.isLoaded(QUARK_CONFIG.quarkId.get())) {
      addSpawnRatesForPassiveMobs(QUARK_CONFIG.quarkId.get(),
          new HashSet<>(QUARK_CONFIG.quarkPassiveMobsList.get()),
          QUARK_CONFIG.quarkMaxPassiveMobsPerPlayer.get(),
          QUARK_CONFIG.quarkMaxPassiveMobsPerWorld.get());
      addSpawnRatesForNeutralMobs(QUARK_CONFIG.quarkId.get(),
          new HashSet<>(QUARK_CONFIG.quarkNeutralMobsList.get()),
          QUARK_CONFIG.quarkMaxNeutralMobsPerPlayer.get(),
          QUARK_CONFIG.quarkMaxNeutralMobsPerWorld.get());
      addSpawnRatesForHostileMobs(QUARK_CONFIG.quarkId.get(),
          new HashSet<>(QUARK_CONFIG.quarkHostileMobsList.get()),
          QUARK_CONFIG.quarkMaxHostileMobsPerPlayer.get(),
          QUARK_CONFIG.quarkMaxHostileMobsPerWorld.get());
    }

    // Tinkers Construct
    if (Boolean.TRUE.equals(TINKERS_CONSTRUCT_CONFIG.tinkersConstructEnabled.get())
        && modList.isLoaded(TINKERS_CONSTRUCT_CONFIG.tinkersConstructId.get())) {
      addSpawnRatesForHostileMobs(TINKERS_CONSTRUCT_CONFIG.tinkersConstructId.get(),
          new HashSet<>(TINKERS_CONSTRUCT_CONFIG.tinkersConstructHostileMobsList.get()),
          TINKERS_CONSTRUCT_CONFIG.tinkersConstructMaxHostileMobsPerPlayer.get(),
          TINKERS_CONSTRUCT_CONFIG.tinkersConstructMaxHostileMobsPerWorld.get());
    }

    // Untamed Wilds
    if (Boolean.TRUE.equals(UNTAMED_WILDS_CONFIG.untamedWildsEnabled.get())
        && modList.isLoaded(UNTAMED_WILDS_CONFIG.untamedWildsId.get())) {
      addSpawnRatesForPassiveMobs(UNTAMED_WILDS_CONFIG.untamedWildsId.get(),
          new HashSet<>(UNTAMED_WILDS_CONFIG.untamedWildsPassiveMobsList.get()),
          UNTAMED_WILDS_CONFIG.untamedWildsMaxPassiveMobsPerPlayer.get(),
          UNTAMED_WILDS_CONFIG.untamedWildsMaxPassiveMobsPerWorld.get());
      addSpawnRatesForNeutralMobs(UNTAMED_WILDS_CONFIG.untamedWildsId.get(),
          new HashSet<>(UNTAMED_WILDS_CONFIG.untamedWildsNeutralMobsList.get()),
          UNTAMED_WILDS_CONFIG.untamedWildsMaxNeutralMobsPerPlayer.get(),
          UNTAMED_WILDS_CONFIG.untamedWildsMaxNeutralMobsPerWorld.get());
      addSpawnRatesForHostileMobs(UNTAMED_WILDS_CONFIG.untamedWildsId.get(),
          new HashSet<>(UNTAMED_WILDS_CONFIG.untamedWildsHostileMobsList.get()),
          UNTAMED_WILDS_CONFIG.untamedWildsMaxHostileMobsPerPlayer.get(),
          UNTAMED_WILDS_CONFIG.untamedWildsMaxHostileMobsPerWorld.get());
    }

    // Custom Spawn Config overwrites former definitions!
    if (Boolean.TRUE.equals(CUSTOM_CONFIG.customEnabled.get())) {
      addSpawnRatesForPassiveMobs(CUSTOM_CONFIG.customId.get(),
          new HashSet<>(CUSTOM_CONFIG.customPassiveMobsList.get()),
          CUSTOM_CONFIG.customMaxPassiveMobsPerPlayer.get(),
          CUSTOM_CONFIG.customMaxPassiveMobsPerWorld.get());
      addSpawnRatesForNeutralMobs(CUSTOM_CONFIG.customId.get(),
          new HashSet<>(CUSTOM_CONFIG.customNeutralMobsList.get()),
          CUSTOM_CONFIG.customMaxNeutralMobsPerPlayer.get(),
          CUSTOM_CONFIG.customMaxNeutralMobsPerWorld.get());
      addSpawnRatesForHostileMobs(CUSTOM_CONFIG.customId.get(),
          new HashSet<>(CUSTOM_CONFIG.customHostileMobsList.get()),
          CUSTOM_CONFIG.customMaxHostileMobsPerPlayer.get(),
          CUSTOM_CONFIG.customMaxHostileMobsPerWorld.get());
    }

    log.info("Added {} player spawn rules, {} world spawn rules and {} special spawn rules.",
        spawnConfigPerPlayer.size(), spawnConfigPerWorld.size(), spawnConfigSpecial.size());
  }

  public static void clearSpawnRates() {
    log.info("Clearing spawn rates calculation ...");
    spawnConfigPerPlayer.clear();
    spawnConfigPerPlayer.clear();
    spawnConfigPerWorld.clear();
    spawnConfigSpecial.clear();
  }

  public static boolean hasSpawnLimit(String entityName) {
    return spawnConfigPerPlayer.containsKey(entityName)
        || spawnConfigPerWorld.containsKey(entityName);
  }

  public static void addSpawnConfigPerPlayer(String entityName, int maxNumberOfEntities) {
    spawnConfigPerPlayer.put(entityName, maxNumberOfEntities);
  }

  public static void addSpawnConfigPerWorld(String entityName, int maxNumberOfEntities) {
    spawnConfigPerWorld.put(entityName, maxNumberOfEntities);
  }

  public static void addSpawnConfigSpecial(String entityName, int maxNumberOfEntities) {
    spawnConfigSpecial.put(entityName, maxNumberOfEntities);
  }

  public static int getSpawnLimitPerPlayer(String entityName) {
    return spawnConfigPerPlayer.getOrDefault(entityName,
        COMMON.spawnLimitationMaxMobsPerPlayer.get());
  }

  public static int getSpawnLimitPerWorld(String entityName) {
    return spawnConfigPerWorld.getOrDefault(entityName,
        COMMON.spawnLimitationMaxMobsPerWorld.get());
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

  public static void addSpawnRatesForGeneralMobs(String name, Set<String> generalMobList,
      int maxGeneralMobsPerPlayer, int maxGeneralMobsPerWorld) {
    if (generalMobList.isEmpty()) {
      return;
    }
    log.info(
        "\u2713 Enable general mobs spawn rate control for {} and {} mobs with maxPerPlayer:{} and maxPerWorld:{} ...",
        name, generalMobList.size(), maxGeneralMobsPerPlayer, maxGeneralMobsPerWorld);
    addSpawnRatesForGeneralMobs(generalMobList, maxGeneralMobsPerPlayer, maxGeneralMobsPerWorld);
  }

  public static void addSpawnRatesForPassiveMobs(String name, Set<String> passiveMobList,
      int maxPassiveMobsPerPlayer, int maxPassiveMobsPerWorld) {
    if (passiveMobList.isEmpty()) {
      return;
    }
    log.info(
        "\u2713 Enable passive mobs spawn rate control for {} and {} mobs with maxPerPlayer:{} and maxPerWorld:{} ...",
        name, passiveMobList.size(), maxPassiveMobsPerPlayer, maxPassiveMobsPerWorld);
    addSpawnRatesForPassiveMobs(passiveMobList, maxPassiveMobsPerPlayer, maxPassiveMobsPerWorld);
  }

  public static void addSpawnRatesForNeutralMobs(String name, Set<String> neutralMobList,
      int maxNeutralMobsPerPlayer, int maxNeutralMobsPerWorld) {
    if (neutralMobList.isEmpty()) {
      return;
    }
    log.info(
        "\u2713 Enable neutral mobs spawn rate control for {} and {} mobs with maxPerPlayer:{} and maxPerWorld:{} ...",
        name, neutralMobList.size(), maxNeutralMobsPerPlayer, maxNeutralMobsPerWorld);
    addSpawnRatesForNeutralMobs(neutralMobList, maxNeutralMobsPerPlayer, maxNeutralMobsPerWorld);
  }

  public static void addSpawnRatesForHostileMobs(String name, Set<String> hostileMobList,
      int maxHostileMobsPerPlayer, int maxHostileMobsPerWorld) {
    if (hostileMobList.isEmpty()) {
      return;
    }
    log.info(
        "\u2713 Enable hostile mobs spawn rate control for {} and {} mobs with maxPerPlayer:{} and maxPerWorld:{} ...",
        name, hostileMobList.size(), maxHostileMobsPerPlayer, maxHostileMobsPerWorld);
    addSpawnRatesForHostileMobs(hostileMobList, maxHostileMobsPerPlayer, maxHostileMobsPerWorld);
  }

  public static void addSpawnRatesForBossMobs(String name, Set<String> bossMobList,
      int maxBossMobsPerPlayer, int maxBossMobsPerWorld) {
    if (bossMobList.isEmpty()) {
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

  public static void addSpawnRatesForGeneralMobs(Set<String> generalMobList,
      int maxGeneralMobsPerPlayer, int maxGeneralMobsPerWorld) {
    for (String entity : generalMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxGeneralMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxGeneralMobsPerWorld);
    }
  }

  public static void addSpawnRatesForPassiveMobs(Set<String> passiveMobList,
      int maxPassiveMobsPerPlayer, int maxPassiveMobsPerWorld) {
    for (String entity : passiveMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxPassiveMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxPassiveMobsPerWorld);
    }
  }

  public static void addSpawnRatesForNeutralMobs(Set<String> neutralMobList,
      int maxNeutralMobsPerPlayer, int maxNeutralMobsPerWorld) {
    for (String entity : neutralMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxNeutralMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxNeutralMobsPerWorld);
    }
  }

  public static void addSpawnRatesForHostileMobs(Set<String> hostileMobList,
      int maxHostileMobsPerPlayer, int maxHostileMobsPerWorld) {
    for (String entity : hostileMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxHostileMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxHostileMobsPerWorld);
    }
  }

  public static void addSpawnRatesForBossMobs(Set<String> bossMobList, int maxBossMobsPerPlayer,
      int maxBossMobsPerWorld) {
    for (String entity : bossMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxBossMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxBossMobsPerWorld);
    }
  }

  public static void addSpecialSpawnRates(Map<String, Integer> specialMobList) {
    for (Map.Entry<String, Integer> entry : specialMobList.entrySet()) {
      String entity = entry.getKey();
      SpawnConfigManager.addSpawnConfigSpecial(entity, entry.getValue());
    }
  }

  public static void addSpawnRateForMob(String entity, int maxMobsPerPlayer, int maxMobsPerWorld) {
    SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxMobsPerPlayer);
    SpawnConfigManager.addSpawnConfigPerWorld(entity, maxMobsPerWorld);
  }

}
