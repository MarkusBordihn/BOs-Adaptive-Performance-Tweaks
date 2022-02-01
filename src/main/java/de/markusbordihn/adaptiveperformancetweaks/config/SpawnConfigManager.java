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

import org.apache.logging.log4j.Logger;

import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.config.mods.*;

@Mod.EventBusSubscriber
public class SpawnConfigManager extends Manager {

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  private static Map<String, Integer> spawnConfigPerPlayer = new HashMap<>();
  private static Map<String, Integer> spawnConfigPerWorld = new HashMap<>();
  private static Map<String, Integer> spawnConfigSpecial = new HashMap<>();
  private static Set<String> spawnConfigEntity = new HashSet<>();
  private static final Logger log = getLogger(SpawnConfigManager.class.getSimpleName());

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    checkForBadCombinationOfMods();
    clearSpawnRates();
    calculateSpawnRates();
  }

  @SubscribeEvent
  public static void handleCommonConfigReloadEvent(CommonConfigReloadEvent event) {
    clearSpawnRates();
    calculateSpawnRates();
  }

  public static void checkForBadCombinationOfMods() {
    if (ModList.get().isLoaded(Constants.PERFORMANT_MOD)) {
      log.error(getCoreModWarning(Constants.PERFORMANT_NAME));
    }

    if (ModList.get().isLoaded(Constants.LAZYDFU_MOD)) {
      log.warn(getCoreModWarning(Constants.LAZYDFU_NAME));
    }

    if (ModList.get().isLoaded(Constants.SODIUM_MOD)) {
      log.warn(getCoreModWarning(Constants.SODIUM_NAME));
    }

    if (ModList.get().isLoaded(Constants.DYNVIEW_MOD)) {
      log.error(
          "Dynamic View optimizing the view distance in a similar way like this mod. Don't use both optimizations together!");
    }

    if (ModList.get().isLoaded(Constants.INCONTROL_MOD)) {
      log.error(
          "InControl controls the mob spawns and entity spawns, which could conflict with this spawn control of this mod. Don't use both optimizations together!");
    }

    if (ModList.get().isLoaded(Constants.RATS_MOD)) {
      log.warn(
          "There are known issue with the rats mod and spawn control, please not report any issue!");
    }
  }

  public static String getCoreModWarning(String modName) {
    return String.format(
        "The mod %s use core modifications which could conflicting with this none-core mod. Do not report any issues with both mods enabled.",
        modName);
  }

  public static void calculateSpawnRates() {
    if (Boolean.FALSE.equals(COMMON.optimizePassiveMobs.get())
        && Boolean.FALSE.equals(COMMON.optimizeNeutralMobs.get())
        && Boolean.FALSE.equals(COMMON.optimizeHostileMobs.get())
        && Boolean.FALSE.equals(COMMON.optimizeBossMobs.get())) {
      log.info("Disable entity spawn rates for Players and World!");
      return;
    }
    log.info("Enable pre-calculate entity spawn rates for Players and World");
    log.info("Optimize Passive Mobs: {}", Boolean.TRUE.equals(COMMON.optimizePassiveMobs.get()));
    log.info("Optimize Neutral Mobs: {}", Boolean.TRUE.equals(COMMON.optimizeNeutralMobs.get()));
    log.info("Optimize Hostile Mobs: {}", Boolean.TRUE.equals(COMMON.optimizeHostileMobs.get()));
    log.info("Optimize Boss Mobs: {}", Boolean.TRUE.equals(COMMON.optimizeBossMobs.get()));

    // Add pre-defined spawn rates for minecraft, if enabled.
    MinecraftConfig.addSpawnRates();

    // Add pre-defined spawn rates for supported Mods, if installed and enabled.
    AlexMobsConfig.addSpawnRates();
    AquacultureConfig.addSpawnRates();
    ArtifactsConfig.addSpawnRates();
    DungeonsmodConfig.addSpawnRates();
    IceAndFireConfig.addSpawnRates();
    MekanismAdditions.addSpawnRates();
    MowziesMobsConfig.addSpawnRates();
    MutantBeastsConfig.addSpawnRates();
    QuarkConfig.addSpawnRates();
    RatsConfig.addSpawnRates();
    SavageAndRavageConfig.addSpawnRates();
    StatuesConfig.addSpawnRates();
    SupplementariesConfig.addSpawnRates();
    TheAbyssConfig.addSpawnRates();
    TheFarlandersConfig.addSpawnRates();
    TinkersConstructConfig.addSpawnRates();
    TwistConfig.addSpawnRates();
    UntamedWildsConfig.addSpawnRates();
    Whisperwoods.addSpawnRates();

    log.info("Added {} player spawn rules, {} world spawn rules and {} special spawn rules.",
        spawnConfigPerPlayer.size(), spawnConfigPerWorld.size(), spawnConfigSpecial.size());
  }

  public static void clearSpawnRates() {
    log.info("Clearing spawn rates calculation ...");
    spawnConfigPerPlayer.clear();
    spawnConfigPerPlayer.clear();
    spawnConfigPerWorld.clear();
    spawnConfigSpecial.clear();
    spawnConfigEntity.clear();
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

  public static void addSpawnConfigEntity(String entityName) {
    spawnConfigEntity.add(entityName);
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
