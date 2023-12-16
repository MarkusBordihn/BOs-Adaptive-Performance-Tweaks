/*
 * Copyright 2021 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.adaptiveperformancetweaksspawn.spawn;

import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.CommonConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.AlexsMobsSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.AquacultureSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.BornInChaosSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.CustomSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.FishOfThievesSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.FriendsAndFoesSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.GothicSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.InfernalExpansionSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.MekanismAdditionsSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.MinecraftSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.NetherSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.PanthalassaSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.QuarkSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.TinkersConstructConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.UntamedWildsSpawnConfig;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn.UntitledDuckSpawnConfig;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod.EventBusSubscriber
public class SpawnConfigManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  private static final AlexsMobsSpawnConfig.Config ALEXS_MOBS_CONFIG = AlexsMobsSpawnConfig.COMMON;
  private static final AquacultureSpawnConfig.Config AQUACULTURE_CONFIG =
      AquacultureSpawnConfig.COMMON;
  private static final BornInChaosSpawnConfig.Config BORN_IN_CHAOS_CONFIG =
      BornInChaosSpawnConfig.COMMON;
  private static final CustomSpawnConfig.Config CUSTOM_CONFIG = CustomSpawnConfig.COMMON;
  private static final FishOfThievesSpawnConfig.Config FISH_OF_THIEVES_CONFIG =
      FishOfThievesSpawnConfig.COMMON;
  private static final FriendsAndFoesSpawnConfig.Config FRIENDS_AND_FOES_CONFIG =
      FriendsAndFoesSpawnConfig.COMMON;
  private static final GothicSpawnConfig.Config GOTHIC_CONFIG = GothicSpawnConfig.COMMON;
  private static final InfernalExpansionSpawnConfig.Config INFERNAL_EXPANSION_CONFIG =
      InfernalExpansionSpawnConfig.COMMON;
  private static final MekanismAdditionsSpawnConfig.Config MEKANISM_ADDITIONS_CONFIG =
      MekanismAdditionsSpawnConfig.COMMON;
  private static final MinecraftSpawnConfig.Config MINECRAFT_CONFIG = MinecraftSpawnConfig.COMMON;
  private static final NetherSpawnConfig.Config NETHER_CONFIG = NetherSpawnConfig.COMMON;
  private static final PanthalassaSpawnConfig.Config PANTHALASSA_CONFIG =
      PanthalassaSpawnConfig.COMMON;
  private static final QuarkSpawnConfig.Config QUARK_CONFIG = QuarkSpawnConfig.COMMON;
  private static final TinkersConstructConfig.Config TINKERS_CONSTRUCT_CONFIG =
      TinkersConstructConfig.COMMON;
  private static final UntamedWildsSpawnConfig.Config UNTAMED_WILDS_CONFIG =
      UntamedWildsSpawnConfig.COMMON;
  private static final UntitledDuckSpawnConfig.Config UNTITLED_DUCK_CONFIG =
      UntitledDuckSpawnConfig.COMMON;

  private static final Map<String, Integer> spawnConfigPerPlayer = new HashMap<>();
  private static final Map<String, Integer> spawnConfigPerWorld = new HashMap<>();
  private static final Map<String, Integer> spawnConfigPerServer = new HashMap<>();
  private static final Map<String, Integer> spawnConfigSpecial = new HashMap<>();

  protected SpawnConfigManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    clearSpawnRates();
    calculateSpawnRates();
  }

  public static void calculateSpawnRates() {

    ModList modList = ModList.get();

    // Minecraft Vanilla Mobs
    if (Boolean.TRUE.equals(MINECRAFT_CONFIG.enabled.get())) {
      // Normal vanilla mobs
      addSpawnRatesForPassiveMobs(
          MINECRAFT_CONFIG.id.get(),
          new HashSet<>(MINECRAFT_CONFIG.passiveMobsList.get()),
          MINECRAFT_CONFIG.passiveMobsPerPlayer.get(),
          MINECRAFT_CONFIG.passiveMobsPerWorld.get(),
          MINECRAFT_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          MINECRAFT_CONFIG.id.get(),
          new HashSet<>(MINECRAFT_CONFIG.neutralMobsList.get()),
          MINECRAFT_CONFIG.neutralMobsPerPlayer.get(),
          MINECRAFT_CONFIG.neutralMobsPerWorld.get(),
          MINECRAFT_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          MINECRAFT_CONFIG.id.get(),
          new HashSet<>(MINECRAFT_CONFIG.hostileMobsList.get()),
          MINECRAFT_CONFIG.hostileMobsPerPlayer.get(),
          MINECRAFT_CONFIG.hostileMobsPerWorld.get(),
          MINECRAFT_CONFIG.hostileMobsPerServer.get());

      // Nether vanilla mobs
      addSpawnRatesForPassiveMobs(
          NETHER_CONFIG.id.get(),
          new HashSet<>(NETHER_CONFIG.passiveMobsList.get()),
          NETHER_CONFIG.passiveMobsPerPlayer.get(),
          NETHER_CONFIG.passiveMobsPerWorld.get(),
          NETHER_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          NETHER_CONFIG.id.get(),
          new HashSet<>(NETHER_CONFIG.neutralMobsList.get()),
          NETHER_CONFIG.neutralMobsPerPlayer.get(),
          NETHER_CONFIG.neutralMobsPerWorld.get(),
          NETHER_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          NETHER_CONFIG.id.get(),
          new HashSet<>(NETHER_CONFIG.hostileMobsList.get()),
          NETHER_CONFIG.hostileMobsPerPlayer.get(),
          NETHER_CONFIG.hostileMobsPerWorld.get(),
          NETHER_CONFIG.hostileMobsPerServer.get());

      // Water vanilla mobs
      addSpawnRatesForPassiveMobs(
          MINECRAFT_CONFIG.id.get(),
          new HashSet<>(MINECRAFT_CONFIG.waterPassiveMobsList.get()),
          MINECRAFT_CONFIG.waterPassiveMobsPerPlayer.get(),
          MINECRAFT_CONFIG.waterPassiveMobsPerWorld.get(),
          MINECRAFT_CONFIG.waterPassiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          MINECRAFT_CONFIG.id.get(),
          new HashSet<>(MINECRAFT_CONFIG.waterNeutralMobsList.get()),
          MINECRAFT_CONFIG.waterNeutralMobsPerPlayer.get(),
          MINECRAFT_CONFIG.waterNeutralMobsPerWorld.get(),
          MINECRAFT_CONFIG.waterNeutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          MINECRAFT_CONFIG.id.get(),
          new HashSet<>(MINECRAFT_CONFIG.waterHostileMobsList.get()),
          MINECRAFT_CONFIG.waterHostileMobsPerPlayer.get(),
          MINECRAFT_CONFIG.waterHostileMobsPerWorld.get(),
          MINECRAFT_CONFIG.waterHostileMobsPerServer.get());
    }

    // Aquaculture Mobs
    if (Boolean.TRUE.equals(AQUACULTURE_CONFIG.enabled.get())
        && modList.isLoaded(AQUACULTURE_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          AQUACULTURE_CONFIG.id.get(),
          new HashSet<>(AQUACULTURE_CONFIG.fishList.get()),
          AQUACULTURE_CONFIG.fishPerPlayer.get(),
          AQUACULTURE_CONFIG.fishPerWorld.get(),
          AQUACULTURE_CONFIG.fishPerServer.get());
    }

    // Born in Chaos Mobs
    if (Boolean.TRUE.equals(BORN_IN_CHAOS_CONFIG.enabled.get())
        && modList.isLoaded(BORN_IN_CHAOS_CONFIG.id.get())) {
      addSpawnRatesForHostileMobs(
          BORN_IN_CHAOS_CONFIG.id.get(),
          new HashSet<>(BORN_IN_CHAOS_CONFIG.hostileMobsList.get()),
          BORN_IN_CHAOS_CONFIG.hostileMobsPerPlayer.get(),
          BORN_IN_CHAOS_CONFIG.hostileMobsPerWorld.get(),
          BORN_IN_CHAOS_CONFIG.hostileMobsPerServer.get());
    }

    // Alex's Mobs
    if (Boolean.TRUE.equals(ALEXS_MOBS_CONFIG.enabled.get())
        && modList.isLoaded(ALEXS_MOBS_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          ALEXS_MOBS_CONFIG.id.get(),
          new HashSet<>(ALEXS_MOBS_CONFIG.passiveMobsList.get()),
          ALEXS_MOBS_CONFIG.passiveMobsPerPlayer.get(),
          ALEXS_MOBS_CONFIG.passiveMobsPerWorld.get(),
          ALEXS_MOBS_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          ALEXS_MOBS_CONFIG.id.get(),
          new HashSet<>(ALEXS_MOBS_CONFIG.neutralMobsList.get()),
          ALEXS_MOBS_CONFIG.neutralMobsPerPlayer.get(),
          ALEXS_MOBS_CONFIG.neutralMobsPerWorld.get(),
          ALEXS_MOBS_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          ALEXS_MOBS_CONFIG.id.get(),
          new HashSet<>(ALEXS_MOBS_CONFIG.hostileMobsList.get()),
          ALEXS_MOBS_CONFIG.hostileMobsPerPlayer.get(),
          ALEXS_MOBS_CONFIG.hostileMobsPerWorld.get(),
          ALEXS_MOBS_CONFIG.hostileMobsPerServer.get());
    }

    // Fish of Thieves Mobs
    if (Boolean.TRUE.equals(FISH_OF_THIEVES_CONFIG.enabled.get())
        && modList.isLoaded(FISH_OF_THIEVES_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          FISH_OF_THIEVES_CONFIG.id.get(),
          new HashSet<>(FISH_OF_THIEVES_CONFIG.fishList.get()),
          FISH_OF_THIEVES_CONFIG.fishPerPlayer.get(),
          FISH_OF_THIEVES_CONFIG.fishPerWorld.get(),
          FISH_OF_THIEVES_CONFIG.fishPerServer.get());
    }

    // Friends and Foes
    if (Boolean.TRUE.equals(FRIENDS_AND_FOES_CONFIG.enabled.get())
        && modList.isLoaded(FRIENDS_AND_FOES_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          FRIENDS_AND_FOES_CONFIG.id.get(),
          new HashSet<>(FRIENDS_AND_FOES_CONFIG.passiveMobsList.get()),
          FRIENDS_AND_FOES_CONFIG.passiveMobsPerPlayer.get(),
          FRIENDS_AND_FOES_CONFIG.passiveMobsPerWorld.get(),
          FRIENDS_AND_FOES_CONFIG.passiveMobsPerServer.get());
    }

    // Gothic RPG
    if (Boolean.TRUE.equals(GOTHIC_CONFIG.enabled.get())
        && modList.isLoaded(GOTHIC_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          GOTHIC_CONFIG.id.get(),
          new HashSet<>(GOTHIC_CONFIG.passiveMobsList.get()),
          GOTHIC_CONFIG.passiveMobsPerPlayer.get(),
          GOTHIC_CONFIG.passiveMobsPerWorld.get(),
          GOTHIC_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          GOTHIC_CONFIG.id.get(),
          new HashSet<>(GOTHIC_CONFIG.neutralMobsList.get()),
          GOTHIC_CONFIG.neutralMobsPerPlayer.get(),
          GOTHIC_CONFIG.neutralMobsPerWorld.get(),
          GOTHIC_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          GOTHIC_CONFIG.id.get(),
          new HashSet<>(GOTHIC_CONFIG.hostileMobsList.get()),
          GOTHIC_CONFIG.hostileMobsPerPlayer.get(),
          GOTHIC_CONFIG.hostileMobsPerWorld.get(),
          GOTHIC_CONFIG.hostileMobsPerServer.get());
    }

    // Infernal Expansion
    if (Boolean.TRUE.equals(INFERNAL_EXPANSION_CONFIG.enabled.get())
        && modList.isLoaded(INFERNAL_EXPANSION_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          INFERNAL_EXPANSION_CONFIG.id.get(),
          new HashSet<>(INFERNAL_EXPANSION_CONFIG.passiveMobsList.get()),
          INFERNAL_EXPANSION_CONFIG.passiveMobsPerPlayer.get(),
          INFERNAL_EXPANSION_CONFIG.passiveMobsPerWorld.get(),
          INFERNAL_EXPANSION_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          INFERNAL_EXPANSION_CONFIG.id.get(),
          new HashSet<>(INFERNAL_EXPANSION_CONFIG.neutralMobsList.get()),
          INFERNAL_EXPANSION_CONFIG.neutralMobsPerPlayer.get(),
          INFERNAL_EXPANSION_CONFIG.neutralMobsPerWorld.get(),
          INFERNAL_EXPANSION_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          INFERNAL_EXPANSION_CONFIG.id.get(),
          new HashSet<>(INFERNAL_EXPANSION_CONFIG.hostileMobsList.get()),
          INFERNAL_EXPANSION_CONFIG.hostileMobsPerPlayer.get(),
          INFERNAL_EXPANSION_CONFIG.hostileMobsPerWorld.get(),
          INFERNAL_EXPANSION_CONFIG.hostileMobsPerServer.get());
    }

    // Mekanism Additions
    if (Boolean.TRUE.equals(MEKANISM_ADDITIONS_CONFIG.enabled.get())
        && modList.isLoaded(MEKANISM_ADDITIONS_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          MEKANISM_ADDITIONS_CONFIG.id.get(),
          new HashSet<>(MEKANISM_ADDITIONS_CONFIG.passiveMobsList.get()),
          MEKANISM_ADDITIONS_CONFIG.passiveMobsPerPlayer.get(),
          MEKANISM_ADDITIONS_CONFIG.passiveMobsPerWorld.get(),
          MEKANISM_ADDITIONS_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          MEKANISM_ADDITIONS_CONFIG.id.get(),
          new HashSet<>(MEKANISM_ADDITIONS_CONFIG.neutralMobsList.get()),
          MEKANISM_ADDITIONS_CONFIG.neutralMobsPerPlayer.get(),
          MEKANISM_ADDITIONS_CONFIG.neutralMobsPerWorld.get(),
          MEKANISM_ADDITIONS_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          MEKANISM_ADDITIONS_CONFIG.id.get(),
          new HashSet<>(MEKANISM_ADDITIONS_CONFIG.hostileMobsList.get()),
          MEKANISM_ADDITIONS_CONFIG.hostileMobsPerPlayer.get(),
          MEKANISM_ADDITIONS_CONFIG.hostileMobsPerWorld.get(),
          MEKANISM_ADDITIONS_CONFIG.hostileMobsPerServer.get());
    }

    // Panthalassa
    if (Boolean.TRUE.equals(PANTHALASSA_CONFIG.enabled.get())
        && modList.isLoaded(PANTHALASSA_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          PANTHALASSA_CONFIG.id.get(),
          new HashSet<>(PANTHALASSA_CONFIG.passiveMobsList.get()),
          PANTHALASSA_CONFIG.passiveMobsPerPlayer.get(),
          PANTHALASSA_CONFIG.passiveMobsPerWorld.get(),
          PANTHALASSA_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          PANTHALASSA_CONFIG.id.get(),
          new HashSet<>(PANTHALASSA_CONFIG.neutralMobsList.get()),
          PANTHALASSA_CONFIG.neutralMobsPerPlayer.get(),
          PANTHALASSA_CONFIG.neutralMobsPerWorld.get(),
          PANTHALASSA_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          PANTHALASSA_CONFIG.id.get(),
          new HashSet<>(PANTHALASSA_CONFIG.hostileMobsList.get()),
          PANTHALASSA_CONFIG.hostileMobsPerPlayer.get(),
          PANTHALASSA_CONFIG.hostileMobsPerWorld.get(),
          PANTHALASSA_CONFIG.hostileMobsPerServer.get());
    }

    // Quark
    if (Boolean.TRUE.equals(QUARK_CONFIG.enabled.get())
        && modList.isLoaded(QUARK_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          QUARK_CONFIG.id.get(),
          new HashSet<>(QUARK_CONFIG.passiveMobsList.get()),
          QUARK_CONFIG.passiveMobsPerPlayer.get(),
          QUARK_CONFIG.passiveMobsPerWorld.get(),
          QUARK_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          QUARK_CONFIG.id.get(),
          new HashSet<>(QUARK_CONFIG.neutralMobsList.get()),
          QUARK_CONFIG.neutralMobsPerPlayer.get(),
          QUARK_CONFIG.neutralMobsPerWorld.get(),
          QUARK_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          QUARK_CONFIG.id.get(),
          new HashSet<>(QUARK_CONFIG.hostileMobsList.get()),
          QUARK_CONFIG.hostileMobsPerPlayer.get(),
          QUARK_CONFIG.hostileMobsPerWorld.get(),
          QUARK_CONFIG.hostileMobsPerServer.get());
    }

    // Tinkers Construct
    if (Boolean.TRUE.equals(TINKERS_CONSTRUCT_CONFIG.enabled.get())
        && modList.isLoaded(TINKERS_CONSTRUCT_CONFIG.id.get())) {
      addSpawnRatesForHostileMobs(
          TINKERS_CONSTRUCT_CONFIG.id.get(),
          new HashSet<>(TINKERS_CONSTRUCT_CONFIG.hostileMobsList.get()),
          TINKERS_CONSTRUCT_CONFIG.hostileMobsPerPlayer.get(),
          TINKERS_CONSTRUCT_CONFIG.hostileMobsPerWorld.get(),
          TINKERS_CONSTRUCT_CONFIG.hostileMobsPerServer.get());
    }

    // Untamed Wilds
    if (Boolean.TRUE.equals(UNTAMED_WILDS_CONFIG.enabled.get())
        && modList.isLoaded(UNTAMED_WILDS_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          UNTAMED_WILDS_CONFIG.id.get(),
          new HashSet<>(UNTAMED_WILDS_CONFIG.passiveMobsList.get()),
          UNTAMED_WILDS_CONFIG.passiveMobsPerPlayer.get(),
          UNTAMED_WILDS_CONFIG.passiveMobsPerWorld.get(),
          UNTAMED_WILDS_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          UNTAMED_WILDS_CONFIG.id.get(),
          new HashSet<>(UNTAMED_WILDS_CONFIG.neutralMobsList.get()),
          UNTAMED_WILDS_CONFIG.neutralMobsPerPlayer.get(),
          UNTAMED_WILDS_CONFIG.neutralMobsPerWorld.get(),
          UNTAMED_WILDS_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          UNTAMED_WILDS_CONFIG.id.get(),
          new HashSet<>(UNTAMED_WILDS_CONFIG.hostileMobsList.get()),
          UNTAMED_WILDS_CONFIG.hostileMobsPerPlayer.get(),
          UNTAMED_WILDS_CONFIG.hostileMobsPerWorld.get(),
          UNTAMED_WILDS_CONFIG.hostileMobsPerServer.get());
    }

    // Untitled Duck
    if (Boolean.TRUE.equals(UNTITLED_DUCK_CONFIG.enabled.get())
        && modList.isLoaded(UNTITLED_DUCK_CONFIG.id.get())) {
      addSpawnRatesForPassiveMobs(
          UNTITLED_DUCK_CONFIG.id.get(),
          new HashSet<>(UNTITLED_DUCK_CONFIG.passiveMobsList.get()),
          UNTITLED_DUCK_CONFIG.passiveMobsPerPlayer.get(),
          UNTITLED_DUCK_CONFIG.passiveMobsPerWorld.get(),
          UNTITLED_DUCK_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          UNTITLED_DUCK_CONFIG.id.get(),
          new HashSet<>(UNTITLED_DUCK_CONFIG.neutralMobsList.get()),
          UNTITLED_DUCK_CONFIG.neutralMobsPerPlayer.get(),
          UNTITLED_DUCK_CONFIG.neutralMobsPerWorld.get(),
          UNTITLED_DUCK_CONFIG.neutralMobsPerServer.get());
    }

    // Custom Spawn Config overwrites former definitions!
    if (Boolean.TRUE.equals(CUSTOM_CONFIG.enabled.get())) {
      addSpawnRatesForPassiveMobs(
          CUSTOM_CONFIG.id.get(),
          new HashSet<>(CUSTOM_CONFIG.passiveMobsList.get()),
          CUSTOM_CONFIG.passiveMobsPerPlayer.get(),
          CUSTOM_CONFIG.passiveMobsPerWorld.get(),
          CUSTOM_CONFIG.passiveMobsPerServer.get());
      addSpawnRatesForNeutralMobs(
          CUSTOM_CONFIG.id.get(),
          new HashSet<>(CUSTOM_CONFIG.neutralMobsList.get()),
          CUSTOM_CONFIG.neutralMobsPerPlayer.get(),
          CUSTOM_CONFIG.neutralMobsPerWorld.get(),
          CUSTOM_CONFIG.neutralMobsPerServer.get());
      addSpawnRatesForHostileMobs(
          CUSTOM_CONFIG.id.get(),
          new HashSet<>(CUSTOM_CONFIG.hostileMobsList.get()),
          CUSTOM_CONFIG.hostileMobsPerPlayer.get(),
          CUSTOM_CONFIG.hostileMobsPerWorld.get(),
          CUSTOM_CONFIG.hostileMobsPerServer.get());
    }

    log.info(
        "Added {} player spawn rules, {} world spawn rules and {} special spawn rules.",
        spawnConfigPerPlayer.size(),
        spawnConfigPerWorld.size(),
        spawnConfigSpecial.size());
  }

  public static void clearSpawnRates() {
    log.info("Clearing spawn rates calculation ...");
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

  public static void addSpawnConfigPerServer(String entityName, int maxNumberOfEntities) {
    spawnConfigPerServer.put(entityName, maxNumberOfEntities);
  }

  public static void addSpawnConfigSpecial(String entityName, int maxNumberOfEntities) {
    spawnConfigSpecial.put(entityName, maxNumberOfEntities);
  }

  public static int getSpawnLimitPerPlayer(String entityName) {
    return spawnConfigPerPlayer.getOrDefault(
        entityName, COMMON.spawnLimitationMaxMobsPerPlayer.get());
  }

  public static int getSpawnLimitPerWorld(String entityName) {
    return spawnConfigPerWorld.getOrDefault(
        entityName, COMMON.spawnLimitationMaxMobsPerWorld.get());
  }

  public static int getSpawnLimitPerServer(String entityName) {
    return spawnConfigPerServer.getOrDefault(
        entityName, COMMON.spawnLimitationMaxMobsPerServer.get());
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

  public static Map<String, Integer> getSpawnConfigPerServer() {
    return spawnConfigPerServer;
  }

  public static Map<String, Integer> getSpawnConfigSpecial() {
    return spawnConfigSpecial;
  }

  public static void addSpawnRatesForGeneralMobs(
      String name,
      Set<String> generalMobList,
      int maxGeneralMobsPerPlayer,
      int maxGeneralMobsPerWorld,
      int maxGeneralMobsPerServer) {
    if (generalMobList.isEmpty()) {
      return;
    }
    log.info(
        "✓ Enable general mobs spawn rate control for {} and {} mobs with maxPerPlayer:{}, maxPerWorld:{} and maxPerServer:{}",
        name,
        generalMobList.size(),
        maxGeneralMobsPerPlayer,
        maxGeneralMobsPerWorld,
        maxGeneralMobsPerServer);
    addSpawnRatesForGeneralMobs(
        generalMobList, maxGeneralMobsPerPlayer, maxGeneralMobsPerWorld, maxGeneralMobsPerServer);
  }

  public static void addSpawnRatesForPassiveMobs(
      String name,
      Set<String> passiveMobList,
      int maxPassiveMobsPerPlayer,
      int maxPassiveMobsPerWorld,
      int maxPassiveMobsPerServer) {
    if (passiveMobList.isEmpty()) {
      return;
    }
    log.info(
        "✓ Enable passive mobs spawn rate control for {} and {} mobs with maxPerPlayer:{}, maxPerWorld:{} and maxPerServer:{}",
        name,
        passiveMobList.size(),
        maxPassiveMobsPerPlayer,
        maxPassiveMobsPerWorld,
        maxPassiveMobsPerServer);
    addSpawnRatesForPassiveMobs(
        passiveMobList, maxPassiveMobsPerPlayer, maxPassiveMobsPerWorld, maxPassiveMobsPerServer);
  }

  public static void addSpawnRatesForNeutralMobs(
      String name,
      Set<String> neutralMobList,
      int maxNeutralMobsPerPlayer,
      int maxNeutralMobsPerWorld,
      int maxNeutralMobsPerServer) {
    if (neutralMobList.isEmpty()) {
      return;
    }
    log.info(
        "✓ Enable neutral mobs spawn rate control for {} and {} mobs with maxPerPlayer:{}, maxPerWorld:{} and maxPerServer:{}",
        name,
        neutralMobList.size(),
        maxNeutralMobsPerPlayer,
        maxNeutralMobsPerWorld,
        maxNeutralMobsPerServer);
    addSpawnRatesForNeutralMobs(
        neutralMobList, maxNeutralMobsPerPlayer, maxNeutralMobsPerWorld, maxNeutralMobsPerServer);
  }

  public static void addSpawnRatesForHostileMobs(
      String name,
      Set<String> hostileMobList,
      int maxHostileMobsPerPlayer,
      int maxHostileMobsPerWorld,
      int maxHostileMobsPerServer) {
    if (hostileMobList.isEmpty()) {
      return;
    }
    log.info(
        "✓ Enable hostile mobs spawn rate control for {} and {} mobs with maxPerPlayer:{}, maxPerWorld:{} and maxPerServer:{}",
        name,
        hostileMobList.size(),
        maxHostileMobsPerPlayer,
        maxHostileMobsPerWorld,
        maxHostileMobsPerServer);
    addSpawnRatesForHostileMobs(
        hostileMobList, maxHostileMobsPerPlayer, maxHostileMobsPerWorld, maxHostileMobsPerServer);
  }

  public static void addSpawnRatesForBossMobs(
      String name,
      Set<String> bossMobList,
      int maxBossMobsPerPlayer,
      int maxBossMobsPerWorld,
      int maxBossMobsPerServer) {
    if (bossMobList.isEmpty()) {
      return;
    }
    log.info(
        "✓ Enable boss mobs spawn rate control for {} and {} mobs with maxPerPlayer:{}, maxPerWorld:{} and maxPerServer:{}",
        name,
        bossMobList.size(),
        maxBossMobsPerPlayer,
        maxBossMobsPerWorld,
        maxBossMobsPerServer);
    addSpawnRatesForBossMobs(
        bossMobList, maxBossMobsPerPlayer, maxBossMobsPerWorld, maxBossMobsPerServer);
  }

  public static void addSpecialSpawnRates(String name, Map<String, Integer> specialMobList) {
    if (specialMobList.isEmpty()) {
      return;
    }
    log.info(
        "✓ Enable special mobs spawn rate control for {} and {} mobs with: {}",
        name,
        specialMobList.size(),
        specialMobList);
    addSpecialSpawnRates(specialMobList);
  }

  public static void addSpawnRatesForGeneralMobs(
      Set<String> generalMobList,
      int maxGeneralMobsPerPlayer,
      int maxGeneralMobsPerWorld,
      int maxGeneralMobsPerServer) {
    for (String entity : generalMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxGeneralMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxGeneralMobsPerWorld);
      SpawnConfigManager.addSpawnConfigPerServer(entity, maxGeneralMobsPerServer);
    }
  }

  public static void addSpawnRatesForPassiveMobs(
      Set<String> passiveMobList,
      int maxPassiveMobsPerPlayer,
      int maxPassiveMobsPerWorld,
      int maxPassiveMobsPerServer) {
    for (String entity : passiveMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxPassiveMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxPassiveMobsPerWorld);
      SpawnConfigManager.addSpawnConfigPerServer(entity, maxPassiveMobsPerServer);
    }
  }

  public static void addSpawnRatesForNeutralMobs(
      Set<String> neutralMobList,
      int maxNeutralMobsPerPlayer,
      int maxNeutralMobsPerWorld,
      int maxNeutralMobsPerServer) {
    for (String entity : neutralMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxNeutralMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxNeutralMobsPerWorld);
      SpawnConfigManager.addSpawnConfigPerServer(entity, maxNeutralMobsPerServer);
    }
  }

  public static void addSpawnRatesForHostileMobs(
      Set<String> hostileMobList,
      int maxHostileMobsPerPlayer,
      int maxHostileMobsPerWorld,
      int maxHostileMobsPerServer) {
    for (String entity : hostileMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxHostileMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxHostileMobsPerWorld);
      SpawnConfigManager.addSpawnConfigPerServer(entity, maxHostileMobsPerServer);
    }
  }

  public static void addSpawnRatesForBossMobs(
      Set<String> bossMobList,
      int maxBossMobsPerPlayer,
      int maxBossMobsPerWorld,
      int maxBossMobsPerServer) {
    for (String entity : bossMobList) {
      SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxBossMobsPerPlayer);
      SpawnConfigManager.addSpawnConfigPerWorld(entity, maxBossMobsPerWorld);
      SpawnConfigManager.addSpawnConfigPerServer(entity, maxBossMobsPerServer);
    }
  }

  public static void addSpecialSpawnRates(Map<String, Integer> specialMobList) {
    for (Map.Entry<String, Integer> entry : specialMobList.entrySet()) {
      String entity = entry.getKey();
      SpawnConfigManager.addSpawnConfigSpecial(entity, entry.getValue());
    }
  }

  public static void addSpawnRateForMob(
      String entity, int maxMobsPerPlayer, int maxMobsPerWorld, int maxMobsPerServer) {
    SpawnConfigManager.addSpawnConfigPerPlayer(entity, maxMobsPerPlayer);
    SpawnConfigManager.addSpawnConfigPerWorld(entity, maxMobsPerWorld);
    SpawnConfigManager.addSpawnConfigPerServer(entity, maxMobsPerServer);
  }
}
