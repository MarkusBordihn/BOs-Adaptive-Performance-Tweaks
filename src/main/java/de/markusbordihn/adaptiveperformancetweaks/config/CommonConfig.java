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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;

import de.markusbordihn.adaptiveperformancetweaks.Constants;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public final class CommonConfig {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final int PER_PLAYER_MAX = 64;
  private static final int PER_WORLD_MAX = 512;

  private CommonConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering common config ...");
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec);
  }

  public static class Config {
    public final ForgeConfigSpec.BooleanValue optimizePlayerLogin;
    public final ForgeConfigSpec.BooleanValue optimizeViewDistance;
    public final ForgeConfigSpec.BooleanValue optimizeGeneralMobs;
    public final ForgeConfigSpec.BooleanValue optimizePassiveMobs;
    public final ForgeConfigSpec.BooleanValue optimizeNeutralMobs;
    public final ForgeConfigSpec.BooleanValue optimizeHostileMobs;
    public final ForgeConfigSpec.BooleanValue optimizeBossMobs;

    public final ForgeConfigSpec.BooleanValue optimizeItems;
    public final ForgeConfigSpec.IntValue maxNumberOfItems;
    public final ForgeConfigSpec.IntValue maxNumberOfItemsPerType;
    public final ForgeConfigSpec.IntValue itemsClusterRange;

    public final ForgeConfigSpec.BooleanValue optimizeExperienceOrbs;
    public final ForgeConfigSpec.IntValue experienceOrbsClusterRange;

    public final ForgeConfigSpec.ConfigValue<String> logLevel;
    public final ForgeConfigSpec.BooleanValue logServerLoad;

    public final ForgeConfigSpec.IntValue viewAreaXFactor;
    public final ForgeConfigSpec.IntValue viewAreaYFactor;
    public final ForgeConfigSpec.IntValue viewAreaZFactor;
    public final ForgeConfigSpec.DoubleValue viewAreaDistanceFactor;

    public final ForgeConfigSpec.IntValue viewDistanceMin;
    public final ForgeConfigSpec.IntValue viewDistanceMax;
    public final ForgeConfigSpec.IntValue viewDistanceDefault;

    public final ForgeConfigSpec.BooleanValue gameruleEnabled;
    public final ForgeConfigSpec.BooleanValue entityCrammingEnabled;
    public final ForgeConfigSpec.IntValue minEntityCramming;
    public final ForgeConfigSpec.IntValue maxEntityCramming;
    public final ForgeConfigSpec.BooleanValue randomTickSpeedEnabled;
    public final ForgeConfigSpec.IntValue randomTickSpeed;

    public final ForgeConfigSpec.BooleanValue spawnerEnabled;
    public final ForgeConfigSpec.IntValue spawnerMaxEntityPerWorld;
    public final ForgeConfigSpec.IntValue spawnerMaxEntityPerChunk;

    public final ForgeConfigSpec.ConfigValue<List<String>> spawnAllowList;
    public final ForgeConfigSpec.ConfigValue<List<String>> spawnDenyList;

    public final ForgeConfigSpec.BooleanValue spawnLimitationEnabled;
    public final ForgeConfigSpec.IntValue spawnLimitationLimiter;
    public final ForgeConfigSpec.IntValue spawnLimitationMaxMobsPerPlayer;
    public final ForgeConfigSpec.IntValue spawnLimitationMaxMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue minecraftEnabled;
    public final ForgeConfigSpec.IntValue minecraftMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue minecraftMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue minecraftMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue minecraftMaxWaterPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxWaterPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue minecraftMaxWaterNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxWaterNeutralMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modAlexmobsEnabled;
    public final ForgeConfigSpec.IntValue modAlexmobsMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modAlexmobsMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modAlexmobsMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modAlexmobsMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue modAlexmobsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modAlexmobsMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modAquacultureEnabled;
    public final ForgeConfigSpec.IntValue modAquacultureMaxFishPerPlayer;
    public final ForgeConfigSpec.IntValue modAquacultureMaxFishPerWorld;

    public final ForgeConfigSpec.BooleanValue modArtifactsEnabled;
    public final ForgeConfigSpec.IntValue modArtifactsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modArtifactsMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modDungeonsmodEnabled;
    public final ForgeConfigSpec.BooleanValue modDungeonsmodOptimizeWhirlwind;
    public final ForgeConfigSpec.IntValue modDungeonsmodMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modDungeonsmodMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue modDungeonsmodMaxBossesPerPlayer;
    public final ForgeConfigSpec.IntValue modDungeonsmodMaxBossesPerWorld;

    public final ForgeConfigSpec.BooleanValue modIceandfireEnabled;
    public final ForgeConfigSpec.IntValue modIceandfireMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modIceandfireMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modIceandfireMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modIceandfireMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue modIceandfireMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modIceandfireMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue modIceandfireMaxBossesPerPlayer;
    public final ForgeConfigSpec.IntValue modIceandfireMaxBossesPerWorld;

    public final ForgeConfigSpec.BooleanValue modMekanismadditionsEnabled;
    public final ForgeConfigSpec.IntValue modMekanismadditionsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modMekanismadditionsMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modMowziesMobsEnabled;
    public final ForgeConfigSpec.IntValue modMowziesMobsMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modMowziesMobsMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue modMowziesMobsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modMowziesMobsMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue modMowziesMobsMaxBossesPerPlayer;
    public final ForgeConfigSpec.IntValue modMowziesMobsMaxBossesPerWorld;

    public final ForgeConfigSpec.BooleanValue modMutantBeastsEnabled;
    public final ForgeConfigSpec.IntValue modMutantBeastsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modMutantBeastsMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modQuarkEnabled;
    public final ForgeConfigSpec.IntValue modQuarkMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modQuarkMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modQuarkMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modQuarkMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue modQuarkMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modQuarkMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modRatsEnabled;
    public final ForgeConfigSpec.IntValue modRatsMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modRatsMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modRatsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modRatsMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue modRatsMaxBossesPerPlayer;
    public final ForgeConfigSpec.IntValue modRatsMaxBossesPerWorld;

    public final ForgeConfigSpec.BooleanValue modSavageandravageEnabled;
    public final ForgeConfigSpec.IntValue modSavageandravageMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modSavageandravageMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modStatuesEnabled;
    public final ForgeConfigSpec.IntValue modStatuesMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modStatuesMaxPassiveMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modSupplementariesEnabled;
    public final ForgeConfigSpec.IntValue modSupplementariesMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modSupplementariesMaxPassiveMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modTheabyssEnabled;
    public final ForgeConfigSpec.IntValue modTheabyssMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTheabyssMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modTheabyssMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTheabyssMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modTheFarlandersEnabled;
    public final ForgeConfigSpec.IntValue modTheFarlandersMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTheFarlandersMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modTheFarlandersMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTheFarlandersMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue modTheFarlandersMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTheFarlandersMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue modTheFarlandersMaxBossesPerPlayer;
    public final ForgeConfigSpec.IntValue modTheFarlandersMaxBossesPerWorld;

    public final ForgeConfigSpec.BooleanValue modTinkersconstructEnabled;
    public final ForgeConfigSpec.IntValue modTinkersconstructMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTinkersconstructMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modTwilightForestEnabled;
    public final ForgeConfigSpec.IntValue modTwilightForestMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTwilightForestMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modTwilightForestMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTwilightForestMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue modTwilightForestMaxBossesPerPlayer;
    public final ForgeConfigSpec.IntValue modTwilightForestMaxBossesPerWorld;

    public final ForgeConfigSpec.BooleanValue modTwistEnabled;
    public final ForgeConfigSpec.IntValue modTwistMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTwistMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modTwistMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTwistMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue modTwistMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTwistMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modUntamedWildsEnabled;
    public final ForgeConfigSpec.IntValue modUntamedWildsMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modUntamedWildsMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modUntamedWildsMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modUntamedWildsMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue modUntamedWildsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modUntamedWildsMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modWhisperwoodsEnabled;
    public final ForgeConfigSpec.IntValue modWhisperwoodsMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modWhisperwoodsMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modWhisperwoodsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modWhisperwoodsMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue modWhisperwoodsMaxBossesPerPlayer;
    public final ForgeConfigSpec.IntValue modWhisperwoodsMaxBossesPerWorld;

    public final ForgeConfigSpec.BooleanValue burnCreeperDuringDaylight;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment("Adaptive Performance Tweaks (General configuration)");

      builder.push("Optimization");
      optimizePlayerLogin =
          builder.comment("Enable/Disable optimization which are happening during player login.")
              .define("optimizePlayerLogin", true);
      optimizeViewDistance =
          builder.comment("Enable/Disable view distance optimization depending on the server load.")
              .define("optimizeViewDistance", true);
      optimizeGeneralMobs =
          builder.comment("Enable/Disable general mobs optimization depending on the server load.")
              .define("optimizeGeneralMobs", true);
      optimizePassiveMobs =
          builder.comment("Enable/Disable passive mobs optimization depending on the server load.")
              .define("optimizePassiveMobs", true);
      optimizeNeutralMobs =
          builder.comment("Enable/Disable neutral mobs optimization depending on the server load.")
              .define("optimizeNeutralMobs", true);
      optimizeHostileMobs =
          builder.comment("Enable/Disable hostile mobs optimization depending on the server load.")
              .define("optimizeHostileMobs", true);
      optimizeBossMobs =
          builder.comment("Enable/Disable boss mobs optimization depending on the server load.")
              .define("optimizeBossMobs", true);
      builder.pop();

      builder.push("Items");
      optimizeItems =
          builder.comment("Enable/Disable items optimization.").define("optimizeItems", true);
      maxNumberOfItemsPerType = builder.comment(
          "Defines the max. number of items per type which are allowed to lay around in the world.")
          .defineInRange("maxNumberOfItemsPerType", 16, 10, 128);
      maxNumberOfItems = builder
          .comment(
              "Defines the max. number of items which are allowed to lay around in a single world.")
          .defineInRange("maxNumberOfItems", 64, 10, 1000);
      itemsClusterRange =
          builder.comment("Defines the radius in which items will be clustered together.")
              .defineInRange("itemsClusterRange", 4, 1, 16);
      builder.pop();


      builder.push("Experience Orbs");
      optimizeExperienceOrbs = builder.comment("Enable/Disable experience orbs optimization.")
          .define("optimizeExperienceOrbs", true);
      experienceOrbsClusterRange =
          builder.comment("Defines the radius in which experience orbs will be clustered together.")
              .defineInRange("experienceOrbsClusterRange", 4, 1, 16);
      builder.pop();

      builder.comment("View area optimization, should be only adjusted if really needed!")
          .push("View Area");
      viewAreaXFactor = builder.comment(
          "Max. number of X blocks relative to the player position considered inside a chunk for the view area.")
          .defineInRange("viewAreaXFactor", 16, 1, 16);
      viewAreaYFactor = builder.comment(
          "Max. number of Y blocks relative to the player position considered inside a chunk for the view area.")
          .defineInRange("viewAreaYFactor", 4, 1, 16);
      viewAreaZFactor = builder.comment(
          "Max. number of Z blocks relative to the player position considered inside a chunk for the view area.")
          .defineInRange("viewAreaZFactor", 16, 1, 16);
      viewAreaDistanceFactor = builder
          .comment(
              "Factor per view-distance which is used to calculated the view area of the player.")
          .defineInRange("viewAreaDistanceFactor", 0.8, 0.1, 1);
      builder.pop();

      builder.comment("View / render distance optimization.").push("View Distance");
      viewDistanceMax = builder.defineInRange("viewDistanceMax", 16, 4, 32);
      viewDistanceMin = builder.defineInRange("viewDistanceMin", 2, 2, 16);
      viewDistanceDefault = builder.defineInRange("viewDistanceDefault", 8, 2, 16);
      builder.pop();

      builder.push("Gamerule");
      gameruleEnabled = builder.comment("Enable/Disable dynamic gamerule adjustments.")
          .define("gameruleEnabled", true);

      builder.push("Entity Cramming");
      entityCrammingEnabled = builder.comment("Enable/Disable dynamic entity cramming adjustments.")
          .define("entityCrammingEnabled", true);
      minEntityCramming =
          builder.comment("Defines the min. numbers of mob singularity on a single block.")
              .defineInRange("minEntityCramming", 5, 0, 64);
      maxEntityCramming =
          builder.comment("Defines the max. numbers of mob singularity on a single block.")
              .defineInRange("maxEntityCramming", 24, 1, 64);
      builder.pop();

      builder.push("Random Tick Speed");
      randomTickSpeedEnabled =
          builder.comment("Enable/Disable dynamic random tick speed adjustments.")
              .define("randomTickSpeedEnabled", true);
      randomTickSpeed = builder
          .comment("Defines the max. speed of crops grow, leaf decay, redstone, ... per chunk")
          .defineInRange("randomTickSpeed", 3, 1, 256);
      builder.pop();

      builder.pop();

      builder.push("Spawner");
      spawnerEnabled =
          builder.comment("Enable/Disable spawner optimizations.").define("spawnerEnabled", true);
      spawnerMaxEntityPerWorld =
          builder.comment("Defines the max. number of entities per world for spawners.")
              .defineInRange("spawnerMaxEntityPerWorld", 1000, 1, 4096);
      spawnerMaxEntityPerChunk =
          builder.comment("Defines the max. number of entities per chunk for spawners.")
              .defineInRange("spawnerMaxEntityPerChunk", 32, 1, 512);
      builder.pop();

      builder.comment(StringUtils.join(Arrays.asList("Spawn optimization",
          "Playing with difficulty HARD could exceed the max number of hostile entity from the ",
          "config file by 1.5x.",
          "These settings affects all mobs which are not covered by other parts of this config"),
          System.lineSeparator())).push("Spawn");
      spawnAllowList = builder.comment(
          "General allow list for spawn entities (e.g. minecraft:squid) which should be ignored for optimization.")
          .define("allowList", new ArrayList<String>(Arrays.asList("")));
      spawnDenyList = builder.comment(
          "General deny list for spawn entities (e.g. minecraft:bat) to no longer spawn in all worlds.")
          .define("denyList", new ArrayList<String>(Arrays.asList("")));

      builder.push("Limitations");
      spawnLimitationEnabled =
          builder.comment("Enable/Disable general spawn limitations for unknown mobs.")
              .define("spawnLimitationEnabled", true);
      spawnLimitationLimiter = builder.comment(
          "Blocks every x spawn of unknown mobs to avoid an over population with the limited spawn area. Use 0 to disable this optimization.")
          .defineInRange("spawnLimitationLimiter", 10, 0, 100);
      spawnLimitationMaxMobsPerPlayer = builder.comment(
          "Defines the max. number of unknown entities of a specific type, which could spawn within the player view area. Use 0 to disable this optimization.")
          .defineInRange("spawnLimitationMaxMobsPerPlayer", 16, 1, 256);
      spawnLimitationMaxMobsPerWorld = builder.comment(
          "Defines the max. number of unknown entities of a specific type, which could spawn within a single world. Use 0 to disable this optimization.")
          .defineInRange("spawnLimitationMaxMobsPerWorld", 64, 1, 512);
      builder.pop();

      builder.push("Minecraft");
      minecraftEnabled = builder.define("minecraftEnabled", true);
      minecraftMaxPassiveMobsPerPlayer = builder.comment("e.g. mostly bats")
          .defineInRange("minecraftMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      minecraftMaxPassiveMobsPerWorld =
          builder.defineInRange("minecraftMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      minecraftMaxNeutralMobsPerPlayer = builder.comment("e.g. sheep, pig, horse, fox, ...")
          .defineInRange("minecraftMaxNeutralMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      minecraftMaxNeutralMobsPerWorld =
          builder.defineInRange("minecraftMaxNeutralMobsPerWorld", 16, 1, PER_WORLD_MAX);
      minecraftMaxHostileMobsPerPlayer = builder.comment("e.g. slime, spider, zombie, ravager, ...")
          .defineInRange("minecraftMaxHostileMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      minecraftMaxHostileMobsPerWorld =
          builder.defineInRange("minecraftMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      minecraftMaxWaterPassiveMobsPerPlayer = builder.comment("e.g. mostly fish")
          .defineInRange("minecraftMaxWaterPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      minecraftMaxWaterPassiveMobsPerWorld =
          builder.defineInRange("minecraftMaxWaterPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      minecraftMaxWaterNeutralMobsPerPlayer = builder.comment("e.g. squid, dolphin, ...")
          .defineInRange("minecraftMaxWaterNeutralMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      minecraftMaxWaterNeutralMobsPerWorld =
          builder.defineInRange("minecraftMaxWaterNeutralMobsPerWorld", 16, 1, PER_WORLD_MAX);
      builder.pop();

      builder.pop();

      // @formatter:off
      builder.comment(StringUtils.join(Arrays.asList(
        "This sections allows to control specific setting for some mods.",
        "You don't need to disable any mod support if the mod is not installed.",
        "In general there are 3 settings per supported mod.",
        "",
        " [Enabled] Enables / disables the optimization for this specific Mod.",
        " [MaxEntityPerPlayer] Defines the max. number of entities of a specific type, which could spawn",
        "   within the player radius. This value will be adjusted according the server load and game difficulty.",
        " [MaxEntityPerWorld] Defines the max. number of entities of a specific type, which could spawn",
        "   within the world. This value will be adjusted according the server load and game difficulty.",
        "",
        "Note: A value of 1 means that you can only have one entity of a specific type (e.g. minecraft:creeper)",
        "at a time. You still could have 2 monster from different types likes minecraft:creeper and",
        "minecraft:skeleton at the same time."), System.lineSeparator())).push("Mod");
        // @formatter:on

      builder.push("Alex's Mobs");
      modAlexmobsEnabled = builder.define("modAlexmobsEnabled", true);
      modAlexmobsMaxPassiveMobsPerPlayer =
          builder.defineInRange("modAlexmobsMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modAlexmobsMaxPassiveMobsPerWorld =
          builder.defineInRange("modAlexmobsMaxPassiveMobsPerWorld", 8, 1, PER_WORLD_MAX);
      modAlexmobsMaxNeutralMobsPerPlayer =
          builder.defineInRange("modAlexmobsMaxNeutralMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modAlexmobsMaxNeutralMobsPerWorld =
          builder.defineInRange("modAlexmobsMaxNeutralMobsPerWorld", 8, 1, PER_WORLD_MAX);
      modAlexmobsMaxHostileMobsPerPlayer =
          builder.defineInRange("modAlexmobsMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modAlexmobsMaxHostileMobsPerWorld =
          builder.defineInRange("modAlexmobsMaxHostileMobsPerWorld", 10, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Aquaculture 2");
      modAquacultureEnabled = builder.define("modAquacultureEnabled", true);
      modAquacultureMaxFishPerPlayer =
          builder.defineInRange("modAquacultureMaxFishPerPlayer", 2, 1, PER_PLAYER_MAX);
      modAquacultureMaxFishPerWorld =
          builder.defineInRange("modAquacultureMaxFishPerWorld", 8, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Artifacts Mod");
      modArtifactsEnabled = builder.define("modArtifactsEnabled", true);
      modArtifactsMaxHostileMobsPerPlayer =
          builder.defineInRange("modArtifactsMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modArtifactsMaxHostileMobsPerWorld =
          builder.defineInRange("modArtifactsMaxHostileMobsPerWorld", 12, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Dungeons Mod");
      modDungeonsmodEnabled = builder.define("modDungeonsmodEnabled", true);
      modDungeonsmodOptimizeWhirlwind = builder.define("modDungeonsmodOptimizeWhirlwind", true);
      modDungeonsmodMaxHostileMobsPerPlayer =
          builder.defineInRange("modDungeonsmodMaxHostileMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      modDungeonsmodMaxHostileMobsPerWorld =
          builder.defineInRange("modDungeonsmodMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modDungeonsmodMaxBossesPerPlayer =
          builder.defineInRange("modDungeonsmodMaxBossesPerPlayer", 2, 1, PER_PLAYER_MAX);
      modDungeonsmodMaxBossesPerWorld =
          builder.defineInRange("modDungeonsmodMaxBossesPerWorld", 4, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Ice and Fire: Dragons");
      modIceandfireEnabled = builder.define("modIceandfireEnabled", true);
      modIceandfireMaxPassiveMobsPerPlayer =
          builder.defineInRange("modIceandfireMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modIceandfireMaxPassiveMobsPerWorld =
          builder.defineInRange("modIceandfireMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modIceandfireMaxNeutralMobsPerPlayer =
          builder.defineInRange("modIceandfireMaxNeutralMobsPerPlayer", 1, 1, PER_PLAYER_MAX);
      modIceandfireMaxNeutralMobsPerWorld =
          builder.defineInRange("modIceandfireMaxNeutralMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modIceandfireMaxHostileMobsPerPlayer =
          builder.defineInRange("modIceandfireMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modIceandfireMaxHostileMobsPerWorld =
          builder.defineInRange("modIceandfireMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modIceandfireMaxBossesPerPlayer =
          builder.defineInRange("modIceandfireMaxBossesPerPlayer", 4, 1, PER_PLAYER_MAX);
      modIceandfireMaxBossesPerWorld =
          builder.defineInRange("modIceandfireMaxBossesPerWorld", 8, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Mekanism Additions");
      modMekanismadditionsEnabled = builder.define("modMekanismadditionsEnabled", true);
      modMekanismadditionsMaxHostileMobsPerPlayer = builder
          .defineInRange("modMekanismadditionsMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modMekanismadditionsMaxHostileMobsPerWorld =
          builder.defineInRange("modMekanismadditionsMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Mowzie's Mobs");
      modMowziesMobsEnabled = builder.define("modMowziesMobsEnabled", true);
      modMowziesMobsMaxNeutralMobsPerPlayer =
          builder.defineInRange("modMowziesMobsMaxNeutralMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modMowziesMobsMaxNeutralMobsPerWorld =
          builder.defineInRange("modMowziesMobsMaxNeutralMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modMowziesMobsMaxHostileMobsPerPlayer =
          builder.defineInRange("modMowziesMobsMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modMowziesMobsMaxHostileMobsPerWorld =
          builder.defineInRange("modMowziesMobsMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modMowziesMobsMaxBossesPerPlayer =
          builder.defineInRange("modMowziesMobsMaxBossesPerPlayer", 1, 1, PER_PLAYER_MAX);
      modMowziesMobsMaxBossesPerWorld =
          builder.defineInRange("modMowziesMobsMaxBossesPerWorld", 8, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Mutant Beasts");
      modMutantBeastsEnabled = builder.define("modMutantBeastsEnabled", true);
      modMutantBeastsMaxHostileMobsPerPlayer =
          builder.defineInRange("modMutantBeastsMaxHostileMobsPerPlayer", 1, 1, PER_PLAYER_MAX);
      modMutantBeastsMaxHostileMobsPerWorld =
          builder.defineInRange("modMutantBeastsMaxHostileMobsPerWorld", 8, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Quark");
      modQuarkEnabled = builder.define("modQuarkEnabled", true);
      modQuarkMaxPassiveMobsPerPlayer =
          builder.defineInRange("modQuarkMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modQuarkMaxPassiveMobsPerWorld =
          builder.defineInRange("modQuarkMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modQuarkMaxNeutralMobsPerPlayer =
          builder.defineInRange("modQuarkMaxNeutralMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modQuarkMaxNeutralMobsPerWorld =
          builder.defineInRange("modQuarkMaxNeutralMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modQuarkMaxHostileMobsPerPlayer =
          builder.defineInRange("modQuarkMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modQuarkMaxHostileMobsPerWorld =
          builder.defineInRange("modQuarkMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Rats");
      modRatsEnabled = builder.define("modRatsEnabled", true);
      modRatsMaxPassiveMobsPerPlayer =
          builder.defineInRange("modRatsMaxPassiveMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      modRatsMaxPassiveMobsPerWorld =
          builder.defineInRange("modRatsMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modRatsMaxHostileMobsPerPlayer =
          builder.defineInRange("modRatsMaxHostileMobsPerPlayer", 3, 1, PER_PLAYER_MAX);
      modRatsMaxHostileMobsPerWorld =
          builder.defineInRange("modRatsMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modRatsMaxBossesPerPlayer =
          builder.defineInRange("modRatsMaxBossesPerPlayer", 2, 1, PER_PLAYER_MAX);
      modRatsMaxBossesPerWorld =
          builder.defineInRange("modRatsMaxBossesPerWorld", 8, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Savage & Ravage");
      modSavageandravageEnabled = builder.define("modSavageandravageEnabled", true);
      modSavageandravageMaxHostileMobsPerPlayer =
          builder.defineInRange("modSavageandravageMaxHostileMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      modSavageandravageMaxHostileMobsPerWorld =
          builder.defineInRange("modSavageandravageMaxHostileMobsPerWorld", 10, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Statues");
      modStatuesEnabled = builder.define(" modStatuesEnabled", true);
      modStatuesMaxPassiveMobsPerPlayer =
          builder.defineInRange("modStatuesMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modStatuesMaxPassiveMobsPerWorld =
          builder.defineInRange("modStatuesMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Supplementaries");
      modSupplementariesEnabled = builder.define(" modSupplementariesEnabled", true);
      modSupplementariesMaxPassiveMobsPerPlayer =
          builder.defineInRange("modSupplementariesMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modSupplementariesMaxPassiveMobsPerWorld =
          builder.defineInRange("modSupplementariesMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("The Abyss: Chapter II");
      modTheabyssEnabled = builder.define("modTheabyssEnabled", true);
      modTheabyssMaxPassiveMobsPerPlayer =
          builder.defineInRange("modTheabyssMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTheabyssMaxPassiveMobsPerWorld =
          builder.defineInRange("modTheabyssMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modTheabyssMaxHostileMobsPerPlayer =
          builder.defineInRange("modTheabyssMaxHostileMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      modTheabyssMaxHostileMobsPerWorld =
          builder.defineInRange("modTheabyssMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("The Farlanders");
      modTheFarlandersEnabled = builder.define("modTheFarlandersEnabled", true);
      modTheFarlandersMaxPassiveMobsPerPlayer =
          builder.defineInRange("modTheFarlandersMaxPassiveMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      modTheFarlandersMaxPassiveMobsPerWorld =
          builder.defineInRange("modTheFarlandersMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modTheFarlandersMaxNeutralMobsPerPlayer =
          builder.defineInRange("modTheFarlandersMaxNeutralMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTheFarlandersMaxNeutralMobsPerWorld =
          builder.defineInRange("modTheFarlandersMaxNeutralMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modTheFarlandersMaxHostileMobsPerPlayer =
          builder.defineInRange("modTheFarlandersMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTheFarlandersMaxHostileMobsPerWorld =
          builder.defineInRange("modTheFarlandersMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modTheFarlandersMaxBossesPerPlayer =
          builder.defineInRange("modTheFarlandersMaxBossesPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTheFarlandersMaxBossesPerWorld =
          builder.defineInRange("modTheFarlandersMaxBossesPerWorld", 4, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Tinkers Construct");
      modTinkersconstructEnabled = builder.define("modTinkersconstructEnabled", true);
      modTinkersconstructMaxHostileMobsPerPlayer =
          builder.defineInRange("modTinkersconstructMaxHostileMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      modTinkersconstructMaxHostileMobsPerWorld =
          builder.defineInRange("modTinkersconstructMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("The Twilight Forest");
      modTwilightForestEnabled = builder.define("modTwilightForestEnabled", true);
      modTwilightForestMaxPassiveMobsPerPlayer =
          builder.defineInRange("modTwilightForestMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTwilightForestMaxPassiveMobsPerWorld =
          builder.defineInRange("modTwilightForestMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modTwilightForestMaxHostileMobsPerPlayer =
          builder.defineInRange("modTwilightForestMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTwilightForestMaxHostileMobsPerWorld =
          builder.defineInRange("modTwilightForestMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modTwilightForestMaxBossesPerPlayer =
          builder.defineInRange("modTwilightForestMaxBossesPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTwilightForestMaxBossesPerWorld =
          builder.defineInRange("modTwilightForestMaxBossesPerWorld", 8, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Twist");
      modTwistEnabled = builder.define("modTwistEnabled", true);
      modTwistMaxPassiveMobsPerPlayer =
          builder.defineInRange("modTwistMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTwistMaxPassiveMobsPerWorld =
          builder.defineInRange("modTwistMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modTwistMaxNeutralMobsPerPlayer =
          builder.defineInRange("modTwistMaxNeutralMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTwistMaxNeutralMobsPerWorld =
          builder.defineInRange("modTwistMaxNeutralMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modTwistMaxHostileMobsPerPlayer =
          builder.defineInRange("modTwistMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modTwistMaxHostileMobsPerWorld =
          builder.defineInRange("modTwistMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Untamed Wilds");
      modUntamedWildsEnabled = builder.define("modUntamedWildsEnabled", false);
      modUntamedWildsMaxPassiveMobsPerPlayer =
          builder.defineInRange("modUntamedWildsMaxPassiveMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      modUntamedWildsMaxPassiveMobsPerWorld =
          builder.defineInRange("modUntamedWildsMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modUntamedWildsMaxNeutralMobsPerPlayer =
          builder.defineInRange("modUntamedWildsMaxNeutralMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      modUntamedWildsMaxNeutralMobsPerWorld =
          builder.defineInRange("modUntamedWildsMaxNeutralMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modUntamedWildsMaxHostileMobsPerPlayer =
          builder.defineInRange("modUntamedWildsMaxHostileMobsPerPlayer", 4, 1, PER_PLAYER_MAX);
      modUntamedWildsMaxHostileMobsPerWorld =
          builder.defineInRange("modUntamedWildsMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      builder.pop();

      builder.push("Whisperwoods");
      modWhisperwoodsEnabled = builder.define("modWhisperwoodsEnabled", true);
      modWhisperwoodsMaxPassiveMobsPerPlayer =
          builder.defineInRange("modWhisperwoodsMaxPassiveMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modWhisperwoodsMaxPassiveMobsPerWorld =
          builder.defineInRange("modWhisperwoodsMaxPassiveMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modWhisperwoodsMaxHostileMobsPerPlayer =
          builder.defineInRange("modWhisperwoodsMaxHostileMobsPerPlayer", 2, 1, PER_PLAYER_MAX);
      modWhisperwoodsMaxHostileMobsPerWorld =
          builder.defineInRange("modWhisperwoodsMaxHostileMobsPerWorld", 16, 1, PER_WORLD_MAX);
      modWhisperwoodsMaxBossesPerPlayer =
          builder.defineInRange("modWhisperwoodsMaxBossesPerPlayer", 1, 1, PER_PLAYER_MAX);
      modWhisperwoodsMaxBossesPerWorld =
          builder.defineInRange("modWhisperwoodsMaxBossesPerWorld", 4, 1, PER_WORLD_MAX);
      builder.pop();

      builder.pop();

      builder.push("Experimental");
      burnCreeperDuringDaylight = builder.comment("Burn Crepper during day light.")
          .define("burnCreeperDuringDaylight", false);
      builder.pop();

      builder.push("Debug");
      logLevel = builder.comment("Changed the default log level to get more output.")
          .define("logLevel", "info");
      logServerLoad = builder.comment("Enable/Disable logging of the server/world load.")
          .define("logServerLoad", true);
      builder.pop();
    }
  }

  @SubscribeEvent
  public static void handleModConfigLoadEvent(ModConfig.Loading event) {
    ModConfig config = event.getConfig();
    if (config.getSpec() != commonSpec) {
      return;
    }
    log.info("Loaded common config file {} ...", config.getFileName());
  }

  @SubscribeEvent
  public static void handleModConfigReloadEvent(ModConfig.Reloading event) {
    ModConfig config = event.getConfig();
    if (config.getSpec() != commonSpec) {
      return;
    }
    log.info("Reload common config file {} ...", config.getFileName());
    log.warn("Changed values are not considered until the next server restart!");
    MinecraftForge.EVENT_BUS.post(new CommonConfigReloadEvent(CommonConfig.COMMON));
  }

}
