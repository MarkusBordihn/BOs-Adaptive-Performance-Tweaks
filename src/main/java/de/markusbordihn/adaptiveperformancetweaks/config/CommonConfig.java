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
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;

import de.markusbordihn.adaptiveperformancetweaks.Constants;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public final class CommonConfig {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

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
    public final ForgeConfigSpec.BooleanValue optimizePassiveMobs;
    public final ForgeConfigSpec.BooleanValue optimizeNeutralMobs;
    public final ForgeConfigSpec.BooleanValue optimizeHostileMobs;
    public final ForgeConfigSpec.BooleanValue optimizeBossMobs;

    public final ForgeConfigSpec.IntValue maxNumberOfItems;
    public final ForgeConfigSpec.IntValue maxNumberOfItemsPerType;

    public final ForgeConfigSpec.ConfigValue<String> logLevel;

    public final ForgeConfigSpec.IntValue viewAreaXFactor;
    public final ForgeConfigSpec.IntValue viewAreaYFactor;
    public final ForgeConfigSpec.IntValue viewAreaZFactor;
    public final ForgeConfigSpec.DoubleValue viewAreaDistanceFactor;

    public final ForgeConfigSpec.IntValue viewDistanceMin;
    public final ForgeConfigSpec.IntValue viewDistanceMax;
    public final ForgeConfigSpec.IntValue viewDistanceDefault;

    public final ForgeConfigSpec.BooleanValue gameruleEnabled;
    public final ForgeConfigSpec.IntValue maxEntityCramming;
    public final ForgeConfigSpec.IntValue randomTickSpeed;

    public final ForgeConfigSpec.IntValue maxEntityPerPlayer;
    public final ForgeConfigSpec.IntValue maxEntityPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> spawnAllowList;
    public final ForgeConfigSpec.ConfigValue<List<String>> spawnDenyList;

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

      builder.push("Limits");
      maxNumberOfItemsPerType = builder.comment(
          "Defines the max. number of items per type which are allowed to lay around in the world.")
          .defineInRange("maxNumberOfItemsPerType", 16, 10, 100);
      maxNumberOfItems = builder
          .comment(
              "Defines the max. number of items which are allowed to lay around in a single world.")
          .defineInRange("maxNumberOfItems", 32, 10, 1000);
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
          .defineInRange("viewAreaDistanceFactor", 0.75, 0.1, 1);
      builder.pop();

      builder.comment("View / render distance optimization.").push("View Distance");
      viewDistanceMax = builder.defineInRange("viewDistanceMax", 16, 2, 32);
      viewDistanceMin = builder.defineInRange("viewDistanceMin", 2, 2, 16);
      viewDistanceDefault = builder.defineInRange("viewDistanceDefault", 8, 2, 16);
      builder.pop();

      builder.push("Gamerule");
      gameruleEnabled = builder.comment("Enable/Disable dynamic gamerule adjustments.")
          .define("gameruleEnabled", true);
      maxEntityCramming =
          builder.comment("Defines the max. numbers of mob singularity on a single block.")
              .defineInRange("maxEntityCramming", 24, 3, 64);
      randomTickSpeed =
          builder.comment("Defines the speed of crops grow, leaf decay, redstone, ... per chunk")
              .defineInRange("randomTickSpeed", 3, 1, 256);
      builder.pop();

      builder.comment(StringUtils.join(Arrays.asList("Spawn optimization",
          "Playing with difficulty HARD could exceed the max number of hostile entity from the ",
          "config file by 1.5x.",
          "These settings affects all mobs which are not covered by other parts of this config"),
          System.lineSeparator())).push("Spawn");
      maxEntityPerPlayer =
          builder.comment("Defines the max. number of a mobs per entity type within player radius.")
              .defineInRange("maxEntityPerPlayer", 8, 1, 16);
      maxEntityPerWorld =
          builder.comment("Defines the max. number of mobs per entity type within a world.")
              .defineInRange("maxEntityPerWorld", 40, 1, 128);
      spawnAllowList = builder.comment(
          "General allow list for spawn entities (e.g. minecraft:squid) which should be ignored for optimization.")
          .define("allowList", new ArrayList<String>(Arrays.asList("")));
      spawnDenyList = builder.comment(
          "General deny list for spawn entities (e.g. minecraft:bat) to no longer spawn in all worlds.")
          .define("denyList", new ArrayList<String>(Arrays.asList("")));

      builder.push("Minecraft");
      minecraftEnabled = builder.define("minecraftEnabled", true);
      minecraftMaxPassiveMobsPerPlayer = builder.comment("e.g. mostly bats")
          .defineInRange("minecraftMaxPassiveMobsPerPlayer", 1, 1, 16);
      minecraftMaxPassiveMobsPerWorld =
          builder.defineInRange("minecraftMaxPassiveMobsPerWorld", 6, 1, 128);
      minecraftMaxNeutralMobsPerPlayer = builder.comment("e.g. sheep, pig, horse, fox, ...")
          .defineInRange("minecraftMaxNeutralMobsPerPlayer", 4, 1, 16);
      minecraftMaxNeutralMobsPerWorld =
          builder.defineInRange("minecraftMaxNeutralMobsPerWorld", 20, 1, 128);
      minecraftMaxHostileMobsPerPlayer = builder.comment("e.g. slime, spider, zombie, ravager, ...")
          .defineInRange("minecraftMaxHostileMobsPerPlayer", 2, 1, 16);
      minecraftMaxHostileMobsPerWorld =
          builder.defineInRange("minecraftMaxHostileMobsPerWorld", 10, 1, 128);
      minecraftMaxWaterPassiveMobsPerPlayer = builder.comment("e.g. mostly fish")
          .defineInRange("minecraftMaxWaterPassiveMobsPerPlayer", 2, 1, 16);
      minecraftMaxWaterPassiveMobsPerWorld =
          builder.defineInRange("minecraftMaxWaterPassiveMobsPerWorld", 10, 1, 128);
      minecraftMaxWaterNeutralMobsPerPlayer = builder.comment("e.g. squid, dolphin, ...")
          .defineInRange("minecraftMaxWaterNeutralMobsPerPlayer", 2, 1, 16);
      minecraftMaxWaterNeutralMobsPerWorld =
          builder.defineInRange("minecraftMaxWaterNeutralMobsPerWorld", 10, 1, 128);
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
          builder.defineInRange("modAlexmobsMaxPassiveMobsPerPlayer", 2, 1, 16);
      modAlexmobsMaxPassiveMobsPerWorld =
          builder.defineInRange("modAlexmobsMaxPassiveMobsPerWorld", 8, 1, 128);
      modAlexmobsMaxNeutralMobsPerPlayer =
          builder.defineInRange("modAlexmobsMaxNeutralMobsPerPlayer", 2, 1, 16);
      modAlexmobsMaxNeutralMobsPerWorld =
          builder.defineInRange("modAlexmobsMaxNeutralMobsPerWorld", 8, 1, 128);
      modAlexmobsMaxHostileMobsPerPlayer =
          builder.defineInRange("modAlexmobsMaxHostileMobsPerPlayer", 1, 1, 16);
      modAlexmobsMaxHostileMobsPerWorld =
          builder.defineInRange("modAlexmobsMaxHostileMobsPerWorld", 10, 1, 128);
      builder.pop();

      builder.push("Aquaculture 2");
      modAquacultureEnabled = builder.define("modAquacultureEnabled", true);
      modAquacultureMaxFishPerPlayer =
          builder.defineInRange("modAquacultureMaxFishPerPlayer", 1, 1, 16);
      modAquacultureMaxFishPerWorld =
          builder.defineInRange("modAquacultureMaxFishPerWorld", 8, 1, 128);
      builder.pop();

      builder.push("Artifacts Mod");
      modArtifactsEnabled = builder.define("modArtifactsEnabled", true);
      modArtifactsMaxHostileMobsPerPlayer =
          builder.defineInRange("modArtifactsMaxHostileMobsPerPlayer", 2, 1, 16);
      modArtifactsMaxHostileMobsPerWorld =
          builder.defineInRange("modArtifactsMaxHostileMobsPerWorld", 12, 1, 128);
      builder.pop();

      builder.push("Dungeons Mod");
      modDungeonsmodEnabled = builder.define("modDungeonsmodEnabled", true);
      modDungeonsmodOptimizeWhirlwind = builder.define("modDungeonsmodOptimizeWhirlwind", true);
      modDungeonsmodMaxHostileMobsPerPlayer =
          builder.defineInRange("modDungeonsmodMaxHostileMobsPerPlayer", 4, 1, 16);
      modDungeonsmodMaxHostileMobsPerWorld =
          builder.defineInRange("modDungeonsmodMaxHostileMobsPerWorld", 12, 1, 128);
      modDungeonsmodMaxBossesPerPlayer =
          builder.defineInRange("modDungeonsmodMaxBossesPerPlayer", 2, 1, 16);
      modDungeonsmodMaxBossesPerWorld =
          builder.defineInRange("modDungeonsmodMaxBossesPerWorld", 4, 1, 128);
      builder.pop();

      builder.push("Ice and Fire: Dragons");
      modIceandfireEnabled = builder.define("modIceandfireEnabled", true);
      modIceandfireMaxPassiveMobsPerPlayer =
          builder.defineInRange("modIceandfireMaxPassiveMobsPerPlayer", 2, 1, 16);
      modIceandfireMaxPassiveMobsPerWorld =
          builder.defineInRange("modIceandfireMaxPassiveMobsPerWorld", 10, 1, 128);
      modIceandfireMaxNeutralMobsPerPlayer =
          builder.defineInRange("modIceandfireMaxNeutralMobsPerPlayer", 1, 1, 16);
      modIceandfireMaxNeutralMobsPerWorld =
          builder.defineInRange("modIceandfireMaxNeutralMobsPerWorld", 10, 1, 128);
      modIceandfireMaxHostileMobsPerPlayer =
          builder.defineInRange("modIceandfireMaxHostileMobsPerPlayer", 2, 1, 16);
      modIceandfireMaxHostileMobsPerWorld =
          builder.defineInRange("modIceandfireMaxHostileMobsPerWorld", 10, 1, 128);
      modIceandfireMaxBossesPerPlayer =
          builder.defineInRange("modIceandfireMaxBossesPerPlayer", 4, 1, 16);
      modIceandfireMaxBossesPerWorld =
          builder.defineInRange("modIceandfireMaxBossesPerWorld", 20, 1, 128);
      builder.pop();

      builder.push("Mekanism Additions");
      modMekanismadditionsEnabled = builder.define("modMekanismadditionsEnabled", true);
      modMekanismadditionsMaxHostileMobsPerPlayer =
          builder.defineInRange("modMekanismadditionsMaxHostileMobsPerPlayer", 2, 1, 16);
      modMekanismadditionsMaxHostileMobsPerWorld =
          builder.defineInRange("modMekanismadditionsMaxHostileMobsPerWorld", 10, 1, 128);
      builder.pop();

      builder.push("Mowzie's Mobs");
      modMowziesMobsEnabled = builder.define("modMowziesMobsEnabled", true);
      modMowziesMobsMaxNeutralMobsPerPlayer =
          builder.defineInRange("modMowziesMobsMaxNeutralMobsPerPlayer", 2, 1, 16);
      modMowziesMobsMaxNeutralMobsPerWorld =
          builder.defineInRange("modMowziesMobsMaxNeutralMobsPerWorld", 12, 1, 128);
      modMowziesMobsMaxHostileMobsPerPlayer =
          builder.defineInRange("modMowziesMobsMaxHostileMobsPerPlayer", 2, 1, 16);
      modMowziesMobsMaxHostileMobsPerWorld =
          builder.defineInRange("modMowziesMobsMaxHostileMobsPerWorld", 12, 1, 128);
      modMowziesMobsMaxBossesPerPlayer =
          builder.defineInRange("modMowziesMobsMaxBossesPerPlayer", 1, 1, 16);
      modMowziesMobsMaxBossesPerWorld =
          builder.defineInRange("modMowziesMobsMaxBossesPerWorld", 12, 1, 128);
      builder.pop();

      builder.push("Quark");
      modQuarkEnabled = builder.define("modQuarkEnabled", true);
      modQuarkMaxPassiveMobsPerPlayer =
          builder.defineInRange("modQuarkMaxPassiveMobsPerPlayer", 2, 1, 16);
      modQuarkMaxPassiveMobsPerWorld =
          builder.defineInRange("modQuarkMaxPassiveMobsPerWorld", 10, 1, 128);
      modQuarkMaxNeutralMobsPerPlayer =
          builder.defineInRange("modQuarkMaxNeutralMobsPerPlayer", 1, 1, 16);
      modQuarkMaxNeutralMobsPerWorld =
          builder.defineInRange("modQuarkMaxNeutralMobsPerWorld", 10, 1, 128);
      modQuarkMaxHostileMobsPerPlayer =
          builder.defineInRange("modQuarkMaxHostileMobsPerPlayer", 2, 1, 16);
      modQuarkMaxHostileMobsPerWorld =
          builder.defineInRange("modQuarkMaxHostileMobsPerWorld", 10, 1, 128);
      builder.pop();

      builder.push("Rats");
      modRatsEnabled = builder.define(" modRatsEnabled", true);
      modRatsMaxPassiveMobsPerPlayer =
          builder.defineInRange("modRatsMaxPassiveMobsPerPlayer", 4, 1, 16);
      modRatsMaxPassiveMobsPerWorld =
          builder.defineInRange("modRatsMaxPassiveMobsPerWorld", 16, 1, 128);
      modRatsMaxHostileMobsPerPlayer =
          builder.defineInRange("modRatsMaxHostileMobsPerPlayer", 3, 1, 16);
      modRatsMaxHostileMobsPerWorld =
          builder.defineInRange("modRatsMaxHostileMobsPerWorld", 16, 1, 128);
      modRatsMaxBossesPerPlayer = builder.defineInRange("modRatsMaxBossesPerPlayer", 2, 1, 16);
      modRatsMaxBossesPerWorld = builder.defineInRange("modRatsMaxBossesPerWorld", 10, 1, 128);
      builder.pop();

      builder.push("Savage & Ravage");
      modSavageandravageEnabled = builder.define("modSavageandravageEnabled", true);
      modSavageandravageMaxHostileMobsPerPlayer =
          builder.defineInRange("modSavageandravageMaxHostileMobsPerPlayer", 4, 1, 16);
      modSavageandravageMaxHostileMobsPerWorld =
          builder.defineInRange("modSavageandravageMaxHostileMobsPerWorld", 10, 1, 128);
      builder.pop();

      builder.push("Statues");
      modStatuesEnabled = builder.define(" modStatuesEnabled", true);
      modStatuesMaxPassiveMobsPerPlayer =
          builder.defineInRange("modStatuesMaxPassiveMobsPerPlayer", 2, 1, 16);
      modStatuesMaxPassiveMobsPerWorld =
          builder.defineInRange("modStatuesMaxPassiveMobsPerWorld", 16, 1, 128);
      builder.pop();

      builder.push("Supplementaries");
      modSupplementariesEnabled = builder.define(" modSupplementariesEnabled", true);
      modSupplementariesMaxPassiveMobsPerPlayer =
          builder.defineInRange("modSupplementariesMaxPassiveMobsPerPlayer", 2, 1, 16);
      modSupplementariesMaxPassiveMobsPerWorld =
          builder.defineInRange("modSupplementariesMaxPassiveMobsPerWorld", 16, 1, 128);
      builder.pop();

      builder.push("The Abyss: Chapter II");
      modTheabyssEnabled = builder.define("modTheabyssEnabled", true);
      modTheabyssMaxPassiveMobsPerPlayer =
          builder.defineInRange("modTheabyssMaxPassiveMobsPerPlayer", 2, 1, 16);
      modTheabyssMaxPassiveMobsPerWorld =
          builder.defineInRange("modTheabyssMaxPassiveMobsPerWorld", 10, 1, 128);
      modTheabyssMaxHostileMobsPerPlayer =
          builder.defineInRange("modTheabyssMaxHostileMobsPerPlayer", 4, 1, 16);
      modTheabyssMaxHostileMobsPerWorld =
          builder.defineInRange("modTheabyssMaxHostileMobsPerWorld", 10, 1, 128);
      builder.pop();

      builder.push("Tinkers Construct");
      modTinkersconstructEnabled = builder.define("modTinkersconstructEnabled", true);
      modTinkersconstructMaxHostileMobsPerPlayer =
          builder.defineInRange("modTinkersconstructMaxHostileMobsPerPlayer", 4, 1, 16);
      modTinkersconstructMaxHostileMobsPerWorld =
          builder.defineInRange("modTinkersconstructMaxHostileMobsPerWorld", 16, 1, 128);
      builder.pop();

      builder.push("The Twilight Forest");
      modTwilightForestEnabled = builder.define("modTwilightForestEnabled", true);
      modTwilightForestMaxPassiveMobsPerPlayer =
          builder.defineInRange("modTwilightForestMaxPassiveMobsPerPlayer", 2, 1, 16);
      modTwilightForestMaxPassiveMobsPerWorld =
          builder.defineInRange("modTwilightForestMaxPassiveMobsPerWorld", 10, 1, 128);
      modTwilightForestMaxHostileMobsPerPlayer =
          builder.defineInRange("modTwilightForestMaxHostileMobsPerPlayer", 2, 1, 16);
      modTwilightForestMaxHostileMobsPerWorld =
          builder.defineInRange("modTwilightForestMaxHostileMobsPerWorld", 10, 1, 128);
      modTwilightForestMaxBossesPerPlayer =
          builder.defineInRange("modTwilightForestMaxBossesPerPlayer", 2, 1, 16);
      modTwilightForestMaxBossesPerWorld =
          builder.defineInRange("modTwilightForestMaxBossesPerWorld", 8, 1, 128);
      builder.pop();

      builder.pop();

      builder.push("Experimental");
      burnCreeperDuringDaylight = builder.comment("Burn Crepper during day light.")
          .define("burnCreeperDuringDaylight", false);
      builder.pop();

      builder.push("Debug");
      logLevel = builder.comment("Changed the default log level to get more output.")
          .define("logLevel", "info");
      builder.pop();
    }
  }

  @SubscribeEvent
  public static void handleModConfigLoadEvent(ModConfig.Loading event) {
    log.info("Loaded common config file {} ...", event.getConfig().getFileName());
  }

  @SubscribeEvent
  public static void handleModConfigReloadEvent(ModConfig.Reloading event) {
    log.info("Reloaded common config file {} ...", event.getConfig().getFileName());
  }
}
