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

  private CommonConfig() {
  }

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

    public final ForgeConfigSpec.BooleanValue modAquacultureEnabled;
    public final ForgeConfigSpec.IntValue modAquacultureMaxFishPerPlayer;
    public final ForgeConfigSpec.IntValue modAquacultureMaxFishPerWorld;

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

    public final ForgeConfigSpec.BooleanValue modQuarkEnabled;
    public final ForgeConfigSpec.IntValue modQuarkMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modQuarkMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modQuarkMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modQuarkMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue modQuarkMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modQuarkMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modSavageandravageEnabled;
    public final ForgeConfigSpec.IntValue modSavageandravageMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modSavageandravageMaxHostileMobsPerWorld;

    public final ForgeConfigSpec.BooleanValue modTheabyssEnabled;
    public final ForgeConfigSpec.IntValue modTheabyssMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTheabyssMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue modTheabyssMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue modTheabyssMaxHostileMobsPerWorld;

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
      builder.pop();

      builder.push("Limits");
      maxNumberOfItemsPerType = builder.comment(
          "Defines the max. number of items per type which are allowed to lay around in the world.")
          .defineInRange("maxNumberOfItemsPerType", 32, 10, 100);
      maxNumberOfItems = builder
          .comment("Defines the max. number of items which are allowed to lay around in the world.")
          .defineInRange("maxNumberOfItems", 128, 10, 1000);
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
              .defineInRange("maxEntityCramming", 24, 0, 64);
      randomTickSpeed = builder.comment("Defines the speed of crops grow, leaf decay, redstone, ... per chunk")
          .defineInRange("randomTickSpeed", 3, 1, 256);
      builder.pop();

      builder.comment(StringUtils.join(Arrays.asList("Spawn optimization",
          "Playing with difficulty HARD could exceed the max number of hostile entity from the ",
          "config file by 1.5x.",
          "These settings affects all mobs which are not covered by other parts of this config"),
          System.lineSeparator())).push("Spawn");
      maxEntityPerPlayer =
          builder.comment("Defines the max. number of a mobs per entity type within player radius.")
              .defineInRange("maxEntityPerPlayer", 8, 1, 10);
      maxEntityPerWorld =
          builder.comment("Defines the max. number of mobs per entity type within a world.")
              .defineInRange("maxEntityPerWorld", 40, 1, 100);
      spawnAllowList = builder.comment(
          "General allow list for spawn entities (e.g. minecraft:squid) which should be ignored for optimization.")
          .define("allowList", new ArrayList<String>(Arrays.asList("")));
      spawnDenyList = builder.comment(
          "General deny list for spawn entities (e.g. minecraft:bat) to no longer spawn in all worlds.")
          .define("denyList", new ArrayList<String>(Arrays.asList("")));

      builder.push("Minecraft");
      minecraftEnabled = builder.define("minecraftEnabled", true);
      minecraftMaxPassiveMobsPerPlayer = builder.comment("e.g. mostly bats")
          .defineInRange("minecraftMaxPassiveMobsPerPlayer", 1, 1, 10);
      minecraftMaxPassiveMobsPerWorld =
          builder.defineInRange("minecraftMaxPassiveMobsPerWorld", 6, 1, 100);
      minecraftMaxNeutralMobsPerPlayer = builder.comment("e.g. sheep, pig, horse, fox, ...")
          .defineInRange("minecraftMaxNeutralMobsPerPlayer", 4, 1, 10);
      minecraftMaxNeutralMobsPerWorld =
          builder.defineInRange("minecraftMaxNeutralMobsPerWorld", 20, 1, 100);
      minecraftMaxHostileMobsPerPlayer = builder.comment("e.g. slime, spider, zombie, ravager, ...")
          .defineInRange("minecraftMaxHostileMobsPerPlayer", 2, 1, 10);
      minecraftMaxHostileMobsPerWorld =
          builder.defineInRange("minecraftMaxHostileMobsPerWorld", 10, 1, 100);
      minecraftMaxWaterPassiveMobsPerPlayer = builder.comment("e.g. mostly fish")
          .defineInRange("minecraftMaxWaterPassiveMobsPerPlayer", 1, 1, 10);
      minecraftMaxWaterPassiveMobsPerWorld =
          builder.defineInRange("minecraftMaxWaterPassiveMobsPerWorld", 10, 1, 100);
      minecraftMaxWaterNeutralMobsPerPlayer = builder.comment("e.g. squid, dolphin, ...")
          .defineInRange("minecraftMaxWaterNeutralMobsPerPlayer", 2, 1, 10);
      minecraftMaxWaterNeutralMobsPerWorld =
          builder.defineInRange("minecraftMaxWaterNeutralMobsPerWorld", 10, 1, 100);
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

      builder.push("Aquaculture 2");
      modAquacultureEnabled = builder.define("modAquacultureEnabled", true);
      modAquacultureMaxFishPerPlayer =
          builder.defineInRange("modAquacultureMaxFishPerPlayer", 1, 1, 10);
      modAquacultureMaxFishPerWorld =
          builder.defineInRange("modAquacultureMaxFishPerWorld", 8, 1, 100);
      builder.pop();

      builder.push("Ice and Fire: Dragons");
      modIceandfireEnabled = builder.define("modIceandfireEnabled", true);
      modIceandfireMaxPassiveMobsPerPlayer =
          builder.defineInRange("modIceandfireMaxPassiveMobsPerPlayer", 2, 1, 10);
      modIceandfireMaxPassiveMobsPerWorld =
          builder.defineInRange("modIceandfireMaxPassiveMobsPerWorld", 10, 1, 100);
      modIceandfireMaxNeutralMobsPerPlayer =
          builder.defineInRange("modIceandfireMaxNeutralMobsPerPlayer", 1, 1, 10);
      modIceandfireMaxNeutralMobsPerWorld =
          builder.defineInRange("modIceandfireMaxNeutralMobsPerWorld", 10, 1, 100);
      modIceandfireMaxHostileMobsPerPlayer =
          builder.defineInRange("modIceandfireMaxHostileMobsPerPlayer", 2, 1, 10);
      modIceandfireMaxHostileMobsPerWorld =
          builder.defineInRange("modIceandfireMaxHostileMobsPerWorld", 10, 1, 100);
      modIceandfireMaxBossesPerPlayer =
          builder.defineInRange("modIceandfireMaxBossesPerPlayer", 4, 1, 10);
      modIceandfireMaxBossesPerWorld =
          builder.defineInRange("modIceandfireMaxBossesPerWorld", 20, 1, 100);
      builder.pop();

      builder.push("Mekanism Additions");
      modMekanismadditionsEnabled = builder.define("modMekanismadditionsEnabled", true);
      modMekanismadditionsMaxHostileMobsPerPlayer =
          builder.defineInRange("modMekanismadditionsMaxHostileMobsPerPlayer", 2, 1, 10);
      modMekanismadditionsMaxHostileMobsPerWorld =
          builder.defineInRange("modMekanismadditionsMaxHostileMobsPerWorld", 10, 1, 100);
      builder.pop();

      builder.push("Quark");
      modQuarkEnabled = builder.define("modQuarkEnabled", true);
      modQuarkMaxPassiveMobsPerPlayer =
          builder.defineInRange("modQuarkMaxPassiveMobsPerPlayer", 2, 1, 10);
      modQuarkMaxPassiveMobsPerWorld =
          builder.defineInRange("modQuarkMaxPassiveMobsPerWorld", 10, 1, 100);
      modQuarkMaxNeutralMobsPerPlayer =
          builder.defineInRange("modQuarkMaxNeutralMobsPerPlayer", 1, 1, 10);
      modQuarkMaxNeutralMobsPerWorld =
          builder.defineInRange("modQuarkMaxNeutralMobsPerWorld", 10, 1, 100);
      modQuarkMaxHostileMobsPerPlayer =
          builder.defineInRange("modQuarkMaxHostileMobsPerPlayer", 2, 1, 10);
      modQuarkMaxHostileMobsPerWorld =
          builder.defineInRange("modQuarkMaxHostileMobsPerWorld", 10, 1, 100);
      builder.pop();

      builder.push("Savage & Ravage");
      modSavageandravageEnabled = builder.define("modSavageandravageEnabled", true);
      modSavageandravageMaxHostileMobsPerPlayer =
          builder.defineInRange("modSavageandravageMaxHostileMobsPerPlayer", 4, 1, 10);
      modSavageandravageMaxHostileMobsPerWorld =
          builder.defineInRange("modSavageandravageMaxHostileMobsPerWorld", 10, 1, 100);
      builder.pop();

      builder.push("The Abyss: Chapter II");
      modTheabyssEnabled = builder.define("modTheabyssEnabled", true);
      modTheabyssMaxPassiveMobsPerPlayer =
          builder.defineInRange("modTheabyssMaxPassiveMobsPerPlayer", 2, 1, 10);
      modTheabyssMaxPassiveMobsPerWorld =
          builder.defineInRange("modTheabyssMaxPassiveMobsPerWorld", 10, 1, 100);
      modTheabyssMaxHostileMobsPerPlayer =
          builder.defineInRange("modTheabyssMaxHostileMobsPerPlayer", 4, 1, 10);
      modTheabyssMaxHostileMobsPerWorld =
          builder.defineInRange("modTheabyssMaxHostileMobsPerWorld", 10, 1, 100);
      builder.pop();

      builder.pop();

      builder.push("Debug");
      logLevel = builder.comment("Changed the default log level to get more output.")
          .define("logLevel", "info");
      builder.pop();
    }
  }

  @SubscribeEvent
  public static void handleModConfigLoadEvent(ModConfig.Loading event) {
    log.info("Loaded common config file {}", event.getConfig().getFileName());
  }

  @SubscribeEvent
  public static void handleModConfigReloadEvent(ModConfig.Reloading event) {
    log.info("Reloaded common config file {}", event.getConfig().getFileName());
  }
}
