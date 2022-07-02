/**
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.loading.FMLPaths;
import net.minecraftforge.fml.loading.FileUtils;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public final class MinecraftSpawnConfig {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private MinecraftSpawnConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} Minecraft Spawn config ...", Constants.MOD_NAME);
    try {
      FileUtils.getOrCreateDirectory(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID),
          CoreConstants.CONFIG_ID);
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "/spawn/MinecraftSpawn.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue minecraftEnabled;
    public final ForgeConfigSpec.ConfigValue<String> minecraftId;

    public final ForgeConfigSpec.IntValue minecraftMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> minecraftPassiveMobsList;

    public final ForgeConfigSpec.IntValue minecraftMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> minecraftNeutralMobsList;

    public final ForgeConfigSpec.IntValue minecraftMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> minecraftHostileMobsList;

    public final ForgeConfigSpec.IntValue minecraftMaxWaterPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxWaterPassiveMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> minecraftWaterPassiveMobsList;

    public final ForgeConfigSpec.IntValue minecraftMaxWaterNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxWaterNeutralMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> minecraftWaterNeutralMobsList;

    public final ForgeConfigSpec.IntValue minecraftMaxWaterHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue minecraftMaxWaterHostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> minecraftWaterHostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Minecraft Spawn Config");
      minecraftEnabled = builder.define("minecraftEnabled", true);
      minecraftId = builder.define("minecraftId", "minecraft");

      minecraftMaxPassiveMobsPerPlayer = builder.comment("e.g. mostly bats")
          .defineInRange("minecraftMaxPassiveMobsPerPlayer", 3, 1, 64);
      minecraftMaxPassiveMobsPerWorld =
          builder.defineInRange("minecraftMaxPassiveMobsPerWorld", 12, 1, 512);
      minecraftPassiveMobsList = builder.comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
          .define("minecraftPassiveMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:bat"
          // @formatter:on
          )));

      minecraftMaxNeutralMobsPerPlayer = builder.comment("e.g. sheep, pig, horse, fox, ...")
          .defineInRange("minecraftMaxNeutralMobsPerPlayer", 8, 1, 64);
      minecraftMaxNeutralMobsPerWorld =
          builder.defineInRange("minecraftMaxNeutralMobsPerWorld", 32, 1, 512);
      minecraftNeutralMobsList = builder.comment(Constants.CONFIG_LIST_NEUTRAL_MOBS)
          .define("minecraftNeutralMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:allay",
            "minecraft:bee",
            "minecraft:cat",
            "minecraft:chicken",
            "minecraft:cow",
            "minecraft:donkey",
            "minecraft:fox",
            "minecraft:goat",
            "minecraft:horse",
            "minecraft:llama",
            "minecraft:mooshroom",
            "minecraft:mule",
            "minecraft:ocelot",
            "minecraft:panda",
            "minecraft:parrot",
            "minecraft:pig",
            "minecraft:polar_bear",
            "minecraft:rabbit",
            "minecraft:sheep",
            "minecraft:skeleton_horse",
            "minecraft:strider",
            "minecraft:trader_llama",
            "minecraft:turtle",
            "minecraft:wandering_trader",
            "minecraft:wolf",
            "minecraft:zombie_horse"
          // @formatter:on
          )));

      minecraftMaxHostileMobsPerPlayer = builder.comment("e.g. slime, spider, zombie, ravager, ...")
          .defineInRange("minecraftMaxHostileMobsPerPlayer", 8, 1, 64);
      minecraftMaxHostileMobsPerWorld =
          builder.defineInRange("minecraftMaxHostileMobsPerWorld", 32, 1, 512);
      minecraftHostileMobsList = builder.comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
          .define("minecraftHostileMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:blaze",
            "minecraft:cave_spider",
            "minecraft:creeper",
            "minecraft:ender_dragon",
            "minecraft:enderman",
            "minecraft:endermite",
            "minecraft:evoker",
            "minecraft:ghast",
            "minecraft:giant",
            "minecraft:hoglin",
            "minecraft:husk",
            "minecraft:illusioner",
            "minecraft:magma_cube",
            "minecraft:phantom",
            "minecraft:piglin",
            "minecraft:piglin_brute",
            "minecraft:pillager",
            "minecraft:ravager",
            "minecraft:shulker",
            "minecraft:silverfish",
            "minecraft:skeleton",
            "minecraft:slime",
            "minecraft:spider",
            "minecraft:stray",
            "minecraft:vex",
            "minecraft:vindicator",
            "minecraft:warden",
            "minecraft:witch",
            "minecraft:wither",
            "minecraft:wither_skeleton",
            "minecraft:zoglin",
            "minecraft:zombie",
            "minecraft:zombie_villager",
            "minecraft:zombified_piglin"
          // @formatter:on
          )));

      minecraftMaxWaterPassiveMobsPerPlayer = builder.comment("e.g. mostly fish")
          .defineInRange("minecraftMaxWaterPassiveMobsPerPlayer", 4, 1, 64);
      minecraftMaxWaterPassiveMobsPerWorld =
          builder.defineInRange("minecraftMaxWaterPassiveMobsPerWorld", 16, 1, 512);
      minecraftWaterPassiveMobsList = builder.comment("List of passive water Mobs to optimize")
          .define("minecraftWaterPassiveMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:axolotl",
            "minecraft:cod",
            "minecraft:pufferfish",
            "minecraft:salmon",
            "minecraft:tropical_fish"
          // @formatter:on
          )));

      minecraftMaxWaterNeutralMobsPerPlayer = builder.comment("e.g. squid, dolphin, ...")
          .defineInRange("minecraftMaxWaterNeutralMobsPerPlayer", 4, 1, 64);
      minecraftMaxWaterNeutralMobsPerWorld =
          builder.defineInRange("minecraftMaxWaterNeutralMobsPerWorld", 16, 1, 512);
      minecraftWaterNeutralMobsList = builder.comment("List of neutral water Mobs to optimize")
          .define("minecraftWaterNeutralMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:dolphin",
            "minecraft:squid"
          // @formatter:on
          )));

      minecraftMaxWaterHostileMobsPerPlayer = builder.comment("e.g. drowned, guardian...")
          .defineInRange("minecraftMaxWaterHostileMobsPerPlayer", 12, 1, 64);
      minecraftMaxWaterHostileMobsPerWorld =
          builder.defineInRange("minecraftMaxWaterHostileMobsPerWorld", 48, 1, 512);
      minecraftWaterHostileMobsList = builder.comment("List of hostile water Mobs to optimize")
          .define("minecraftWaterHostileMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:drowned",
            "minecraft:elder_guardian",
            "minecraft:guardian"
          // @formatter:on
          )));

      builder.pop();
    }
  }

}
