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

import java.nio.file.Files;
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
      Files.createDirectories(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID));
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "/spawn/MinecraftSpawn.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue enabled;
    public final ForgeConfigSpec.ConfigValue<String> id;

    public final ForgeConfigSpec.IntValue passiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue passiveMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> passiveMobsList;

    public final ForgeConfigSpec.IntValue neutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue neutralMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> neutralMobsList;

    public final ForgeConfigSpec.IntValue hostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue hostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> hostileMobsList;

    public final ForgeConfigSpec.IntValue waterPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue waterPassiveMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> waterPassiveMobsList;

    public final ForgeConfigSpec.IntValue waterNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue waterNeutralMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> waterNeutralMobsList;

    public final ForgeConfigSpec.IntValue waterHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue waterHostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> waterHostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Minecraft Spawn Config");
      enabled = builder.define("Enabled", true);
      id = builder.define("Id", "minecraft");

      passiveMobsPerPlayer = builder.comment("e.g. mobs which will not attack you")
          .defineInRange("MaxPassiveMobsPerPlayer", 8, 1, 64);
      passiveMobsPerWorld = builder.defineInRange("MaxPassiveMobsPerWorld", 32, 1, 512);
      passiveMobsList = builder.comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
          .define("PassiveMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:allay",
            "minecraft:axolotl",
            "minecraft:bat",
            "minecraft:cat",
            "minecraft:chicken",
            "minecraft:cow",
            "minecraft:donkey",
            "minecraft:fox",
            "minecraft:frog",
            "minecraft:horse",
            "minecraft:mooshroom",
            "minecraft:mule",
            "minecraft:ocelot",
            "minecraft:parrot",
            "minecraft:pig",
            "minecraft:rabbit",
            "minecraft:sheep",
            "minecraft:skeleton_horse",
            "minecraft:snow_golem",
            "minecraft:turtle"
          // @formatter:on
          )));

      neutralMobsPerPlayer =
          builder.comment("e.g. mobs which will attack under certain conditions ...")
              .defineInRange("MaxNeutralMobsPerPlayer", 8, 1, 64);
      neutralMobsPerWorld = builder.defineInRange("MaxNeutralMobsPerWorld", 32, 1, 512);
      neutralMobsList = builder.comment(Constants.CONFIG_LIST_NEUTRAL_MOBS)
          .define("NeutralMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:bee",
            "minecraft:cave_spider",
            "minecraft:enderman",
            "minecraft:goat",
            "minecraft:iron_golem",
            "minecraft:llama",
            "minecraft:panda",
            "minecraft:polar_bear",
            "minecraft:spider",
            "minecraft:trader_llama",
            "minecraft:wandering_trader",
            "minecraft:wolf",
            "minecraft:zombie_horse"
          // @formatter:on
          )));

      hostileMobsPerPlayer = builder.comment("e.g. mobs which will always attack yon ...")
          .defineInRange("MaxHostileMobsPerPlayer", 8, 1, 64);
      hostileMobsPerWorld = builder.defineInRange("MaxHostileMobsPerWorld", 32, 1, 512);
      hostileMobsList = builder.comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
          .define("HostileMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:chicken_jockey",
            "minecraft:creeper",
            "minecraft:ender_dragon",
            "minecraft:endermite",
            "minecraft:evoker",
            "minecraft:giant",
            "minecraft:husk",
            "minecraft:illusioner",
            "minecraft:phantom",
            "minecraft:pillager",
            "minecraft:ravager",
            "minecraft:ravager_jockey",
            "minecraft:shulker",
            "minecraft:silverfish",
            "minecraft:skeleton",
            "minecraft:skeleton_horseman",
            "minecraft:slime",
            "minecraft:stray",
            "minecraft:vex",
            "minecraft:vindicator",
            "minecraft:warden",
            "minecraft:witch",
            "minecraft:wither",
            "minecraft:zoglin",
            "minecraft:zombie",
            "minecraft:zombie_villager"
          // @formatter:on
          )));

      waterPassiveMobsPerPlayer = builder.comment("e.g. mostly fish")
          .defineInRange("MaxWaterPassiveMobsPerPlayer", 8, 1, 64);
      waterPassiveMobsPerWorld = builder.defineInRange("MaxWaterPassiveMobsPerWorld", 32, 1, 512);
      waterPassiveMobsList = builder.comment("List of passive water Mobs to optimize")
          .define("WaterPassiveMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:axolotl",
            "minecraft:cod",
            "minecraft:pufferfish",
            "minecraft:salmon",
            "minecraft:tropical_fish"
          // @formatter:on
          )));

      waterNeutralMobsPerPlayer = builder.comment("e.g. squid, dolphin, ...")
          .defineInRange("MaxWaterNeutralMobsPerPlayer", 6, 1, 64);
      waterNeutralMobsPerWorld = builder.defineInRange("MaxWaterNeutralMobsPerWorld", 24, 1, 512);
      waterNeutralMobsList = builder.comment("List of neutral water Mobs to optimize")
          .define("WaterNeutralMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "minecraft:dolphin",
            "minecraft:squid"
          // @formatter:on
          )));

      waterHostileMobsPerPlayer = builder.comment("e.g. drowned, guardian...")
          .defineInRange("MaxWaterHostileMobsPerPlayer", 16, 1, 64);
      waterHostileMobsPerWorld = builder.defineInRange("MaxWaterHostileMobsPerWorld", 64, 1, 512);
      waterHostileMobsList = builder.comment("List of hostile water Mobs to optimize")
          .define("WaterHostileMobsList", new ArrayList<String>(Arrays.asList(
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
