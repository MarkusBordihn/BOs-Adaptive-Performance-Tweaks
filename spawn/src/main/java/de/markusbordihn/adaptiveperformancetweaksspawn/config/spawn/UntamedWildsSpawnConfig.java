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
public final class UntamedWildsSpawnConfig {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private UntamedWildsSpawnConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} {} spawn config ...", Constants.MOD_NAME,
        CoreConstants.UNTAMED_WILDS_NAME);
    FileUtils.getOrCreateDirectory(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID),
        CoreConstants.CONFIG_ID);
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "/spawn/UntamedWildsSpawn.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue untamedWildsEnabled;
    public final ForgeConfigSpec.ConfigValue<String> untamedWildsId;

    public final ForgeConfigSpec.IntValue untamedWildsMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue untamedWildsMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> untamedWildsPassiveMobsList;

    public final ForgeConfigSpec.IntValue untamedWildsMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue untamedWildsMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> untamedWildsNeutralMobsList;

    public final ForgeConfigSpec.IntValue untamedWildsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue untamedWildsMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> untamedWildsHostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Untamed Wilds Spawn Config");
      untamedWildsEnabled = builder.define("untamedWildsEnabled", true);
      untamedWildsId = builder.define("untamedWildsId", CoreConstants.UNTAMED_WILDS_MOD);

      untamedWildsMaxPassiveMobsPerPlayer =
          builder.defineInRange("untamedWildsMaxPassiveMobsPerPlayer", 4, 1, 64);
      untamedWildsMaxPassiveMobsPerWorld =
          builder.defineInRange("untamedWildsMaxPassiveMobsPerWorld", 16, 1, 512);
      untamedWildsPassiveMobsList = builder.comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
          .define("untamedWildsPassiveMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "untamedwilds:arowana",
            "untamedwilds:giant_clam",
            "untamedwilds:giant_salamander",
            "untamedwilds:rhino",
            "untamedwilds:sunfish",
            "untamedwilds:trevally"
          // @formatter:on
          )));

      untamedWildsMaxNeutralMobsPerPlayer =
          builder.defineInRange("untamedWildsMaxNeutralMobsPerPlayer", 4, 1, 64);
      untamedWildsMaxNeutralMobsPerWorld =
          builder.defineInRange("untamedWildsMaxNeutralMobsPerWorld", 16, 1, 512);
      untamedWildsNeutralMobsList = builder.comment(Constants.CONFIG_LIST_NEUTRAL_MOBS)
          .define("untamedWildsNeutralMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "untamedwilds:aardvark",
            "untamedwilds:softshell_turtle",
            "untamedwilds:tortoise"
          // @formatter:on
          )));

      untamedWildsMaxHostileMobsPerPlayer =
          builder.defineInRange("untamedWildsMaxHostileMobsPerPlayer", 4, 1, 64);
      untamedWildsMaxHostileMobsPerWorld =
          builder.defineInRange("untamedWildsMaxHostileMobsPerWorld", 16, 1, 512);
      untamedWildsHostileMobsList = builder.comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
          .define("untamedWildsHostileMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "untamedwilds:bear_black",
            "untamedwilds:bear_blind",
            "untamedwilds:bear_brown",
            "untamedwilds:bear_cave",
            "untamedwilds:bear_panda",
            "untamedwilds:bear_polar",
            "untamedwilds:bear_spectacled",
            "untamedwilds:bear_sun",
            "untamedwilds:bigcat_jaguar",
            "untamedwilds:bigcat_leopard",
            "untamedwilds:bigcat_lion",
            "untamedwilds:bigcat_puma",
            "untamedwilds:bigcat_snow_leopard",
            "untamedwilds:bigcat_tiger",
            "untamedwilds:football_fish",
            "untamedwilds:hippo",
            "untamedwilds:shark",
            "untamedwilds:snake",
            "untamedwilds:tarantula"
          // @formatter:on
          )));

      builder.pop();
    }
  }

}
