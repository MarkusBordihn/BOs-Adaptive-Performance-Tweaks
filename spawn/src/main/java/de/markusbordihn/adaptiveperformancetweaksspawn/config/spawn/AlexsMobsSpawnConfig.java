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
public final class AlexsMobsSpawnConfig {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private AlexsMobsSpawnConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} {} spawn config ...", Constants.MOD_NAME,
        CoreConstants.ALEXSMOBS_NAME);
    FileUtils.getOrCreateDirectory(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID),
        CoreConstants.CONFIG_ID);
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "/spawn/AlexsMobsSpawn.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue alexsMobsEnabled;
    public final ForgeConfigSpec.ConfigValue<String> alexsMobsId;

    public final ForgeConfigSpec.IntValue alexsMobsMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue alexsMobsMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> alexsMobsPassiveMobsList;

    public final ForgeConfigSpec.IntValue alexsMobsMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue alexsMobsMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> alexsMobsNeutralMobsList;

    public final ForgeConfigSpec.IntValue alexsMobsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue alexsMobsMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> alexsMobsHostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Alexs Mobs Spawn Config");
      alexsMobsEnabled = builder.define("alexsMobsEnabled", true);
      alexsMobsId =
          builder.define("alexsMobsId", CoreConstants.ALEXSMOBS_MOD);

      alexsMobsMaxPassiveMobsPerPlayer =
          builder.defineInRange("alexsMobsMaxPassiveMobsPerPlayer", 2, 1, 64);
      alexsMobsMaxPassiveMobsPerWorld =
          builder.defineInRange("alexsMobsMaxPassiveMobsPerWorld", 8, 1, 512);
      alexsMobsPassiveMobsList = builder.comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
          .define("alexsMobsPassiveMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "alexsmobs:blobfish",
            "alexsmobs:endergrade",
            "alexsmobs:fly",
            "alexsmobs:gazelle",
            "alexsmobs:hummingbird",
            "alexsmobs:mungus",
            "alexsmobs:seal",
            "alexsmobs:spectre",
            "alexsmobs:stradpole",
            "alexsmobs:sunbird"
          // @formatter:on
          )));

      alexsMobsMaxNeutralMobsPerPlayer =
          builder.defineInRange("alexsMobsMaxNeutralMobsPerPlayer", 2, 1, 64);
      alexsMobsMaxNeutralMobsPerWorld =
          builder.defineInRange("alexsMobsMaxNeutralMobsPerWorld", 8, 1, 512);
      alexsMobsNeutralMobsList = builder.comment(Constants.CONFIG_LIST_NEUTRAL_MOBS)
          .define("alexsMobsNeutralMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "alexsmobs:cachalot_whale",
            "alexsmobs:capuchin_monkey",
            "alexsmobs:cockroach",
            "alexsmobs:crow",
            "alexsmobs:elephant",
            "alexsmobs:emu",
            "alexsmobs:gorilla",
            "alexsmobs:kangaroo",
            "alexsmobs:lobster",
            "alexsmobs:mantis_shrimp",
            "alexsmobs:moose",
            "alexsmobs:orca",
            "alexsmobs:platypus",
            "alexsmobs:raccoon",
            "alexsmobs:roadrunner",
            "alexsmobs:shoebill",
            "alexsmobs:snow_leopard",
            "alexsmobs:tasmanian_devil",
            "alexsmobs:warped_toad"
          // @formatter:on
          )));

      alexsMobsMaxHostileMobsPerPlayer =
          builder.defineInRange("alexsMobsMaxHostileMobsPerPlayer", 2, 1, 64);
      alexsMobsMaxHostileMobsPerWorld =
          builder.defineInRange("alexsMobsMaxHostileMobsPerWorld", 8, 1, 512);
      alexsMobsHostileMobsList = builder.comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
          .define("alexsMobsHostileMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "alexsmobs:alligator_snapping_turtle",
            "alexsmobs:bone_serpent",
            "alexsmobs:centipede_head",
            "alexsmobs:crimson_mosquito",
            "alexsmobs:crocodile",
            "alexsmobs:dropbear",
            "alexsmobs:enderiophage",
            "alexsmobs:grizzly_bear",
            "alexsmobs:guster",
            "alexsmobs:hammerhead_shark",
            "alexsmobs:komodo_dragon",
            "alexsmobs:leafcutter_ant",
            "alexsmobs:mimicube",
            "alexsmobs:rattlesnake",
            "alexsmobs:soul_vulture",
            "alexsmobs:straddler",
            "alexsmobs:warped_mosco"
          // @formatter:on
          )));

      builder.pop();
    }
  }

}
