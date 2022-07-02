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
public final class QuarkSpawnConfig {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private QuarkSpawnConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} {} spawn config ...", Constants.MOD_NAME,
        CoreConstants.QUARK_NAME);
    try {
      FileUtils.getOrCreateDirectory(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID),
        CoreConstants.CONFIG_ID);
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "/spawn/QuarkSpawn.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue quarkEnabled;
    public final ForgeConfigSpec.ConfigValue<String> quarkId;

    public final ForgeConfigSpec.IntValue quarkMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue quarkMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> quarkPassiveMobsList;

    public final ForgeConfigSpec.IntValue quarkMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue quarkMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> quarkNeutralMobsList;

    public final ForgeConfigSpec.IntValue quarkMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue quarkMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> quarkHostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Quark Spawn Config");
      quarkEnabled = builder.define("quarkEnabled", true);
      quarkId =
          builder.define("quarkId", CoreConstants.QUARK_MOD);

      quarkMaxPassiveMobsPerPlayer =
          builder.defineInRange("quarkMaxPassiveMobsPerPlayer", 2, 1, 64);
      quarkMaxPassiveMobsPerWorld =
          builder.defineInRange("quarkMaxPassiveMobsPerWorld", 16, 1, 512);
      quarkPassiveMobsList = builder.comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
          .define("quarkPassiveMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "quark:crab",
            "quark:frog",
            "quark:stoneling"
          // @formatter:on
          )));

      quarkMaxNeutralMobsPerPlayer =
          builder.defineInRange("quarkMaxNeutralMobsPerPlayer", 2, 1, 64);
      quarkMaxNeutralMobsPerWorld =
          builder.defineInRange("quarkMaxNeutralMobsPerWorld", 16, 1, 512);
      quarkNeutralMobsList = builder.comment(Constants.CONFIG_LIST_NEUTRAL_MOBS)
          .define("quarkNeutralMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "quark:toretoise"
          // @formatter:on
          )));

      quarkMaxHostileMobsPerPlayer =
          builder.defineInRange("quarkMaxHostileMobsPerPlayer", 2, 1, 64);
      quarkMaxHostileMobsPerWorld =
          builder.defineInRange("quarkMaxHostileMobsPerWorld", 16, 1, 512);
      quarkHostileMobsList = builder.comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
          .define("quarkHostileMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "quark:forgotten",
            "quark:foxhound",
            "quark:wraith"
          // @formatter:on
          )));

      builder.pop();
    }
  }

}
