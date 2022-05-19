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
public final class MekanismAdditionsSpawnConfig {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private MekanismAdditionsSpawnConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} {} spawn config ...", Constants.MOD_NAME,
        CoreConstants.MEKANISMADDITIONS_NAME);
    FileUtils.getOrCreateDirectory(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID),
        CoreConstants.CONFIG_ID);
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "/spawn/MekanismAdditionsSpawn.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue mekanismAdditionsEnabled;
    public final ForgeConfigSpec.ConfigValue<String> mekanismAdditionsId;

    public final ForgeConfigSpec.IntValue mekanismAdditionsMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue mekanismAdditionsMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> mekanismAdditionsPassiveMobsList;

    public final ForgeConfigSpec.IntValue mekanismAdditionsMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue mekanismAdditionsMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> mekanismAdditionsNeutralMobsList;

    public final ForgeConfigSpec.IntValue mekanismAdditionsMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue mekanismAdditionsMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> mekanismAdditionsHostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Mekanism Spawn Config");
      mekanismAdditionsEnabled = builder.define("mekanismAdditionsEnabled", true);
      mekanismAdditionsId =
          builder.define("mekanismAdditionsId", CoreConstants.MEKANISMADDITIONS_MOD);

      mekanismAdditionsMaxPassiveMobsPerPlayer =
          builder.defineInRange("mekanismAdditionsMaxPassiveMobsPerPlayer", 3, 1, 64);
      mekanismAdditionsMaxPassiveMobsPerWorld =
          builder.defineInRange("mekanismAdditionsMaxPassiveMobsPerWorld", 12, 1, 512);
      mekanismAdditionsPassiveMobsList = builder.comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
          .define("mekanismAdditionsPassiveMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
          // @formatter:on
          )));

      mekanismAdditionsMaxNeutralMobsPerPlayer =
          builder.defineInRange("mekanismAdditionsMaxNeutralMobsPerPlayer", 4, 1, 64);
      mekanismAdditionsMaxNeutralMobsPerWorld =
          builder.defineInRange("mekanismAdditionsMaxNeutralMobsPerWorld", 16, 1, 512);
      mekanismAdditionsNeutralMobsList = builder.comment(Constants.CONFIG_LIST_NEUTRAL_MOBS)
          .define("mekanismAdditionsNeutralMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
          // @formatter:on
          )));

      mekanismAdditionsMaxHostileMobsPerPlayer =
          builder.defineInRange("mekanismAdditionsMaxHostileMobsPerPlayer", 2, 1, 64);
      mekanismAdditionsMaxHostileMobsPerWorld =
          builder.defineInRange("mekanismAdditionsMaxHostileMobsPerWorld", 8, 1, 512);
      mekanismAdditionsHostileMobsList = builder.comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
          .define("mekanismAdditionsHostileMobsList", new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "mekanismadditions:baby_creeper",
            "mekanismadditions:baby_enderman",
            "mekanismadditions:baby_skeleton",
            "mekanismadditions:baby_stray",
            "mekanismadditions:baby_wither_skeleton"
          // @formatter:on
          )));

      builder.pop();
    }
  }

}
