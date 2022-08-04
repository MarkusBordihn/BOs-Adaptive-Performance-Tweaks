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

package de.markusbordihn.adaptiveperformancetweaksspawn.config;

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
public final class CommonConfig {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private CommonConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} common config ...", Constants.MOD_NAME);
    try {
      FileUtils.getOrCreateDirectory(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID),
          CoreConstants.CONFIG_ID);
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "spawn.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.ConfigValue<List<String>> spawnAllowList;
    public final ForgeConfigSpec.ConfigValue<List<String>> spawnDenyList;
    public final ForgeConfigSpec.ConfigValue<List<String>> spawnIgnoreDimensionList;

    public final ForgeConfigSpec.BooleanValue spawnAggressiveMode;

    public final ForgeConfigSpec.BooleanValue spawnLimitationEnabled;
    public final ForgeConfigSpec.IntValue spawnLimitationLimiter;
    public final ForgeConfigSpec.IntValue spawnLimitationMaxMobsPerPlayer;
    public final ForgeConfigSpec.IntValue spawnLimitationMaxMobsPerWorld;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("General");
      spawnAllowList = builder.comment(
          "General allow list for spawn entities (e.g. minecraft:squid) which should be ignored for optimization.")
          .define("allowList", new ArrayList<String>(Arrays.asList("")));
      spawnDenyList = builder.comment(
          "General deny list for spawn entities (e.g. minecraft:bat) to no longer spawn in all worlds.")
          .define("denyList", new ArrayList<String>(Arrays.asList("")));
      spawnIgnoreDimensionList = builder.comment("General list of ignored dimensions.").define(
          "spawnIgnoreDimensionList", new ArrayList<String>(Arrays.asList("minecraft:the_end")));
      spawnAggressiveMode = builder.comment("Enable/Disable more aggressive spawn limitations.")
          .define("spawnAggressiveMode", false);
      builder.pop();

      builder.push("Spawn Limitations");
      spawnLimitationEnabled = builder.comment("Enable/Disable general spawn limitations.")
          .define("spawnLimitationEnabled", true);
      spawnLimitationLimiter = builder.comment(
          "Blocks every x spawn to avoid an over population with the limited spawn area. Use 0 to disable this optimization.")
          .defineInRange("spawnLimitationLimiter", 10, 0, 100);
      spawnLimitationMaxMobsPerPlayer = builder.comment(
          "Defines the max. number of entities of a specific type, which could spawn within the player view area. Use 0 to disable this optimization.")
          .defineInRange("spawnLimitationMaxMobsPerPlayer", 8, 0, 256);
      spawnLimitationMaxMobsPerWorld = builder.comment(
          "Defines the max. number of entities of a specific type, which could spawn within a single world. Use 0 to disable this optimization.")
          .defineInRange("spawnLimitationMaxMobsPerWorld", 128, 0, 1024);
      builder.pop();
    }
  }

}
