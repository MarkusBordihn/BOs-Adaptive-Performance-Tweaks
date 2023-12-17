/*
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

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import java.util.ArrayList;
import java.util.List;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.loading.FMLPaths;
import net.minecraftforge.fml.loading.FileUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public final class CommonConfig {

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} common config ...", Constants.MOD_NAME);
    try {
      FileUtils.getOrCreateDirectory(
          FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID), CoreConstants.CONFIG_ID);
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get()
        .registerConfig(
            ModConfig.Type.COMMON, commonSpec, CoreConstants.CONFIG_ID_PREFIX + "spawn.toml");
  }

  private CommonConfig() {}

  public static class Config {

    public final ForgeConfigSpec.ConfigValue<List<String>> spawnAllowList;
    public final ForgeConfigSpec.ConfigValue<List<String>> spawnDenyList;
    public final ForgeConfigSpec.ConfigValue<List<String>> spawnIgnoreDimensionList;
    public final ForgeConfigSpec.BooleanValue spawnAggressiveMode;

    public final ForgeConfigSpec.BooleanValue viewAreaEnabled;
    public final ForgeConfigSpec.IntValue friendlyChunkSpawnRate;

    public final ForgeConfigSpec.BooleanValue spawnLimitationEnabled;
    public final ForgeConfigSpec.IntValue spawnLimitationLimiter;
    public final ForgeConfigSpec.IntValue spawnLimitationMaxMobsPerPlayer;
    public final ForgeConfigSpec.IntValue spawnLimitationMaxMobsPerWorld;
    public final ForgeConfigSpec.IntValue spawnLimitationMaxMobsPerServer;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("General");
      spawnAllowList =
          builder
              .comment(
                  "General allow list for spawn entities (e.g. minecraft:squid) which should be ignored for optimization.")
              .define("allowList", new ArrayList<>(List.of("")));
      spawnDenyList =
          builder
              .comment(
                  "General deny list for spawn entities (e.g. minecraft:bat) to no longer spawn in all worlds.")
              .define("denyList", new ArrayList<>(List.of("")));
      spawnIgnoreDimensionList =
          builder
              .comment("General list of ignored dimensions.")
              .define("spawnIgnoreDimensionList", new ArrayList<>(List.of("minecraft:the_end")));
      spawnAggressiveMode =
          builder
              .comment("Enable/Disable more aggressive and strict spawn limitations.")
              .define("spawnAggressiveMode", false);
      builder.pop();

      builder.push("Chunk Optimization");
      friendlyChunkSpawnRate =
          builder
              .comment(
                  "Allows to spawn at least one mob (regardless of the type) per selected chunk to avoid side effects."
                      + "A value of 9 means every 9 chunk request will be allowed. Use 0 to disable this optimization.")
              .defineInRange("friendlyChunkSpawnRate", 9, 0, 256);
      builder.pop();

      builder.push("View Area");
      viewAreaEnabled =
          builder
              .comment("Enable/Disable player based view area optimization.")
              .define("viewAreaEnabled", true);
      builder.pop();

      builder.push("Global Spawn Limitations");
      spawnLimitationEnabled =
          builder
              .comment("Enable/Disable general spawn limitations.")
              .define("spawnLimitationEnabled", true);
      spawnLimitationLimiter =
          builder
              .comment(
                  "Blocks every x spawn to avoid an over population with the limited spawn area. Use 0 to disable this optimization.")
              .defineInRange("spawnLimitationLimiter", 16, 0, 100);
      spawnLimitationMaxMobsPerPlayer =
          builder
              .comment(
                  "Defines the max. number of entities of a specific type, which could spawn within the player view area. Use 0 to disable this optimization.")
              .defineInRange("spawnLimitationMaxMobsPerPlayer", 32, 0, 256);
      spawnLimitationMaxMobsPerWorld =
          builder
              .comment(
                  "Defines the max. number of entities of a specific type, which could spawn within a single world. Use 0 to disable this optimization.")
              .defineInRange("spawnLimitationMaxMobsPerWorld", 128, 0, 1024);
      spawnLimitationMaxMobsPerServer =
          builder
              .comment(
                  "Defines the max. number of entities of a specific type, which could spawn within the server. Use 0 to disable this optimization.")
              .defineInRange("spawnLimitationMaxMobsPerServer", 512, 0, 1024);
      builder.pop();
    }
  }
}
