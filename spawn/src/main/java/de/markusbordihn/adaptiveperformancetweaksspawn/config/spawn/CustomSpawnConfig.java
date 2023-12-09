/**
 * Copyright 2022 Markus Bordihn
 *
 * <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * <p>The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * <p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.adaptiveperformancetweaksspawn.config.spawn;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import java.util.ArrayList;
import java.util.Arrays;
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
public final class CustomSpawnConfig {

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} custom spawn config ...", Constants.MOD_NAME);
    try {
      FileUtils.getOrCreateDirectory(
          FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID), CoreConstants.CONFIG_ID);
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get()
        .registerConfig(
            ModConfig.Type.COMMON,
            commonSpec,
            CoreConstants.CONFIG_ID_PREFIX + "/spawn/CustomSpawn.toml");
  }

  private CustomSpawnConfig() {}

  public static class Config {

    public final ForgeConfigSpec.BooleanValue customEnabled;
    public final ForgeConfigSpec.ConfigValue<String> customId;

    public final ForgeConfigSpec.IntValue customMaxPassiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue customMaxPassiveMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> customPassiveMobsList;

    public final ForgeConfigSpec.IntValue customMaxNeutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue customMaxNeutralMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> customNeutralMobsList;

    public final ForgeConfigSpec.IntValue customMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue customMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> customHostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Custom Spawn Config");
      customEnabled = builder.define("customEnabled", false);
      customId = builder.define("customId", "custom_config");

      customMaxPassiveMobsPerPlayer =
          builder.defineInRange("customMaxPassiveMobsPerPlayer", 2, 1, 64);
      customMaxPassiveMobsPerWorld =
          builder.defineInRange("customMaxPassiveMobsPerWorld", 16, 1, 512);
      customPassiveMobsList =
          builder
              .comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
              .define(
                  "customPassiveMobsList",
                  new ArrayList<String>(
                      Arrays.asList(
                          // @formatter:off
                          // @formatter:on
                          )));

      customMaxNeutralMobsPerPlayer =
          builder.defineInRange("customMaxNeutralMobsPerPlayer", 2, 1, 64);
      customMaxNeutralMobsPerWorld =
          builder.defineInRange("customMaxNeutralMobsPerWorld", 16, 1, 512);
      customNeutralMobsList =
          builder
              .comment(Constants.CONFIG_LIST_NEUTRAL_MOBS)
              .define(
                  "customNeutralMobsList",
                  new ArrayList<String>(
                      Arrays.asList(
                          // @formatter:off
                          // @formatter:on
                          )));

      customMaxHostileMobsPerPlayer =
          builder.defineInRange("customMaxHostileMobsPerPlayer", 2, 1, 64);
      customMaxHostileMobsPerWorld =
          builder.defineInRange("customMaxHostileMobsPerWorld", 16, 1, 512);
      customHostileMobsList =
          builder
              .comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
              .define(
                  "customHostileMobsList",
                  new ArrayList<String>(
                      Arrays.asList(
                          // @formatter:off
                          // @formatter:on
                          )));

      builder.pop();
    }
  }
}
