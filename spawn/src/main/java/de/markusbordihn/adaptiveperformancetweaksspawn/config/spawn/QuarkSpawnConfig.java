/*
 * Copyright 2022 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
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
public final class QuarkSpawnConfig {

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} {} spawn config ...", Constants.MOD_NAME, CoreConstants.QUARK_NAME);
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
            CoreConstants.CONFIG_ID_PREFIX + "/spawn/QuarkSpawn.toml");
  }

  private QuarkSpawnConfig() {}

  public static class Config {

    public final ForgeConfigSpec.BooleanValue enabled;
    public final ForgeConfigSpec.ConfigValue<String> id;

    public final ForgeConfigSpec.IntValue passiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue passiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue passiveMobsPerServer;
    public final ForgeConfigSpec.ConfigValue<List<String>> passiveMobsList;

    public final ForgeConfigSpec.IntValue neutralMobsPerPlayer;
    public final ForgeConfigSpec.IntValue neutralMobsPerWorld;
    public final ForgeConfigSpec.IntValue neutralMobsPerServer;
    public final ForgeConfigSpec.ConfigValue<List<String>> neutralMobsList;

    public final ForgeConfigSpec.IntValue hostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue hostileMobsPerWorld;
    public final ForgeConfigSpec.IntValue hostileMobsPerServer;
    public final ForgeConfigSpec.ConfigValue<List<String>> hostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Quark Spawn Config");
      enabled = builder.define("Enabled", true);
      id = builder.define("Id", CoreConstants.QUARK_MOD);

      passiveMobsPerPlayer = builder.defineInRange("MaxPassiveMobsPerPlayer", 4, 1, 64);
      passiveMobsPerWorld = builder.defineInRange("MaxPassiveMobsPerWorld", 16, 1, 512);
      passiveMobsPerServer = builder.defineInRange("MaxPassiveMobsPerServer", 320, 1, 1204);
      passiveMobsList =
          builder
              .comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
              .define(
                  "PassiveMobsList",
                  new ArrayList<>(
                      Arrays.asList(
                          // @formatter:off
                          "quark:crab", "quark:frog", "quark:stoneling"
                          // @formatter:on
                          )));

      neutralMobsPerPlayer = builder.defineInRange("MaxNeutralMobsPerPlayer", 4, 1, 64);
      neutralMobsPerWorld = builder.defineInRange("MaxNeutralMobsPerWorld", 16, 1, 512);
      neutralMobsPerServer = builder.defineInRange("MaxNeutralMobsPerServer", 320, 1, 1204);
      neutralMobsList =
          builder
              .comment(Constants.CONFIG_LIST_NEUTRAL_MOBS)
              .define(
                  "NeutralMobsList",
                  new ArrayList<>(
                      List.of(
                          // @formatter:off
                          "quark:toretoise"
                          // @formatter:on
                          )));

      hostileMobsPerPlayer = builder.defineInRange("MaxHostileMobsPerPlayer", 8, 1, 64);
      hostileMobsPerWorld = builder.defineInRange("MaxHostileMobsPerWorld", 32, 1, 512);
      hostileMobsPerServer = builder.defineInRange("MaxHostileMobsPerServer", 320, 1, 1204);
      hostileMobsList =
          builder
              .comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
              .define(
                  "HostileMobsList",
                  new ArrayList<>(
                      Arrays.asList(
                          // @formatter:off
                          "quark:forgotten", "quark:foxhound", "quark:wraith"
                          // @formatter:on
                          )));

      builder.pop();
    }
  }
}
