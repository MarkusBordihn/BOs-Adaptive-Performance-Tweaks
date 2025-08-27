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
public final class ExoticBirdsSpawnConfig {

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} Exotic Birds spawn config ...", Constants.MOD_NAME);
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
            CoreConstants.CONFIG_ID_PREFIX + "/spawn/ExoticBirdsSpawn.toml");
  }

  private ExoticBirdsSpawnConfig() {}

  public static class Config {

    public final ForgeConfigSpec.BooleanValue enabled;
    public final ForgeConfigSpec.ConfigValue<String> id;

    public final ForgeConfigSpec.IntValue passiveMobsPerPlayer;
    public final ForgeConfigSpec.IntValue passiveMobsPerWorld;
    public final ForgeConfigSpec.IntValue passiveMobsPerServer;

    public final ForgeConfigSpec.ConfigValue<List<String>> passiveMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Exotic Birds Spawn Config");
      enabled = builder.define("Enabled", true);
      id = builder.define("Id", "exoticbirds");

      passiveMobsPerPlayer = builder.defineInRange("MaxPassiveMobsPerPlayer", 4, 1, 64);
      passiveMobsPerWorld = builder.defineInRange("MaxPassiveMobsPerWorld", 16, 1, 512);
      passiveMobsPerServer = builder.defineInRange("MaxPassiveMobsPerServer", 64, 1, 1024);
      passiveMobsList =
          builder
              .comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
              .define(
                  "PassiveMobsList",
                  new ArrayList<>(
                      Arrays.asList(
                          // @formatter:off
                          "exoticbirds:peafowl",
                          "exoticbirds:hummingbird",
                          "exoticbirds:woodpecker",
                          "exoticbirds:kingfisher",
                          "exoticbirds:roadrunner",
                          "exoticbirds:toucan",
                          "exoticbirds:swan",
                          "exoticbirds:macaw",
                          "exoticbirds:cassowary",
                          "exoticbirds:magpie",
                          "exoticbirds:flamingo",
                          "exoticbirds:kiwi",
                          "exoticbirds:owl",
                          "exoticbirds:pelican",
                          "exoticbirds:lyrebird",
                          "exoticbirds:ostrich",
                          "exoticbirds:gouldianfinch",
                          "exoticbirds:gull",
                          "exoticbirds:pigeon",
                          "exoticbirds:duck",
                          "exoticbirds:penguin",
                          "exoticbirds:heron",
                          "exoticbirds:booby",
                          "exoticbirds:bluejay",
                          "exoticbirds:cardinal",
                          "exoticbirds:robin",
                          "exoticbirds:crane",
                          "exoticbirds:kookaburra",
                          "exoticbirds:budgerigar",
                          "exoticbirds:cockatoo",
                          "exoticbirds:fire_phoenix",
                          "exoticbirds:cloud_phoenix",
                          "exoticbirds:water_phoenix",
                          "exoticbirds:nether_phoenix",
                          "exoticbirds:skeleton_phoenix",
                          "exoticbirds:snowy_phoenix",
                          "exoticbirds:desert_phoenix",
                          "exoticbirds:ender_phoenix",
                          "exoticbirds:twilight_phoenix"
                          // @formatter:on
                          )));

      builder.pop();
    }
  }
}
