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
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.loading.FMLPaths;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public final class AquacultureSpawnConfig {

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info(
        "Registering {} {} spawn config ...", Constants.MOD_NAME, CoreConstants.AQUACULTURE_NAME);
    try {
      Files.createDirectories(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID));
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get()
        .registerConfig(
            ModConfig.Type.COMMON,
            commonSpec,
            CoreConstants.CONFIG_ID_PREFIX + "/spawn/AquacultureSpawn.toml");
  }

  private AquacultureSpawnConfig() {}

  public static class Config {

    public final ForgeConfigSpec.BooleanValue enabled;
    public final ForgeConfigSpec.ConfigValue<String> id;

    public final ForgeConfigSpec.IntValue fishPerPlayer;
    public final ForgeConfigSpec.IntValue fishPerWorld;
    public final ForgeConfigSpec.IntValue fishPerServer;
    public final ForgeConfigSpec.ConfigValue<List<String>> fishList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Aquaculture 2 Spawn Config");
      enabled = builder.define("Enabled", true);
      id = builder.define("Id", CoreConstants.AQUACULTURE_MOD);
      fishPerPlayer = builder.defineInRange("MaxFishPerPlayer", 4, 1, 64);
      fishPerWorld = builder.defineInRange("MaxFishPerWorld", 16, 1, 512);
      fishPerServer = builder.defineInRange("MaxFishPerServer", 320, 1, 1024);
      fishList =
          builder
              .comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
              .define(
                  "FishList",
                  new ArrayList<>(
                      Arrays.asList(
                          // @formatter:off
                          "aquaculture:acacia_fish_mount",
                          "aquaculture:arapaima",
                          "aquaculture:arrau_turtle",
                          "aquaculture:atlantic_cod",
                          "aquaculture:atlantic_halibut",
                          "aquaculture:atlantic_herring",
                          "aquaculture:bayad",
                          "aquaculture:birch_fish_mount",
                          "aquaculture:blackfish",
                          "aquaculture:bluegill",
                          "aquaculture:bobber",
                          "aquaculture:boulti",
                          "aquaculture:box_turtle",
                          "aquaculture:brown_shrooma",
                          "aquaculture:brown_trout",
                          "aquaculture:capitaine",
                          "aquaculture:carp",
                          "aquaculture:catfish",
                          "aquaculture:dark_oak_fish_mount",
                          "aquaculture:gar",
                          "aquaculture:jellyfish",
                          "aquaculture:jungle_fish_mount",
                          "aquaculture:minnow",
                          "aquaculture:muskellunge",
                          "aquaculture:oak_fish_mount",
                          "aquaculture:pacific_halibut",
                          "aquaculture:perch",
                          "aquaculture:pink_salmon",
                          "aquaculture:piranha",
                          "aquaculture:pollock",
                          "aquaculture:rainbow_trout",
                          "aquaculture:red_grouper",
                          "aquaculture:red_shrooma",
                          "aquaculture:smallmouth_bass",
                          "aquaculture:spruce_fish_mount",
                          "aquaculture:starshell_turtle",
                          "aquaculture:synodontis",
                          "aquaculture:tambaqui",
                          "aquaculture:tuna",
                          "aquaculture:water_arrow"
                          // @formatter:on
                          )));
      builder.pop();
    }
  }
}
