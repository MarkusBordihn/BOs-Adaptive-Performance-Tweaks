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
public final class AquacultureSpawnConfig {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private AquacultureSpawnConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} {} spawn config ...", Constants.MOD_NAME,
        CoreConstants.AQUACULTURE_NAME);
    try {
      FileUtils.getOrCreateDirectory(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID),
        CoreConstants.CONFIG_ID);
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "/spawn/AquacultureSpawn.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue modAquacultureEnabled;
    public final ForgeConfigSpec.ConfigValue<String> modAquacultureId;

    public final ForgeConfigSpec.IntValue modAquacultureMaxFishPerPlayer;
    public final ForgeConfigSpec.IntValue modAquacultureMaxFishPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> modAquacultureFishList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Aquaculture 2 Spawn Config");
      modAquacultureEnabled = builder.define("modAquacultureEnabled", true);
      modAquacultureId = builder.define("modAquacultureId", CoreConstants.AQUACULTURE_MOD);
      modAquacultureMaxFishPerPlayer =
          builder.defineInRange("modAquacultureMaxFishPerPlayer", 2, 1, 64);
      modAquacultureMaxFishPerWorld =
          builder.defineInRange("modAquacultureMaxFishPerWorld", 8, 1, 512);
      modAquacultureFishList = builder.comment(Constants.CONFIG_LIST_PASSIVE_MOBS)
          .define("modAquacultureFishList", new ArrayList<String>(Arrays.asList(
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
