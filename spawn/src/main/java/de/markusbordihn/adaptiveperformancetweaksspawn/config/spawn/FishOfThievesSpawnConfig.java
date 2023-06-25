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

import java.nio.file.Files;
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

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public final class FishOfThievesSpawnConfig {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private FishOfThievesSpawnConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} {} spawn config ...", Constants.MOD_NAME,
        CoreConstants.FISH_OF_THIEVES_NAME);
    try {
      Files.createDirectories(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID));
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "/spawn/FishOfThievesSpawn.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue enabled;
    public final ForgeConfigSpec.ConfigValue<String> id;

    public final ForgeConfigSpec.IntValue fishPerPlayer;
    public final ForgeConfigSpec.IntValue fishPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> fishList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Fish of Thieves Spawn Config");
      enabled = builder.define("Enabled", true);
      id = builder.define("Id", CoreConstants.FISH_OF_THIEVES_MOD);

      fishPerPlayer = builder.defineInRange("MaxFishPerPlayer", 16, 1, 64);
      fishPerWorld = builder.defineInRange("MaxFishPerWorld", 64, 1, 512);
      fishList = builder.comment(Constants.CONFIG_LIST_PASSIVE_MOBS).define("FishList",
          new ArrayList<String>(Arrays.asList(
          // @formatter:off
            "fishofthieves:ancientscales",
            "fishofthieves:battlegills",
            "fishofthieves:devilfish",
            "fishofthieves:islehoppers",
            "fishofthieves:plentifins",
            "fishofthieves:pondies",
            "fishofthieves:splashtail",
            "fishofthieves:stormfish",
            "fishofthieves:wildsplash",
            "fishofthieves:wreckers"
          // @formatter:on
          )));

      builder.pop();
    }
  }

}
