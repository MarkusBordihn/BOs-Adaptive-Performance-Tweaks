/**
 * Copyright 2021 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweakscore.config;

import java.nio.file.Files;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.loading.FMLPaths;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;
import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;

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
    log.info("{} common config ...", Constants.LOG_REGISTER_PREFIX);
    try {
      Files.createDirectories(FMLPaths.CONFIGDIR.get().resolve(CoreConstants.CONFIG_ID));
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        CoreConstants.CONFIG_ID_PREFIX + "core.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.IntValue timeBetweenUpdates;

    public final ForgeConfigSpec.BooleanValue logServerLoad;
    public final ForgeConfigSpec.BooleanValue logServerLevelLoad;

    public final ForgeConfigSpec.DoubleValue gameDifficultyFactorEasy;
    public final ForgeConfigSpec.DoubleValue gameDifficultyFactorNormal;
    public final ForgeConfigSpec.DoubleValue gameDifficultyFactorPeaceful;
    public final ForgeConfigSpec.DoubleValue gameDifficultyFactorHard;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("General");
      timeBetweenUpdates = builder.comment(
          "The time after a high to low load change is considered as valid. High loads are always considered immediately.")
          .defineInRange("timeBetweenUpdates", 10, 1, 90);
      logServerLoad = builder.comment("Enable/Disable logging of the overall server load.")
          .define("logServerLoad", true);
      logServerLevelLoad = builder.comment("Enable/Disable logging of the individual level load.")
          .define("logServerLevelLoad", true);
      builder.pop();

      builder.push("Game Difficulty Factors");
      gameDifficultyFactorEasy = builder.defineInRange("gameDifficultyFactorEasy", 0.9, 0.10, 10);
      gameDifficultyFactorNormal = builder.defineInRange("gameDifficultyFactorNormal", 1, 0.10, 10);
      gameDifficultyFactorPeaceful =
          builder.defineInRange("gameDifficultyFactorPeaceful", 1, 0.10, 10);
      gameDifficultyFactorHard = builder.defineInRange("gameDifficultyFactorHard", 2, 0.10, 10);
      builder.pop();
    }
  }

}
