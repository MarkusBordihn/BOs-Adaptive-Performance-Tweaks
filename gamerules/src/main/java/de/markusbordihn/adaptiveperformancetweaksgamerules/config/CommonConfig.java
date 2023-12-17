/*
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

package de.markusbordihn.adaptiveperformancetweaksgamerules.config;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweaksgamerules.Constants;
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
            ModConfig.Type.COMMON, commonSpec, CoreConstants.CONFIG_ID_PREFIX + "gamerules.toml");
  }

  private CommonConfig() {}

  public static class Config {

    public final ForgeConfigSpec.IntValue timeBetweenUpdates;

    public final ForgeConfigSpec.BooleanValue entityCrammingEnabled;
    public final ForgeConfigSpec.IntValue minEntityCramming;
    public final ForgeConfigSpec.IntValue maxEntityCramming;
    public final ForgeConfigSpec.IntValue minEntityCrammingMineColonies;

    public final ForgeConfigSpec.BooleanValue randomTickSpeedEnabled;
    public final ForgeConfigSpec.IntValue randomTickSpeed;

    public final ForgeConfigSpec.BooleanValue blockExplodesEnabled;
    public final ForgeConfigSpec.BooleanValue elytraMovementCheckEnabled;
    public final ForgeConfigSpec.BooleanValue insomniaEnabled;
    public final ForgeConfigSpec.BooleanValue mobExplodesEnabled;
    public final ForgeConfigSpec.BooleanValue patrolSpawningEnabled;
    public final ForgeConfigSpec.BooleanValue raidsEnabled;
    public final ForgeConfigSpec.BooleanValue tntExplodesEnabled;
    public final ForgeConfigSpec.BooleanValue traderSpawningEnabled;
    public final ForgeConfigSpec.BooleanValue wardenSpawningEnabled;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("General");
      timeBetweenUpdates =
          builder
              .comment(
                  "The time after a high to low load change is considered as valid. High loads are always considered immediately.")
              .defineInRange("timeBetweenUpdates", 30, 1, 90);
      builder.pop();

      builder.push("Entity Cramming");
      entityCrammingEnabled =
          builder
              .comment("Enable/Disable dynamic entity cramming adjustments.")
              .define("entityCrammingEnabled", true);
      minEntityCramming =
          builder
              .comment("Defines the min. numbers of mob singularity on a single block.")
              .defineInRange("minEntityCramming", 5, 0, 64);
      maxEntityCramming =
          builder
              .comment("Defines the max. numbers of mob singularity on a single block.")
              .defineInRange("maxEntityCramming", 24, 1, 64);
      minEntityCrammingMineColonies =
          builder
              .comment(
                  "Defines the min. numbers of mob singularity on a single block, in the case MineColonies is installed.")
              .defineInRange("minEntityCrammingMineColonies", 16, 16, 64);
      builder.pop();

      builder.push("Random Tick Speed");
      randomTickSpeedEnabled =
          builder
              .comment("Enable/Disable dynamic random tick speed adjustments.")
              .define("randomTickSpeedEnabled", true);
      randomTickSpeed =
          builder
              .comment("Defines the max. speed of crops grow, leaf decay, redstone, ... per chunk")
              .defineInRange("randomTickSpeed", 3, 1, 256);
      builder.pop();

      builder.push("Block Explodes");
      blockExplodesEnabled =
          builder
              .comment("Enable/Disable block explodes adjustments.")
              .define("blockExplodesEnabled", true);
      builder.pop();

      builder.push("Elytra Movement Check");
      elytraMovementCheckEnabled =
          builder
              .comment("Enable/Disable dynamic elytra movement check adjustments.")
              .define("elytraMovementCheckEnabled", true);
      builder.pop();

      builder.push("Raids");
      raidsEnabled =
          builder.comment("Enable/Disable dynamic raids adjustments.").define("raidsEnabled", true);
      builder.pop();

      builder.push("Insomnia");
      insomniaEnabled =
          builder
              .comment("Enable/Disable insomnia (phantoms) adjustments.")
              .define("insomniaEnabled", true);
      builder.pop();

      builder.push("Mob Explodes");
      mobExplodesEnabled =
          builder
              .comment("Enable/Disable mob explodes adjustments.")
              .define("mobExplodesEnabled", true);
      builder.pop();

      builder.push("Patrol Spawning");
      patrolSpawningEnabled =
          builder
              .comment("Enable/Disable patrol spawning adjustments.")
              .define("patrolSpawningEnabled", true);
      builder.pop();

      builder.push("Trader Spawning");
      traderSpawningEnabled =
          builder
              .comment("Enable/Disable trader spawning adjustments.")
              .define("traderSpawningEnabled", true);
      builder.pop();

      builder.push("TNT Explodes");
      tntExplodesEnabled =
          builder
              .comment("Enable/Disable tnt explodes adjustments.")
              .define("tntExplodesEnabled", true);
      builder.pop();

      builder.push("Warden Spawning");
      wardenSpawningEnabled =
          builder
              .comment("Enable/Disable warden spawning adjustments.")
              .define("wardenSpawningEnabled", true);
      builder.pop();
    }
  }
}
