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

package de.markusbordihn.adaptiveperformancetweaksplayer.config;

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

import de.markusbordihn.adaptiveperformancetweaksplayer.Constants;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public final class CommonConfig {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

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
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec);
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue optimizePlayerLogin;
    public final ForgeConfigSpec.IntValue playerLoginValidationTimeout;

    public final ForgeConfigSpec.BooleanValue protectPlayerDuringLogin;
    public final ForgeConfigSpec.BooleanValue protectPlayerDuringLoginLogging;

    public final ForgeConfigSpec.BooleanValue enableChildPlayerProtection;
    public final ForgeConfigSpec.ConfigValue<List<String>> childPlayerProtectionList;

    public final ForgeConfigSpec.BooleanValue optimizeSimulationDistance;
    public final ForgeConfigSpec.IntValue simulationDistanceMin;
    public final ForgeConfigSpec.IntValue simulationDistanceMax;
    public final ForgeConfigSpec.IntValue simulationDistanceDefault;
    public final ForgeConfigSpec.IntValue simulationDistanceTimeBetweenUpdates;

    public final ForgeConfigSpec.BooleanValue optimizeViewDistance;
    public final ForgeConfigSpec.IntValue viewDistanceMin;
    public final ForgeConfigSpec.IntValue viewDistanceMax;
    public final ForgeConfigSpec.IntValue viewDistanceDefault;
    public final ForgeConfigSpec.IntValue viewDistanceTimeBetweenUpdates;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Player Login");
      optimizePlayerLogin =
          builder.comment("Enable/Disable optimization which are happening during player login.")
              .define("optimizePlayerLogin", true);
      playerLoginValidationTimeout =
          builder.comment("Timeout in seconds for the player validation.")
              .defineInRange("playerLoginValidationTimeout", 90, 16, 300);
      builder.pop();

      builder.push("Player Protection");
      protectPlayerDuringLogin =
          builder.comment("Protect player during login within the validation timeout.")
              .define("protectPlayerDuringLogin", true);
      protectPlayerDuringLoginLogging =
          builder.comment("Enable/Disable player protection logging with login time.")
              .define("protectPlayerDuringLoginLogging", true);
      builder.pop();

      builder.push("Child Player Protection");
      enableChildPlayerProtection =
          builder.comment("Protect child player and give them a more enjoyable play experience.")
              .define("enableChildPlayerProtection", true);
      childPlayerProtectionList =
          builder.comment("List of child player username for the child player protection.")
              .define("childPlayerProtectionList", new ArrayList<String>(Arrays.asList("")));
      builder.pop();

      builder.push("Simulation Distance");
      optimizeSimulationDistance = builder
          .comment("Enable/Disable simulation distance optimization depending on the server load.")
          .define("optimizeSimulationDistance", true);
      simulationDistanceMax = builder.defineInRange("simulationDistanceMax", 10, 6, 32);
      simulationDistanceMin = builder.defineInRange("simulationDistanceMin", 2, 2, 16);
      simulationDistanceDefault = builder.defineInRange("simulationDistanceDefault", 5, 5, 16);
      simulationDistanceTimeBetweenUpdates = builder.comment(
          "The time after a high to low load change is considered as valid. High loads are always considered immediately.")
          .defineInRange("simulationDistanceTimeBetweenUpdates", 30, 1, 90);
      builder.pop();

      builder.push("View Distance");
      optimizeViewDistance =
          builder.comment("Enable/Disable view distance optimization depending on the server load.")
              .define("optimizeViewDistance", true);
      viewDistanceMax = builder.defineInRange("viewDistanceMax", 16, 4, 32);
      viewDistanceMin = builder.defineInRange("viewDistanceMin", 5, 2, 16);
      viewDistanceDefault = builder.defineInRange("viewDistanceDefault", 8, 2, 16);
      viewDistanceTimeBetweenUpdates = builder.comment(
          "The time after a high to low load change is considered as valid. High loads are always considered immediately.")
          .defineInRange("viewDistanceTimeBetweenUpdates", 60, 1, 90);
      builder.pop();
    }
  }

}
