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

package de.markusbordihn.adaptiveperformancetweaksitems.config;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import de.markusbordihn.adaptiveperformancetweaksitems.Constants;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;

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

    public final ForgeConfigSpec.BooleanValue optimizeItems;
    public final ForgeConfigSpec.IntValue maxNumberOfItems;
    public final ForgeConfigSpec.IntValue maxNumberOfItemsPerType;
    public final ForgeConfigSpec.IntValue itemsClusterRange;

    public final ForgeConfigSpec.BooleanValue optimizeExperienceOrbs;
    public final ForgeConfigSpec.IntValue experienceOrbsClusterRange;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Items");
      optimizeItems =
          builder.comment("Enable/Disable items optimization.").define("optimizeItems", true);
      maxNumberOfItemsPerType = builder.comment(
          "Defines the max. number of items / itemstacks per type which are allowed to lay around in the world.")
          .defineInRange("maxNumberOfItemsPerType", 32, 16, 128);
      maxNumberOfItems = builder
          .comment(
              "Defines the max. number of items which are allowed to lay around in a single world.")
          .defineInRange("maxNumberOfItems", 128, 16, 1000);
      itemsClusterRange =
          builder.comment("Defines the radius in blocks which items will be clustered together.")
              .defineInRange("itemsClusterRange", 3, 1, 16);
      builder.pop();


      builder.push("Experience Orbs");
      optimizeExperienceOrbs = builder.comment("Enable/Disable experience orbs optimization.")
          .define("optimizeExperienceOrbs", true);
      experienceOrbsClusterRange =
          builder.comment("Defines the radius in which experience orbs will be clustered together.")
              .defineInRange("experienceOrbsClusterRange", 4, 1, 16);
      builder.pop();
    }
  }

}
