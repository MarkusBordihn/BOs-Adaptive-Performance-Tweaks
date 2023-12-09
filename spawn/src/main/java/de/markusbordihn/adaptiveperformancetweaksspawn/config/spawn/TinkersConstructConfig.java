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
public final class TinkersConstructConfig {

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
        "Registering {} {} spawn config ...", Constants.MOD_NAME, CoreConstants.TCONSTRUCT_NAME);
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
            CoreConstants.CONFIG_ID_PREFIX + "/spawn/TinkersConstructSpawn.toml");
  }

  private TinkersConstructConfig() {}

  public static class Config {

    public final ForgeConfigSpec.BooleanValue tinkersConstructEnabled;
    public final ForgeConfigSpec.ConfigValue<String> tinkersConstructId;

    public final ForgeConfigSpec.IntValue tinkersConstructMaxHostileMobsPerPlayer;
    public final ForgeConfigSpec.IntValue tinkersConstructMaxHostileMobsPerWorld;
    public final ForgeConfigSpec.ConfigValue<List<String>> tinkersConstructHostileMobsList;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("Tinkers Construct Spawn Config");
      tinkersConstructEnabled = builder.define("tinkersConstructEnabled", true);
      tinkersConstructId = builder.define("tinkersConstructId", CoreConstants.TCONSTRUCT_MOD);

      tinkersConstructMaxHostileMobsPerPlayer =
          builder.defineInRange("tinkersConstructMaxHostileMobsPerPlayer", 4, 1, 64);
      tinkersConstructMaxHostileMobsPerWorld =
          builder.defineInRange("tinkersConstructMaxHostileMobsPerWorld", 16, 1, 512);
      tinkersConstructHostileMobsList =
          builder
              .comment(Constants.CONFIG_LIST_HOSTILE_MOBS)
              .define(
                  "tinkersConstructHostileMobsList",
                  new ArrayList<String>(
                      Arrays.asList(
                          // @formatter:off
                          "tconstruct:earth_slime", "tconstruct:sky_slime", "tconstruct:ender_slime"
                          // @formatter:on
                          )));
      builder.pop();
    }
  }
}
