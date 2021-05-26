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

package de.markusbordihn.adaptiveperformancetweaks.config;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;

import de.markusbordihn.adaptiveperformancetweaks.Constants;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public class ServerConfig {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ServerConfig() {
  }

  static final ForgeConfigSpec serverSpec;
  public static final Config SERVER;
  static {
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    serverSpec = specPair.getRight();
    SERVER = specPair.getLeft();
    log.info("Registering server config ...");
    ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, serverSpec);
  }

  /**
   * Server specific configuration - only loaded server-side from
   * adaptive-performance-tweaks-server.toml
   */
  public static class Config {

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment("Adaptive Performance Tweaks (Server configuration)").push("server");

      builder.pop();
    }
  }

  @SubscribeEvent
  public static void handleModConfigLoadEvent(final ModConfig.Loading event) {
    ModConfig config = event.getConfig();
    if (config.getSpec() != serverSpec) {
        return;
    }
    log.info("Loading server config file {} ...", config.getFileName());
  }

  @SubscribeEvent
  public static void handleModConfigReloadEvent(final ModConfig.Reloading event) {
    ModConfig config = event.getConfig();
    if (config.getSpec() != serverSpec) {
        return;
    }
    log.info("Reloading server config file {} ...", config.getFileName());
  }
}
