/*
 * Copyright 2024 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweaks;

import cpw.mods.modlauncher.Launcher;
import cpw.mods.modlauncher.api.IEnvironment;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModCompat;
import de.markusbordihn.adaptiveperformancetweaks.core.config.Config;
import de.markusbordihn.adaptiveperformancetweaks.core.debug.DebugManager;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureRegistry;
import java.util.Optional;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.fml.loading.FMLEnvironment;
import net.minecraftforge.fml.loading.FMLPaths;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@SuppressWarnings("unused")
@Mod(Constants.MOD_ID)
public class AdaptivePerformanceTweaks {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  @SuppressWarnings("java:S2440")
  public AdaptivePerformanceTweaks() {
    final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();
    log.info("Initializing {} (Forge) ...", Constants.MOD_NAME);

    log.debug("{} Debug Manager ...", Constants.LOG_REGISTER_PREFIX);
    Optional<String> version =
      Launcher.INSTANCE.environment().getProperty(IEnvironment.Keys.VERSION.get());
    if (version.isPresent() && "MOD_DEV".equals(version.get())) {
      DebugManager.setDevelopmentEnvironment(true);
    }
    DebugManager.checkForDebugLogging(Constants.LOG_NAME);

    log.debug("{} Constants ...", Constants.LOG_REGISTER_PREFIX);
    Constants.GAME_DIR = FMLPaths.GAMEDIR.get();
    Constants.CONFIG_DIR = FMLPaths.CONFIGDIR.get();

    boolean isDedicatedServer = FMLEnvironment.dist == Dist.DEDICATED_SERVER;

    log.debug("{} Mod Compat ...", Constants.LOG_REGISTER_PREFIX);
    ModCompat.setModLoadedChecker(
      modId -> "minecraft".equals(modId) || ModList.get().isLoaded(modId));

    log.debug("{} Configuration ({}) ...", Constants.LOG_REGISTER_PREFIX,
      isDedicatedServer ? "server" : "client");
    Config.register();

    log.debug("{} Feature Registry ...", Constants.LOG_REGISTER_PREFIX);
    FeatureRegistry.registerCommon();

    DistExecutor.unsafeRunWhenOn(Dist.CLIENT,
      () -> () -> new AdaptivePerformanceTweaksClient(modEventBus));
  }
}
