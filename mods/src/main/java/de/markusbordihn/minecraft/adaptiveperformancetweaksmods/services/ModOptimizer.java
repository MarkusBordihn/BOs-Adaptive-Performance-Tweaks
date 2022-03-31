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


package de.markusbordihn.minecraft.adaptiveperformancetweaksmods.services;

import java.io.File;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import cpw.mods.modlauncher.Environment;
import cpw.mods.modlauncher.Launcher;
import cpw.mods.modlauncher.api.IEnvironment;

import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.loading.FMLPaths;
import net.minecraftforge.forgespi.locating.IModFile;
import net.minecraftforge.forgespi.locating.IModLocator;

import de.markusbordihn.minecraft.adaptiveperformancetweaksmods.Constants;
import de.markusbordihn.minecraft.adaptiveperformancetweaksmods.utils.ClientSideMods;
import de.markusbordihn.minecraft.adaptiveperformancetweaksmods.utils.DuplicatedMods;

@Mod(Constants.MOD_ID)
public class ModOptimizer implements IModLocator {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final File GAME_DIR = FMLPaths.GAMEDIR.get().toFile();
  private static final File MODS_DIR = FMLPaths.MODSDIR.get().toFile();
  private boolean isClient = true;

  public ModOptimizer() {
    Environment environment = Launcher.INSTANCE.environment();
    Optional<String> launchTarget = environment.getProperty(IEnvironment.Keys.LAUNCHTARGET.get());
    if (launchTarget.isPresent()) {
      if (launchTarget.get().contains("server")) {
        isClient = false;
      }
    } else if (GAME_DIR != null) {
      log.warn("Unable to detect environment will check game dir for additional hints ...");
      File[] gameFiles = GAME_DIR.listFiles();
      for (File gameFile : gameFiles) {
        if (gameFile.getName().contains("server")) {
          isClient = false;
        }
      }
    } else {
      log.error("Unable to detected running environment, will stop to avoid any possible damage!");
      return;
    }

    log.info("Init Mod Optimizer with game dir {} and mods dir {} for target {}", GAME_DIR,
        MODS_DIR, isClient ? "CLIENT" : "SERVER");

    log.info("Optimize Duplicated Mods ...");
    DuplicatedMods.searchDuplicatedMods(MODS_DIR);

    log.info("Optimize client / server side mods ...");
    if (isClient) {
      log.info("Enable possible client side mods ...");
      ClientSideMods.enable(MODS_DIR);
    } else {
      log.info("Disable possible client side mods ...");
      ClientSideMods.disable(MODS_DIR);
    }
  }

  @Override
  public List<IModFile> scanMods() {
    log.info("scanMods");
    return Collections.emptyList();
  }

  @Override
  public String name() {
    return "aptweaks_mods";
  }

  @Override
  public void scanFile(IModFile modFile, Consumer<Path> pathConsumer) {
    log.info("scanFile {} {}", modFile, pathConsumer);
  }

  @Override
  public void initArguments(Map<String, ?> arguments) {
    log.info("initArguments {}", arguments);
  }

  @Override
  public boolean isValid(IModFile modFile) {
    log.info("isValid {}", modFile);
    return false;
  }

}
