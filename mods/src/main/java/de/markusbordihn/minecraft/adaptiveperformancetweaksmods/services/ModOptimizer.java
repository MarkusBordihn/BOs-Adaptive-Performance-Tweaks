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
import java.util.concurrent.TimeUnit;
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
  private static final String LOG_PREFIX = "[Mod Optimizer]";

  private boolean isClient = true;

  public ModOptimizer() {
    Environment environment = Launcher.INSTANCE.environment();
    Optional<String> launchTarget = environment.getProperty(IEnvironment.Keys.LAUNCHTARGET.get());
    if (launchTarget.isPresent()) {
      if (launchTarget.get().contains("server")) {
        isClient = false;
      }
    } else if (GAME_DIR != null) {
      log.warn("{} ⚠️ Unable to detect environment will check game dir for additional hints ...",
          LOG_PREFIX);
      File[] gameFiles = GAME_DIR.listFiles();
      for (File gameFile : gameFiles) {
        if (gameFile.getName().contains("server")) {
          isClient = false;
        }
      }
    } else {
      log.error(
          "{} ⚠️ Unable to detected running environment, will stop here to avoid any possible damage!",
          LOG_PREFIX);
      return;
    }

    log.info("{} ♻️ init with game dir {} and mods dir {} for target {}", LOG_PREFIX, GAME_DIR,
        MODS_DIR, isClient ? "CLIENT" : "SERVER");

    log.info("{} 🚀 optimize Duplicated Mods ...", LOG_PREFIX);
    long start = System.nanoTime();
    int numDuplicatedMods = DuplicatedMods.searchDuplicatedMods(MODS_DIR);
    if (numDuplicatedMods > 0) {
      log.info("{} Removed {} duplicated mods in {} ms.", LOG_PREFIX, numDuplicatedMods,
          TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start));
    }

    if (isClient) {
      start = System.nanoTime();
      log.info("{} ✔️ Re-Enable possible client side mods ...", LOG_PREFIX);
      int numClientSideModsEnabled = ClientSideMods.enable(MODS_DIR);
      if (numClientSideModsEnabled > 0) {
        log.info("{} ✔️ Re-Enabled {} possible client side mods in {} ms.", LOG_PREFIX,
            numClientSideModsEnabled, TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start));
      }
    } else {
      start = System.nanoTime();
      log.info("{} ❌ Disable possible client side mods ...", LOG_PREFIX);
      int numClientSideModsDisabled = ClientSideMods.disable(MODS_DIR);
      if (numClientSideModsDisabled > 0) {
        DuplicatedMods.searchDuplicatedClientMods(MODS_DIR);
        log.info("{} ❌ Disabled {} possible client side mods in {} ms.", LOG_PREFIX,
            numClientSideModsDisabled, TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start));
      }
    }
  }

  @Override
  public List<ModFileOrException> scanMods() {
    return Collections.emptyList();
  }

  @Override
  public String name() {
    return "aptweaks_mods";
  }

  @Override
  public void scanFile(IModFile modFile, Consumer<Path> pathConsumer) {
    log.debug("scanFile {} {}", modFile, pathConsumer);
  }

  @Override
  public void initArguments(Map<String, ?> arguments) {
    log.debug("Env: {}", arguments);
  }

  @Override
  public boolean isValid(IModFile modFile) {
    log.debug("isValid {}", modFile);
    return false;
  }

}
