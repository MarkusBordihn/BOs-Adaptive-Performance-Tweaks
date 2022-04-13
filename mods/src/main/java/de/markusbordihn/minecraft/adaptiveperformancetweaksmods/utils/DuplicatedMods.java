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

package de.markusbordihn.minecraft.adaptiveperformancetweaksmods.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.markusbordihn.minecraft.adaptiveperformancetweaksmods.Constants;
import de.markusbordihn.minecraft.adaptiveperformancetweaksmods.config.ModsDatabase;

public class DuplicatedMods {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String LOG_PREFIX = "[Duplicated Mods]";

  protected DuplicatedMods() {}

  public static int searchDuplicatedMods(File modPath) {
    return searchDuplicatedMods(modPath, ".jar", false);
  }

  public static int searchDuplicatedClientMods(File modPath) {
    return searchDuplicatedMods(modPath, ".jar.client", false);
  }

  public static int searchDuplicatedMods(File modPath, String fileExtension, boolean testMode) {
    int result = 0;
    if (modPath == null || !modPath.exists()) {
      log.error("{} unable to find valid mod path: {}", LOG_PREFIX, modPath);
      return result;
    }
    File[] modsFiles = modPath.listFiles();
    log.info("{} checking ~{} mods in {} for duplication with file extension {} ...", LOG_PREFIX,
        modsFiles.length, modPath, fileExtension);
    List<File> checkedFiles = new ArrayList<>();
    for (File modFile : modsFiles) {
      String modFileName = modFile.getName();
      if (modFileName.endsWith(fileExtension) && !checkedFiles.contains(modFile)) {
        String simplifiedModName = ModsDatabase.stripeVersionNumbers(modFileName);
        List<File> duplicatedMods = new ArrayList<>();
        for (File modFileToCompare : modsFiles) {
          String modFileToCompareName = modFileToCompare.getName();
          if (modFileToCompareName.endsWith(fileExtension)) {
            String simplifiedModNameToCompare =
                ModsDatabase.stripeVersionNumbers(modFileToCompareName);
            if (modFile != modFileToCompare
                && simplifiedModName.equals(simplifiedModNameToCompare)) {
              duplicatedMods.add(modFileToCompare);
              checkedFiles.add(modFileToCompare);
            }
          }
        }
        if (!duplicatedMods.isEmpty()) {
          duplicatedMods.add(modFile);
          log.info("{} ⚠️ Found duplicated Mods: {}", LOG_PREFIX, duplicatedMods);
          log.info("{} ✔️ Will keep most recent Mod: {}", LOG_PREFIX,
              findLatestMod(duplicatedMods));
          archiveDuplicatedMods(duplicatedMods, testMode);
          result++;
        }
      }
    }
    return result;
  }

  public static void archiveDuplicatedMods(List<File> modList, boolean testMode) {
    File newestMod = findLatestMod(modList);
    for (File modFile : modList) {
      if (modFile != newestMod) {
        if (testMode) {
          log.info("{} Would remove duplicated mod {} ...", LOG_PREFIX, modFile);
        } else if (!modFile.delete()) {
          log.error("{} ⚠️ Was unable to remove outdated mod {}!", LOG_PREFIX, modFile);
        }
      }
    }
  }

  public static File findLatestMod(List<File> modList) {
    modList.sort(Collections.reverseOrder());
    return modList.get(0);
  }
}
