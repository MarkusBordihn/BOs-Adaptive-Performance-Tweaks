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

import de.markusbordihn.minecraft.adaptiveperformancetweaksmods.config.ModsDatabase;

public class DuplicatedMods {

  protected DuplicatedMods() {}

  public static void searchDuplicatedMods(File modPath) {
    if (modPath == null) {
      return;
    }
    File[] modsFiles = modPath.listFiles();
    List<File> checkedFiles = new ArrayList<>();
    for (File modFile : modsFiles) {
      String modFileName = modFile.getName();
      if (modFileName.endsWith(".jar") && !checkedFiles.contains(modFile)) {
        String simplifiedModName = ModsDatabase.stripeVersionNumbers(modFileName);
        List<File> duplicatedMods = new ArrayList<>();
        for (File modFileToCompare : modsFiles) {
          String simplifiedModNameToCompare =
              ModsDatabase.stripeVersionNumbers(modFileToCompare.getName());
          if (modFile != modFileToCompare && simplifiedModName.equals(simplifiedModNameToCompare)) {
            duplicatedMods.add(modFileToCompare);
            checkedFiles.add(modFileToCompare);
          }
        }
        if (!duplicatedMods.isEmpty()) {
          duplicatedMods.add(modFile);
          System.out.printf("Found duplicated Mods: %s\n", duplicatedMods);
          System.out.printf("Most recent Mod: %s\n", findLatestMod(duplicatedMods));
          archiveDuplicatedMods(duplicatedMods);
        }
      }
    }
  }

  public static void archiveDuplicatedMods(List<File> modList) {
    File newestMod = findLatestMod(modList);
    for (File modFile : modList) {
      if (modFile != newestMod) {
        if (!modFile.delete()) {
          System.out.printf("Was unable to remove outdated mod %s!\n", modFile);
        }
      }
    }
  }

  public static File findLatestMod(List<File> modList) {
    modList.sort(Collections.reverseOrder());
    return modList.get(0);
  }
}
