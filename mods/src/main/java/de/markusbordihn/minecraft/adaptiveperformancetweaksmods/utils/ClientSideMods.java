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

import de.markusbordihn.minecraft.adaptiveperformancetweaksmods.config.ModsDatabase;

public class ClientSideMods {

  public static final String CLIENT_MOD_EXTENSION = ".client";

  protected ClientSideMods() {}

  public static void enable(File modPath) {
    if (modPath == null) {
      return;
    }
    File[] modsFiles = modPath.listFiles();
    for (File modFile : modsFiles) {
      String modFileName = modFile.getName();
      if (modFileName.endsWith(CLIENT_MOD_EXTENSION) && isClientSide(modFileName)) {
        File clientFile = new File(
            modFile.getAbsoluteFile().toString().replace(".jar" + CLIENT_MOD_EXTENSION, ".jar"));
        if (clientFile.exists()) {
          if (!modFile.delete()) {
            System.out.printf("Was unable to remove duplicated client side mod %s!\n", modFile);
          }
        } else if (!modFile.renameTo(clientFile)) {
          System.out.printf("Was unable to enable client side mod %s!\n", modFile);
        }
      }
    }
  }

  public static void disable(File modPath) {
    if (modPath == null) {
      return;
    }
    File[] modsFiles = modPath.listFiles();
    for (File modFile : modsFiles) {
      String modFileName = modFile.getName();
      if (!modFileName.endsWith(CLIENT_MOD_EXTENSION) && modFileName.endsWith(".jar")
          && isClientSide(modFileName)) {
        File clientFile = new File(modFile.getAbsoluteFile() + CLIENT_MOD_EXTENSION);
        if (clientFile.exists()) {
          if (!modFile.delete()) {
            System.out.printf("Was unable to remove duplicated client side mod %s!\n", modFile);
          }
        } else if (!modFile.renameTo(clientFile)) {
          System.out.printf("Was unable to disable client side mod %s!\n", modFile);
        }
      }
    }
  }

  public static boolean isClientSide(String name) {
    String shortedModName = ModsDatabase.stripeVersionNumbers(name);
    return ModsDatabase.clientSideMods.contains(shortedModName);
  }

}
