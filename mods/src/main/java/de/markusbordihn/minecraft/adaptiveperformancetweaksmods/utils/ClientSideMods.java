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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.markusbordihn.minecraft.adaptiveperformancetweaksmods.Constants;
import de.markusbordihn.minecraft.adaptiveperformancetweaksmods.config.ModsDatabase;

public class ClientSideMods {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

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
        log.info("[Client Side Mod] ✔️ Try to enable client side mod {} ...", modFileName);
        if (clientFile.exists()) {
          if (!modFile.delete()) {
            log.error("[Client Side Mod] ⚠️ Was unable to remove duplicated client side mod {}!", modFile);
          }
        } else if (!modFile.renameTo(clientFile)) {
          log.error("[Client Side Mod] ⚠️ Was unable to enable client side mod {}!", modFile);
        }
      } else {

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
        log.info("[Client Side Mod] ❌ Try to disable client side mod {} ...", modFileName);
        if (clientFile.exists()) {
          if (!modFile.delete()) {
            log.error("[Client Side Mod] ⚠️ Was unable to remove duplicated client side mod {}!", modFile);
          }
        } else if (!modFile.renameTo(clientFile)) {
          log.error("[Client Side Mod] ⚠️ Was unable to disable client side mod {}!", modFile);
        }
      }
    }
  }

  public static boolean isClientSide(String name) {
    String shortedModName = ModsDatabase.stripeVersionNumbers(name);
    return ModsDatabase.clientSideMods.contains(shortedModName);
  }

}
