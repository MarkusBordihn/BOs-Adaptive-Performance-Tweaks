/*
 * Copyright 2024 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweaks.core.compat;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureState;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ModConflictDetector {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ModConflictDetector() {
  }

  public static boolean resolveFeatureState(FeatureToggle toggle, FeatureState configuredState) {
    if (configuredState == FeatureState.DISABLED) {
      return false;
    }

    String conflictingMod = findFirstConflictingMod(toggle);

    if (configuredState == FeatureState.ENABLED) {
      if (conflictingMod != null) {
        log.warn(
          "Feature '{}' is ENABLED but mod '{}' also handles this functionality."
            + " Consider setting 'feature.{}=auto' to let the mod take over.",
          toggle.getId(),
          conflictingMod,
          toggle.getId());
      }
      return true;
    }

    if (conflictingMod != null) {
      log.warn(
        "Feature '{}' auto-disabled: mod '{}' already handles this functionality.",
        toggle.getId(),
        conflictingMod);
      return false;
    }

    return true;
  }

  private static String findFirstConflictingMod(FeatureToggle toggle) {
    for (String modId : toggle.getConflictingMods()) {
      if (ModCompat.isModLoaded(modId)) {
        return modId;
      }
    }

    return null;
  }
}
