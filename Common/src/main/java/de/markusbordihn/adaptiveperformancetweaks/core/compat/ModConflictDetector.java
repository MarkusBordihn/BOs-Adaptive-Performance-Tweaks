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
import java.util.LinkedHashMap;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ModConflictDetector {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final Map<String, String> LEGACY_MODULES = new LinkedHashMap<>();
  private static boolean compatibilityWarningsLogged = false;

  static {
    LEGACY_MODULES.put("adaptive_performance_tweaks_core", "APTweaks: Core");
    LEGACY_MODULES.put("adaptive_performance_tweaks_spawn", "APTweaks: Spawn");
    LEGACY_MODULES.put("adaptive_performance_tweaks_player", "APTweaks: Player/Login");
    LEGACY_MODULES.put("adaptive_performance_tweaks_gamerules", "APTweaks: Gamerules");
    LEGACY_MODULES.put("adaptive_performance_tweaks_items", "APTweaks: Items");
  }

  private ModConflictDetector() {
  }

  public static FeatureDecision resolveFeatureDecision(
    FeatureToggle toggle, FeatureState configuredState) {
    if (configuredState == FeatureState.DISABLED) {
      return new FeatureDecision(false, FeatureActivation.MANUAL_DISABLED, null);
    }

    String conflictingMod = findFirstLoadedMod(toggle.getConflictingMods());
    String warningMod = findFirstLoadedMod(toggle.getWarningOnlyMods());

    if (configuredState == FeatureState.ENABLED) {
      if (conflictingMod != null) {
        log.warn(
          "Feature '{}' is ENABLED but mod '{}' also handles this functionality."
            + " Consider setting 'feature.{}=auto' to let the mod take over.",
          toggle.getId(),
          conflictingMod,
          toggle.getId());
      } else if (warningMod != null) {
        log.warn(
          "Feature '{}' is ENABLED while mod '{}' may overlap with this functionality."
            + " Watch for duplicate behavior or unexpected performance changes.",
          toggle.getId(),
          warningMod);
      }
      return new FeatureDecision(true, FeatureActivation.MANUAL_ENABLED, conflictingMod);
    }

    if (conflictingMod != null) {
      log.warn(
        "Feature '{}' auto-disabled: mod '{}' already handles this functionality.",
        toggle.getId(),
        conflictingMod);
      return new FeatureDecision(false, FeatureActivation.CONFLICT_DISABLED, conflictingMod);
    }

    if (warningMod != null) {
      log.warn(
        "Feature '{}' remains enabled, but mod '{}' may overlap with this functionality.",
        toggle.getId(),
        warningMod);
    }

    return new FeatureDecision(true, FeatureActivation.AUTO_ENABLED, warningMod);
  }

  public static boolean resolveFeatureState(FeatureToggle toggle, FeatureState configuredState) {
    return resolveFeatureDecision(toggle, configuredState).enabled();
  }

  public static void logCompatibilityWarnings() {
    if (compatibilityWarningsLogged) {
      return;
    }

    compatibilityWarningsLogged = true;
    for (Map.Entry<String, String> entry : LEGACY_MODULES.entrySet()) {
      if (ModCompat.isModLoaded(entry.getKey())) {
        log.warn(
          "Detected legacy module '{}' (mod id '{}'). The bundled Adaptive Performance Tweaks mod"
            + " already includes this functionality. Remove the old standalone module to avoid"
            + " duplicate handlers and confusing config behavior.",
          entry.getValue(),
          entry.getKey());
      }
    }
  }

  private static String findFirstLoadedMod(Iterable<String> modIds) {
    for (String modId : modIds) {
      if (ModCompat.isModLoaded(modId)) {
        return modId;
      }
    }

    return null;
  }

  public enum FeatureActivation {
    MANUAL_ENABLED,
    AUTO_ENABLED,
    MANUAL_DISABLED,
    CONFLICT_DISABLED
  }

  public record FeatureDecision(
    boolean enabled,
    FeatureActivation activation,
    String relatedModId) {

  }
}
