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

package de.markusbordihn.adaptiveperformancetweaks.feature.spawn;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SpawnPresetRegistry {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_SPAWN);
  private static final Map<String, Optional<SpawnPreset>> entityPresetCache = new HashMap<>();
  private static List<SpawnPreset> loadedPresets = Collections.emptyList();

  private SpawnPresetRegistry() {
  }

  public static void reload(List<SpawnPreset> presets) {
    entityPresetCache.clear();
    List<SpawnPreset> sorted = new ArrayList<>(presets);
    sorted.sort((presetA, presetB) -> Integer.compare(presetB.priority(), presetA.priority()));
    loadedPresets = Collections.unmodifiableList(sorted);
    log.info("SpawnPresetRegistry loaded {} presets", loadedPresets.size());
    if (log.isDebugEnabled()) {
      for (SpawnPreset preset : loadedPresets) {
        log.debug(
          "[Preset] mod={} priority={} perPlayer={} perWorld={} perServer={} perChunk={} allow={} deny={}",
          preset.modId(), preset.priority(),
          preset.entities().perPlayerMax(), preset.entities().perWorldMax(),
          preset.entities().perServerMax(), preset.entities().perChunkMax(),
          preset.entities().allowList(), preset.entities().denyList());
      }
    }
  }

  public static SpawnDecision evaluate(String entityId, String dimensionId) {
    SpawnPreset preset = getEffectivePreset(entityId, dimensionId);
    if (preset == null) {
      return SpawnDecision.ALLOW;
    }

    Set<String> denyList = preset.entities().denyList();
    if (!denyList.isEmpty() && isEntityInList(denyList, entityId)) {
      return SpawnDecision.DENY;
    }

    Set<String> allowList = preset.entities().allowList();
    if (!allowList.isEmpty() && !isEntityInList(allowList, entityId)) {
      return SpawnDecision.DENY;
    }

    return SpawnDecision.ALLOW;
  }

  public static int getEffectivePerPlayerMax(String entityId, String dimensionId,
    ServerLoadLevel loadLevel) {
    return getEffectiveLimit(entityId, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerPlayer,
      preset -> preset.entities().perPlayerMax());
  }

  public static int getEffectivePerWorldMax(String entityId, String dimensionId,
    ServerLoadLevel loadLevel) {
    return getEffectiveLimit(entityId, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerWorld,
      preset -> preset.entities().perWorldMax());
  }

  public static int getEffectivePerServerMax(String entityId, String dimensionId,
    ServerLoadLevel loadLevel) {
    return getEffectiveLimit(entityId, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerServer,
      preset -> preset.entities().perServerMax());
  }

  public static int getEffectivePerChunkMax(String entityId, String dimensionId,
    ServerLoadLevel loadLevel) {
    return getEffectiveLimit(entityId, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerChunk,
      preset -> preset.entities().perChunkMax());
  }

  public static boolean isEntityInList(Set<String> list, String entityId) {
    if (list.isEmpty()) {
      return false;
    }

    if (list.contains(entityId)) {
      return true;
    }

    int colonIdx = entityId.indexOf(':');
    if (colonIdx > 0) {
      String namespaceWildcard = entityId.substring(0, colonIdx + 1) + "*";
      if (list.contains(namespaceWildcard)) {
        return true;
      }
    }

    return list.contains("*");
  }

  private static int getEffectiveLimit(
    String entityId, String dimensionId, ServerLoadLevel loadLevel,
    int globalDefault, java.util.function.ToIntFunction<SpawnPreset> limitExtractor) {
    SpawnPreset preset = getEffectivePreset(entityId, dimensionId);
    int rawLimit = preset != null ? limitExtractor.applyAsInt(preset) : globalDefault;
    if (rawLimit < 0) {
      return -1;
    }

    double factor = preset != null ? preset.loadFactors().forLevel(loadLevel) : 1.0;

    return Math.max(1, (int) Math.round(rawLimit * factor));
  }

  private static SpawnPreset getEffectivePreset(String entityId, String dimensionId) {
    return entityPresetCache.computeIfAbsent(entityId + "@" + dimensionId,
      key -> findPreset(entityId, dimensionId)).orElse(null);
  }

  private static Optional<SpawnPreset> findPreset(String entityId, String dimensionId) {
    for (SpawnPreset preset : loadedPresets) {
      if (!appliesToDimension(preset, dimensionId)) {
        continue;
      }
      if (presetCoversEntity(preset, entityId)) {
        return Optional.of(preset);
      }
    }

    return Optional.empty();
  }

  private static boolean presetCoversEntity(SpawnPreset preset, String entityId) {
    Set<String> allowList = preset.entities().allowList();
    if (!allowList.isEmpty() && isEntityInList(allowList, entityId)) {
      return true;
    }

    Set<String> denyList = preset.entities().denyList();
    if (!denyList.isEmpty() && isEntityInList(denyList, entityId)) {
      return true;
    }

    if (allowList.isEmpty() && denyList.isEmpty() && preset.modId() != null) {
      int colonIdx = entityId.indexOf(':');
      if (colonIdx > 0) {
        return entityId.substring(0, colonIdx).equals(preset.modId());
      }
    }
    return false;
  }

  private static boolean appliesToDimension(SpawnPreset preset, String dimensionId) {
    SpawnPreset.DimensionFilter dimensions = preset.dimensions();
    if (dimensions.ignore().contains(dimensionId)) {
      return false;
    }

    if (dimensions.deny().contains(dimensionId)) {
      return false;
    }

    if (!dimensions.allow().isEmpty() && !dimensions.allow().contains(dimensionId)) {
      return false;
    }

    return true;
  }
}
