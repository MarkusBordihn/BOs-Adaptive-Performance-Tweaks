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

package de.markusbordihn.adaptiveperformancetweaks.feature.benchmark;

import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.core.debug.DebugManager;
import de.markusbordihn.adaptiveperformancetweaks.core.debug.DebugModule;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.gamerules.GameRulesConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ArrowsConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ExperienceOrbsConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemsConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnConfig;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Map;
import java.util.Set;

final class BenchmarkFeatureState {

  private static final Set<FeatureToggle> CONFLICT_GATED_BENCHMARK_FEATURES =
    EnumSet.of(
      FeatureToggle.GAMERULES,
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE,
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE);
  private static final EnumMap<FeatureToggle, Boolean> savedFeatureState =
    new EnumMap<>(FeatureToggle.class);
  private static final EnumMap<FeatureToggle, ModConflictDetector.FeatureDecision>
    savedFeatureDecision = new EnumMap<>(FeatureToggle.class);
  private static final EnumMap<DebugModule, Boolean> savedDebugStates =
    new EnumMap<>(DebugModule.class);
  private static ServerLoadLevel savedItemsMinLoad;
  private static ServerLoadLevel savedXpMinLoad;
  private static ServerLoadLevel savedArrowsMinLoad;
  private static ServerLoadLevel savedSpawnMinLoad;
  private static ServerLoadLevel savedGameRulesMinLoad;
  private static ServerLoadLevel savedSimDistMinLoad;

  private BenchmarkFeatureState() {
  }

  static void saveMinLoadLevels() {
    savedItemsMinLoad = ItemsConfig.minOptimizationLoadLevel;
    savedXpMinLoad = ExperienceOrbsConfig.minOptimizationLoadLevel;
    savedArrowsMinLoad = ArrowsConfig.minOptimizationLoadLevel;
    savedSpawnMinLoad = SpawnConfig.minOptimizationLoadLevel;
    savedGameRulesMinLoad = GameRulesConfig.minOptimizationLoadLevel;
    savedSimDistMinLoad = SimulationDistanceConfig.minOptimizationLoadLevel;
  }

  static void forceMinLoadLevels() {
    ItemsConfig.minOptimizationLoadLevel = ServerLoadLevel.VERY_LOW;
    ExperienceOrbsConfig.minOptimizationLoadLevel = ServerLoadLevel.VERY_LOW;
    ArrowsConfig.minOptimizationLoadLevel = ServerLoadLevel.VERY_LOW;
    SpawnConfig.minOptimizationLoadLevel = ServerLoadLevel.VERY_LOW;
    GameRulesConfig.minOptimizationLoadLevel = ServerLoadLevel.VERY_LOW;
    SimulationDistanceConfig.minOptimizationLoadLevel = ServerLoadLevel.VERY_LOW;
  }

  static void restoreMinLoadLevels() {
    ItemsConfig.minOptimizationLoadLevel = savedItemsMinLoad;
    ExperienceOrbsConfig.minOptimizationLoadLevel = savedXpMinLoad;
    ArrowsConfig.minOptimizationLoadLevel = savedArrowsMinLoad;
    SpawnConfig.minOptimizationLoadLevel = savedSpawnMinLoad;
    GameRulesConfig.minOptimizationLoadLevel = savedGameRulesMinLoad;
    SimulationDistanceConfig.minOptimizationLoadLevel = savedSimDistMinLoad;
  }

  static void saveFeatureState() {
    savedFeatureState.clear();
    savedFeatureDecision.clear();
    for (FeatureToggle featureToggle : FeatureToggle.values()) {
      savedFeatureState.put(featureToggle, featureToggle.isEnabled());
      if (featureToggle != FeatureToggle.CORE) {
        savedFeatureDecision.put(featureToggle, CoreConfig.getFeatureDecision(featureToggle));
      }
    }
  }

  static void disableAllFeatures() {
    for (FeatureToggle featureToggle : FeatureToggle.values()) {
      if (featureToggle != FeatureToggle.CORE) {
        featureToggle.setEnabled(false);
      }
    }
  }

  static void restoreFeatures() {
    for (Map.Entry<FeatureToggle, Boolean> entry : savedFeatureState.entrySet()) {
      FeatureToggle featureToggle = entry.getKey();
      if (featureToggle != FeatureToggle.CORE && hasBenchmarkConflict(featureToggle)) {
        featureToggle.setEnabled(false);
        continue;
      }

      featureToggle.setEnabled(entry.getValue());
    }
  }

  private static boolean hasBenchmarkConflict(FeatureToggle featureToggle) {
    if (!CONFLICT_GATED_BENCHMARK_FEATURES.contains(featureToggle)) {
      return false;
    }

    ModConflictDetector.FeatureDecision decision = savedFeatureDecision.get(featureToggle);
    return decision != null && decision.relatedModId() != null;
  }

  static int countFeaturesByActivation(ModConflictDetector.FeatureActivation activation) {
    int count = 0;
    for (Map.Entry<FeatureToggle, ModConflictDetector.FeatureDecision> entry
      : savedFeatureDecision.entrySet()) {
      if (entry.getKey().scope() == FeatureToggle.Scope.CLIENT) {
        continue;
      }

      if (entry.getValue().activation() == activation) {
        count++;
      }
    }

    return count;
  }

  static void saveAllDebugStates() {
    savedDebugStates.clear();
    for (DebugModule module : DebugModule.values()) {
      savedDebugStates.put(module, DebugManager.isDebugLevel(module.getLoggerName()));
    }
  }

  static boolean hasAnyDebugActive() {
    return savedDebugStates.containsValue(true);
  }

  static void disableAllDebug() {
    for (DebugModule module : DebugModule.values()) {
      DebugManager.enableDebugLevel(module.getLoggerName(), false);
    }
  }

  static void restoreDebugState() {
    savedDebugStates.forEach((module, wasEnabled) -> {
      if (wasEnabled) {
        DebugManager.enableDebugLevel(module.getLoggerName(), true);
      }
    });
    savedDebugStates.clear();
  }

  static void clearAll() {
    savedFeatureState.clear();
    savedFeatureDecision.clear();
    savedDebugStates.clear();
    savedItemsMinLoad = null;
    savedXpMinLoad = null;
    savedArrowsMinLoad = null;
    savedSpawnMinLoad = null;
    savedGameRulesMinLoad = null;
    savedSimDistMinLoad = null;
  }
}
