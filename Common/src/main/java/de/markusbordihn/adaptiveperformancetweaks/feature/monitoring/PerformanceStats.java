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

package de.markusbordihn.adaptiveperformancetweaks.feature.monitoring;

import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;

public final class PerformanceStats {

  private static final long[] trackingExcludedByCategory =
    new long[TrackingCategory.values().length];
  public static long mobSpawnChecks;
  public static long mobSpawnsExcluded;
  public static long mobSpawnsDenied;
  public static long specialSpawnBonusesApplied;
  public static long naturalSpawnChecks;
  public static long naturalSpawnsDenied;
  public static long itemsMerged;
  public static long itemsRemoved;
  public static long trackingEvaluations;
  public static long trackingExcludedEarlyCache;
  public static long trackingExcludedManualNamespace;
  public static long trackingExcludedManualEntity;
  public static long trackingExcludedAutoNamespace;
  public static long trackingExcludedAutoEntity;
  public static long trackingProtectedLiving;
  public static long trackingProtectedPersistent;
  public static long trackingTracked;
  public static long xpOrbsMerged;
  public static long xpOrbsRemoved;
  public static long entityChunkCleanupRemoved;
  public static long arrowsRemoved;
  public static long gameRulesChanged;
  public static long viewDistanceChanges;
  public static long simulationDistanceChanges;
  public static long simulationDistanceMovementAdjustments;
  public static long simulationDistanceMovementThrottleSamples;
  public static long simulationDistanceMovementMaxReduction;
  private static boolean detailedTrackingStatsEnabled = false;

  static {
    resetTrackingCategoryCounters();
  }

  private PerformanceStats() {
  }

  public static boolean isDetailedTrackingStatsEnabled() {
    return detailedTrackingStatsEnabled;
  }

  public static void setDetailedTrackingStatsEnabled(boolean enabled) {
    detailedTrackingStatsEnabled = enabled;
  }

  public static void recordTrackingCategory(TrackingCategory category) {
    if (!detailedTrackingStatsEnabled) {
      return;
    }

    TrackingCategory normalized = category != null ? category : TrackingCategory.UNKNOWN;
    trackingExcludedByCategory[normalized.ordinal()]++;
  }

  public static void reset() {
    mobSpawnChecks = 0;
    mobSpawnsExcluded = 0;
    mobSpawnsDenied = 0;
    specialSpawnBonusesApplied = 0;
    naturalSpawnChecks = 0;
    naturalSpawnsDenied = 0;
    itemsMerged = 0;
    itemsRemoved = 0;
    trackingEvaluations = 0;
    trackingExcludedEarlyCache = 0;
    trackingExcludedManualNamespace = 0;
    trackingExcludedManualEntity = 0;
    trackingExcludedAutoNamespace = 0;
    trackingExcludedAutoEntity = 0;
    trackingProtectedLiving = 0;
    trackingProtectedPersistent = 0;
    trackingTracked = 0;
    xpOrbsMerged = 0;
    xpOrbsRemoved = 0;
    entityChunkCleanupRemoved = 0;
    arrowsRemoved = 0;
    gameRulesChanged = 0;
    viewDistanceChanges = 0;
    simulationDistanceChanges = 0;
    simulationDistanceMovementAdjustments = 0;
    simulationDistanceMovementThrottleSamples = 0;
    simulationDistanceMovementMaxReduction = 0;
    resetTrackingCategoryCounters();
  }

  public static Snapshot snapshot() {
    return new Snapshot(
      mobSpawnChecks, mobSpawnsExcluded, mobSpawnsDenied,
      specialSpawnBonusesApplied,
      naturalSpawnChecks, naturalSpawnsDenied,
      itemsMerged, itemsRemoved,
      trackingEvaluations, trackingExcludedEarlyCache,
      trackingExcludedManualNamespace, trackingExcludedManualEntity,
      trackingExcludedAutoNamespace, trackingExcludedAutoEntity,
      trackingProtectedLiving, trackingProtectedPersistent, trackingTracked,
      Collections.unmodifiableMap(buildTrackingCategorySnapshot()),
      xpOrbsMerged, xpOrbsRemoved,
      entityChunkCleanupRemoved,
      arrowsRemoved,
      gameRulesChanged, viewDistanceChanges, simulationDistanceChanges,
      simulationDistanceMovementAdjustments,
      simulationDistanceMovementThrottleSamples,
      simulationDistanceMovementMaxReduction);
  }

  public static Snapshot delta(Snapshot start, Snapshot end) {
    EnumMap<TrackingCategory, Long> trackingDelta = new EnumMap<>(TrackingCategory.class);
    for (TrackingCategory category : TrackingCategory.values()) {
      long startValue = start.trackingExcludedByCategory().getOrDefault(category, 0L);
      long endValue = end.trackingExcludedByCategory().getOrDefault(category, 0L);
      trackingDelta.put(category, endValue - startValue);
    }

    return new Snapshot(
      end.mobSpawnChecks() - start.mobSpawnChecks(),
      end.mobSpawnsExcluded() - start.mobSpawnsExcluded(),
      end.mobSpawnsDenied() - start.mobSpawnsDenied(),
      end.specialSpawnBonusesApplied() - start.specialSpawnBonusesApplied(),
      end.naturalSpawnChecks() - start.naturalSpawnChecks(),
      end.naturalSpawnsDenied() - start.naturalSpawnsDenied(),
      end.itemsMerged() - start.itemsMerged(),
      end.itemsRemoved() - start.itemsRemoved(),
      end.trackingEvaluations() - start.trackingEvaluations(),
      end.trackingExcludedEarlyCache() - start.trackingExcludedEarlyCache(),
      end.trackingExcludedManualNamespace() - start.trackingExcludedManualNamespace(),
      end.trackingExcludedManualEntity() - start.trackingExcludedManualEntity(),
      end.trackingExcludedAutoNamespace() - start.trackingExcludedAutoNamespace(),
      end.trackingExcludedAutoEntity() - start.trackingExcludedAutoEntity(),
      end.trackingProtectedLiving() - start.trackingProtectedLiving(),
      end.trackingProtectedPersistent() - start.trackingProtectedPersistent(),
      end.trackingTracked() - start.trackingTracked(),
      Collections.unmodifiableMap(trackingDelta),
      end.xpOrbsMerged() - start.xpOrbsMerged(),
      end.xpOrbsRemoved() - start.xpOrbsRemoved(),
      end.entityChunkCleanupRemoved() - start.entityChunkCleanupRemoved(),
      end.arrowsRemoved() - start.arrowsRemoved(),
      end.gameRulesChanged() - start.gameRulesChanged(),
      end.viewDistanceChanges() - start.viewDistanceChanges(),
      end.simulationDistanceChanges() - start.simulationDistanceChanges(),
      end.simulationDistanceMovementAdjustments() - start.simulationDistanceMovementAdjustments(),
      end.simulationDistanceMovementThrottleSamples()
        - start.simulationDistanceMovementThrottleSamples(),
      end.simulationDistanceMovementMaxReduction()
        - start.simulationDistanceMovementMaxReduction());
  }

  private static void resetTrackingCategoryCounters() {
    Arrays.fill(trackingExcludedByCategory, 0L);
  }

  private static Map<TrackingCategory, Long> buildTrackingCategorySnapshot() {
    EnumMap<TrackingCategory, Long> snapshot = new EnumMap<>(TrackingCategory.class);
    for (TrackingCategory category : TrackingCategory.values()) {
      snapshot.put(category, trackingExcludedByCategory[category.ordinal()]);
    }
    return snapshot;
  }

  public record Snapshot(
    long mobSpawnChecks,
    long mobSpawnsExcluded,
    long mobSpawnsDenied,
    long specialSpawnBonusesApplied,
    long naturalSpawnChecks,
    long naturalSpawnsDenied,
    long itemsMerged,
    long itemsRemoved,
    long trackingEvaluations,
    long trackingExcludedEarlyCache,
    long trackingExcludedManualNamespace,
    long trackingExcludedManualEntity,
    long trackingExcludedAutoNamespace,
    long trackingExcludedAutoEntity,
    long trackingProtectedLiving,
    long trackingProtectedPersistent,
    long trackingTracked,
    Map<TrackingCategory, Long> trackingExcludedByCategory,
    long xpOrbsMerged,
    long xpOrbsRemoved,
    long entityChunkCleanupRemoved,
    long arrowsRemoved,
    long gameRulesChanged,
    long viewDistanceChanges,
    long simulationDistanceChanges,
    long simulationDistanceMovementAdjustments,
    long simulationDistanceMovementThrottleSamples,
    long simulationDistanceMovementMaxReduction) {

  }
}
