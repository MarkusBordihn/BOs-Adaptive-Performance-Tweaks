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

import static org.junit.jupiter.api.Assertions.assertEquals;

import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class PerformanceStatsTest {

  @AfterEach
  void cleanup() {
    PerformanceStats.reset();
  }

  @Test
  void resetClearsAllCounters() {
    PerformanceStats.mobSpawnChecks = 1;
    PerformanceStats.mobSpawnsDenied = 2;
    PerformanceStats.specialSpawnBonusesApplied = 3;
    PerformanceStats.naturalSpawnChecks = 4;
    PerformanceStats.naturalSpawnsDenied = 5;
    PerformanceStats.itemsMerged = 6;
    PerformanceStats.itemsRemoved = 7;
    PerformanceStats.trackingEvaluations = 8;
    PerformanceStats.trackingExcludedEarlyCache = 9;
    PerformanceStats.trackingExcludedManualNamespace = 10;
    PerformanceStats.trackingExcludedManualEntity = 11;
    PerformanceStats.trackingExcludedAutoNamespace = 12;
    PerformanceStats.trackingExcludedAutoEntity = 13;
    PerformanceStats.trackingProtectedLiving = 14;
    PerformanceStats.trackingProtectedPersistent = 15;
    PerformanceStats.trackingTracked = 16;
    PerformanceStats.recordTrackingCategory(TrackingCategory.TECHNICAL);
    PerformanceStats.xpOrbsMerged = 17;
    PerformanceStats.xpOrbsRemoved = 18;
    PerformanceStats.gameRulesChanged = 19;
    PerformanceStats.viewDistanceChanges = 20;
    PerformanceStats.simulationDistanceChanges = 21;
    PerformanceStats.simulationDistanceMovementAdjustments = 22;
    PerformanceStats.simulationDistanceMovementThrottleSamples = 23;
    PerformanceStats.simulationDistanceMovementMaxReduction = 24;

    PerformanceStats.reset();

    PerformanceStats.Snapshot snapshot = PerformanceStats.snapshot();
    assertEquals(0, snapshot.mobSpawnChecks());
    assertEquals(0, snapshot.mobSpawnsDenied());
    assertEquals(0, snapshot.specialSpawnBonusesApplied());
    assertEquals(0, snapshot.naturalSpawnChecks());
    assertEquals(0, snapshot.naturalSpawnsDenied());
    assertEquals(0, snapshot.itemsMerged());
    assertEquals(0, snapshot.itemsRemoved());
    assertEquals(0, snapshot.trackingEvaluations());
    assertEquals(0, snapshot.trackingExcludedEarlyCache());
    assertEquals(0, snapshot.trackingExcludedManualNamespace());
    assertEquals(0, snapshot.trackingExcludedManualEntity());
    assertEquals(0, snapshot.trackingExcludedAutoNamespace());
    assertEquals(0, snapshot.trackingExcludedAutoEntity());
    assertEquals(0, snapshot.trackingProtectedLiving());
    assertEquals(0, snapshot.trackingProtectedPersistent());
    assertEquals(0, snapshot.trackingTracked());
    assertEquals(0, snapshot.trackingExcludedByCategory().get(TrackingCategory.TECHNICAL));
    assertEquals(0, snapshot.xpOrbsMerged());
    assertEquals(0, snapshot.xpOrbsRemoved());
    assertEquals(0, snapshot.gameRulesChanged());
    assertEquals(0, snapshot.viewDistanceChanges());
    assertEquals(0, snapshot.simulationDistanceChanges());
    assertEquals(0, snapshot.simulationDistanceMovementAdjustments());
    assertEquals(0, snapshot.simulationDistanceMovementThrottleSamples());
    assertEquals(0, snapshot.simulationDistanceMovementMaxReduction());
  }
}
