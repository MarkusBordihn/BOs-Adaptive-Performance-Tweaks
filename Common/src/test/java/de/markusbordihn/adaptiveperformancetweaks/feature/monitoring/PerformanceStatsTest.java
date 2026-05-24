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
    PerformanceStats.naturalSpawnChecks = 3;
    PerformanceStats.naturalSpawnsDenied = 4;
    PerformanceStats.itemsMerged = 5;
    PerformanceStats.itemsRemoved = 6;
    PerformanceStats.xpOrbsMerged = 7;
    PerformanceStats.xpOrbsRemoved = 8;

    PerformanceStats.reset();

    PerformanceStats.Snapshot snapshot = PerformanceStats.snapshot();
    assertEquals(0, snapshot.mobSpawnChecks());
    assertEquals(0, snapshot.mobSpawnsDenied());
    assertEquals(0, snapshot.naturalSpawnChecks());
    assertEquals(0, snapshot.naturalSpawnsDenied());
    assertEquals(0, snapshot.itemsMerged());
    assertEquals(0, snapshot.itemsRemoved());
    assertEquals(0, snapshot.xpOrbsMerged());
    assertEquals(0, snapshot.xpOrbsRemoved());
  }
}
