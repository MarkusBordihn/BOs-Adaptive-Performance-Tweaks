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

public final class PerformanceStats {

  public static long mobSpawnChecks;
  public static long mobSpawnsExcluded;
  public static long mobSpawnsDenied;
  public static long naturalSpawnChecks;
  public static long naturalSpawnsDenied;
  public static long itemsMerged;
  public static long itemsRemoved;
  public static long xpOrbsMerged;
  public static long xpOrbsRemoved;

  private PerformanceStats() {
  }

  public static void reset() {
    mobSpawnChecks = 0;
    mobSpawnsExcluded = 0;
    mobSpawnsDenied = 0;
    naturalSpawnChecks = 0;
    naturalSpawnsDenied = 0;
    itemsMerged = 0;
    itemsRemoved = 0;
    xpOrbsMerged = 0;
    xpOrbsRemoved = 0;
  }

  public static Snapshot snapshot() {
    return new Snapshot(
      mobSpawnChecks, mobSpawnsExcluded, mobSpawnsDenied,
      naturalSpawnChecks, naturalSpawnsDenied,
      itemsMerged, itemsRemoved,
      xpOrbsMerged, xpOrbsRemoved);
  }

  public record Snapshot(
    long mobSpawnChecks,
    long mobSpawnsExcluded,
    long mobSpawnsDenied,
    long naturalSpawnChecks,
    long naturalSpawnsDenied,
    long itemsMerged,
    long itemsRemoved,
    long xpOrbsMerged,
    long xpOrbsRemoved) {

  }
}
