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

import static org.junit.jupiter.api.Assertions.assertEquals;

import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingMode;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;

class SpawnPresetPartitionerTest {

  @Test
  void partitionSeparatesTrackingAndSpawnPresets() {
    SpawnPreset spawnPreset = new SpawnPreset(
      false,
      "minecraft",
      List.of(),
      50,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of("minecraft:zombie"), Set.of(), 4, 8, 16, 2),
      SpawnPreset.LoadFactors.defaults(),
      null,
      null,
      "",
      Set.of());
    SpawnPreset trackingPreset = new SpawnPreset(
      false,
      "immersive_aircraft",
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of(), Set.of(), 1, 1, 1, 1),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_NAMESPACE,
      TrackingCategory.VEHICLE_STRUCTURE,
      "Vehicle helpers should not be tracked.",
      Set.of());

    SpawnPresetPartitioner.Partition partition =
      SpawnPresetPartitioner.partition(List.of(trackingPreset, spawnPreset));

    assertEquals(List.of(trackingPreset), partition.trackingPresets());
    assertEquals(List.of(spawnPreset), partition.spawnPresets());
  }
}
