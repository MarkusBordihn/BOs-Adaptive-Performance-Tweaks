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

package de.markusbordihn.adaptiveperformancetweaks.feature.distance;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class SimulationDistanceConfigTest {

  @Test
  void defaultDistanceLevels() {
    assertEquals(12, SimulationDistanceConfig.simDistanceVeryLow);
    assertEquals(10, SimulationDistanceConfig.simDistanceLow);
    assertEquals(8, SimulationDistanceConfig.simDistanceNormal);
    assertEquals(6, SimulationDistanceConfig.simDistanceMedium);
    assertEquals(4, SimulationDistanceConfig.simDistanceHigh);
    assertEquals(2, SimulationDistanceConfig.simDistanceVeryHigh);
  }

  @Test
  void defaultBounds() {
    assertEquals(2, SimulationDistanceConfig.simDistanceMin);
    assertEquals(12, SimulationDistanceConfig.simDistanceMax);
  }

  @Test
  void distancesDecreaseAsLoadIncreases() {
    assertTrue(
      SimulationDistanceConfig.simDistanceVeryLow > SimulationDistanceConfig.simDistanceLow);
    assertTrue(
      SimulationDistanceConfig.simDistanceLow > SimulationDistanceConfig.simDistanceNormal);
    assertTrue(
      SimulationDistanceConfig.simDistanceNormal > SimulationDistanceConfig.simDistanceMedium);
    assertTrue(
      SimulationDistanceConfig.simDistanceMedium > SimulationDistanceConfig.simDistanceHigh);
    assertTrue(
      SimulationDistanceConfig.simDistanceHigh > SimulationDistanceConfig.simDistanceVeryHigh);
  }

  @Test
  void allDistancesWithinBounds() {
    assertTrue(
      SimulationDistanceConfig.simDistanceVeryHigh >= SimulationDistanceConfig.simDistanceMin);
    assertTrue(
      SimulationDistanceConfig.simDistanceVeryLow <= SimulationDistanceConfig.simDistanceMax);
  }

  @Test
  void defaultMovementThrottleValues() {
    assertTrue(SimulationDistanceConfig.movementThrottleEnabled);
    assertEquals(3, SimulationDistanceConfig.movementThrottleWindowSamples);
    assertEquals(5, SimulationDistanceConfig.movementThrottleWindowSamplesMax);
    assertEquals(20, SimulationDistanceConfig.movementThrottleSampleTicks);
    assertEquals(5, SimulationDistanceConfig.movementThrottleSpeedBlocksPerSecond);
    assertEquals(20, SimulationDistanceConfig.movementThrottleFastSpeedBlocksPerSecond);
    assertEquals(140, SimulationDistanceConfig.movementThrottleRecoveryDelayTicks);
    assertEquals(40, SimulationDistanceConfig.movementThrottleRecoveryStepTicks);
    assertEquals(60, SimulationDistanceConfig.movementThrottleLoginTicks);
    assertEquals(1, SimulationDistanceConfig.movementThrottleMinReduction);
    assertEquals(2, SimulationDistanceConfig.movementThrottleMaxReduction);
  }
}
