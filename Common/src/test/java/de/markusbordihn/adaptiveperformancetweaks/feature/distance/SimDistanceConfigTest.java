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

class SimDistanceConfigTest {

  @Test
  void defaultDistanceLevels() {
    assertEquals(12, SimDistanceConfig.simDistanceVeryLow);
    assertEquals(10, SimDistanceConfig.simDistanceLow);
    assertEquals(8, SimDistanceConfig.simDistanceNormal);
    assertEquals(6, SimDistanceConfig.simDistanceMedium);
    assertEquals(4, SimDistanceConfig.simDistanceHigh);
    assertEquals(2, SimDistanceConfig.simDistanceVeryHigh);
  }

  @Test
  void defaultBounds() {
    assertEquals(2, SimDistanceConfig.simDistanceMin);
    assertEquals(12, SimDistanceConfig.simDistanceMax);
  }

  @Test
  void distancesDecreaseAsLoadIncreases() {
    assertTrue(SimDistanceConfig.simDistanceVeryLow > SimDistanceConfig.simDistanceLow);
    assertTrue(SimDistanceConfig.simDistanceLow > SimDistanceConfig.simDistanceNormal);
    assertTrue(SimDistanceConfig.simDistanceNormal > SimDistanceConfig.simDistanceMedium);
    assertTrue(SimDistanceConfig.simDistanceMedium > SimDistanceConfig.simDistanceHigh);
    assertTrue(SimDistanceConfig.simDistanceHigh > SimDistanceConfig.simDistanceVeryHigh);
  }

  @Test
  void allDistancesWithinBounds() {
    assertTrue(SimDistanceConfig.simDistanceVeryHigh >= SimDistanceConfig.simDistanceMin);
    assertTrue(SimDistanceConfig.simDistanceVeryLow <= SimDistanceConfig.simDistanceMax);
  }
}
