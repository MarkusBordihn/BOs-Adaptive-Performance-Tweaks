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

class ViewDistanceConfigTest {

  @Test
  void defaultDistanceLevels() {
    assertEquals(12, ViewDistanceConfig.viewDistanceVeryLow);
    assertEquals(10, ViewDistanceConfig.viewDistanceLow);
    assertEquals(8, ViewDistanceConfig.viewDistanceNormal);
    assertEquals(6, ViewDistanceConfig.viewDistanceMedium);
    assertEquals(5, ViewDistanceConfig.viewDistanceHigh);
    assertEquals(4, ViewDistanceConfig.viewDistanceVeryHigh);
  }

  @Test
  void defaultBounds() {
    assertEquals(4, ViewDistanceConfig.viewDistanceMin);
    assertEquals(32, ViewDistanceConfig.viewDistanceMax);
  }

  @Test
  void distancesDecreaseAsLoadIncreases() {
    assertTrue(ViewDistanceConfig.viewDistanceVeryLow > ViewDistanceConfig.viewDistanceLow);
    assertTrue(ViewDistanceConfig.viewDistanceLow > ViewDistanceConfig.viewDistanceNormal);
    assertTrue(ViewDistanceConfig.viewDistanceNormal > ViewDistanceConfig.viewDistanceMedium);
    assertTrue(ViewDistanceConfig.viewDistanceMedium > ViewDistanceConfig.viewDistanceHigh);
    assertTrue(ViewDistanceConfig.viewDistanceHigh > ViewDistanceConfig.viewDistanceVeryHigh);
  }

  @Test
  void allDistancesWithinBounds() {
    assertTrue(ViewDistanceConfig.viewDistanceVeryHigh >= ViewDistanceConfig.viewDistanceMin);
    assertTrue(ViewDistanceConfig.viewDistanceVeryLow <= ViewDistanceConfig.viewDistanceMax);
  }
}
