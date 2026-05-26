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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import org.junit.jupiter.api.Test;

class SimulationDistanceManagerTest {

  @Test
  void lowAndNormalLoadDoNotThrottleMovement() {
    assertFalse(SimulationDistanceManager.supportsMovementThrottle(ServerLoadLevel.LOW));
    assertFalse(SimulationDistanceManager.supportsMovementThrottle(ServerLoadLevel.NORMAL));
    assertEquals(0, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.NORMAL, 4, 4));
  }

  @Test
  void mediumLoadNeedsHighExplorerShareForMaxReduction() {
    assertEquals(1, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.MEDIUM, 4, 1));
    assertEquals(2, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.MEDIUM, 4, 3));
  }

  @Test
  void highAndVeryHighLoadScaleMoreAggressively() {
    assertEquals(2, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.HIGH, 4, 2));
    assertEquals(2, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.VERY_HIGH, 4, 1));
    assertTrue(SimulationDistanceManager.supportsMovementThrottle(ServerLoadLevel.HIGH));
  }
}
