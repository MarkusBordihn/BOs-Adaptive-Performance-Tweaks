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

import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import org.junit.jupiter.api.Test;

class SimulationDistanceManagerTest {

  private static void writeStaticField(String fieldName, Object value) throws Exception {
    Field field = SimulationDistanceManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static int invokeIntMethod(String methodName) throws Exception {
    Method method = SimulationDistanceManager.class.getDeclaredMethod(methodName);
    method.setAccessible(true);
    return (int) method.invoke(null);
  }

  private static Object readStaticField(String fieldName) throws Exception {
    Field field = SimulationDistanceManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    return field.get(null);
  }

  private static void invokeUpdateMovementThrottle(boolean recordMovementSample) throws Exception {
    Method method = SimulationDistanceManager.class.getDeclaredMethod("updateMovementThrottle",
      boolean.class);
    method.setAccessible(true);
    method.invoke(null, recordMovementSample);
  }

  @Test
  void allLoadLevelsThrottleMovement() {
    assertTrue(SimulationDistanceManager.supportsMovementThrottle(ServerLoadLevel.VERY_LOW));
    assertTrue(SimulationDistanceManager.supportsMovementThrottle(ServerLoadLevel.LOW));
    assertTrue(SimulationDistanceManager.supportsMovementThrottle(ServerLoadLevel.NORMAL));
    assertTrue(SimulationDistanceManager.supportsMovementThrottle(ServerLoadLevel.HIGH));
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

  @Test
  void configuredDistanceMaxRespectsServerStartupLimit() throws Exception {
    writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMax + 4);
    assertEquals(SimulationDistanceConfig.simDistanceMax,
      invokeIntMethod("getConfiguredDistanceMax"));

    writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMin + 3);
    assertEquals(SimulationDistanceConfig.simDistanceMin + 3,
      invokeIntMethod("getConfiguredDistanceMax"));
  }

  @Test
  void warmupReductionDropsToConfiguredMinimumDistance() throws Exception {
    writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMin + 4);
    assertEquals(4, invokeIntMethod("getWarmupReduction"));

    writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMin);
    assertEquals(0, invokeIntMethod("getWarmupReduction"));
  }

  @Test
  void movementRecoveryResumesAtLowLoad() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("currentLoadLevel", ServerLoadLevel.LOW);
    writeStaticField("currentMovementReduction", 2);
    writeStaticField("recoveryStartTick", 0);
    writeStaticField("nextRecoveryTick", 0);

    invokeUpdateMovementThrottle(false);

    assertEquals(1, readStaticField("currentMovementReduction"));
  }

  @Test
  void movementRecoveryPausesAtNormalLoad() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("currentLoadLevel", ServerLoadLevel.NORMAL);
    writeStaticField("currentMovementReduction", 2);
    writeStaticField("recoveryStartTick", 0);
    writeStaticField("nextRecoveryTick", 0);

    invokeUpdateMovementThrottle(false);

    assertEquals(2, readStaticField("currentMovementReduction"));
  }

  @Test
  void loadBasedBaselineDropsByOneStepAtHighLoad() {
    assertEquals(7, SimulationDistanceManager.resolveNextLoadBaselineDistance(
      ServerLoadLevel.HIGH, 8));
    assertEquals(6, SimulationDistanceManager.resolveNextLoadBaselineDistance(
      ServerLoadLevel.VERY_HIGH, 7));
  }
}
