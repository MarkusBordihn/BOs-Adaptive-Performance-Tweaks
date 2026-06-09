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

import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.UUID;
import org.junit.jupiter.api.Test;

class ViewDistanceManagerTest {

  private static void writeStaticField(String fieldName, Object value) throws Exception {
    Field field = ViewDistanceManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static int invokeIntMethod(String methodName) throws Exception {
    Method method = ViewDistanceManager.class.getDeclaredMethod(methodName);
    method.setAccessible(true);
    return (int) method.invoke(null);
  }

  private static Object readStaticField(String fieldName) throws Exception {
    Field field = ViewDistanceManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    return field.get(null);
  }

  private static void invokeUpdateWarmupReduction() throws Exception {
    Method method = ViewDistanceManager.class.getDeclaredMethod("updateWarmupReduction");
    method.setAccessible(true);
    method.invoke(null);
  }

  private static void writePlayerPositionManagerField(String fieldName, Object value)
    throws Exception {
    Field field = PlayerPositionManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static int resolveBaseline(ServerLoadLevel level, int currentBaseline) throws Exception {
    Method method = ViewDistanceManager.class.getDeclaredMethod(
      "resolveNextLoadBaselineDistance", ServerLoadLevel.class, int.class);
    method.setAccessible(true);
    return (int) method.invoke(null, level, currentBaseline);
  }

  private static int movementReduction(int trackedPlayers, int activeExplorers) throws Exception {
    Method method = ViewDistanceManager.class.getDeclaredMethod(
      "calculateMovementReduction", int.class, int.class);
    method.setAccessible(true);
    return (int) method.invoke(null, trackedPlayers, activeExplorers);
  }

  @Test
  void configuredDistanceMaxRespectsServerStartupLimit() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax + 4);
    assertEquals(ViewDistanceConfig.viewDistanceMax, invokeIntMethod("getConfiguredDistanceMax"));

    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMin + 2);
    assertEquals(ViewDistanceConfig.viewDistanceMin + 2,
      invokeIntMethod("getConfiguredDistanceMax"));
  }

  @Test
  void loadBaselineKeepsServerMaxBelowOptimizationThreshold() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    assertEquals(ViewDistanceConfig.viewDistanceMax, resolveBaseline(ServerLoadLevel.NORMAL, -1));
  }

  @Test
  void loadBaselineInitializesToLevelTargetAtThreshold() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    assertEquals(ViewDistanceConfig.viewDistanceMedium,
      resolveBaseline(ServerLoadLevel.MEDIUM, -1));
  }

  @Test
  void loadBaselineStepsDownByOneOnHigherLoad() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    int start = ViewDistanceConfig.viewDistanceMedium;
    assertEquals(start - 1, resolveBaseline(ServerLoadLevel.VERY_HIGH, start));
  }

  @Test
  void loadBaselineStepsUpTowardServerMaxBelowThreshold() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    int start = ViewDistanceConfig.viewDistanceMedium;
    assertEquals(start + 1, resolveBaseline(ServerLoadLevel.NORMAL, start));
  }

  @Test
  void loadBaselineStepsUpTowardLevelTargetAtThreshold() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    int start = ViewDistanceConfig.viewDistanceVeryHigh;
    assertEquals(start + 1, resolveBaseline(ServerLoadLevel.HIGH, start));
  }

  @Test
  void warmupReducesBaselineDownToMinimum() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    int reduction = invokeIntMethod("getWarmupReduction");
    int baseline = ViewDistanceConfig.viewDistanceMax;
    int target = Math.max(ViewDistanceConfig.viewDistanceMin,
      Math.min(ViewDistanceConfig.viewDistanceMax, baseline) - reduction);
    assertEquals(ViewDistanceConfig.viewDistanceMin, target);
  }

  @Test
  void movementReductionUsesMaxAtVeryHighLoad() throws Exception {
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_HIGH);
    assertEquals(ViewDistanceConfig.movementReductionMax, movementReduction(4, 1));
  }

  @Test
  void movementReductionUsesMinForLowExplorerRatio() throws Exception {
    writeStaticField("currentLoadLevel", ServerLoadLevel.MEDIUM);
    assertEquals(1, movementReduction(4, 1));
  }

  @Test
  void movementReductionStaysGentleAtLowLoad() throws Exception {
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    assertEquals(1, movementReduction(4, 4));
  }

  @Test
  void lowLoadRecoveryDelayRespectsConfiguredMinimumHold() throws Exception {
    int originalMinDelayTicks = ViewDistanceConfig.recoveryMinDelayTicks;
    int originalFastDelayTicks = ViewDistanceConfig.recoveryFastDelayTicks;
    try {
      ViewDistanceConfig.recoveryMinDelayTicks = 200;
      ViewDistanceConfig.recoveryFastDelayTicks = 40;
      writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);

      assertEquals(200, invokeIntMethod("currentRecoveryDelayTicks"));
    } finally {
      ViewDistanceConfig.recoveryMinDelayTicks = originalMinDelayTicks;
      ViewDistanceConfig.recoveryFastDelayTicks = originalFastDelayTicks;
    }
  }

  @Test
  void higherRecoveryDelayWinsWhenAboveConfiguredMinimumHold() throws Exception {
    int originalMinDelayTicks = ViewDistanceConfig.recoveryMinDelayTicks;
    int originalRecoveryDelayTicks = ViewDistanceConfig.recoveryDelayTicks;
    try {
      ViewDistanceConfig.recoveryMinDelayTicks = 200;
      ViewDistanceConfig.recoveryDelayTicks = 260;
      writeStaticField("currentLoadLevel", ServerLoadLevel.MEDIUM);

      assertEquals(260, invokeIntMethod("currentRecoveryDelayTicks"));
    } finally {
      ViewDistanceConfig.recoveryMinDelayTicks = originalMinDelayTicks;
      ViewDistanceConfig.recoveryDelayTicks = originalRecoveryDelayTicks;
    }
  }

  @Test
  void movementWarmupDoesNotRelaxTeleportWarmupBeforeStability() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    writeStaticField("currentWarmupReduction", invokeIntMethod("getWarmupReduction"));

    PlayerPosition playerPosition = new PlayerPosition("Benchmark", UUID.randomUUID(),
      "minecraft:overworld", 0, 64, 0, 128);
    playerPosition.setLoginWarmup(0, ViewDistanceConfig.loginWarmupTicks);
    playerPosition.updateMovement(0.0d, 64.0d, 0.0d, "minecraft:overworld", 0, 3, 20);
    playerPosition.updateMovement(16.0d, 64.0d, 0.0d, "minecraft:overworld", 20, 3, 20);
    playerPosition.updateMovement(32.0d, 64.0d, 0.0d, "minecraft:overworld", 40, 3, 20);
    playerPosition.updateMovement(48.0d, 64.0d, 0.0d, "minecraft:overworld", 60, 3, 20);
    PlayerPositionManager.getPlayerPositionMap().put("benchmark", playerPosition);
    writePlayerPositionManagerField("ticks", 61);

    invokeUpdateWarmupReduction();

    assertEquals(invokeIntMethod("getWarmupReduction"), readStaticField("currentWarmupReduction"));
  }
}
