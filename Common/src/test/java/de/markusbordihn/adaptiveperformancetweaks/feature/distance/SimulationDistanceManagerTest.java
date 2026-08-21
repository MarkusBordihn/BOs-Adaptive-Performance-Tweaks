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
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.UUID;
import org.junit.jupiter.api.DisplayName;
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

  private static void writePlayerPositionManagerField(String fieldName, Object value)
    throws Exception {
    Field field = PlayerPositionManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static void invokeUpdateMovementThrottle(boolean recordMovementSample) throws Exception {
    Method method = SimulationDistanceManager.class.getDeclaredMethod("updateMovementThrottle",
      boolean.class);
    method.setAccessible(true);
    method.invoke(null, recordMovementSample);
  }

  @Test
  void mediumLoadNeedsHighExplorerShareForMaxReduction() {
    assertEquals(1, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.MEDIUM, 4, 1, 0));
    assertEquals(2, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.MEDIUM, 4, 3, 0));
  }

  @Test
  void highAndVeryHighLoadScaleMoreAggressively() {
    assertEquals(2, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.HIGH, 4, 2, 0));
    assertEquals(2, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.VERY_HIGH, 4, 1, 0));
  }

  @Test
  @DisplayName("A single fast traveller reaches the max reduction even at the lowest load")
  void fastExplorerReachesMaxReductionRegardlessOfLoad() {
    assertEquals(1, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.VERY_LOW, 4, 1, 0));
    assertEquals(2, SimulationDistanceManager.calculateMovementReduction(
      ServerLoadLevel.VERY_LOW, 4, 1, 1));
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
  @DisplayName("Issue #92: a warmup removes at most loginWarmupReductionMax chunks")
  void warmupReductionStaysWithinConfiguredReductionMaximum() throws Exception {
    int originalReductionMax = SimulationDistanceConfig.loginWarmupReductionMax;
    try {
      SimulationDistanceConfig.loginWarmupReductionMax = 2;
      writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMin + 4);
      assertEquals(2, invokeIntMethod("getWarmupReduction"));

      writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMin);
      assertEquals(0, invokeIntMethod("getWarmupReduction"));
    } finally {
      SimulationDistanceConfig.loginWarmupReductionMax = originalReductionMax;
    }
  }

  @Test
  void largeReductionMaximumKeepsDroppingToConfiguredMinimumDistance() throws Exception {
    int originalReductionMax = SimulationDistanceConfig.loginWarmupReductionMax;
    try {
      SimulationDistanceConfig.loginWarmupReductionMax = 64;
      writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMin + 4);
      assertEquals(4, invokeIntMethod("getWarmupReduction"));
    } finally {
      SimulationDistanceConfig.loginWarmupReductionMax = originalReductionMax;
    }
  }

  @Test
  @DisplayName("loginWarmupEnabled=false suppresses the warmup reduction in sim_distance.cfg")
  void disabledLoginWarmupSuppressesWarmupReduction() throws Exception {
    boolean originalLoginWarmupEnabled = SimulationDistanceConfig.loginWarmupEnabled;
    boolean originalMovementThrottleEnabled = SimulationDistanceConfig.movementThrottleEnabled;
    try {
      SimulationDistanceConfig.loginWarmupEnabled = false;
      SimulationDistanceConfig.movementThrottleEnabled = false;
      PlayerPositionManager.reset();
      writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMax);
      writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
      writeStaticField("currentMovementReduction", 0);
      writeStaticField("recoveryStartTick", -1);
      writeStaticField("nextRecoveryTick", -1);

      PlayerPosition playerPosition = new PlayerPosition("Newcomer", UUID.randomUUID(),
        "minecraft:overworld", 0, 64, 0, 128);
      playerPosition.setLoginWarmup(0, SimulationDistanceConfig.movementThrottleLoginTicks);
      PlayerPositionManager.getPlayerPositionMap().put("newcomer", playerPosition);
      writePlayerPositionManagerField("ticks", 1);

      invokeUpdateMovementThrottle(false);

      assertEquals(0, readStaticField("currentMovementReduction"));
    } finally {
      SimulationDistanceConfig.loginWarmupEnabled = originalLoginWarmupEnabled;
      SimulationDistanceConfig.movementThrottleEnabled = originalMovementThrottleEnabled;
    }
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
  void movementRecoverySchedulesConfiguredMinimumHoldBeforeFirstIncrease() throws Exception {
    int originalMinDelayTicks = SimulationDistanceConfig.movementThrottleRecoveryMinDelayTicks;
    int originalDelayTicks = SimulationDistanceConfig.movementThrottleRecoveryDelayTicks;
    try {
      PlayerPositionManager.reset();
      SimulationDistanceConfig.movementThrottleRecoveryMinDelayTicks = 200;
      SimulationDistanceConfig.movementThrottleRecoveryDelayTicks = 140;
      writeStaticField("currentLoadLevel", ServerLoadLevel.LOW);
      writeStaticField("currentMovementReduction", 2);
      writeStaticField("recoveryStartTick", -1);
      writeStaticField("nextRecoveryTick", -1);

      invokeUpdateMovementThrottle(false);

      assertEquals(2, readStaticField("currentMovementReduction"));
      assertEquals(200, readStaticField("recoveryStartTick"));
      assertEquals(200, readStaticField("nextRecoveryTick"));
    } finally {
      SimulationDistanceConfig.movementThrottleRecoveryMinDelayTicks = originalMinDelayTicks;
      SimulationDistanceConfig.movementThrottleRecoveryDelayTicks = originalDelayTicks;
    }
  }

  @Test
  @DisplayName("Warmup reduction also recovers at NORMAL load, the baseline handles the load")
  void movementRecoveryContinuesAtNormalLoad() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("currentLoadLevel", ServerLoadLevel.NORMAL);
    writeStaticField("currentMovementReduction", 2);
    writeStaticField("recoveryStartTick", 0);
    writeStaticField("nextRecoveryTick", 0);

    invokeUpdateMovementThrottle(false);

    assertEquals(1, readStaticField("currentMovementReduction"));
  }

  @Test
  @DisplayName("A walking player below the explorer threshold does not block recovery")
  void movementRecoveryProceedsWhilePlayerStaysBelowExplorerThreshold() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMax);
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    writeStaticField("currentMovementReduction", 2);
    writeStaticField("recoveryStartTick", 0);
    writeStaticField("nextRecoveryTick", 0);

    PlayerPosition playerPosition = new PlayerPosition("Walker", UUID.randomUUID(),
      "minecraft:overworld", 0, 64, 0, 128);
    playerPosition.updateMovement(0.0d, 64.0d, 0.0d, "minecraft:overworld", 0, 3, 20);
    playerPosition.updateMovement(4.0d, 64.0d, 0.0d, "minecraft:overworld", 20, 3, 20);
    playerPosition.updateMovement(8.0d, 64.0d, 0.0d, "minecraft:overworld", 40, 3, 20);
    playerPosition.updateMovement(12.0d, 64.0d, 0.0d, "minecraft:overworld", 60, 3, 20);
    PlayerPositionManager.getPlayerPositionMap().put("walker", playerPosition);
    writePlayerPositionManagerField("ticks", 61);

    invokeUpdateMovementThrottle(false);

    assertEquals(0, readStaticField("activeExplorerCount"));
    assertEquals(1, readStaticField("currentMovementReduction"));
  }

  @Test
  void loadBasedBaselineDropsByOneStepAtHighLoad() {
    assertEquals(7, SimulationDistanceManager.resolveNextLoadBaselineDistance(
      ServerLoadLevel.HIGH, 8));
    assertEquals(6, SimulationDistanceManager.resolveNextLoadBaselineDistance(
      ServerLoadLevel.VERY_HIGH, 7));
  }

  @Test
  @DisplayName("Teleport warmup is held until the recovery delay has elapsed")
  void movementWarmupHoldsTeleportWarmupUntilRecoveryDelayElapsed() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMax);
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    writeStaticField("currentMovementReduction", invokeIntMethod("getWarmupReduction"));
    writeStaticField("recoveryStartTick", -1);
    writeStaticField("nextRecoveryTick", -1);

    PlayerPosition playerPosition = new PlayerPosition("Benchmark", UUID.randomUUID(),
      "minecraft:overworld", 0, 64, 0, 128);
    playerPosition.setLoginWarmup(0, SimulationDistanceConfig.movementThrottleLoginTicks);
    playerPosition.updateMovement(0.0d, 64.0d, 0.0d, "minecraft:overworld", 0, 3, 20);
    playerPosition.updateMovement(16.0d, 64.0d, 0.0d, "minecraft:overworld", 20, 3, 20);
    playerPosition.updateMovement(32.0d, 64.0d, 0.0d, "minecraft:overworld", 40, 3, 20);
    playerPosition.updateMovement(48.0d, 64.0d, 0.0d, "minecraft:overworld", 60, 3, 20);
    PlayerPositionManager.getPlayerPositionMap().put("benchmark", playerPosition);
    writePlayerPositionManagerField("ticks", 61);

    invokeUpdateMovementThrottle(true);

    assertEquals(invokeIntMethod("getWarmupReduction"),
      readStaticField("currentMovementReduction"));
  }

  @Test
  @DisplayName("Issue #91: warmup reduction decays toward the movement target while exploring")
  void warmupReductionDecaysTowardMovementTargetWhileExploring() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("configuredDistanceMax", SimulationDistanceConfig.simDistanceMax);
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    writeStaticField("currentMovementReduction", invokeIntMethod("getWarmupReduction"));
    writeStaticField("recoveryStartTick", -1);
    writeStaticField("nextRecoveryTick", -1);

    PlayerPosition playerPosition = new PlayerPosition("Benchmark", UUID.randomUUID(),
      "minecraft:overworld", 0, 64, 0, 128);
    playerPosition.setLoginWarmup(0, SimulationDistanceConfig.movementThrottleLoginTicks);
    playerPosition.updateMovement(0.0d, 64.0d, 0.0d, "minecraft:overworld", 0, 3, 20);
    playerPosition.updateMovement(16.0d, 64.0d, 0.0d, "minecraft:overworld", 20, 3, 20);
    playerPosition.updateMovement(32.0d, 64.0d, 0.0d, "minecraft:overworld", 40, 3, 20);
    playerPosition.updateMovement(48.0d, 64.0d, 0.0d, "minecraft:overworld", 60, 3, 20);
    PlayerPositionManager.getPlayerPositionMap().put("benchmark", playerPosition);

    writePlayerPositionManagerField("ticks", 61);
    invokeUpdateMovementThrottle(true);
    assertEquals(invokeIntMethod("getWarmupReduction"),
      readStaticField("currentMovementReduction"));

    writePlayerPositionManagerField("ticks", readStaticField("recoveryStartTick"));
    invokeUpdateMovementThrottle(true);
    int currentReduction = (int) readStaticField("currentMovementReduction");
    assertEquals(invokeIntMethod("getWarmupReduction") - 1, currentReduction);

    while (currentReduction > 1) {
      writePlayerPositionManagerField("ticks", readStaticField("nextRecoveryTick"));
      invokeUpdateMovementThrottle(true);
      currentReduction = (int) readStaticField("currentMovementReduction");
      assertTrue(currentReduction >= 1);
    }
    assertEquals(1, currentReduction);
    assertEquals(-1, readStaticField("recoveryStartTick"));
  }
}
