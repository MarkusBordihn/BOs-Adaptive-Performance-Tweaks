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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.UUID;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.players.PlayerList;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockMakers;

class ViewDistanceManagerTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

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

  private static void invokeDetectExternalDistanceChange(MinecraftServer server) throws Exception {
    Method method = ViewDistanceManager.class.getDeclaredMethod("detectExternalDistanceChange",
      MinecraftServer.class);
    method.setAccessible(true);
    method.invoke(null, server);
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

  private static int movementReduction(int trackedPlayers, int activeExplorers, int fastExplorers)
    throws Exception {
    Method method = ViewDistanceManager.class.getDeclaredMethod(
      "calculateMovementReduction", int.class, int.class, int.class);
    method.setAccessible(true);
    return (int) method.invoke(null, trackedPlayers, activeExplorers, fastExplorers);
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
  @DisplayName("Issue #92: a warmup removes at most loginWarmupReductionMax chunks")
  void warmupReductionStaysWithinConfiguredReductionMaximum() throws Exception {
    int originalReductionMax = ViewDistanceConfig.loginWarmupReductionMax;
    try {
      ViewDistanceConfig.loginWarmupReductionMax = 3;
      writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
      assertEquals(3, invokeIntMethod("getWarmupReduction"));

      writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMin);
      assertEquals(0, invokeIntMethod("getWarmupReduction"));
    } finally {
      ViewDistanceConfig.loginWarmupReductionMax = originalReductionMax;
    }
  }

  @Test
  void largeReductionMaximumKeepsReducingBaselineDownToMinimum() throws Exception {
    int originalReductionMax = ViewDistanceConfig.loginWarmupReductionMax;
    try {
      ViewDistanceConfig.loginWarmupReductionMax = 64;
      writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
      int reduction = invokeIntMethod("getWarmupReduction");
      int target = Math.max(ViewDistanceConfig.viewDistanceMin,
        ViewDistanceConfig.viewDistanceMax - reduction);
      assertEquals(ViewDistanceConfig.viewDistanceMin, target);
    } finally {
      ViewDistanceConfig.loginWarmupReductionMax = originalReductionMax;
    }
  }

  @Test
  void effectiveViewDistanceFallsBackWhileNoDistanceIsApplied() throws Exception {
    writeStaticField("currentDistance", -1);
    assertEquals(16, ViewDistanceManager.getEffectiveViewDistance(16));

    writeStaticField("currentDistance", 6);
    assertEquals(6, ViewDistanceManager.getEffectiveViewDistance(16));
  }

  @Test
  @DisplayName("A view distance changed by another mod is adopted as the new maximum")
  void externalDistanceChangeIsAdoptedAsConfiguredMaximum() throws Exception {
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    PlayerList playerList = mock(PlayerList.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    when(server.getPlayerList()).thenReturn(playerList);
    when(playerList.getViewDistance()).thenReturn(12);
    writeStaticField("configuredDistanceMax", 32);
    writeStaticField("lastPlayerListDistance", -1);

    invokeDetectExternalDistanceChange(server);

    assertEquals(12, readStaticField("configuredDistanceMax"));
  }

  @Test
  void unchangedPlayerListDistanceKeepsConfiguredMaximum() throws Exception {
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    PlayerList playerList = mock(PlayerList.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    when(server.getPlayerList()).thenReturn(playerList);
    when(playerList.getViewDistance()).thenReturn(32);
    writeStaticField("configuredDistanceMax", 32);
    writeStaticField("lastPlayerListDistance", -1);

    invokeDetectExternalDistanceChange(server);

    assertEquals(32, readStaticField("configuredDistanceMax"));
  }

  @Test
  void movementReductionUsesMaxAtVeryHighLoad() throws Exception {
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_HIGH);
    assertEquals(ViewDistanceConfig.movementReductionMax, movementReduction(4, 1, 0));
  }

  @Test
  void movementReductionUsesMinForLowExplorerRatio() throws Exception {
    writeStaticField("currentLoadLevel", ServerLoadLevel.MEDIUM);
    assertEquals(1, movementReduction(4, 1, 0));
  }

  @Test
  void movementReductionStaysGentleAtLowLoad() throws Exception {
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    assertEquals(1, movementReduction(4, 4, 0));
  }

  @Test
  @DisplayName("A single fast traveller reaches the max reduction even at the lowest load")
  void movementReductionUsesMaxForFastExplorer() throws Exception {
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    assertEquals(ViewDistanceConfig.movementReductionMax, movementReduction(4, 1, 1));
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
  @DisplayName("A walking player below the explorer threshold does not block recovery")
  void warmupRecoveryProceedsWhilePlayerStaysBelowExplorerThreshold() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    writeStaticField("currentWarmupReduction", 2);
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

    invokeUpdateWarmupReduction();

    assertEquals(0, readStaticField("activeExplorerCount"));
    assertEquals(1, readStaticField("currentWarmupReduction"));
  }

  @Test
  @DisplayName("Teleport warmup is held until the recovery delay has elapsed")
  void movementWarmupHoldsTeleportWarmupUntilRecoveryDelayElapsed() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    writeStaticField("currentWarmupReduction", invokeIntMethod("getWarmupReduction"));
    writeStaticField("recoveryStartTick", -1);
    writeStaticField("nextRecoveryTick", -1);

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

  @Test
  @DisplayName("Issue #91: warmup reduction decays toward the movement target while exploring")
  void warmupReductionDecaysTowardMovementTargetWhileExploring() throws Exception {
    PlayerPositionManager.reset();
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    writeStaticField("currentWarmupReduction", invokeIntMethod("getWarmupReduction"));
    writeStaticField("recoveryStartTick", -1);
    writeStaticField("nextRecoveryTick", -1);

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

    writePlayerPositionManagerField("ticks", readStaticField("recoveryStartTick"));
    invokeUpdateWarmupReduction();
    int currentReduction = (int) readStaticField("currentWarmupReduction");
    assertEquals(invokeIntMethod("getWarmupReduction") - 1, currentReduction);

    while (currentReduction > 1) {
      writePlayerPositionManagerField("ticks", readStaticField("nextRecoveryTick"));
      invokeUpdateWarmupReduction();
      currentReduction = (int) readStaticField("currentWarmupReduction");
      assertTrue(currentReduction >= 1);
    }
    assertEquals(1, currentReduction);
    assertEquals(-1, readStaticField("recoveryStartTick"));
  }

  @Test
  @DisplayName("Login warmup recovers while exploring when the movement warmup is disabled")
  void warmupRecoversWithDisabledMovementWarmupWhilePlayerExplores() throws Exception {
    boolean originalMovementWarmupEnabled = ViewDistanceConfig.movementWarmupEnabled;
    try {
      ViewDistanceConfig.movementWarmupEnabled = false;
      PlayerPositionManager.reset();
      writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
      writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
      writeStaticField("currentWarmupReduction", 2);
      writeStaticField("recoveryStartTick", -1);
      writeStaticField("nextRecoveryTick", -1);

      PlayerPosition playerPosition = new PlayerPosition("Explorer", UUID.randomUUID(),
        "minecraft:overworld", 0, 64, 0, 128);
      playerPosition.updateMovement(0.0d, 64.0d, 0.0d, "minecraft:overworld", 0, 3, 20);
      playerPosition.updateMovement(16.0d, 64.0d, 0.0d, "minecraft:overworld", 20, 3, 20);
      playerPosition.updateMovement(32.0d, 64.0d, 0.0d, "minecraft:overworld", 40, 3, 20);
      playerPosition.updateMovement(48.0d, 64.0d, 0.0d, "minecraft:overworld", 60, 3, 20);
      PlayerPositionManager.getPlayerPositionMap().put("explorer", playerPosition);
      writePlayerPositionManagerField("ticks", 61);

      invokeUpdateWarmupReduction();
      assertEquals(1, readStaticField("activeExplorerCount"));
      assertTrue((int) readStaticField("recoveryStartTick") > 0);

      writePlayerPositionManagerField("ticks", readStaticField("recoveryStartTick"));
      invokeUpdateWarmupReduction();

      assertEquals(1, readStaticField("currentWarmupReduction"));
    } finally {
      ViewDistanceConfig.movementWarmupEnabled = originalMovementWarmupEnabled;
    }
  }
}
