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

package de.markusbordihn.adaptiveperformancetweaks.feature.gamerules;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.UUID;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.level.GameRules;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockMakers;

class GameRuleManagerTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

  private static void writeStaticField(String fieldName, Object value) throws Exception {
    Field field = GameRuleManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static Object readStaticField(String fieldName) throws Exception {
    Field field = GameRuleManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    return field.get(null);
  }

  private static void writeServerManagerField(String fieldName, Object value) throws Exception {
    Field field = ServerManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static int invokeIntMethod(String methodName) throws Exception {
    Method method = GameRuleManager.class.getDeclaredMethod(methodName);
    method.setAccessible(true);
    return (int) method.invoke(null);
  }

  private static void invokePlayerWarmup(String triggerSource) throws Exception {
    Method method = GameRuleManager.class.getDeclaredMethod("applyPlayerWarmup", String.class);
    method.setAccessible(true);
    method.invoke(null, triggerSource);
  }

  @BeforeEach
  void resetState() throws Exception {
    writeServerManagerField("minecraftServer", null);
    PlayerPositionManager.reset();
    FeatureToggle.GAMERULES.setEnabled(true);

    GameRulesConfig.minOptimizationLoadLevel = ServerLoadLevel.VERY_LOW;
    GameRulesConfig.randomTickSpeedEnabled = true;
    GameRulesConfig.loginWarmupEnabled = true;
    GameRulesConfig.movementWarmupEnabled = true;
    GameRulesConfig.randomTickSpeed = 3;

    writeStaticField("gameRules", null);
    writeStaticField("configuredRandomTickSpeedMax", GameRulesConfig.randomTickSpeed);
    writeStaticField("configuredMaxEntityCramming", GameRulesConfig.maxEntityCramming);
    writeStaticField("currentLoadLevel", ServerLoadLevel.NORMAL);
    writeStaticField("lastUpdateTime", System.currentTimeMillis());
    writeStaticField("lastRandomTickRecoveryTime", System.currentTimeMillis());
    writeStaticField("randomTickWarmupUntilTime", 0L);
    writeStaticField("randomTickPlayerActivityRecoveryPending", false);
  }

  @Test
  void configuredRandomTickSpeedMaxRespectsStartupServerLimit() throws Exception {
    int previousConfig = GameRulesConfig.randomTickSpeed;
    try {
      GameRulesConfig.randomTickSpeed = 6;
      writeStaticField("configuredRandomTickSpeedMax", 8);
      assertEquals(6, invokeIntMethod("getConfiguredRandomTickSpeedMax"));

      writeStaticField("configuredRandomTickSpeedMax", 4);
      assertEquals(4, invokeIntMethod("getConfiguredRandomTickSpeedMax"));
    } finally {
      GameRulesConfig.randomTickSpeed = previousConfig;
    }
  }

  @Test
  void handleFeatureDisabledWithoutServerClearsWarmupState() throws Exception {
    writeStaticField("gameRules", mock(GameRules.class));
    writeStaticField("randomTickWarmupUntilTime", System.currentTimeMillis() + 5_000L);
    writeStaticField("randomTickPlayerActivityRecoveryPending", true);

    GameRuleManager.handleFeatureDisabled();

    assertNull(readStaticField("gameRules"));
    assertEquals(0L, readStaticField("randomTickWarmupUntilTime"));
    assertEquals(false, readStaticField("randomTickPlayerActivityRecoveryPending"));
  }

  @Test
  void handleServerStartingCapturesServerDefaultsEvenWhenFeatureIsDisabled() throws Exception {
    boolean previousState = FeatureToggle.GAMERULES.isEnabled();
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = mock(GameRules.class);
    when(server.getGameRules()).thenReturn(rules);
    when(rules.getInt(GameRules.RULE_RANDOMTICKING)).thenReturn(5);
    when(rules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING)).thenReturn(19);

    try {
      FeatureToggle.GAMERULES.setEnabled(false);
      GameRuleManager.handleServerStarting(server);

      assertEquals(rules, readStaticField("gameRules"));
      assertEquals(5, readStaticField("configuredRandomTickSpeedMax"));
      assertEquals(19, readStaticField("configuredMaxEntityCramming"));
    } finally {
      FeatureToggle.GAMERULES.setEnabled(previousState);
    }
  }

  @Test
  void handleServerStartingCapturesDoFireTickDefault() throws Exception {
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules();
    rules.getRule(GameRules.RULE_DOFIRETICK).set(false, null);
    when(server.getGameRules()).thenReturn(rules);

    GameRuleManager.handleServerStarting(server);

    assertEquals(false, readStaticField("configuredDoFireTick"));
  }

  @Test
  void highLoadDisablesAndNormalLoadRestoresDoFireTick() {
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules();
    when(server.getGameRules()).thenReturn(rules);

    try {
      writeServerManagerField("minecraftServer", server);
      GameRuleManager.handleServerStarting(server);
      assertTrue(rules.getBoolean(GameRules.RULE_DOFIRETICK));

      GameRuleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.HIGH, ServerLoadLevel.NORMAL, 75.0, 50.0));
      assertFalse(rules.getBoolean(GameRules.RULE_DOFIRETICK));

      writeStaticField("lastUpdateTime", 0L);
      GameRuleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.NORMAL, ServerLoadLevel.HIGH, 50.0, 75.0));
      assertTrue(rules.getBoolean(GameRules.RULE_DOFIRETICK));
    } catch (Exception exception) {
      throw new AssertionError(exception);
    } finally {
      try {
        writeServerManagerField("minecraftServer", null);
      } catch (Exception ignored) {
      }
    }
  }

  @Test
  void loginWarmupIgnoresMinOptimizationLoadLevel() throws Exception {
    boolean previousState = FeatureToggle.GAMERULES.isEnabled();
    ServerLoadLevel previousMinOptimizationLoadLevel = GameRulesConfig.minOptimizationLoadLevel;
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules();
    when(server.getGameRules()).thenReturn(rules);

    try {
      writeServerManagerField("minecraftServer", server);
      FeatureToggle.GAMERULES.setEnabled(true);
      GameRulesConfig.minOptimizationLoadLevel = ServerLoadLevel.VERY_HIGH;
      GameRuleManager.handleServerStarting(server);
      writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);

      invokePlayerWarmup("login");

      assertTrue((Long) readStaticField("randomTickWarmupUntilTime") > System.currentTimeMillis());
      assertEquals(1, rules.getInt(GameRules.RULE_RANDOMTICKING));
      assertEquals(true, readStaticField("randomTickPlayerActivityRecoveryPending"));
    } finally {
      FeatureToggle.GAMERULES.setEnabled(previousState);
      GameRulesConfig.minOptimizationLoadLevel = previousMinOptimizationLoadLevel;
      writeServerManagerField("minecraftServer", null);
    }
  }

  @Test
  void movementWarmupTriggersFromTrackedMovement() throws Exception {
    boolean previousState = FeatureToggle.GAMERULES.isEnabled();
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules();
    when(server.getGameRules()).thenReturn(rules);
    PlayerPosition playerPosition =
      new PlayerPosition("tester", UUID.randomUUID(), "minecraft:overworld", 0, 64, 0, 128);
    Field movementWindowField = PlayerPosition.class.getDeclaredField("movementWindow");
    Field movementWindowCountField = PlayerPosition.class.getDeclaredField("movementWindowCount");
    Field movementWindowDistanceField =
      PlayerPosition.class.getDeclaredField("movementWindowDistance");
    movementWindowField.setAccessible(true);
    movementWindowCountField.setAccessible(true);
    movementWindowDistanceField.setAccessible(true);
    movementWindowField.set(playerPosition, new double[]{32.0D, 0.0D, 0.0D});
    movementWindowCountField.set(playerPosition, 3);
    movementWindowDistanceField.set(playerPosition, 32.0D);

    try {
      writeServerManagerField("minecraftServer", server);
      FeatureToggle.GAMERULES.setEnabled(true);
      PlayerPositionManager.reset();
      PlayerPositionManager.getPlayerPositionMap().put("tester", playerPosition);
      GameRuleManager.handleServerStarting(server);
      writeStaticField("currentLoadLevel", ServerLoadLevel.NORMAL);

      GameRuleManager.handleServerTick();

      assertTrue((Long) readStaticField("randomTickWarmupUntilTime") > System.currentTimeMillis());
      assertEquals(1, rules.getInt(GameRules.RULE_RANDOMTICKING));
      assertEquals(true, readStaticField("randomTickPlayerActivityRecoveryPending"));
    } finally {
      FeatureToggle.GAMERULES.setEnabled(previousState);
      PlayerPositionManager.reset();
      writeServerManagerField("minecraftServer", null);
    }
  }

  @Test
  void playerWarmupRecoveryRunsBelowNormalLoad() throws Exception {
    boolean previousState = FeatureToggle.GAMERULES.isEnabled();
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules();
    rules.getRule(GameRules.RULE_RANDOMTICKING).set(3, null);
    when(server.getGameRules()).thenReturn(rules);

    try {
      writeServerManagerField("minecraftServer", server);
      FeatureToggle.GAMERULES.setEnabled(true);
      GameRuleManager.handleServerStarting(server);
      rules.getRule(GameRules.RULE_RANDOMTICKING).set(1, null);
      writeStaticField("currentLoadLevel", ServerLoadLevel.LOW);
      writeStaticField("randomTickWarmupUntilTime", 0L);
      writeStaticField("randomTickPlayerActivityRecoveryPending", true);
      writeStaticField("lastRandomTickRecoveryTime", 0L);

      GameRuleManager.handleServerTick();

      assertEquals(2, rules.getInt(GameRules.RULE_RANDOMTICKING));
      assertEquals(true, readStaticField("randomTickPlayerActivityRecoveryPending"));
    } finally {
      FeatureToggle.GAMERULES.setEnabled(previousState);
      writeServerManagerField("minecraftServer", null);
    }
  }

  @Test
  void playerWarmupRecoveryPausesAtNormalLoad() throws Exception {
    boolean previousState = FeatureToggle.GAMERULES.isEnabled();
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules();
    rules.getRule(GameRules.RULE_RANDOMTICKING).set(3, null);
    when(server.getGameRules()).thenReturn(rules);

    try {
      writeServerManagerField("minecraftServer", server);
      FeatureToggle.GAMERULES.setEnabled(true);
      GameRuleManager.handleServerStarting(server);
      rules.getRule(GameRules.RULE_RANDOMTICKING).set(1, null);
      writeStaticField("currentLoadLevel", ServerLoadLevel.NORMAL);
      writeStaticField("randomTickWarmupUntilTime", 0L);
      writeStaticField("randomTickPlayerActivityRecoveryPending", true);
      writeStaticField("lastRandomTickRecoveryTime", 0L);

      GameRuleManager.handleServerTick();

      assertEquals(1, rules.getInt(GameRules.RULE_RANDOMTICKING));
      assertEquals(true, readStaticField("randomTickPlayerActivityRecoveryPending"));
    } finally {
      FeatureToggle.GAMERULES.setEnabled(previousState);
      writeServerManagerField("minecraftServer", null);
    }
  }

  @Test
  void highLoadRandomTickSpeedDropsByOneStep() {
    boolean previousState = FeatureToggle.GAMERULES.isEnabled();
    int previousRandomTickSpeed = GameRulesConfig.randomTickSpeed;
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules();
    rules.getRule(GameRules.RULE_RANDOMTICKING).set(5, null);
    when(server.getGameRules()).thenReturn(rules);

    try {
      writeServerManagerField("minecraftServer", server);
      FeatureToggle.GAMERULES.setEnabled(true);
      GameRulesConfig.randomTickSpeed = 5;
      GameRuleManager.handleServerStarting(server);

      GameRuleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.HIGH, ServerLoadLevel.NORMAL, 75.0, 50.0));

      assertEquals(4, rules.getInt(GameRules.RULE_RANDOMTICKING));
    } catch (Exception exception) {
      throw new AssertionError(exception);
    } finally {
      FeatureToggle.GAMERULES.setEnabled(previousState);
      GameRulesConfig.randomTickSpeed = previousRandomTickSpeed;
      try {
        writeServerManagerField("minecraftServer", null);
      } catch (Exception ignored) {
      }
    }
  }
}
