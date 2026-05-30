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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.flag.FeatureFlags;
import net.minecraft.world.level.gamerules.GameRule;
import net.minecraft.world.level.gamerules.GameRules;
import org.junit.jupiter.api.BeforeAll;
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

    GameRuleManager.handleFeatureDisabled();

    assertNull(readStaticField("gameRules"));
    assertEquals(0L, readStaticField("randomTickWarmupUntilTime"));
  }

  @Test
  void handleServerStartingCapturesServerDefaultsEvenWhenFeatureIsDisabled() throws Exception {
    boolean previousState = FeatureToggle.GAMERULES.isEnabled();
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = mock(GameRules.class);
    doAnswer(inv -> {
      Object key = inv.getArgument(0);
      if (key == GameRules.RANDOM_TICK_SPEED) {
        return 5;
      }
      if (key == GameRules.MAX_ENTITY_CRAMMING) {
        return 19;
      }
      if (key == GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER) {
        return 128;
      }
      return false;
    }).when(rules).get(any(GameRule.class));
    when(server.getGameRules()).thenReturn(rules);

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
  void handleServerStartingCapturesFireSpreadRadiusDefault() throws Exception {
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules(FeatureFlags.DEFAULT_FLAGS);
    rules.set(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER, 32, null);
    when(server.getGameRules()).thenReturn(rules);

    GameRuleManager.handleServerStarting(server);

    assertEquals(32, readStaticField("configuredFireSpreadRadiusAroundPlayer"));
  }

  @Test
  void highLoadDisablesAndNormalLoadRestoresFireSpreadRadius() {
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules(FeatureFlags.DEFAULT_FLAGS);
    rules.set(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER, 64, null);
    when(server.getGameRules()).thenReturn(rules);

    try {
      writeServerManagerField("minecraftServer", server);
      GameRuleManager.handleServerStarting(server);
      assertEquals(64, (Integer) rules.get(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER));

      GameRuleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.HIGH, ServerLoadLevel.NORMAL, 75.0, 50.0));
      assertEquals(0, (Integer) rules.get(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER));

      writeStaticField("lastUpdateTime", 0L);
      GameRuleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.NORMAL, ServerLoadLevel.HIGH, 50.0, 75.0));
      assertEquals(64, (Integer) rules.get(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER));
    } catch (Exception exception) {
      throw new AssertionError(exception);
    } finally {
      try {
        writeServerManagerField("minecraftServer", null);
      } catch (Exception ignored) {
      }
    }
  }
}
