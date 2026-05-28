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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.world.level.GameRules;
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
}
