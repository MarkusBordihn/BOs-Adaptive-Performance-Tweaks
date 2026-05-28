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

import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
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

  @Test
  void configuredDistanceMaxRespectsServerStartupLimit() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax + 4);
    assertEquals(ViewDistanceConfig.viewDistanceMax, invokeIntMethod("getConfiguredDistanceMax"));

    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMin + 2);
    assertEquals(ViewDistanceConfig.viewDistanceMin + 2,
      invokeIntMethod("getConfiguredDistanceMax"));
  }

  @Test
  void resolveTargetDistanceKeepsCurrentDistanceAtNormalLoad() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    writeStaticField("warmupUntilTime", 0L);
    writeStaticField("lastRecoveryTime", 0L);
    writeStaticField("currentLoadLevel", ServerLoadLevel.NORMAL);
    writeStaticField("currentDistance", ViewDistanceConfig.viewDistanceMin);

    assertEquals(ViewDistanceConfig.viewDistanceMin, invokeIntMethod("resolveTargetDistance"));
  }

  @Test
  void resolveTargetDistanceUsesMinimumDuringWarmup() throws Exception {
    writeStaticField("configuredDistanceMax", ViewDistanceConfig.viewDistanceMax);
    writeStaticField("warmupUntilTime", System.currentTimeMillis() + 10_000L);
    writeStaticField("currentLoadLevel", ServerLoadLevel.VERY_LOW);
    writeStaticField("currentDistance", ViewDistanceConfig.viewDistanceMax);

    assertEquals(ViewDistanceConfig.viewDistanceMin, invokeIntMethod("resolveTargetDistance"));
  }
}
