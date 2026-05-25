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

package de.markusbordihn.adaptiveperformancetweaks.gametest;

import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.Objects;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;

public final class GameTestHelpers {

  private GameTestHelpers() {}

  public static void assertTrue(GameTestHelper helper, String message, boolean condition) {
    if (!condition) {
      helper.fail(message);
    }
  }

  public static void assertFalse(GameTestHelper helper, String message, boolean condition) {
    if (condition) {
      helper.fail(message);
    }
  }

  public static void assertNotNull(GameTestHelper helper, String message, Object object) {
    if (object == null) {
      helper.fail(message);
    }
  }

  public static void assertEquals(
      GameTestHelper helper, String message, Object expected, Object actual) {
    if (!Objects.equals(expected, actual)) {
      helper.fail(message + " (expected=" + expected + ", actual=" + actual + ")");
    }
  }

  public static ServerLevel getRequiredLevel(GameTestHelper helper, ResourceKey<Level> levelKey) {
    ServerLevel serverLevel = helper.getLevel().getServer().getLevel(levelKey);
    if (serverLevel == null) {
      helper.fail("Expected level not available: " + levelKey.location());
      return helper.getLevel();
    }
    return serverLevel;
  }

  public static void resetMeasuredLevelLoad() {
    ServerLevelLoad.reset();
  }

  public static void setMeasuredLevelLoad(
      ServerLevel serverLevel, ServerLoadLevel loadLevel, double averageTickTime) {
    try {
      getLevelTickTimes().put(serverLevel, averageTickTime);
      getReportedLevelTickTimes().put(serverLevel, averageTickTime);
      getLevelLoadLevels().put(serverLevel, loadLevel);
    } catch (ReflectiveOperationException exception) {
      throw new IllegalStateException("Unable to set measured level load for tests", exception);
    }
  }

  @SuppressWarnings("unchecked")
  private static Map<ServerLevel, Double> getLevelTickTimes() throws ReflectiveOperationException {
    Field field = ServerLevelLoad.class.getDeclaredField("levelTickTimes");
    field.setAccessible(true);
    return (Map<ServerLevel, Double>) field.get(null);
  }

  @SuppressWarnings("unchecked")
  private static Map<ServerLevel, Double> getReportedLevelTickTimes()
      throws ReflectiveOperationException {
    Field field = ServerLevelLoad.class.getDeclaredField("levelReportedTickTimes");
    field.setAccessible(true);
    return (Map<ServerLevel, Double>) field.get(null);
  }

  @SuppressWarnings("unchecked")
  private static Map<ServerLevel, ServerLoadLevel> getLevelLoadLevels()
      throws ReflectiveOperationException {
    Field field = ServerLevelLoad.class.getDeclaredField("levelLoadLevels");
    field.setAccessible(true);
    return (Map<ServerLevel, ServerLoadLevel>) field.get(null);
  }
}
