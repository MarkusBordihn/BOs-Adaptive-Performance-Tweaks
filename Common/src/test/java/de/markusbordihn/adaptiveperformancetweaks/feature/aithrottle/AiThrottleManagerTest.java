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

package de.markusbordihn.adaptiveperformancetweaks.feature.aithrottle;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class AiThrottleManagerTest {

  private static ServerLoadLevel readCurrentLoadLevel() throws Exception {
    Field field = AiThrottleManager.class.getDeclaredField("currentLoadLevel");
    field.setAccessible(true);
    return (ServerLoadLevel) field.get(null);
  }

  private static ServerLoadEvent eventFor(ServerLoadLevel level) {
    return new ServerLoadEvent(level, ServerLoadLevel.NORMAL, 0.0, 0.0);
  }

  @BeforeEach
  void resetLoadLevel() throws Exception {
    Field field = AiThrottleManager.class.getDeclaredField("currentLoadLevel");
    field.setAccessible(true);
    field.set(null, ServerLoadLevel.NORMAL);
  }

  @Test
  void initialLoadLevelIsNormal() throws Exception {
    assertEquals(ServerLoadLevel.NORMAL, readCurrentLoadLevel());
  }

  @ParameterizedTest
  @EnumSource(ServerLoadLevel.class)
  void handleServerLoadEventUpdatesLoadLevel(ServerLoadLevel level) throws Exception {
    AiThrottleManager.handleServerLoadEvent(eventFor(level));
    assertEquals(level, readCurrentLoadLevel());
  }

  @Test
  void loadLevelResetAfterDropToNormal() throws Exception {
    AiThrottleManager.handleServerLoadEvent(eventFor(ServerLoadLevel.VERY_HIGH));
    assertEquals(ServerLoadLevel.VERY_HIGH, readCurrentLoadLevel());

    AiThrottleManager.handleServerLoadEvent(eventFor(ServerLoadLevel.NORMAL));
    assertEquals(ServerLoadLevel.NORMAL, readCurrentLoadLevel());
  }

  @Test
  void nearbyRadiusIsPositive() {
    assertTrue(AiThrottleConfig.aiThrottleNearbyRadius > 0);
  }

  @Test
  void usesMeasuredPerLevelLoadWhenAvailable() throws IOException {
    String source =
        Files.readString(
            Path.of(
                "src/main/java/de/markusbordihn/adaptiveperformancetweaks/feature/aithrottle/AiThrottleManager.java"));
    assertTrue(source.contains("mob.level() instanceof ServerLevel serverLevel"));
    assertTrue(source.contains("ServerLevelLoad.hasMeasuredLoad("));
    assertTrue(source.contains("ServerLevelLoad.getLevelLoad(serverLevel)"));
  }
}
