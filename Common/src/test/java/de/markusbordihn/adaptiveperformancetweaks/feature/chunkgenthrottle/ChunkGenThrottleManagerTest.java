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

package de.markusbordihn.adaptiveperformancetweaks.feature.chunkgenthrottle;

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
import org.junit.jupiter.params.provider.CsvSource;

class ChunkGenThrottleManagerTest {

  private static ServerLoadEvent eventFor(ServerLoadLevel level) {
    return new ServerLoadEvent(level, ServerLoadLevel.NORMAL, 0.0, 0.0);
  }

  @BeforeEach
  void resetLoadLevel() throws Exception {
    Field field = ChunkGenThrottleManager.class.getDeclaredField("currentLoadLevel");
    field.setAccessible(true);
    field.set(null, ServerLoadLevel.NORMAL);
  }

  @Test
  void initialDivisorIsOne() {
    assertEquals(1, ChunkGenThrottleManager.getThrottleDivisor());
  }

  @ParameterizedTest
  @CsvSource({
    "VERY_LOW,  1",
    "LOW,       1",
    "NORMAL,    1",
    "MEDIUM,    2",
    "HIGH,      4",
    "VERY_HIGH, 8"
  })
  void divisorMatchesLoadLevel(ServerLoadLevel level, int expectedDivisor) {
    ChunkGenThrottleManager.handleServerLoadEvent(eventFor(level));
    assertEquals(expectedDivisor, ChunkGenThrottleManager.getThrottleDivisor());
  }

  @Test
  void divisorReturnsToOneAfterLoadDrops() {
    ChunkGenThrottleManager.handleServerLoadEvent(eventFor(ServerLoadLevel.VERY_HIGH));
    assertEquals(8, ChunkGenThrottleManager.getThrottleDivisor());

    ChunkGenThrottleManager.handleServerLoadEvent(eventFor(ServerLoadLevel.NORMAL));
    assertEquals(1, ChunkGenThrottleManager.getThrottleDivisor());
  }

  @Test
  void divisorsAreStrictlyIncreasing() {
    int medium = ChunkGenThrottleConfig.chunkGenThrottleMediumDivisor;
    int high = ChunkGenThrottleConfig.chunkGenThrottleHighDivisor;
    int veryHigh = ChunkGenThrottleConfig.chunkGenThrottleVeryHighDivisor;
    assertTrue(medium < high, "MEDIUM divisor must be less than HIGH");
    assertTrue(high < veryHigh, "HIGH divisor must be less than VERY_HIGH");
  }

  @Test
  void includesPerLevelLoadOverride() throws IOException {
    String source =
        Files.readString(
            Path.of(
                "src/main/java/de/markusbordihn/adaptiveperformancetweaks/feature/chunkgenthrottle/ChunkGenThrottleManager.java"));
    assertTrue(source.contains("getThrottleDivisor(ServerLevel serverLevel)"));
    assertTrue(source.contains("ServerLevelLoad.hasMeasuredLoad(serverLevel)"));
    assertTrue(source.contains("ServerLevelLoad.getLevelLoad(serverLevel)"));
  }
}
