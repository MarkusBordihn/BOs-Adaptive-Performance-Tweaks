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
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.chunkgenthrottle.ChunkGenThrottleConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.chunkgenthrottle.ChunkGenThrottleManager;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;

public final class ChunkGenThrottleTests {

  private ChunkGenThrottleTests() {
  }

  public static void testDivisorIsOneUnderNormalLoad(GameTestHelper helper) {
    ChunkGenThrottleManager.handleServerLoadEvent(
      new ServerLoadEvent(ServerLoadLevel.NORMAL, ServerLoadLevel.NORMAL, 50.0, 50.0));
    GameTestHelpers.assertEquals(
      helper,
      "Chunk gen throttle divisor should be 1 under NORMAL load",
      1,
      ChunkGenThrottleManager.getThrottleDivisor());
    helper.succeed();
  }

  public static void testDivisorIncreasesUnderVeryHighLoad(GameTestHelper helper) {
    ChunkGenThrottleManager.handleServerLoadEvent(
      new ServerLoadEvent(ServerLoadLevel.VERY_HIGH, ServerLoadLevel.NORMAL, 200.0, 50.0));
    GameTestHelpers.assertEquals(
      helper,
      "Chunk gen throttle divisor should equal config value under VERY_HIGH load",
      ChunkGenThrottleConfig.chunkGenThrottleVeryHighDivisor,
      ChunkGenThrottleManager.getThrottleDivisor());
    ChunkGenThrottleManager.handleServerLoadEvent(
      new ServerLoadEvent(ServerLoadLevel.NORMAL, ServerLoadLevel.VERY_HIGH, 50.0, 200.0));
    helper.succeed();
  }

  public static void testDivisorUsesPerLevelLoad(GameTestHelper helper) {
    ServerLevelLoad.reset();
    ChunkGenThrottleManager.handleServerLoadEvent(
      new ServerLoadEvent(ServerLoadLevel.NORMAL, ServerLoadLevel.NORMAL, 50.0, 50.0));

    ServerLevel overworld = helper.getLevel();
    ServerLevel nether = GameTestHelpers.getRequiredLevel(helper, Level.NETHER);
    GameTestHelpers.setMeasuredLevelLoad(overworld, ServerLoadLevel.VERY_HIGH, 200.0);
    GameTestHelpers.setMeasuredLevelLoad(nether, ServerLoadLevel.NORMAL, 50.0);

    GameTestHelpers.assertEquals(
      helper,
      "Chunk gen throttle divisor should use the measured high-load level value",
      ChunkGenThrottleConfig.chunkGenThrottleVeryHighDivisor,
      ChunkGenThrottleManager.getThrottleDivisor(overworld));
    GameTestHelpers.assertEquals(
      helper,
      "Chunk gen throttle divisor should stay at 1 for the measured normal-load level",
      1,
      ChunkGenThrottleManager.getThrottleDivisor(nether));

    ServerLevelLoad.reset();
    helper.succeed();
  }
}
