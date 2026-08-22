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

import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.ViewDistanceConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.ViewDistanceManager;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.MinecraftServer;

public final class DistanceAdaptationTests {

  private DistanceAdaptationTests() {
  }

  public static void testViewDistanceDecreasesUnderVeryHighLoad(GameTestHelper helper) {
    FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(true);
    try {
      MinecraftServer server = helper.getLevel().getServer();
      ViewDistanceManager.handleServerStarting(server);

      ViewDistanceManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.VERY_HIGH, ServerLoadLevel.NORMAL, 200.0, 50.0));

      int expected = Math.max(ViewDistanceConfig.viewDistanceMin,
        Math.min(ViewDistanceConfig.viewDistanceMax, ViewDistanceConfig.viewDistanceVeryHigh));
      GameTestHelpers.assertEquals(
        helper,
        "View distance should be reduced under VERY_HIGH load",
        expected,
        ViewDistanceManager.getCurrentViewDistance());
      helper.succeed();
    } finally {
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(false);
    }
  }

  public static void testSimulationDistanceDecreasesUnderVeryHighLoad(GameTestHelper helper) {
    MinecraftServer server = helper.getLevel().getServer();
    SimulationDistanceManager.handleServerStarting(server);

    SimulationDistanceManager.handleServerLoadEvent(
      new ServerLoadEvent(ServerLoadLevel.VERY_HIGH, ServerLoadLevel.NORMAL, 200.0, 50.0));

    int expected = Math.max(SimulationDistanceConfig.simDistanceMin,
      Math.min(SimulationDistanceConfig.simDistanceMax,
        SimulationDistanceConfig.simDistanceVeryHigh));
    GameTestHelpers.assertEquals(
      helper,
      "Simulation distance should be reduced under VERY_HIGH load",
      expected,
      server.getPlayerList().getSimulationDistance());
    helper.succeed();
  }
}
