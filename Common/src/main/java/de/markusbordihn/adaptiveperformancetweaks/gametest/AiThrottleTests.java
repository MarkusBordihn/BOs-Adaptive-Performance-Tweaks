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
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.aithrottle.AiThrottleConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.aithrottle.AiThrottleManager;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityTypes;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.level.Level;

public final class AiThrottleTests {

  private static final double NO_PLAYER_NEARBY_Y = -2000.0;

  private AiThrottleTests() {
  }

  public static void testNoThrottleUnderNormalLoad(GameTestHelper helper) {
    FeatureToggle.AI_THROTTLING.setEnabled(true);
    try {
      ServerLevelLoad.reset();
      AiThrottleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.NORMAL, ServerLoadLevel.NORMAL, 50.0, 50.0));

      Mob mob = EntityTypes.ZOMBIE.create(helper.getLevel(), EntitySpawnReason.NATURAL);
      GameTestHelpers.assertNotNull(helper, "Zombie entity could not be created", mob);
      mob.snapTo(0.0, NO_PLAYER_NEARBY_Y, 0.0);

      mob.tickCount = 7;
      GameTestHelpers.assertTrue(
        helper,
        "AI should NOT be throttled under NORMAL load",
        !AiThrottleManager.shouldSkipAiThisTick(mob));
      helper.succeed();
    } finally {
      FeatureToggle.AI_THROTTLING.setEnabled(false);
    }
  }

  public static void testThrottleUnderVeryHighLoad(GameTestHelper helper) {
    FeatureToggle.AI_THROTTLING.setEnabled(true);
    try {
      ServerLevelLoad.reset();
      AiThrottleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.VERY_HIGH, ServerLoadLevel.NORMAL, 200.0, 50.0));

      Mob mob = EntityTypes.ZOMBIE.create(helper.getLevel(), EntitySpawnReason.NATURAL);
      GameTestHelpers.assertNotNull(helper, "Zombie entity could not be created", mob);
      mob.snapTo(0.0, NO_PLAYER_NEARBY_Y, 0.0);

      mob.tickCount = AiThrottleConfig.aiThrottleVeryHighDivisor - 1;
      GameTestHelpers.assertTrue(
        helper,
        "AI should be throttled under VERY_HIGH load when tickCount % divisor != 0",
        AiThrottleManager.shouldSkipAiThisTick(mob));

      mob.tickCount = AiThrottleConfig.aiThrottleVeryHighDivisor;
      GameTestHelpers.assertTrue(
        helper,
        "AI should NOT be throttled when tickCount % divisor == 0",
        !AiThrottleManager.shouldSkipAiThisTick(mob));

      AiThrottleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.NORMAL, ServerLoadLevel.VERY_HIGH, 50.0, 200.0));
      helper.succeed();
    } finally {
      FeatureToggle.AI_THROTTLING.setEnabled(false);
    }
  }

  public static void testThrottleOnlyInHighLoadLevel(GameTestHelper helper) {
    FeatureToggle.AI_THROTTLING.setEnabled(true);
    try {
      ServerLevelLoad.reset();
      AiThrottleManager.handleServerLoadEvent(
        new ServerLoadEvent(ServerLoadLevel.NORMAL, ServerLoadLevel.NORMAL, 50.0, 50.0));

      ServerLevel overworld = helper.getLevel();
      ServerLevel nether = GameTestHelpers.getRequiredLevel(helper, Level.NETHER);
      GameTestHelpers.setMeasuredLevelLoad(overworld, ServerLoadLevel.VERY_HIGH, 200.0);
      GameTestHelpers.setMeasuredLevelLoad(nether, ServerLoadLevel.NORMAL, 50.0);

      Mob overworldMob = EntityTypes.ZOMBIE.create(overworld, EntitySpawnReason.NATURAL);
      Mob netherMob = EntityTypes.ZOMBIE.create(nether, EntitySpawnReason.NATURAL);
      GameTestHelpers.assertNotNull(helper, "Overworld zombie entity could not be created",
        overworldMob);
      GameTestHelpers.assertNotNull(helper, "Nether zombie entity could not be created", netherMob);
      overworldMob.snapTo(0.0, NO_PLAYER_NEARBY_Y, 0.0);
      netherMob.snapTo(0.0, NO_PLAYER_NEARBY_Y, 0.0);

      overworldMob.tickCount = AiThrottleConfig.aiThrottleVeryHighDivisor - 1;
      netherMob.tickCount = AiThrottleConfig.aiThrottleVeryHighDivisor - 1;

      GameTestHelpers.assertTrue(
        helper,
        "AI should be throttled in the measured high-load level",
        AiThrottleManager.shouldSkipAiThisTick(overworldMob));
      GameTestHelpers.assertFalse(
        helper,
        "AI should not be throttled in the measured normal-load level",
        AiThrottleManager.shouldSkipAiThisTick(netherMob));

      helper.succeed();
    } finally {
      ServerLevelLoad.reset();
      FeatureToggle.AI_THROTTLING.setEnabled(false);
    }
  }
}
