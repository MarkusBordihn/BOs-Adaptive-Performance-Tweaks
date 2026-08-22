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

import de.markusbordihn.adaptiveperformancetweaks.accessor.ExperienceOrbAccessor;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnManager;
import java.util.concurrent.atomic.AtomicInteger;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.level.NaturalSpawner;

public final class MixinTests {

  private MixinTests() {
  }

  public static void testExperienceOrbAccessorMixin(GameTestHelper helper) {
    ExperienceOrb orb = new ExperienceOrb(EntityType.EXPERIENCE_ORB, helper.getLevel());
    GameTestHelpers.assertTrue(
      helper,
      "ExperienceOrbAccessor mixin was not applied - check mixin config and refmap!",
      orb instanceof ExperienceOrbAccessor);
    ExperienceOrbAccessor accessor = (ExperienceOrbAccessor) orb;
    accessor.setValue(42);
    GameTestHelpers.assertEquals(
      helper,
      "ExperienceOrbAccessor.getValue() returned wrong value - field remapping broken?",
      42,
      accessor.getValue());
    helper.succeed();
  }

  public static void testNaturalSpawnerThrottleMixin(GameTestHelper helper) {
    boolean wasSpawnEnabled = FeatureToggle.SPAWN.isEnabled();
    boolean originalLimitationEnabled = SpawnConfig.naturalSpawnLimitationEnabled;
    boolean originalPrioritizeByTimeOfDay = SpawnConfig.naturalSpawnPrioritizeByTimeOfDay;
    double originalPassRateNormal = SpawnConfig.naturalSpawnPassRateNormal;
    ServerLoadLevel originalMinOptimizationLoadLevel = SpawnConfig.minOptimizationLoadLevel;

    FeatureToggle.SPAWN.setEnabled(true);
    SpawnConfig.naturalSpawnLimitationEnabled = true;
    SpawnConfig.naturalSpawnPrioritizeByTimeOfDay = false;
    SpawnConfig.naturalSpawnPassRateNormal = 0.0;
    SpawnConfig.minOptimizationLoadLevel = ServerLoadLevel.NORMAL;
    ServerLevelLoad.reset();

    SpawnManager.handleServerAboutToStart();
    SpawnManager.handleServerStarted();
    for (int i = 0; i <= 20 * 20; i++) {
      SpawnManager.handleServerTick();
    }

    ServerLevel level = helper.getLevel();
    BlockPos spawnPos = helper.absolutePos(new BlockPos(0, 1, 0));
    AtomicInteger candidateChecks = new AtomicInteger();
    long deniedBefore = PerformanceStats.naturalSpawnsDenied;

    NaturalSpawner.spawnCategoryForPosition(
      MobCategory.MONSTER,
      level,
      level.getChunk(spawnPos),
      spawnPos,
      (entityType, candidatePos, chunk) -> {
        candidateChecks.incrementAndGet();
        return true;
      },
      (mob, chunk) -> {
      });

    long deniedAfter = PerformanceStats.naturalSpawnsDenied;
    FeatureToggle.SPAWN.setEnabled(wasSpawnEnabled);
    SpawnConfig.naturalSpawnLimitationEnabled = originalLimitationEnabled;
    SpawnConfig.naturalSpawnPrioritizeByTimeOfDay = originalPrioritizeByTimeOfDay;
    SpawnConfig.naturalSpawnPassRateNormal = originalPassRateNormal;
    SpawnConfig.minOptimizationLoadLevel = originalMinOptimizationLoadLevel;
    SpawnManager.handleServerAboutToStart();
    ServerLevelLoad.reset();

    GameTestHelpers.assertEquals(
      helper,
      "NaturalSpawner mixin was not applied - spawn cycle was not cancelled!",
      0,
      candidateChecks.get());
    GameTestHelpers.assertEquals(
      helper,
      "Natural spawn throttle must be evaluated once per position, not per spawn candidate",
      deniedBefore + 1,
      deniedAfter);
    helper.succeed();
  }
}
