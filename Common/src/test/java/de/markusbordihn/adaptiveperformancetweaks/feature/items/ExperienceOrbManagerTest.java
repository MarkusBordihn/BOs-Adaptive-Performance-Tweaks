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

package de.markusbordihn.adaptiveperformancetweaks.feature.items;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicReference;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.level.Level;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockMakers;

class ExperienceOrbManagerTest {

  private boolean previousFeatureState;
  private boolean previousOptimizeExperienceOrbs;
  private int previousClusterRange;
  private boolean previousMoveToLastDrop;
  private boolean previousRemoveStale;
  private int previousStaleTicks;

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

  private static ServerLevel mockOverworldLevel() {
    ServerLevel level = mock(ServerLevel.class, withSettings().mockMaker(MockMakers.SUBCLASS));
    doReturn(Level.OVERWORLD).when(level).dimension();
    return level;
  }

  private static void setTicks(short value) throws Exception {
    Field field = ExperienceOrbManager.class.getDeclaredField("ticks");
    field.setAccessible(true);
    field.setShort(null, value);
  }

  @BeforeEach
  void setUp() {
    previousFeatureState = FeatureToggle.EXPERIENCE_ORBS.isEnabled();
    previousOptimizeExperienceOrbs = ExperienceOrbsConfig.optimizeExperienceOrbs;
    previousClusterRange = ExperienceOrbsConfig.experienceOrbsClusterRange;
    previousMoveToLastDrop = ExperienceOrbsConfig.movePositionToLastDrop;
    previousRemoveStale = ExperienceOrbsConfig.removeStaleExperienceOrbs;
    previousStaleTicks = ExperienceOrbsConfig.staleExperienceOrbAgeTicks;

    FeatureToggle.EXPERIENCE_ORBS.setEnabled(true);
    ExperienceOrbsConfig.optimizeExperienceOrbs = true;
    ExperienceOrbsConfig.experienceOrbsClusterRange = 2;
    ExperienceOrbsConfig.movePositionToLastDrop = false;
    ExperienceOrbsConfig.removeStaleExperienceOrbs = true;
    ExperienceOrbsConfig.staleExperienceOrbAgeTicks = 120_000;
    PerformanceStats.reset();
    ExperienceOrbManager.handleServerAboutToStart();
  }

  @AfterEach
  void tearDown() {
    FeatureToggle.EXPERIENCE_ORBS.setEnabled(previousFeatureState);
    ExperienceOrbsConfig.optimizeExperienceOrbs = previousOptimizeExperienceOrbs;
    ExperienceOrbsConfig.experienceOrbsClusterRange = previousClusterRange;
    ExperienceOrbsConfig.movePositionToLastDrop = previousMoveToLastDrop;
    ExperienceOrbsConfig.removeStaleExperienceOrbs = previousRemoveStale;
    ExperienceOrbsConfig.staleExperienceOrbAgeTicks = previousStaleTicks;
    PerformanceStats.reset();
    ExperienceOrbManager.handleServerStopping();
  }

  @Test
  void nearbyOrbsMergeTheirValues() {
    ServerLevel level = mockOverworldLevel();
    AtomicReference<ExperienceOrb> mergedOrbReference = new AtomicReference<>();
    doAnswer(invocation -> {
      ExperienceOrb mergedOrb = invocation.getArgument(0);
      mergedOrb.setId(100);
      mergedOrbReference.set(mergedOrb);
      ExperienceOrbManager.handleExperienceOrbJoinLevel(mergedOrb, level);
      return true;
    }).when(level).addFreshEntity(any(ExperienceOrb.class));

    TestExperienceOrb first = new TestExperienceOrb(level, 0.0d, 64.0d, 0.0d, 3);
    first.setId(1);
    TestExperienceOrb second = new TestExperienceOrb(level, 1.0d, 64.0d, 1.0d, 5);
    second.setId(2);

    boolean firstMerged = ExperienceOrbManager.handleExperienceOrbJoinLevel(first, level);
    boolean secondMerged = ExperienceOrbManager.handleExperienceOrbJoinLevel(second, level);

    assertFalse(firstMerged);
    assertTrue(secondMerged);
    assertTrue(first.isRemoved());
    assertTrue(second.isRemoved());
    assertEquals(8, mergedOrbReference.get().getValue());
    assertEquals(1, ExperienceOrbManager.getTrackedExperienceOrbCount());
    assertEquals(1L, PerformanceStats.xpOrbsMerged);
  }

  @Test
  void zeroValueOrbIsRemovedImmediately() {
    ServerLevel level = mockOverworldLevel();
    TestExperienceOrb zeroValueOrb = new TestExperienceOrb(level, 0.0d, 64.0d, 0.0d, 0);
    zeroValueOrb.setId(1);

    boolean removed = ExperienceOrbManager.handleExperienceOrbJoinLevel(zeroValueOrb, level);

    assertTrue(removed);
    assertTrue(zeroValueOrb.isRemoved());
    assertEquals(0, ExperienceOrbManager.getTrackedExperienceOrbCount());
    assertEquals(1L, PerformanceStats.xpOrbsRemoved);
  }

  @Test
  void staleOrbIsRemovedDuringVerification() throws Exception {
    ExperienceOrbsConfig.staleExperienceOrbAgeTicks = 20;
    ExperienceOrbManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();
    TestExperienceOrb orb = new TestExperienceOrb(level, 0.0d, 64.0d, 0.0d, 4);
    orb.setId(1);
    orb.tickCount = 25;

    boolean merged = ExperienceOrbManager.handleExperienceOrbJoinLevel(orb, level);
    setTicks((short) 599);
    ExperienceOrbManager.handleServerTick();

    assertFalse(merged);
    assertTrue(orb.isRemoved());
    assertEquals(0, ExperienceOrbManager.getTrackedExperienceOrbCount());
    assertEquals(1L, PerformanceStats.xpOrbsRemoved);
  }

  @Test
  void leaveLevelRemovesTrackedOrb() {
    ServerLevel level = mockOverworldLevel();
    TestExperienceOrb orb = new TestExperienceOrb(level, 0.0d, 64.0d, 0.0d, 4);
    orb.setId(1);

    ExperienceOrbManager.handleExperienceOrbJoinLevel(orb, level);
    ExperienceOrbManager.handleExperienceOrbLeaveLevel(orb, level);

    assertEquals(0, ExperienceOrbManager.getTrackedExperienceOrbCount());
    assertTrue(ExperienceOrbManager.getTrackedExperienceOrbCountsByDimension().isEmpty());
  }

  private static final class TestExperienceOrb extends ExperienceOrb {

    private TestExperienceOrb(ServerLevel level, double x, double y, double z, int value) {
      super(level, x, y, z, value);
    }
  }
}
