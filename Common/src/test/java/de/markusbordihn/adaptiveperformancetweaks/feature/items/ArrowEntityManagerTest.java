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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import net.minecraft.SharedConstants;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.network.chat.Component;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.projectile.arrow.Arrow;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockMakers;

class ArrowEntityManagerTest {

  private boolean previousFeatureState;
  private int previousMaxPerWorld;
  private int previousMaxPerChunk;
  private Set<String> previousAllowList;
  private Set<String> previousDenyList;

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
    Items.ARROW.builtInRegistryHolder().bindComponents(DataComponentMap.EMPTY);
  }

  private static ServerLevel mockOverworldLevel() {
    ServerLevel level = mock(ServerLevel.class, withSettings().mockMaker(MockMakers.SUBCLASS));
    doReturn(Level.OVERWORLD).when(level).dimension();
    return level;
  }

  private static Arrow createArrow(ServerLevel level, int id, int chunkX, int chunkZ,
    double velocitySquared, boolean named, boolean removed) {
    Arrow arrow = new Arrow(EntityType.ARROW, level);
    arrow.setId(id);
    arrow.setPos(chunkX * 16.0d, 64.0d, chunkZ * 16.0d);
    arrow.setDeltaMovement(new Vec3(velocitySquared, 0.0d, 0.0d));
    if (named) {
      arrow.setCustomName(Component.literal("Protected"));
    }
    if (removed) {
      arrow.discard();
    }
    return arrow;
  }

  private static void setTicks(short value) throws Exception {
    Field field = ArrowEntityManager.class.getDeclaredField("ticks");
    field.setAccessible(true);
    field.setShort(null, value);
  }

  @BeforeEach
  void setUp() {
    previousFeatureState = FeatureToggle.ARROWS.isEnabled();
    previousMaxPerWorld = ArrowsConfig.maxNumberOfArrowsPerWorld;
    previousMaxPerChunk = ArrowsConfig.maxNumberOfArrowsPerChunk;
    previousAllowList = new HashSet<>(ArrowsConfig.arrowsAllowList);
    previousDenyList = new HashSet<>(ArrowsConfig.arrowsDenyList);

    FeatureToggle.ARROWS.setEnabled(true);
    ArrowsConfig.maxNumberOfArrowsPerWorld = 512;
    ArrowsConfig.maxNumberOfArrowsPerChunk = 32;
    ArrowsConfig.arrowsAllowList = new HashSet<>();
    ArrowsConfig.arrowsDenyList = new HashSet<>();
    PerformanceStats.reset();
    ArrowEntityManager.handleServerAboutToStart();
  }

  @AfterEach
  void tearDown() {
    FeatureToggle.ARROWS.setEnabled(previousFeatureState);
    ArrowsConfig.maxNumberOfArrowsPerWorld = previousMaxPerWorld;
    ArrowsConfig.maxNumberOfArrowsPerChunk = previousMaxPerChunk;
    ArrowsConfig.arrowsAllowList = previousAllowList;
    ArrowsConfig.arrowsDenyList = previousDenyList;
    PerformanceStats.reset();
    ArrowEntityManager.handleServerStopping();
  }

  @Test
  void allowListSkipsUnlistedArrowTypes() {
    ArrowsConfig.arrowsAllowList = Set.of("minecraft:trident");
    ArrowEntityManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();

    ArrowEntityManager.handleArrowJoinLevel(createArrow(level, 1, 0, 0, 0.0d, false, false),
      level);

    assertEquals(0, ArrowEntityManager.getTrackedArrowCount());
  }

  @Test
  void denyListSkipsListedArrowTypes() {
    ArrowsConfig.arrowsDenyList = Set.of("minecraft:arrow");
    ArrowEntityManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();

    ArrowEntityManager.handleArrowJoinLevel(createArrow(level, 1, 0, 0, 0.0d, false, false),
      level);

    assertEquals(0, ArrowEntityManager.getTrackedArrowCount());
  }

  @Test
  void chunkLimitRemovesOldestStuckArrow() throws Exception {
    ArrowsConfig.maxNumberOfArrowsPerChunk = 1;
    ArrowsConfig.maxNumberOfArrowsPerWorld = 0;
    ArrowEntityManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();
    Arrow oldest = createArrow(level, 1, 0, 0, 0.0d, false, false);
    Arrow newest = createArrow(level, 2, 0, 0, 0.0d, false, false);

    ArrowEntityManager.handleArrowJoinLevel(oldest, level);
    ArrowEntityManager.handleArrowJoinLevel(newest, level);
    setTicks((short) 599);

    ArrowEntityManager.handleServerTick();

    assertTrue(oldest.isRemoved());
    assertFalse(newest.isRemoved());
    assertEquals(1, ArrowEntityManager.getTrackedArrowCount());
    assertEquals(1L, PerformanceStats.arrowsRemoved);
  }

  @Test
  void worldLimitIgnoresNamedProtectedArrows() throws Exception {
    ArrowsConfig.maxNumberOfArrowsPerChunk = 0;
    ArrowsConfig.maxNumberOfArrowsPerWorld = 1;
    ArrowEntityManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();
    Arrow protectedArrow = createArrow(level, 1, 0, 0, 0.0d, true, false);
    Arrow oldestStuck = createArrow(level, 2, 0, 0, 0.0d, false, false);
    Arrow newestStuck = createArrow(level, 3, 1, 0, 0.0d, false, false);

    ArrowEntityManager.handleArrowJoinLevel(protectedArrow, level);
    ArrowEntityManager.handleArrowJoinLevel(oldestStuck, level);
    ArrowEntityManager.handleArrowJoinLevel(newestStuck, level);
    setTicks((short) 599);

    ArrowEntityManager.handleServerTick();

    assertFalse(protectedArrow.isRemoved());
    assertTrue(oldestStuck.isRemoved());
    assertFalse(newestStuck.isRemoved());
    assertEquals(2, ArrowEntityManager.getTrackedArrowCount());
    assertEquals(1L, PerformanceStats.arrowsRemoved);
  }

  @Test
  void verificationRemovesDiscardedTrackedArrows() throws Exception {
    ArrowsConfig.maxNumberOfArrowsPerChunk = 0;
    ArrowsConfig.maxNumberOfArrowsPerWorld = 0;
    ArrowEntityManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();
    Arrow activeArrow = createArrow(level, 1, 0, 0, 0.0d, false, false);
    Arrow removedArrow = createArrow(level, 2, 0, 0, 0.0d, false, true);

    ArrowEntityManager.handleArrowJoinLevel(activeArrow, level);
    ArrowEntityManager.handleArrowJoinLevel(removedArrow, level);
    setTicks((short) 599);

    ArrowEntityManager.handleServerTick();

    assertEquals(1, ArrowEntityManager.getTrackedArrowCount());
    assertEquals(Collections.singletonMap("minecraft:overworld",
        Collections.singletonMap("minecraft:arrow", 1)),
      ArrowEntityManager.getArrowCountsByDimension());
  }
}
