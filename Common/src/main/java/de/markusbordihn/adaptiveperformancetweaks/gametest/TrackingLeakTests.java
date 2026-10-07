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

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.diagnostics.TrackedMapStatistics;
import de.markusbordihn.adaptiveperformancetweaks.core.diagnostics.TrackingDiagnostics;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.monster.Zombie;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class TrackingLeakTests {

  public static final String BATCH = "tracking_leak";

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final int DUPLICATE_SPAWN_ATTEMPTS = 10_000;

  private TrackingLeakTests() {
  }

  public static void testFailedSpawnsDoNotStayTracked(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    BlockPos spawnPos = helper.absolutePos(new BlockPos(0, 1, 0));
    Zombie original = createZombie(level, spawnPos);
    level.addFreshEntity(original);

    List<Zombie> duplicates = new ArrayList<>(DUPLICATE_SPAWN_ATTEMPTS);
    try {
      logSnapshot("before duplicate spawns");

      int failedSpawns = 0;
      for (int i = 0; i < DUPLICATE_SPAWN_ATTEMPTS; i++) {
        Zombie duplicate = createZombie(level, spawnPos);
        duplicate.setUUID(original.getUUID());
        duplicates.add(duplicate);
        if (!level.addFreshEntity(duplicate)) {
          failedSpawns++;
        }
      }
      List<TrackedMapStatistics> afterDuplicateSpawns = logSnapshot("after duplicate spawns");

      CoreEntityManager.verifyTrackedEntities();
      List<TrackedMapStatistics> afterVerification = logSnapshot("after verification");

      GameTestHelpers.assertEquals(
        helper, "Every duplicate UUID spawn should be rejected by vanilla",
        DUPLICATE_SPAWN_ATTEMPTS, failedSpawns);
      GameTestHelpers.assertEquals(
        helper, "Rejected spawns should leave tracking immediately, without verification",
        0, TrackingDiagnostics.getOrphanedEntryCount(afterDuplicateSpawns));
      GameTestHelpers.assertEquals(
        helper, "No orphaned entries should remain after verification",
        0, TrackingDiagnostics.getOrphanedEntryCount(afterVerification));
      GameTestHelpers.assertEquals(
        helper, "Only the original zombie should stay tracked",
        1, countTrackedEntities(original.getUUID()));
    } finally {
      for (Zombie duplicate : duplicates) {
        CoreEntityManager.handleEntityLeaveLevel(duplicate, false);
      }
      original.discard();
    }

    helper.succeed();
  }

  private static Zombie createZombie(ServerLevel level, BlockPos spawnPos) {
    Zombie zombie = new Zombie(EntityType.ZOMBIE, level);
    zombie.setNoAi(true);
    zombie.setPos(spawnPos.getX() + 0.5, spawnPos.getY(), spawnPos.getZ() + 0.5);
    return zombie;
  }

  private static int countTrackedEntities(UUID entityUUID) {
    int count = 0;
    for (Set<Entity> entities : CoreEntityManager.getEntitiesGlobal().values()) {
      for (Entity entity : entities) {
        if (entityUUID.equals(entity.getUUID())) {
          count++;
        }
      }
    }

    return count;
  }

  private static List<TrackedMapStatistics> logSnapshot(String label) {
    for (String line : TrackingDiagnostics.createReportLines()) {
      log.info("[Tracking Leak Test] {}: {}", label, line);
    }

    return TrackingDiagnostics.collectMapStatistics();
  }
}
