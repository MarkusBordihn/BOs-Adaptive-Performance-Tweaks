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

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.accessor.ExperienceOrbAccessor;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.world.entity.Entity.RemovalReason;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ExperienceOrbManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_ITEMS);
  private static final int VERIFICATION_TICK = 30 * 20;

  private static Map<String, Set<ExperienceOrb>> experienceOrbEntityMap = new ConcurrentHashMap<>();
  private static volatile ServerLoadLevel currentLoadLevel = ServerLoadLevel.NORMAL;
  private static short ticks = 0;

  private ExperienceOrbManager() {
  }

  public static void handleServerAboutToStart() {
    resetState();
    if (!FeatureToggle.EXPERIENCE_ORBS.isEnabled()) {
      return;
    }

    if (ExperienceOrbsConfig.optimizeExperienceOrbs) {
      log.info(
        "XP orb clustering enabled with radius of {} blocks.",
        ExperienceOrbsConfig.experienceOrbsClusterRange);
    }
    if (ExperienceOrbsConfig.removeStaleExperienceOrbs) {
      log.info(
        "Stale XP orb cleanup enabled for orbs older than {} ticks.",
        ExperienceOrbsConfig.staleExperienceOrbAgeTicks);
    }
  }

  public static void handleServerStopping() {
    resetState();
  }

  public static int getTrackedExperienceOrbCount() {
    int total = 0;
    for (Set<ExperienceOrb> orbs : experienceOrbEntityMap.values()) {
      total += orbs.size();
    }

    return total;
  }

  public static Map<String, Integer> getTrackedExperienceOrbCountsByDimension() {
    Map<String, Integer> result = new LinkedHashMap<>();
    for (Map.Entry<String, Set<ExperienceOrb>> entry : experienceOrbEntityMap.entrySet()) {
      int count = entry.getValue().size();
      if (count > 0) {
        result.put(entry.getKey(), count);
      }
    }
    return result;
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    currentLoadLevel = event.getServerLoadLevel();
  }

  private static void resetState() {
    experienceOrbEntityMap = new ConcurrentHashMap<>();
    currentLoadLevel = ServerLoadLevel.NORMAL;
    ticks = 0;
  }

  public static void handleServerTick() {
    if (++ticks < VERIFICATION_TICK) {
      return;
    }

    ticks = 0;
    verifyEntities();
  }

  public static boolean handleExperienceOrbJoinLevel(ExperienceOrb orbEntity, Level level) {
    if (level.isClientSide()) {
      return false;
    }

    String levelName = level.dimension().identifier().toString();

    if (removeInvalidOrb(orbEntity, levelName)) {
      return true;
    }

    experienceOrbEntityMap.computeIfAbsent(levelName, ignored -> ConcurrentHashMap.newKeySet());
    Set<ExperienceOrb> worldOrbs = experienceOrbEntityMap.get(levelName);

    if (ExperienceOrbsConfig.optimizeExperienceOrbs
      && currentLoadLevel.isAtLeast(ExperienceOrbsConfig.minOptimizationLoadLevel)
      && !worldOrbs.isEmpty()) {
      int orbX = (int) orbEntity.getX();
      int orbY = (int) orbEntity.getY();
      int orbZ = (int) orbEntity.getZ();
      int range = ExperienceOrbsConfig.experienceOrbsClusterRange;

      for (ExperienceOrb existing : worldOrbs) {
        int existingX = (int) existing.getX();
        int existingY = (int) existing.getY();
        int existingZ = (int) existing.getZ();

        if (orbEntity.getId() != existing.getId()
          && existing.isAlive()
          && (orbX - range < existingX && existingX < orbX + range)
          && (orbY - range < existingY && existingY < orbY + range)
          && (orbZ - range < existingZ && existingZ < orbZ + range)) {
          int mergedValue = existing.getValue() + orbEntity.getValue();
          log.debug(
            "[XP Merge] {}+{}={} xp at {} in {}",
            orbEntity.getValue(),
            existing.getValue(),
            mergedValue,
            orbEntity.blockPosition(),
            levelName);

          ((ExperienceOrbAccessor) existing).setValue(mergedValue);
          orbEntity.entityTags().forEach(existing::addTag);
          if (ExperienceOrbsConfig.movePositionToLastDrop) {
            existing.snapTo(
              orbEntity.getX(),
              Math.max(existing.getY(), orbEntity.getY()),
              orbEntity.getZ());
          }

          orbEntity.remove(RemovalReason.DISCARDED);
          PerformanceStats.xpOrbsMerged++;
          return true;
        }
      }
    }

    worldOrbs.add(orbEntity);

    return false;
  }

  public static void handleExperienceOrbLeaveLevel(ExperienceOrb orbEntity, Level level) {
    if (level.isClientSide()) {
      return;
    }

    String levelName = level.dimension().identifier().toString();
    Set<ExperienceOrb> worldOrbs = experienceOrbEntityMap.get(levelName);
    if (worldOrbs != null) {
      worldOrbs.remove(orbEntity);
      if (worldOrbs.isEmpty()) {
        experienceOrbEntityMap.remove(levelName);
      }
    }
  }

  private static void verifyEntities() {
    Iterator<Map.Entry<String, Set<ExperienceOrb>>> mapIterator =
      experienceOrbEntityMap.entrySet().iterator();
    int removedEntries = 0;
    int removedSets = 0;

    while (mapIterator.hasNext()) {
      Map.Entry<String, Set<ExperienceOrb>> entry = mapIterator.next();
      Set<ExperienceOrb> orbs = entry.getValue();
      Iterator<ExperienceOrb> orbIterator = orbs.iterator();
      while (orbIterator.hasNext()) {
        ExperienceOrb orbEntity = orbIterator.next();
        if (orbEntity == null || orbEntity.isRemoved() || !orbEntity.isAlive()) {
          orbIterator.remove();
          removedEntries++;
          continue;
        }
        if (removeInvalidOrb(orbEntity, entry.getKey()) || removeStaleOrb(orbEntity,
          entry.getKey())) {
          orbIterator.remove();
          removedEntries++;
        }
      }
      if (orbs.isEmpty()) {
        mapIterator.remove();
        removedSets++;
      }
    }

    if (removedEntries > 0 || removedSets > 0) {
      log.debug(
        "[XP Verification] Removed {} stale orbs from {} worlds", removedEntries, removedSets);
    }
  }

  private static boolean removeInvalidOrb(ExperienceOrb orbEntity, String levelName) {
    if (!ExperienceOrbsConfig.optimizeExperienceOrbs
      || orbEntity.getValue() > 0) {
      return false;
    }

    log.debug("[XP Orb] Zero-value orb at {} in {} removed",
      orbEntity.blockPosition(), levelName);
    orbEntity.remove(RemovalReason.DISCARDED);
    PerformanceStats.xpOrbsRemoved++;
    return true;
  }

  private static boolean removeStaleOrb(ExperienceOrb orbEntity, String levelName) {
    if (!ExperienceOrbsConfig.removeStaleExperienceOrbs
      || orbEntity.tickCount < ExperienceOrbsConfig.staleExperienceOrbAgeTicks) {
      return false;
    }

    log.debug("[XP Orb] Stale orb at {} in {} removed after {} ticks",
      orbEntity.blockPosition(), levelName, orbEntity.tickCount);
    orbEntity.remove(RemovalReason.DISCARDED);
    PerformanceStats.xpOrbsRemoved++;
    return true;
  }
}
