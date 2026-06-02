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
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListSet;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Entity.RemovalReason;
import net.minecraft.world.entity.projectile.AbstractArrow;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ArrowEntityManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_ITEMS);
  private static final int VERIFICATION_TICK = 30 * 20;

  private static final double STUCK_VELOCITY_THRESHOLD = 1.0E-8;

  private static Map<String, Set<AbstractArrow>> arrowWorldEntityMap = new ConcurrentHashMap<>();
  private static volatile ServerLoadLevel currentLoadLevel = ServerLoadLevel.NORMAL;
  private static boolean hasArrowsAllowList = false;
  private static boolean hasArrowsDenyList = false;
  private static short ticks = 0;

  private ArrowEntityManager() {
  }

  public static void handleServerAboutToStart() {
    resetState();
    if (!FeatureToggle.ARROWS.isEnabled()) {
      return;
    }

    hasArrowsAllowList = !ArrowsConfig.arrowsAllowList.isEmpty();
    hasArrowsDenyList = !ArrowsConfig.arrowsDenyList.isEmpty();

    log.info(
      "Arrow optimization enabled: maxPerWorld={}, maxPerChunk={}",
      ArrowsConfig.maxNumberOfArrowsPerWorld,
      ArrowsConfig.maxNumberOfArrowsPerChunk);
  }

  public static void handleServerStopping() {
    resetState();
  }

  public static int getTrackedArrowCount() {
    int total = 0;
    for (Set<AbstractArrow> arrows : arrowWorldEntityMap.values()) {
      total += arrows.size();
    }
    return total;
  }

  public static Map<String, Map<String, Integer>> getArrowCountsByDimension() {
    Map<String, Map<String, Integer>> result = new LinkedHashMap<>();
    for (Map.Entry<String, Set<AbstractArrow>> entry : arrowWorldEntityMap.entrySet()) {
      String dimension = entry.getKey();
      Map<String, Integer> typeCounts = new HashMap<>();
      for (AbstractArrow arrow : entry.getValue()) {
        String entityId = BuiltInRegistries.ENTITY_TYPE.getKey(arrow.getType()).toString();
        typeCounts.merge(entityId, 1, Integer::sum);
      }
      if (!typeCounts.isEmpty()) {
        result.put(dimension, typeCounts);
      }
    }
    return result;
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    currentLoadLevel = event.getServerLoadLevel();
  }

  private static void resetState() {
    arrowWorldEntityMap = new ConcurrentHashMap<>();
    currentLoadLevel = ServerLoadLevel.NORMAL;
    hasArrowsAllowList = !ArrowsConfig.arrowsAllowList.isEmpty();
    hasArrowsDenyList = !ArrowsConfig.arrowsDenyList.isEmpty();
    ticks = 0;
  }

  public static void handleServerTick() {
    if (++ticks < VERIFICATION_TICK) {
      return;
    }
    ticks = 0;
    if (currentLoadLevel.isAtLeast(ArrowsConfig.minOptimizationLoadLevel)) {
      enforceArrowLimits();
    }
    verifyEntities();
  }

  public static void handleArrowJoinLevel(AbstractArrow arrowEntity, Level level) {
    if (level.isClientSide || arrowEntity.isRemoved()) {
      return;
    }

    String entityId = BuiltInRegistries.ENTITY_TYPE.getKey(arrowEntity.getType()).toString();
    if (hasArrowsAllowList && !ArrowsConfig.arrowsAllowList.contains(entityId)) {
      return;
    }

    if (hasArrowsDenyList && ArrowsConfig.arrowsDenyList.contains(entityId)) {
      return;
    }

    String levelName = level.dimension().location().toString();
    arrowWorldEntityMap.computeIfAbsent(
      levelName, ignored -> new ConcurrentSkipListSet<>(Comparator.comparingInt(Entity::getId)));
    arrowWorldEntityMap.get(levelName).add(arrowEntity);
  }

  public static void handleArrowLeaveLevel(AbstractArrow arrowEntity, Level level) {
    if (level.isClientSide) {
      return;
    }

    String levelName = level.dimension().location().toString();
    Set<AbstractArrow> worldArrows = arrowWorldEntityMap.get(levelName);
    if (worldArrows != null) {
      worldArrows.remove(arrowEntity);
      if (worldArrows.isEmpty()) {
        arrowWorldEntityMap.remove(levelName);
      }
    }
  }

  private static boolean isStuckArrow(AbstractArrow arrow) {
    return arrow.getDeltaMovement().lengthSqr() < STUCK_VELOCITY_THRESHOLD;
  }

  private static boolean isProtectedArrow(AbstractArrow arrow) {
    return arrow.hasCustomName();
  }

  private static void enforceArrowLimits() {
    for (Map.Entry<String, Set<AbstractArrow>> worldEntry : arrowWorldEntityMap.entrySet()) {
      String levelName = worldEntry.getKey();
      Set<AbstractArrow> worldArrows = worldEntry.getValue();

      enforceChunkLimits(worldArrows);
      enforceWorldLimit(worldArrows, levelName);
    }
  }

  private static void enforceChunkLimits(Set<AbstractArrow> worldArrows) {
    if (ArrowsConfig.maxNumberOfArrowsPerChunk <= 0) {
      return;
    }

    Map<Long, List<AbstractArrow>> byChunk = new HashMap<>();
    for (AbstractArrow arrow : worldArrows) {
      if (!arrow.isAlive() || !isStuckArrow(arrow) || isProtectedArrow(arrow)) {
        continue;
      }

      long chunkKey = arrow.chunkPosition().toLong();
      byChunk.computeIfAbsent(chunkKey, ignored -> new ArrayList<>()).add(arrow);
    }

    for (List<AbstractArrow> chunkArrows : byChunk.values()) {
      int count = chunkArrows.size();
      if (count <= ArrowsConfig.maxNumberOfArrowsPerChunk) {
        continue;
      }

      chunkArrows.sort(Comparator.comparingInt(Entity::getId));
      int removeCount = count - ArrowsConfig.maxNumberOfArrowsPerChunk;
      for (int index = 0; index < removeCount; index++) {
        AbstractArrow oldest = chunkArrows.get(index);
        log.debug("[Chunk Limit] {} at {} removed ({}/{})",
          BuiltInRegistries.ENTITY_TYPE.getKey(oldest.getType()),
          oldest.blockPosition(), count, ArrowsConfig.maxNumberOfArrowsPerChunk);
        oldest.remove(RemovalReason.DISCARDED);
        worldArrows.remove(oldest);
        PerformanceStats.arrowsRemoved++;
      }
    }
  }

  private static void enforceWorldLimit(Set<AbstractArrow> worldArrows, String levelName) {
    if (ArrowsConfig.maxNumberOfArrowsPerWorld <= 0) {
      return;
    }

    List<AbstractArrow> stuckArrows = new ArrayList<>();
    for (AbstractArrow arrow : worldArrows) {
      if (arrow.isAlive() && isStuckArrow(arrow) && !isProtectedArrow(arrow)) {
        stuckArrows.add(arrow);
      }
    }

    int stuckCount = stuckArrows.size();
    if (stuckCount <= ArrowsConfig.maxNumberOfArrowsPerWorld) {
      return;
    }

    stuckArrows.sort(Comparator.comparingInt(Entity::getId));
    int removeCount = stuckCount - ArrowsConfig.maxNumberOfArrowsPerWorld;
    for (int index = 0; index < removeCount; index++) {
      AbstractArrow oldest = stuckArrows.get(index);
      log.debug("[World Limit] {} at {} removed in {} ({}/{})",
        BuiltInRegistries.ENTITY_TYPE.getKey(oldest.getType()),
        oldest.blockPosition(), levelName, stuckCount, ArrowsConfig.maxNumberOfArrowsPerWorld);
      oldest.remove(RemovalReason.DISCARDED);
      worldArrows.remove(oldest);
      PerformanceStats.arrowsRemoved++;
    }
  }

  private static void verifyEntities() {
    for (Map.Entry<String, Set<AbstractArrow>> entry : arrowWorldEntityMap.entrySet()) {
      entry.getValue().removeIf(arrow -> arrow == null || arrow.isRemoved() || !arrow.isAlive());
    }
    arrowWorldEntityMap.entrySet().removeIf(entry -> entry.getValue().isEmpty());
  }
}
