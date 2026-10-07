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

package de.markusbordihn.adaptiveperformancetweaks.core.diagnostics;

import de.markusbordihn.adaptiveperformancetweaks.core.entity.OrphanedEntityDetector;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;

public final class TrackedMapInspector {

  private TrackedMapInspector() {
  }

  public static TrackedMapStatistics inspectEntityCollections(
    String feature, String mapName, Map<?, ? extends Collection<? extends Entity>> map) {
    AffectedEntryTally tally = new AffectedEntryTally();
    for (Collection<? extends Entity> entities : map.values()) {
      for (Entity entity : entities) {
        tally.addEntity(entity);
      }
    }

    return tally.toStatistics(feature, mapName, map.size());
  }

  public static TrackedMapStatistics inspectEntityKeys(
    String feature, String mapName, Map<? extends Entity, ?> map) {
    AffectedEntryTally tally = new AffectedEntryTally();
    for (Entity entity : map.keySet()) {
      tally.addEntity(entity);
    }

    return tally.toStatistics(feature, mapName, map.size());
  }

  public static TrackedMapStatistics inspectLevelKeys(
    String feature, String mapName, Map<ServerLevel, ?> map, Iterable<ServerLevel> loadedLevels) {
    Set<ServerLevel> loadedLevelSet = Collections.newSetFromMap(new IdentityHashMap<>());
    for (ServerLevel serverLevel : loadedLevels) {
      loadedLevelSet.add(serverLevel);
    }

    AffectedEntryTally tally = new AffectedEntryTally();
    for (ServerLevel serverLevel : map.keySet()) {
      tally.addLevel(serverLevel, loadedLevelSet.contains(serverLevel));
    }

    return tally.toStatistics(feature, mapName, map.size());
  }

  public static TrackedMapStatistics inspectSize(
    String feature, String mapName, int keyCount, int entryCount) {
    return new TrackedMapStatistics(feature, mapName, keyCount, entryCount, 0, 0, Map.of());
  }

  private static final class AffectedEntryTally {

    private final Map<String, Integer> affectedCountsById = new HashMap<>();
    private int entryCount = 0;
    private int staleEntryCount = 0;
    private int orphanedEntryCount = 0;

    private void addEntity(Entity entity) {
      this.entryCount++;
      if (entity.isRemoved()) {
        this.staleEntryCount++;
        this.affectedCountsById.merge(getEntityTypeId(entity), 1, Integer::sum);
      } else if (OrphanedEntityDetector.isOrphaned(entity)) {
        this.orphanedEntryCount++;
        this.affectedCountsById.merge(getEntityTypeId(entity), 1, Integer::sum);
      }
    }

    private void addLevel(ServerLevel serverLevel, boolean loaded) {
      this.entryCount++;
      if (!loaded) {
        this.staleEntryCount++;
        this.affectedCountsById.merge(
          serverLevel.dimension().location().toString(), 1, Integer::sum);
      }
    }

    private TrackedMapStatistics toStatistics(String feature, String mapName, int keyCount) {
      return new TrackedMapStatistics(feature, mapName, keyCount, this.entryCount,
        this.staleEntryCount, this.orphanedEntryCount, Map.copyOf(this.affectedCountsById));
    }

    private static String getEntityTypeId(Entity entity) {
      return BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType()).toString();
    }
  }
}
