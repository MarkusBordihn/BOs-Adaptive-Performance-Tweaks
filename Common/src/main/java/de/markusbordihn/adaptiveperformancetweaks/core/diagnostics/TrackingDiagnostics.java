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

import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ArrowEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ExperienceOrbManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerLoginManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.VirtualPlayerManager;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;

public final class TrackingDiagnostics {

  private static final int AFFECTED_ID_LIMIT = 5;
  private static final long BYTES_PER_MEGABYTE = 1024L * 1024L;

  private TrackingDiagnostics() {
  }

  public static List<TrackedMapStatistics> collectMapStatistics() {
    List<TrackedMapStatistics> statistics = new ArrayList<>();
    statistics.addAll(CoreEntityManager.getMapStatistics());
    statistics.addAll(ItemEntityManager.getMapStatistics());
    statistics.addAll(ArrowEntityManager.getMapStatistics());
    statistics.addAll(ExperienceOrbManager.getMapStatistics());

    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    Iterable<ServerLevel> loadedLevels =
      minecraftServer != null ? minecraftServer.getAllLevels() : List.of();
    statistics.addAll(ServerLevelLoad.getMapStatistics(loadedLevels));

    int playerPositionCount = PlayerPositionManager.getPlayerPositionMap().size();
    statistics.add(TrackedMapInspector.inspectSize(
      "core.player", "playerPositionMap", playerPositionCount, playerPositionCount));
    int playerValidationCount = PlayerLoginManager.getPlayerValidationCount();
    statistics.add(TrackedMapInspector.inspectSize(
      "player_login", "playerValidationList", playerValidationCount, playerValidationCount));
    statistics.add(TrackedMapInspector.inspectSize("spawn", "virtualPlayerPositions",
      VirtualPlayerManager.getDimensionCount(), VirtualPlayerManager.getPositionCount()));
    return statistics;
  }

  public static int getEntryCount(List<TrackedMapStatistics> statistics) {
    return statistics.stream().mapToInt(TrackedMapStatistics::entryCount).sum();
  }

  public static int getStaleEntryCount(List<TrackedMapStatistics> statistics) {
    return statistics.stream().mapToInt(TrackedMapStatistics::staleEntryCount).sum();
  }

  public static int getOrphanedEntryCount(List<TrackedMapStatistics> statistics) {
    return statistics.stream().mapToInt(TrackedMapStatistics::orphanedEntryCount).sum();
  }

  public static List<String> createReportLines() {
    List<String> lines = new ArrayList<>();
    JvmMemoryStatistics memoryStatistics = JvmMemoryStatistics.capture();
    lines.add(String.format("JVM heap: used=%d MB committed=%d MB max=%d MB",
      memoryStatistics.heapUsedBytes() / BYTES_PER_MEGABYTE,
      memoryStatistics.heapCommittedBytes() / BYTES_PER_MEGABYTE,
      memoryStatistics.heapMaxBytes() / BYTES_PER_MEGABYTE));
    for (JvmMemoryStatistics.GarbageCollectorStatistics garbageCollector :
      memoryStatistics.garbageCollectors()) {
      lines.add(String.format("GC %s: collections=%d time=%d ms", garbageCollector.name(),
        garbageCollector.collectionCount(), garbageCollector.collectionTimeMillis()));
    }

    for (TrackedMapStatistics mapStatistics : collectMapStatistics()) {
      lines.add(formatMapStatistics(mapStatistics));
    }
    lines.add(String.format("Orphaned entities removed since start: %d",
      CoreEntityManager.getRemovedOrphanedEntityCount()));
    return lines;
  }

  private static String formatMapStatistics(TrackedMapStatistics mapStatistics) {
    String line = String.format("%s/%s: keys=%d entries=%d stale=%d orphaned=%d",
      mapStatistics.feature(), mapStatistics.mapName(), mapStatistics.keyCount(),
      mapStatistics.entryCount(), mapStatistics.staleEntryCount(),
      mapStatistics.orphanedEntryCount());
    if (!mapStatistics.hasAffectedEntries()) {
      return line;
    }

    String affectedIds = mapStatistics.affectedCountsById().entrySet().stream()
      .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
      .limit(AFFECTED_ID_LIMIT)
      .map(affectedEntry -> affectedEntry.getKey() + "=" + affectedEntry.getValue())
      .collect(Collectors.joining(", "));
    return line + " affected=[" + affectedIds + "]";
  }
}
