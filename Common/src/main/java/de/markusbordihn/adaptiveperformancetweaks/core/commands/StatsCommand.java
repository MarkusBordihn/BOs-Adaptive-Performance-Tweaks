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

package de.markusbordihn.adaptiveperformancetweaks.core.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.MsptBucket;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ArrowEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ArrowsConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ExperienceOrbManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.util.List;
import java.util.Map;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;

public class StatsCommand extends CustomCommand {

  private static final StatsCommand command = new StatsCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("stats").requires(source -> source.hasPermission(2)).executes(command)
      .then(Commands.literal("items").executes(context -> {
        showItemDetails(context);
        return 0;
      }))
      .then(Commands.literal("xp_orbs").executes(context -> {
        showXpOrbDetails(context);
        return 0;
      }))
      .then(Commands.literal("arrows").executes(context -> {
        showArrowDetails(context);
        return 0;
      }))
      .then(Commands.literal("reset").executes(context -> {
        PerformanceStats.reset();
        sendFeedback(context, "Performance stats have been reset.");
        return 0;
      }));
  }

  private static String formatUptime(long millis) {
    long totalSeconds = millis / 1000;
    long hours = totalSeconds / 3600;
    long minutes = (totalSeconds % 3600) / 60;
    if (hours > 0) {
      return String.format("%dh %dm", hours, minutes);
    }
    return String.format("%dm %ds", minutes, totalSeconds % 60);
  }

  private static String formatBytes(long bytes) {
    if (bytes >= 1_073_741_824L) {
      return String.format("%.1f GB", bytes / 1_073_741_824.0);
    }
    return String.format("%.0f MB", bytes / 1_048_576.0);
  }

  private static long getCategoryCount(PerformanceStats.Snapshot stats, TrackingCategory category) {
    return stats.trackingExcludedByCategory().getOrDefault(category, 0L);
  }

  private static void showItemDetails(CommandContext<CommandSourceStack> context) {
    PerformanceStats.Snapshot stats = PerformanceStats.snapshot();
    sendFeedback(context, "=== Item Stats (by type) ===");
    Map<String, Map<String, Integer>> byDimension = ItemEntityManager.getItemEntityCountsByDimension();
    if (byDimension.isEmpty()) {
      sendFeedback(context, "No items currently tracked.");
    } else {
      for (Map.Entry<String, Map<String, Integer>> dimEntry : byDimension.entrySet()) {
        sendFeedback(context, "Dimension: " + dimEntry.getKey());
        List<Map.Entry<String, Integer>> sorted = dimEntry.getValue().entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .limit(15)
          .toList();
        for (Map.Entry<String, Integer> entry : sorted) {
          sendFeedback(context, String.format("  %-45s %d", entry.getKey(), entry.getValue()));
        }
      }
    }
    sendFeedback(context, String.format(
      "Total tracked: %d  |  merged: %d  |  removed: %d",
      ItemEntityManager.getTrackedItemEntityCount(), stats.itemsMerged(), stats.itemsRemoved()));
  }

  private static void showArrowDetails(CommandContext<CommandSourceStack> context) {
    PerformanceStats.Snapshot stats = PerformanceStats.snapshot();
    sendFeedback(context, "=== Arrow Stats (by type) ===");
    Map<String, Map<String, Integer>> byDimension = ArrowEntityManager.getArrowCountsByDimension();
    if (byDimension.isEmpty()) {
      sendFeedback(context, "No arrows currently tracked.");
    } else {
      for (Map.Entry<String, Map<String, Integer>> dimEntry : byDimension.entrySet()) {
        sendFeedback(context, "Dimension: " + dimEntry.getKey());
        List<Map.Entry<String, Integer>> sorted = dimEntry.getValue().entrySet().stream()
          .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
          .toList();
        for (Map.Entry<String, Integer> entry : sorted) {
          sendFeedback(context, String.format("  %-45s %d", entry.getKey(), entry.getValue()));
        }
      }
    }
    sendFeedback(context, String.format(
      "Total tracked: %d  |  removed: %d  |  Limits: %d/world  %d/chunk",
      ArrowEntityManager.getTrackedArrowCount(), stats.arrowsRemoved(),
      ArrowsConfig.maxNumberOfArrowsPerWorld, ArrowsConfig.maxNumberOfArrowsPerChunk));
  }

  private static void showXpOrbDetails(CommandContext<CommandSourceStack> context) {
    PerformanceStats.Snapshot stats = PerformanceStats.snapshot();
    sendFeedback(context, "=== XP Orb Stats ===");
    Map<String, Integer> byDimension = ExperienceOrbManager.getTrackedExperienceOrbCountsByDimension();
    if (byDimension.isEmpty()) {
      sendFeedback(context, "No XP orbs currently tracked.");
    } else {
      for (Map.Entry<String, Integer> entry : byDimension.entrySet()) {
        sendFeedback(context, String.format("  %-40s %d orbs", entry.getKey(), entry.getValue()));
      }
    }
    sendFeedback(context, String.format(
      "Total tracked: %d  |  merged: %d  |  removed: %d",
      ExperienceOrbManager.getTrackedExperienceOrbCount(), stats.xpOrbsMerged(),
      stats.xpOrbsRemoved()));
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    PerformanceStats.Snapshot stats = PerformanceStats.snapshot();
    MinecraftServer server = ServerManager.getMinecraftServer();

    double avgTickTime = ServerLoad.getAvgTickTime();
    double tps = Math.min(20.0, 1000.0 / Math.max(1.0, avgTickTime));
    MsptBucket msptBucket = MsptBucket.fromTickTime(avgTickTime);
    sendFeedback(context, String.format("=== %s Performance Stats ===", Constants.MOD_NAME));
    sendFeedback(context, String.format(
      "Server: TPS=%.1f (%.1fms)  Bucket=%s  Load=%s  Uptime=%s",
      tps, avgTickTime,
      msptBucket.getLabel(),
      ServerLoad.getCurrentServerLoad(),
      formatUptime(ServerManager.getUptimeMillis())));

    Runtime runtime = Runtime.getRuntime();
    long usedMemory = runtime.totalMemory() - runtime.freeMemory();
    sendFeedback(
      context,
      String.format(
        "Memory: %s used / %s alloc / %s max",
        formatBytes(usedMemory),
        formatBytes(runtime.totalMemory()),
        formatBytes(runtime.maxMemory())));

    if (server != null) {
      sendFeedback(
        context,
        String.format(
          "Players: %d/%d",
          server.getPlayerList().getPlayerCount(), server.getPlayerList().getMaxPlayers()));
    }
    if (FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled() && server != null) {
      int currentSimulationDistance = SimulationDistanceManager.getCurrentSimulationDistance();
      int baselineDistance = SimulationDistanceManager.getCurrentLoadBaselineDistance();
      int fallbackSimulationDistance = server.getPlayerList().getSimulationDistance();
      sendFeedback(context, String.format(
        "Simulation Distance: baseline=%d  current=%d  movement=%s  explorers=%d  reduction=%d",
        baselineDistance > 0 ? baselineDistance : fallbackSimulationDistance,
        currentSimulationDistance > 0 ? currentSimulationDistance : fallbackSimulationDistance,
        SimulationDistanceManager.isMovementThrottleActive() ? "active" : "idle",
        SimulationDistanceManager.getActiveExplorerCount(),
        SimulationDistanceManager.getCurrentMovementReduction()));
    }

    int mobs = CoreEntityManager.getTotalTrackedEntityCount();
    sendFeedback(context, String.format("Mobs: %d tracked", mobs));

    Map<ServerLevel, ServerLoadLevel> levelLoads = ServerLevelLoad.getAllLevelLoads();
    if (!levelLoads.isEmpty()) {
      StringBuilder dimensions = new StringBuilder("Dimensions:");
      for (Map.Entry<ServerLevel, ServerLoadLevel> entry : levelLoads.entrySet()) {
        ServerLevel level = entry.getKey();
        dimensions.append(
          String.format(
            "  %s=%.1fms (%s)",
            level.dimension().location().getPath(),
            ServerLevelLoad.getAverageTickTime(level),
            entry.getValue()));
      }
      sendFeedback(context, dimensions.toString());
    }

    sendFeedback(context, String.format(
      "Items:   %5d tracked  | %5d merged  | %5d removed  (use 'stats items' for details)",
      ItemEntityManager.getTrackedItemEntityCount(), stats.itemsMerged(), stats.itemsRemoved()));
    sendFeedback(context, String.format(
      "XP Orbs: %5d tracked  | %5d merged  | %5d removed  (use 'stats xp_orbs' for details)",
      ExperienceOrbManager.getTrackedExperienceOrbCount(), stats.xpOrbsMerged(),
      stats.xpOrbsRemoved()));
    if (FeatureToggle.ARROWS.isEnabled()) {
      sendFeedback(context, String.format(
        "Arrows:  %5d tracked  |              | %5d removed  (use 'stats arrows' for details)",
        ArrowEntityManager.getTrackedArrowCount(), stats.arrowsRemoved()));
    }

    sendFeedback(context, "--- Since last server start or manual reset ---");
    long spawnTotal = stats.mobSpawnChecks() + stats.mobSpawnsExcluded();
    sendFeedback(context, String.format(
      "Spawn checks: %d total, %d excluded, %d checked, %d denied (%.1f%%)",
      spawnTotal, stats.mobSpawnsExcluded(), stats.mobSpawnChecks(), stats.mobSpawnsDenied(),
      spawnTotal == 0 ? 0.0 : 100.0 * stats.mobSpawnsDenied() / spawnTotal));
    sendFeedback(context, String.format(
      "Special spawn bonuses: %d applied",
      stats.specialSpawnBonusesApplied()));
    sendFeedback(context, String.format(
      "Natural spawns: %d total, %d blocked (%.1f%%)",
      stats.naturalSpawnChecks(),
      stats.naturalSpawnsDenied(),
      stats.naturalSpawnChecks() == 0 ? 0.0
        : 100.0 * stats.naturalSpawnsDenied() / stats.naturalSpawnChecks()));
    long manualExcluded = stats.trackingExcludedManualNamespace()
      + stats.trackingExcludedManualEntity();
    long autoExcluded = stats.trackingExcludedAutoNamespace()
      + stats.trackingExcludedAutoEntity();
    sendFeedback(context, String.format(
      "Tracking: %d evaluated, %d tracked, %d protected-living, %d protected-persistent",
      stats.trackingEvaluations(), stats.trackingTracked(),
      stats.trackingProtectedLiving(), stats.trackingProtectedPersistent()));
    sendFeedback(context, String.format(
      "Tracking excludes: manual=%d (ns=%d entity=%d) auto=%d (ns=%d entity=%d) cache=%d",
      manualExcluded,
      stats.trackingExcludedManualNamespace(), stats.trackingExcludedManualEntity(),
      autoExcluded,
      stats.trackingExcludedAutoNamespace(), stats.trackingExcludedAutoEntity(),
      stats.trackingExcludedEarlyCache()));
    sendFeedback(context, String.format(
      "Tracking categories: technical=%d vehicle structures=%d world effects=%d managed living=%d",
      getCategoryCount(stats, TrackingCategory.TECHNICAL),
      getCategoryCount(stats, TrackingCategory.VEHICLE_STRUCTURE),
      getCategoryCount(stats, TrackingCategory.WORLD_EFFECT),
      getCategoryCount(stats, TrackingCategory.MANAGED_LIVING)));
    sendFeedback(context, String.format(
      "Tracking categories: storage networks=%d manual override=%d unknown=%d",
      getCategoryCount(stats, TrackingCategory.STORAGE_NETWORK),
      getCategoryCount(stats, TrackingCategory.MANUAL_OVERRIDE),
      getCategoryCount(stats, TrackingCategory.UNKNOWN)));
    if (FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled()) {
      sendFeedback(context, String.format(
        "Simulation Distance: %d changes, %d movement lowers, %d active samples, max reduction=%d",
        stats.simulationDistanceChanges(),
        stats.simulationDistanceMovementAdjustments(),
        stats.simulationDistanceMovementThrottleSamples(),
        stats.simulationDistanceMovementMaxReduction()));
    }

    return 0;
  }
}
