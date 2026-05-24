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
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ExperienceOrbManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.util.Map;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;

public class StatsCommand extends CustomCommand {

  private static final StatsCommand command = new StatsCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("stats").requires(source -> source.hasPermission(2)).executes(command)
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

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    PerformanceStats.Snapshot stats = PerformanceStats.snapshot();
    MinecraftServer server = ServerManager.getMinecraftServer();

    double tps = Math.min(20.0, 1000.0 / Math.max(1.0, ServerLoad.getAvgTickTime()));
    sendFeedback(context, String.format("=== %s Performance Stats ===", Constants.MOD_NAME));
    sendFeedback(context, String.format(
      "Server: TPS=%.1f (%.1fms)  Load=%s  Uptime=%s",
      tps, ServerLoad.getAvgTickTime(),
      ServerLoad.getCurrentServerLoad(),
      formatUptime(ServerManager.getUptimeMillis())));

    Runtime runtime = Runtime.getRuntime();
    long usedMemory = runtime.totalMemory() - runtime.freeMemory();
    sendFeedback(context, String.format(
      "Memory: %s used / %s alloc / %s max",
      formatBytes(usedMemory), formatBytes(runtime.totalMemory()),
      formatBytes(runtime.maxMemory())));

    if (server != null) {
      sendFeedback(context, String.format(
        "Players: %d/%d",
        server.getPlayerList().getPlayerCount(), server.getPlayerList().getMaxPlayers()));
    }

    int mobs = CoreEntityManager.getTotalTrackedEntityCount();
    int items = ItemEntityManager.getTrackedItemEntityCount();
    int xpOrbs = ExperienceOrbManager.getTrackedExperienceOrbCount();
    sendFeedback(context, String.format("Entities: %d mobs  %d items  %d XP orbs",
      mobs, items, xpOrbs));

    Map<ServerLevel, ServerLoadLevel> levelLoads = ServerLevelLoad.getAllLevelLoads();
    if (!levelLoads.isEmpty()) {
      StringBuilder dimensions = new StringBuilder("Dimensions:");
      for (Map.Entry<ServerLevel, ServerLoadLevel> entry : levelLoads.entrySet()) {
        ServerLevel level = entry.getKey();
        dimensions.append(String.format("  %s=%.1fms (%s)",
          level.dimension().location().getPath(),
          ServerLevelLoad.getAverageTickTime(level),
          entry.getValue()));
      }
      sendFeedback(context, dimensions.toString());
    }

    sendFeedback(context, "--- Since last server start or manual reset ---");
    long spawnTotal = stats.mobSpawnChecks() + stats.mobSpawnsExcluded();
    sendFeedback(context, String.format(
      "Spawn checks: %d total, %d excluded, %d checked, %d denied (%.1f%%)",
      spawnTotal, stats.mobSpawnsExcluded(), stats.mobSpawnChecks(), stats.mobSpawnsDenied(),
      spawnTotal == 0 ? 0.0 : 100.0 * stats.mobSpawnsDenied() / spawnTotal));
    sendFeedback(context, String.format(
      "Natural spawns: %d total, %d blocked (%.1f%%)",
      stats.naturalSpawnChecks(),
      stats.naturalSpawnsDenied(),
      stats.naturalSpawnChecks() == 0 ? 0.0
        : 100.0 * stats.naturalSpawnsDenied() / stats.naturalSpawnChecks()));
    sendFeedback(context, String.format(
      "Items: %d merged, %d removed by limit", stats.itemsMerged(), stats.itemsRemoved()));
    sendFeedback(context, String.format(
      "XP orbs: %d merged, %d zero-value removed", stats.xpOrbsMerged(), stats.xpOrbsRemoved()));

    return 0;
  }
}
