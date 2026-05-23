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
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
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

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    PerformanceStats.Snapshot stats = PerformanceStats.snapshot();

    double tps = Math.min(20.0, 1000.0 / Math.max(1.0, ServerLoad.getAvgTickTime()));
    sendFeedback(context, String.format("=== %s Performance Stats ===", Constants.MOD_NAME));
    sendFeedback(context, String.format(
      "Server: TPS=%.1f (%.1fms)  Load=%s  Players=%d",
      tps, ServerLoad.getAvgTickTime(),
      ServerLoad.getCurrentServerLoad(),
      ServerManager.getNumberOfPlayers()));

    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server != null) {
      int totalEntities = 0;
      for (ServerLevel level : server.getAllLevels()) {
        for (var ignored : level.getAllEntities()) {
          totalEntities++;
        }
      }
      sendFeedback(context, String.format("Entities in world: %d", totalEntities));
    }

    sendFeedback(context, "--- Since server start or last reset ---");
    sendFeedback(context, String.format(
      "Spawn checks: %d total, %d denied (%.1f%%)",
      stats.mobSpawnChecks(),
      stats.mobSpawnsDenied(),
      stats.mobSpawnChecks() == 0 ? 0.0
        : 100.0 * stats.mobSpawnsDenied() / stats.mobSpawnChecks()));
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
