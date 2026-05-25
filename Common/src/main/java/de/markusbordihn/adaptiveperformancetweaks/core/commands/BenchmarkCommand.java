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

import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.BenchmarkManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;

public class BenchmarkCommand extends CustomCommand {

  private static final BenchmarkCommand command = new BenchmarkCommand();
  private static final long DEFAULT_PHASE_SECONDS = 240L;

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("benchmark")
        .requires(source -> source.hasPermission(2))
        .executes(command)
        .then(
            Commands.literal("start")
                .executes(ctx -> startBenchmark(ctx, DEFAULT_PHASE_SECONDS, true))
                .then(
                    Commands.argument("seconds", IntegerArgumentType.integer(30, 3600))
                        .executes(
                            ctx ->
                                startBenchmark(
                                    ctx, IntegerArgumentType.getInteger(ctx, "seconds"), false))
                        .then(
                            Commands.literal("move")
                                .executes(
                                    ctx ->
                                        startBenchmark(
                                            ctx,
                                            IntegerArgumentType.getInteger(ctx, "seconds"),
                                            true)))))
        .then(
            Commands.literal("confirm")
                .executes(
                    ctx -> {
                      ServerPlayer player = ctx.getSource().getPlayerOrException();
                      BenchmarkManager.confirm(player);
                      return 0;
                    }))
        .then(
            Commands.literal("cancel")
                .executes(
                    ctx -> {
                      ServerPlayer player = ctx.getSource().getPlayerOrException();
                      BenchmarkManager.cancel(player);
                      return 0;
                    }))
        .then(
            Commands.literal("report")
                .executes(
                    ctx -> {
                      BenchmarkManager.BenchmarkCompareResult result =
                          BenchmarkManager.getLastResult();
                      if (result == null) {
                        sendFeedback(
                            ctx,
                            "No benchmark result available. Run /aptweaks benchmark start first.");
                      } else {
                        for (Component line : result.format()) {
                          sendFeedback(ctx, line);
                        }
                      }
                      return 0;
                    }));
  }

  private static int startBenchmark(
      CommandContext<CommandSourceStack> context, long seconds, boolean autoMove)
      throws CommandSyntaxException {
    ServerPlayer player = context.getSource().getPlayerOrException();
    BenchmarkManager.requestStart(player, seconds, autoMove);
    return 0;
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    sendFeedback(context, BenchmarkManager.getStatusMessage());
    BenchmarkManager.BenchmarkCompareResult lastResult = BenchmarkManager.getLastResult();
    if (lastResult != null && !BenchmarkManager.isRunning()) {
      sendFeedback(context, "Last result available — use /aptweaks benchmark report to view.");
    }
    return 0;
  }
}
