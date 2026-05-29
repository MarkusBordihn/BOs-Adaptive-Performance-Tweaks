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
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioId;
import java.nio.file.Path;
import net.minecraft.ChatFormatting;
import net.minecraft.util.Util;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.permissions.Permissions;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.HoverEvent;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.server.level.ServerPlayer;

public class BenchmarkCommand extends CustomCommand {

  private static final BenchmarkCommand command = new BenchmarkCommand();
  private static final long DEFAULT_PHASE_SECONDS = 240L;

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("benchmark")
      .requires(cs -> cs.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER))
      .executes(command)
      .then(Commands.literal("start")
        .executes(ctx -> startBenchmark(ctx, DEFAULT_PHASE_SECONDS, true))
        .then(Commands.argument("seconds", IntegerArgumentType.integer(30, 3600))
          .executes(ctx -> startBenchmark(ctx,
            IntegerArgumentType.getInteger(ctx, "seconds"), false))
          .then(Commands.literal("move")
            .executes(ctx -> startBenchmark(ctx,
              IntegerArgumentType.getInteger(ctx, "seconds"), true))))
        .then(Commands.literal("scenario")
          .then(registerScenarioStart(BenchmarkScenarioId.GENERAL, true))
          .then(registerScenarioStart(BenchmarkScenarioId.ITEMS, false))
          .then(registerScenarioStart(BenchmarkScenarioId.XP, false))
          .then(registerScenarioStart(BenchmarkScenarioId.ENTITIES, false))
          .then(registerScenarioStart(BenchmarkScenarioId.RECOVERY, false))))
      .then(Commands.literal("confirm")
        .executes(ctx -> {
          ServerPlayer player = ctx.getSource().getPlayerOrException();
          BenchmarkManager.confirm(player);
          return 0;
        }))
      .then(Commands.literal("cancel")
        .executes(ctx -> {
          ServerPlayer player = ctx.getSource().getPlayerOrException();
          BenchmarkManager.cancel(player);
          return 0;
        }))
      .then(Commands.literal("openresult")
        .executes(BenchmarkCommand::openLastResult));
  }

  private static int startBenchmark(
    CommandContext<CommandSourceStack> context, long seconds, boolean autoMove)
    throws CommandSyntaxException {
    ServerPlayer player = context.getSource().getPlayerOrException();
    BenchmarkManager.requestStart(player, seconds, autoMove);
    return 0;
  }

  private static ArgumentBuilder<CommandSourceStack, ?> registerScenarioStart(
    BenchmarkScenarioId scenarioId, boolean defaultAutoMove) {
    return Commands.literal(scenarioId.getId())
      .executes(
        ctx -> startScenarioBenchmark(ctx, scenarioId, DEFAULT_PHASE_SECONDS, defaultAutoMove))
      .then(Commands.argument("seconds", IntegerArgumentType.integer(30, 3600))
        .executes(ctx -> startScenarioBenchmark(ctx, scenarioId,
          IntegerArgumentType.getInteger(ctx, "seconds"), defaultAutoMove))
        .then(Commands.literal("move")
          .executes(ctx -> startScenarioBenchmark(ctx, scenarioId,
            IntegerArgumentType.getInteger(ctx, "seconds"), true))));
  }

  private static int startScenarioBenchmark(CommandContext<CommandSourceStack> context,
    BenchmarkScenarioId scenarioId, long seconds, boolean autoMove)
    throws CommandSyntaxException {
    ServerPlayer player = context.getSource().getPlayerOrException();
    BenchmarkManager.requestScenarioStart(player, scenarioId, seconds, autoMove);
    return 0;
  }

  private static String abbreviatePath(Path path) {
    String full = path.toString();
    if (full.length() <= 60) {
      return full;
    }
    int nameCount = path.getNameCount();
    String sep = java.io.File.separator;
    String parent = nameCount >= 2 ? path.getName(nameCount - 2) + sep : "";
    return "..." + sep + parent + path.getFileName();
  }

  private static MutableComponent buildLastResultLink(Path resultPath) {
    return Component.literal("Last result: ")
      .append(Component.literal(abbreviatePath(resultPath))
        .withStyle(ChatFormatting.AQUA)
        .withStyle(style -> style
          .withClickEvent(new ClickEvent.RunCommand("/aptweaks benchmark openresult"))
          .withHoverEvent(new HoverEvent.ShowText(Component.literal(resultPath.toString())))));
  }

  private static int openLastResult(CommandContext<CommandSourceStack> context) {
    Path resultPath = BenchmarkManager.getLastResultPath();
    if (resultPath == null) {
      sendFeedback(context,
        Component.literal("No benchmark result available.").withStyle(ChatFormatting.RED));
      return 0;
    }
    if (!resultPath.toFile().exists()) {
      sendFeedback(context,
        Component.literal("Benchmark result file not found: " + resultPath)
          .withStyle(ChatFormatting.RED));
      return 0;
    }
    Util.getPlatform().openFile(resultPath.toFile());
    return 0;
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    sendFeedback(context, BenchmarkManager.getStatusMessage());
    if (BenchmarkManager.getLastResult() != null && !BenchmarkManager.isRunning()) {
      Path resultPath = BenchmarkManager.getLastResultPath();
      if (resultPath != null) {
        sendFeedback(context, buildLastResultLink(resultPath));
      }
    }
    return 0;
  }
}
