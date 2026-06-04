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

package de.markusbordihn.adaptiveperformancetweaks.feature.benchmark;

import java.nio.file.Path;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.HoverEvent;
import net.minecraft.server.level.ServerPlayer;

final class BenchmarkMessenger {

  private static final int PROGRESS_BAR_WIDTH = 10;

  private BenchmarkMessenger() {
  }

  static void sendMessage(ServerPlayer player, String message) {
    player.sendSystemMessage(Component.literal(message));
  }

  static void sendMessage(ServerPlayer player, Component message) {
    player.sendSystemMessage(message);
  }

  static void sendNoteMessage(ServerPlayer player, String message) {
    sendPrefixedMessage(player, "[Note] ", message, ChatFormatting.YELLOW, ChatFormatting.GRAY);
  }

  static void sendWarningMessage(ServerPlayer player, String message) {
    sendPrefixedMessage(player, "[Warning] ", message, ChatFormatting.RED, ChatFormatting.YELLOW);
  }

  static void sendCommandMessage(ServerPlayer player, String label, String command) {
    sendMessage(player, Component.literal("[Command] ").withStyle(ChatFormatting.AQUA)
      .append(Component.literal(label + ": ").withStyle(ChatFormatting.YELLOW))
      .append(Component.literal(command)
        .withStyle(ChatFormatting.GOLD)
        .withStyle(style -> style.withClickEvent(
          new ClickEvent(ClickEvent.Action.SUGGEST_COMMAND, command)))));
  }

  static void sendStageMessage(
    ServerPlayer player, BenchmarkBlock block, String scenarioName, String message) {
    sendPrefixedMessage(player,
      '[' + block.getStatusLabel() + "] ",
      scenarioName + ' ' + message,
      block.getStageColor(),
      ChatFormatting.GRAY);
  }

  static void sendPrefixedMessage(ServerPlayer player, String prefix, String message,
    ChatFormatting prefixColor, ChatFormatting messageColor) {
    sendMessage(player, Component.literal(prefix).withStyle(prefixColor)
      .append(Component.literal(message).withStyle(messageColor)));
  }

  static void sendReportLocation(ServerPlayer player, Path resultPath) {
    if (supportsLocalReportLink(player)) {
      sendMessage(player, Component.literal("See Details: ")
        .withStyle(ChatFormatting.GOLD)
        .append(Component.literal(abbreviatePath(resultPath))
          .withStyle(ChatFormatting.AQUA)
          .withStyle(style -> style
            .withClickEvent(new ClickEvent(ClickEvent.Action.OPEN_FILE,
              resultPath.toString()))
            .withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT,
              Component.literal(resultPath.toString()))))));
      return;
    }

    sendMessage(player, Component.literal("Report Path: ").withStyle(ChatFormatting.GOLD)
      .append(Component.literal(resultPath.toString()).withStyle(ChatFormatting.AQUA)));
  }

  static ChatFormatting getStageColor(String stageLabel, BenchmarkBlock block) {
    if (stageLabel.contains("measure")) {
      return block == BenchmarkBlock.BASELINE ? ChatFormatting.AQUA : ChatFormatting.GREEN;
    }

    if ("cleanup settle".equals(stageLabel)) {
      return ChatFormatting.GOLD;
    }

    return ChatFormatting.YELLOW;
  }

  static ChatFormatting getMsptColor(double mspt) {
    if (mspt <= 20.0d) {
      return ChatFormatting.GREEN;
    }

    if (mspt <= 35.0d) {
      return ChatFormatting.YELLOW;
    }

    return ChatFormatting.RED;
  }

  static ChatFormatting getHeadroomColor(double headroom) {
    if (headroom >= 50.0d) {
      return ChatFormatting.GREEN;
    }

    if (headroom >= 20.0d) {
      return ChatFormatting.YELLOW;
    }

    return ChatFormatting.RED;
  }

  static String formatProgressBar(long elapsedMs, long totalMs) {
    if (totalMs <= 0L) {
      return "[..........]";
    }

    double progress = Math.min(1.0d, Math.max(0.0d, (double) elapsedMs / totalMs));
    int filled = (int) Math.round(progress * PROGRESS_BAR_WIDTH);
    StringBuilder builder = new StringBuilder(PROGRESS_BAR_WIDTH + 2);
    builder.append('[');
    for (int index = 0; index < PROGRESS_BAR_WIDTH; index++) {
      builder.append(index < filled ? '|' : '.');
    }
    builder.append(']');

    return builder.toString();
  }

  static String formatStageLabel(String stageLabel) {
    return switch (stageLabel) {
      case "warm-up" -> "warm-up";
      case "settle" -> "settle";
      case "measure" -> "measurement";
      case "cleanup settle" -> "cleanup settle";
      default -> stageLabel;
    };
  }

  static String formatBytes(long bytes) {
    if (bytes >= 1_073_741_824L) {
      return String.format("%.2fGB", bytes / 1_073_741_824.0d);
    }

    if (bytes >= 1_048_576L) {
      return String.format("%.0fMB", bytes / 1_048_576.0d);
    }

    return String.format("%.0fKB", bytes / 1024.0d);
  }

  static String formatDuration(long ms) {
    long secs = Math.max(0L, ms / 1000L);
    long mins = secs / 60L;
    secs %= 60L;
    if (mins > 0L) {
      return String.format("%dm %ds", mins, secs);
    }

    return String.format("%ds", secs);
  }

  private static boolean supportsLocalReportLink(ServerPlayer player) {
    return player != null && player.getServer() != null && !player.getServer().isDedicatedServer();
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
}
