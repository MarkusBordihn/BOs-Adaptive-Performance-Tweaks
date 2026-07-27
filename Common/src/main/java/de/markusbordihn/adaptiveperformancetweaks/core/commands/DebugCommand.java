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

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import de.markusbordihn.adaptiveperformancetweaks.core.debug.DebugManager;
import de.markusbordihn.adaptiveperformancetweaks.core.debug.DebugModule;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;
import net.minecraft.server.permissions.Permissions;

public class DebugCommand extends CustomCommand {

  private DebugCommand() {
  }

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    var debugNode =
      Commands.literal("debug")
        .requires(cs -> cs.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER))
        .executes(DebugCommand::showAllStatus);

    for (DebugModule module : DebugModule.values()) {
      debugNode.then(
        Commands.literal(module.getId())
          .executes(context -> showModuleStatus(context, module))
          .then(Commands.argument("enable", BoolArgumentType.bool())
            .executes(context -> setModuleDebug(context, module))));
    }

    return debugNode;
  }

  private static int showAllStatus(CommandContext<CommandSourceStack> context) {
    CommandSourceStack source = context.getSource();
    source.sendSuccess(
      () -> Component.literal("=== APTweaks Debug Status ===").withStyle(ChatFormatting.GOLD),
      false);
    for (DebugModule module : DebugModule.values()) {
      boolean active = DebugManager.isDebugLevel(module.getLoggerName());
      ChatFormatting color = active ? ChatFormatting.GREEN : ChatFormatting.GRAY;
      String state = active ? "[ON]" : "[OFF]";
      source.sendSuccess(
        () ->
          Component.literal(state + " " + module.getId() + " - " + module.getDescription())
            .withStyle(color),
        false);
    }

    source.sendSuccess(
      () ->
        Component.literal("Use '/aptweaks debug <module> <true|false>' to toggle a module.")
          .withStyle(ChatFormatting.WHITE),
      false);

    return 0;
  }

  private static int showModuleStatus(
    CommandContext<CommandSourceStack> context, DebugModule module) {
    CommandSourceStack source = context.getSource();
    boolean active = DebugManager.isDebugLevel(module.getLoggerName());
    ChatFormatting color = active ? ChatFormatting.GREEN : ChatFormatting.GRAY;
    String state = active ? "[ON]" : "[OFF]";
    source.sendSuccess(() -> Component.literal(
        state + " " + module.getId() + " - " + module.getDescription())
      .withStyle(color), false);
    source.sendSuccess(() -> Component.literal(
        "> Use '/aptweaks debug " + module.getId() + " " + !active + "' to toggle.")
      .withStyle(ChatFormatting.WHITE), false);

    return 0;
  }

  private static int setModuleDebug(CommandContext<CommandSourceStack> context,
    DebugModule module) {
    boolean enable = BoolArgumentType.getBool(context, "enable");
    sendDebugFeedback(context, module.getId(), enable);
    DebugManager.enableDebugLevel(module.getLoggerName(), enable);

    return 0;
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    return 0;
  }
}
