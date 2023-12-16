/*
 * Copyright 2021 Markus Bordihn
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
package de.markusbordihn.adaptiveperformancetweakscore.commands;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.context.CommandContext;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;

public abstract class CustomCommand implements Command<CommandSourceStack> {

  protected CustomCommand() {}

  public static void sendFeedback(CommandContext<CommandSourceStack> context, String feedback) {
    CommandSourceStack commandSource = context.getSource();
    commandSource.sendSuccess(Component.literal(feedback), false);
  }

  public static void sendDebugFeedback(
      CommandContext<CommandSourceStack> context, String module, boolean enabled) {
    CommandSourceStack commandSource = context.getSource();
    String commandName = module.toLowerCase();
    if (enabled) {
      commandSource.sendSuccess(
          Component.literal(
                  "► Enable debug for the "
                      + module
                      + " module, please check debug.log for the full output.")
              .withStyle(ChatFormatting.GREEN),
          false);
      commandSource.sendSuccess(
          Component.literal(
                  "> Use '/aptweaks debug " + commandName + " false' to disable the debug!")
              .withStyle(ChatFormatting.WHITE),
          false);
    } else {
      commandSource.sendSuccess(
          Component.literal("■ Disable debug for the " + commandName + " module!")
              .withStyle(ChatFormatting.RED),
          false);
      commandSource.sendSuccess(
          Component.literal("> Please check the latest.log and/or debug.log for the full output.")
              .withStyle(ChatFormatting.WHITE),
          false);
    }
  }
}
