/**
 * Copyright 2021 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweaks.commands;


import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.core.config.Configurator;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;

import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;

import de.markusbordihn.adaptiveperformancetweaks.Constants;

public class CommandDebug extends CustomCommand {

  private static final CommandDebug command = new CommandDebug();

  public static ArgumentBuilder<CommandSource, ?> register() {
    return Commands.literal("debug").requires(cs -> cs.hasPermission(2))
        .then(Commands.argument("enable", BoolArgumentType.bool()).executes(command));
  }

  @Override
  public int run(CommandContext<CommandSource> context) throws CommandSyntaxException {
    final boolean enable = BoolArgumentType.getBool(context, "enable");
    if (enable) {
      sendFeedback(context,
          "\u25BA Enable debug output, please check debug.log for the full output.");
      sendFeedback(context, "Use '/aptweaks debug false' to disable debug output!");
    } else {
      sendFeedback(context, "\u25A0 Disable debug output!");
      sendFeedback(context, "Please check the debug.log for the full output.");
    }
    Configurator.setAllLevels(LogManager.getLogger(Constants.LOG_NAME).getName(),
        enable ? Level.DEBUG : Level.INFO);
    return 0;
  }
}
