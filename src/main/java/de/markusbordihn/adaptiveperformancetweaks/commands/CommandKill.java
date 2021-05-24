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

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;

import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;

public class CommandKill extends CustomCommand {

  private static final CommandKill command = new CommandKill();

  public static ArgumentBuilder<CommandSource, ?> register() {
    return Commands.literal("kill").requires(cs -> cs.hasPermission(2)).executes(command)
        .then(Commands.literal("entities").executes(command::killEntities))
        .then(Commands.literal("items").executes(command::killItems));
  }

  @Override
  public int run(CommandContext<CommandSource> context) {
    sendFeedback(context,
        "kill entities: kill all entities except players\nkill items: kill items entities");
    return 0;
  }

  public int killEntities(CommandContext<CommandSource> context) {
    CommandManager.executeUserCommand("kill @e[type=!player]");
    return 0;
  }

  public int killItems(CommandContext<CommandSource> context) {
    CommandManager.executeUserCommand("kill @e[type=item]");
    return 0;
  }

}
