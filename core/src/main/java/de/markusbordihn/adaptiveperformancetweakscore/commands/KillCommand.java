/**
 * Copyright 2021 Markus Bordihn
 *
 * <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * <p>The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * <p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.adaptiveperformancetweakscore.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class KillCommand extends CustomCommand {

  private static final KillCommand command = new KillCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("kill")
        .requires(cs -> cs.hasPermission(2))
        .executes(command::run)
        .then(Commands.literal("entities").executes(command::killEntities))
        .then(Commands.literal("items").executes(command::killItems));
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    sendFeedback(
        context,
        "kill entities: kill all entities except players\nkill items: kill items entities");
    return 0;
  }

  public int killEntities(CommandContext<CommandSourceStack> context) {
    CommandManager.executeUserCommand("kill @e[type=!player,distance=0..]");
    return 0;
  }

  public int killItems(CommandContext<CommandSourceStack> context) {
    CommandManager.executeUserCommand("kill @e[type=item,distance=0..]");
    return 0;
  }
}
