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

import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;

import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.world.server.ServerWorld;

import de.markusbordihn.adaptiveperformancetweaks.world.WorldViewManager;

public class CommandViewDistance extends CustomCommand {

  private static final CommandViewDistance command = new CommandViewDistance();

  public static ArgumentBuilder<CommandSource, ?> register() {
    return Commands.literal("setViewDistance").requires(cs -> cs.hasPermission(2))
        .then(Commands.argument("distance", IntegerArgumentType.integer(2, 32)).executes(command));
  }

  @Override
  public int run(CommandContext<CommandSource> context) throws CommandSyntaxException {
    final int viewDistance = IntegerArgumentType.getInteger(context, "distance");
    ServerWorld serverLevel = context.getSource().getLevel();
    sendFeedback(context, "\u25BA Try to change view distance for " + serverLevel.toString()
        + " to " + viewDistance + "...");
    WorldViewManager.setViewDistance(serverLevel, viewDistance);
    return 0;
  }
}
