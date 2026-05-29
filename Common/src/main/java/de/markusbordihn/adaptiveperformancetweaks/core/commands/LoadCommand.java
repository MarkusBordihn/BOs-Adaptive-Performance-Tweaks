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
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.permissions.Permissions;
import net.minecraft.server.level.ServerLevel;

public class LoadCommand extends CustomCommand {

  private static final LoadCommand command = new LoadCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("load").requires(cs -> cs.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER)).executes(command);
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    ServerLoadLevel currentLoad = ServerLoad.getCurrentServerLoad();
    double avgTickTime = ServerLoad.getAvgTickTime();
    StringBuilder message =
      new StringBuilder(String.format("Server Load: %s (avg. %.1fms)", currentLoad, avgTickTime));

    for (ServerLevel serverLevel : ServerManager.getAllLevels()) {
      if (!ServerLevelLoad.hasMeasuredLoad(serverLevel)) {
        continue;
      }

      message.append(
        String.format(
          "%n%s: %s (avg. %.1fms)",
          serverLevel.dimension().identifier(),
          ServerLevelLoad.getLevelLoad(serverLevel),
          ServerLevelLoad.getAverageTickTime(serverLevel)));
    }

    sendFeedback(context, message.toString());

    return 0;
  }
}
