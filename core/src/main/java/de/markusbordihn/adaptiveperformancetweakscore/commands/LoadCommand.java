/*
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

package de.markusbordihn.adaptiveperformancetweakscore.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerManager;
import java.util.Map;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.level.ServerLevel;

public class LoadCommand extends CustomCommand {

  private static final LoadCommand command = new LoadCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("load").requires(cs -> cs.hasPermission(2)).executes(command);
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    Map<ServerLevel, Double> serverLevelLoad = ServerLevelLoad.getLevelLoad();
    sendFeedback(context, "-= Server Load =-");
    sendFeedback(
        context, String.format("○ Average %sms", ServerManager.getAverageTickTime()));
    if (serverLevelLoad.isEmpty()) {
      sendFeedback(context, "Unable to find any levels. Is Server / Level are already loaded?");
    } else {
      for (Map.Entry<ServerLevel, Double> worldEntry : serverLevelLoad.entrySet()) {
        ServerLevel serverLevel = worldEntry.getKey();
        String levelName = serverLevel.getLevel().dimension().location().toString();
        Double avgTickTime = worldEntry.getValue();
        sendFeedback(context, String.format("○ %s %sms", levelName, avgTickTime));
      }
    }
    return 0;
  }
}
