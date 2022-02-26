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

package de.markusbordihn.adaptiveperformancetweakscore.commands;

import java.util.Map;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;

import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPositionManager;

public class PlayerPositionCommand extends CustomCommand {

  private static final PlayerPositionCommand command = new PlayerPositionCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("playerPositions").requires(cs -> cs.hasPermission(2))
        .executes(command);
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    Map<String, PlayerPosition> playerPositionMap = PlayerPositionManager.getPlayerPositionMap();
    if (playerPositionMap.isEmpty()) {
      sendFeedback(context, "Unable to find any player position!?");
    } else {
      sendFeedback(context,
          String.format("Player Positions (%s online)\n===", playerPositionMap.size()));
      for (Map.Entry<String, PlayerPosition> player : playerPositionMap.entrySet()) {
        PlayerPosition playerPosition = player.getValue();
        sendFeedback(context,
            String.format("\u221F %s %s", playerPosition.getPlayerName(), playerPosition));
      }
    }
    return 0;
  }
}
