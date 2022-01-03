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

package de.markusbordihn.adaptiveperformancetweaksplayer.commands;

import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;

import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

import de.markusbordihn.adaptiveperformancetweakscore.commands.CustomCommand;
import de.markusbordihn.adaptiveperformancetweaksplayer.player.PlayerSimulationDistance;

public class SimulationDistanceCommand extends CustomCommand {

  private static final SimulationDistanceCommand command = new SimulationDistanceCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("setSimulationDistance").requires(cs -> cs.hasPermission(2))
        .then(Commands.argument("distance", IntegerArgumentType.integer(2, 32)).executes(command));
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    final int simulationDistance = IntegerArgumentType.getInteger(context, "distance");
    if (simulationDistance > PlayerSimulationDistance.getMaxSimulationDistance()
        || simulationDistance < PlayerSimulationDistance.getMinSimulationDistance()) {
      sendFeedback(context,
          "⚠️ Simulation distance needs to be between "
              + PlayerSimulationDistance.getMinSimulationDistance() + " and "
              + PlayerSimulationDistance.getMaxSimulationDistance() + " !");
    } else {
      sendFeedback(context, "\u25BA Try to change simulation distance from "
          + PlayerSimulationDistance.getSimulationDistance() + " to " + simulationDistance + "...");
      PlayerSimulationDistance.setSimulationDistance(simulationDistance);
    }
    return 0;
  }

}
