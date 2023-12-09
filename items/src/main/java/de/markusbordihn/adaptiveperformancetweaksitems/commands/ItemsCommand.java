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

package de.markusbordihn.adaptiveperformancetweaksitems.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.adaptiveperformancetweakscore.commands.CustomCommand;
import de.markusbordihn.adaptiveperformancetweaksitems.entity.ItemEntityManager;
import java.util.Map;
import java.util.Set;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.world.entity.item.ItemEntity;

public class ItemsCommand extends CustomCommand {

  private static final ItemsCommand command = new ItemsCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("items")
        .requires(cs -> cs.hasPermission(2))
        .executes(command)
        .then(Commands.literal("optimize").executes(command::runOptimize));
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    Map<String, Set<ItemEntity>> itemTypeEntityMap = ItemEntityManager.getItemTypeEntityMap();
    if (itemTypeEntityMap.isEmpty()) {
      sendFeedback(
          context, "Unable to find any items entity. World is not loaded or nor items dropped?");
    } else {
      sendFeedback(
          context, String.format("Items Overview (%s types)\n===", itemTypeEntityMap.size()));
      for (Map.Entry<String, Set<ItemEntity>> itemEntities : itemTypeEntityMap.entrySet()) {
        int numOfItems = itemEntities.getValue().size();
        if (numOfItems > 0) {
          sendFeedback(context, String.format("○ %s %s", itemEntities.getKey(), numOfItems));
        }
      }
    }
    return 0;
  }

  public int runOptimize(CommandContext<CommandSourceStack> context) {
    sendFeedback(context, "Running manual Item Optimization ...");
    int numberOfRemovedItems = ItemEntityManager.optimizeItems();
    if (numberOfRemovedItems > 0) {
      sendFeedback(
          context, String.format("Removed items %s from all worlds!", numberOfRemovedItems));
    } else {
      sendFeedback(context, "Optimization was not needed!");
    }
    return 0;
  }
}
