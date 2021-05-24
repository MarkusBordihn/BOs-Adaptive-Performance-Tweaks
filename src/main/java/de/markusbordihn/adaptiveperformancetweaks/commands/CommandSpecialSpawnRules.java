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

import java.util.Map;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;

import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;

import de.markusbordihn.adaptiveperformancetweaks.config.SpawnConfigManager;

public class CommandSpecialSpawnRules extends CustomCommand {

  private static final CommandSpecialSpawnRules command = new CommandSpecialSpawnRules();

  public static ArgumentBuilder<CommandSource, ?> register() {
    return Commands.literal("specialSpawnRules").requires(cs -> cs.hasPermission(2))
        .executes(command);
  }

  @Override
  public int run(CommandContext<CommandSource> context) throws CommandSyntaxException {
    Map<String, Integer> spawnConfigSpecial = SpawnConfigManager.getSpawnConfigSpecial();

    if (spawnConfigSpecial.isEmpty()) {
      sendFeedback(context, "Unable to find any special spawn config!");
      return 0;
    }
    sendFeedback(context, "Special Spawn Rules, please check info.log for the full output.\n===");
    sendFeedback(context, "World|entityName|perWorld");
    for (Map.Entry<String, Integer> entry : spawnConfigSpecial.entrySet()) {
      String[] worldEntityEntry = entry.getKey().split(":");
      if (worldEntityEntry.length == 4) {
        String entityName = worldEntityEntry[2] + ":" + worldEntityEntry[3];
        String worldName = worldEntityEntry[0] + ":" + worldEntityEntry[1];
        int spawnRatePerWorld = entry.getValue();
        sendFeedback(context, String.format("%s|%s|%s", worldName, entityName, spawnRatePerWorld));
      } else {
        log.error("Invalid Special Spawn entry: {}", entry);
      }
    }
    return 0;
  }

}
