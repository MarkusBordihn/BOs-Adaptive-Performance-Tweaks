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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;

import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.MobSpawnerTileEntity;

import de.markusbordihn.adaptiveperformancetweaks.spawn.SpawnerManager;


public class CommandSpawner extends CustomCommand {

  private static final CommandSpawner command = new CommandSpawner();

  public static ArgumentBuilder<CommandSource, ?> register() {
    return Commands.literal("spawner").requires(cs -> cs.hasPermissionLevel(2)).executes(command);
  }

  @Override
  public int run(CommandContext<CommandSource> context) throws CommandSyntaxException {
    Set<MobSpawnerTileEntity> spawnerList = SpawnerManager.getSpawnerList();
    if (spawnerList.isEmpty()) {
      sendFeedback(context,
          "Unable to find any active mob spawner. World is not loaded or nor mob spawner loaded yet?");
    } else {
      sendFeedback(context,
          "Spawner Overview (please check latest.log for full output with positions)\n===");
      Map<String, Integer> spawnerCounter = new HashMap<>();
      for (MobSpawnerTileEntity spawner : spawnerList) {
        String worldName = spawner.getWorld().getDimensionKey().getLocation().toString();
        CompoundNBT spawnerData = spawner.serializeNBT();
        String spawnEntityId = spawnerData.getCompound("SpawnData").getString("id");
        log.info("[Mob Spawner] {} at {} in {} with {}", spawnEntityId, spawner.getPos(),
            worldName, spawnerData);
        spawnerCounter.put(spawnEntityId, spawnerCounter.getOrDefault(spawnEntityId, 0) + 1);
      }
      for (Map.Entry<String, Integer> spawnerEntry : spawnerCounter.entrySet()) {
        sendFeedback(context,
            String.format("\u221F %s x %s", spawnerEntry.getValue(), spawnerEntry.getKey()));
      }
    }
    return 0;
  }
}
