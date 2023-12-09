/**
 * Copyright 2022 Markus Bordihn
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
package de.markusbordihn.adaptiveperformancetweaksspawn.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.adaptiveperformancetweakscore.commands.CustomCommand;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import de.markusbordihn.adaptiveperformancetweaksspawn.spawn.SpawnerManager;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.block.entity.BlockEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SpawnerCommand extends CustomCommand {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final SpawnerCommand command = new SpawnerCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("spawner").requires(cs -> cs.hasPermission(2)).executes(command);
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    Set<BaseSpawner> spawnerList = SpawnerManager.getSpawnerList();
    if (spawnerList.isEmpty()) {
      sendFeedback(
          context,
          "Unable to find any active mob spawner. World is not loaded or nor spawner are active yet?");
    } else {
      sendFeedback(
          context,
          "Spawner Overview (please check latest.log for full output with position and meta data)\n===");
      Map<String, Integer> spawnerCounter = new HashMap<>();
      for (BaseSpawner spawner : spawnerList) {
        BlockEntity blockEntity = spawner.getSpawnerBlockEntity();
        String worldName = blockEntity.getLevel().dimension().location().toString();
        CompoundTag spawnerData = blockEntity.serializeNBT();
        String spawnerId = spawnerData.getString("id");
        String spawnEntityId =
            spawnerData.getCompound("SpawnData").getCompound("entity").getString("id");
        log.info(
            "[Mob Spawner] {}({}) at {} in {} with {}",
            spawnerId,
            spawnEntityId,
            blockEntity.getBlockPos(),
            worldName,
            spawnerData);
        spawnerCounter.put(spawnEntityId, spawnerCounter.getOrDefault(spawnEntityId, 0) + 1);
      }
      for (Map.Entry<String, Integer> spawnerEntry : spawnerCounter.entrySet()) {
        sendFeedback(
            context,
            String.format("\u221F %s x %s", spawnerEntry.getValue(), spawnerEntry.getKey()));
      }
    }
    return 0;
  }
}
