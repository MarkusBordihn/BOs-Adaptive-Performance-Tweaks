/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweaksspawn.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.adaptiveperformancetweakscore.commands.CustomCommand;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import de.markusbordihn.adaptiveperformancetweaksspawn.spawn.SpawnConfigManager;
import java.util.Set;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.registries.ForgeRegistries;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SpawnRulesCommand extends CustomCommand {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final SpawnRulesCommand command = new SpawnRulesCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("spawnrules").requires(cs -> cs.hasPermission(2)).executes(command);
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    Set<ResourceLocation> entitiesKeys = ForgeRegistries.ENTITY_TYPES.getKeys();
    if (entitiesKeys.isEmpty()) {
      sendFeedback(context, "Unable to find any entities. Server / World is not loaded?");
      return 0;
    }
    sendFeedback(context, "Spawn Rules, please check info.log for the full output.\n===");
    sendFeedback(context, "Entity Name|perPlayer|perWorld");
    for (ResourceLocation entityKey : entitiesKeys) {
      String entityName = entityKey.toString();
      if (entityName != null && SpawnConfigManager.hasSpawnLimit(entityName)) {
        int spawnRatePerPlayer = SpawnConfigManager.getSpawnLimitPerPlayer(entityName);
        int spawnRatePerWorld = SpawnConfigManager.getSpawnLimitPerWorld(entityName);
        sendFeedback(
            context, String.format("%s|%s|%s", entityName, spawnRatePerPlayer, spawnRatePerWorld));
      }
    }
    return 0;
  }
}
