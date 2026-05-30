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
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnConfig;
import java.util.Map;
import java.util.Set;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.server.permissions.Permissions;
import net.minecraft.world.entity.Entity;

public class EntityCommand extends CustomCommand {

  private static final EntityCommand command = new EntityCommand();
  private static final String NO_ENTITIES_TEXT =
    "Unable to find any entities. Is the server / world loaded?";

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("entities")
      .requires(cs -> cs.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER))
      .executes(command)
      .then(Commands.literal("cleanup_per_chunk").executes(command::cleanupPerChunk))
      .then(Commands.literal("overview").executes(command::overview))
      .then(Commands.literal("overview_per_chunk").executes(command::overviewPerChunk))
      .then(Commands.literal("overview_per_level").executes(command::overviewPerLevel));
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) {
    sendFeedback(
      context,
      """
        Usage:
        /aptweaks entities cleanup_per_chunk - trim overloaded mob-farm chunks
        /aptweaks entities overview - entities by type (global)
        /aptweaks entities overview_per_chunk - entities per chunk
        /aptweaks entities overview_per_level - entities per level""");
    return 0;
  }

  public int cleanupPerChunk(CommandContext<CommandSourceStack> context) {
    var result = CoreEntityManager.cleanupChunkMobFarms(SpawnConfig.entityChunkCleanupPerTypeLimit);
    sendFeedback(context, String.format(
      "Chunk mob cleanup removed %d entities across %d chunks and %d entity types.",
      result.removedEntities(),
      result.affectedChunks(),
      result.affectedEntityTypes()));
    return result.removedEntities();
  }

  public int overview(CommandContext<CommandSourceStack> context) {
    Map<String, Set<Entity>> entities = CoreEntityManager.getEntitiesGlobal();
    if (entities.isEmpty()) {
      sendFeedback(context, NO_ENTITIES_TEXT);
      return 0;
    }

    sendFeedback(context, String.format("Entity overview (%s types)\n===", entities.size()));
    for (Map.Entry<String, Set<Entity>> entry : entities.entrySet()) {
      sendFeedback(context, String.format("%s x %s", entry.getValue().size(), entry.getKey()));
    }

    return 0;
  }

  public int overviewPerChunk(CommandContext<CommandSourceStack> context) {
    Map<String, Set<Entity>> entities = CoreEntityManager.getEntitiesPerChunk();
    if (entities.isEmpty()) {
      sendFeedback(context, NO_ENTITIES_TEXT);
      return 0;
    }

    sendFeedback(context, String.format("Entities per chunk (%s chunks)\n===", entities.size()));
    for (Map.Entry<String, Set<Entity>> entry : entities.entrySet()) {
      sendFeedback(context, String.format("%s x %s", entry.getKey(), entry.getValue().size()));
    }

    return 0;
  }

  public int overviewPerLevel(CommandContext<CommandSourceStack> context) {
    Map<String, Set<Entity>> entities = CoreEntityManager.getEntities();
    if (entities.isEmpty()) {
      sendFeedback(context, NO_ENTITIES_TEXT);
      return 0;
    }

    sendFeedback(context, String.format("Entities per level (%s entries)\n===", entities.size()));
    for (Map.Entry<String, Set<Entity>> entry : entities.entrySet()) {
      sendFeedback(context, String.format("%s x %s", entry.getValue().size(), entry.getKey()));
    }

    return 0;
  }
}
