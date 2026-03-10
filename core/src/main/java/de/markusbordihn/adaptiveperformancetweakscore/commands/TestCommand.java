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

package de.markusbordihn.adaptiveperformancetweakscore.commands;

import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.ChestBlock;
import net.minecraft.world.level.block.entity.ChestBlockEntity;
import net.minecraft.world.level.block.state.properties.ChestType;

public class TestCommand extends CustomCommand {

  private static final TestCommand command = new TestCommand();

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("test")
        .requires(cs -> cs.hasPermission(2))
        .then(Commands.literal("full_double_chest").executes(command::spawnFullDoubleChest));
  }

  @Override
  public int run(CommandContext<CommandSourceStack> context) throws CommandSyntaxException {
    sendFeedback(context, "Test commands: full_double_chest");
    return 0;
  }

  private int spawnFullDoubleChest(CommandContext<CommandSourceStack> context)
      throws CommandSyntaxException {
    ServerPlayer player = context.getSource().getPlayerOrException();
    ServerLevel level = (ServerLevel) player.level;

    // Place two chest blocks 2 blocks in front of the player.
    BlockPos leftPos = player.blockPosition().relative(player.getDirection(), 2);
    BlockPos rightPos = leftPos.relative(player.getDirection().getClockWise());
    level.setBlockAndUpdate(
        leftPos,
        Blocks.CHEST
            .defaultBlockState()
            .setValue(ChestBlock.FACING, player.getDirection())
            .setValue(ChestBlock.TYPE, ChestType.LEFT));
    level.setBlockAndUpdate(
        rightPos,
        Blocks.CHEST
            .defaultBlockState()
            .setValue(ChestBlock.FACING, player.getDirection())
            .setValue(ChestBlock.TYPE, ChestType.RIGHT));

    // Fill both halves with cobblestone.
    ItemStack cobblestone = new ItemStack(Items.COBBLESTONE, 64);
    if (level.getBlockEntity(leftPos) instanceof ChestBlockEntity leftChest) {
      for (int i = 0; i < leftChest.getContainerSize(); i++) {
        leftChest.setItem(i, cobblestone.copy());
      }
    }
    if (level.getBlockEntity(rightPos) instanceof ChestBlockEntity rightChest) {
      for (int i = 0; i < rightChest.getContainerSize(); i++) {
        rightChest.setItem(i, cobblestone.copy());
      }
    }

    sendFeedback(context, "Spawned full double chest at " + leftPos.toShortString());
    return 0;
  }
}
