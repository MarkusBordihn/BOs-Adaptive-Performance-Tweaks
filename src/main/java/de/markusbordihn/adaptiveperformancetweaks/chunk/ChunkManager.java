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

package de.markusbordihn.adaptiveperformancetweaks.chunk;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.minecraft.tileentity.MobSpawnerTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.event.world.ChunkEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweaks.Manager;

@EventBusSubscriber
public class ChunkManager extends Manager {

  private static Set<MobSpawnerTileEntity> mobSpawnerList = new HashSet<>();

  @SubscribeEvent
  public static void handleChunkLoadEvent(ChunkEvent.Load event) {
    Chunk chunk = (Chunk) event.getChunk();
    ChunkOptimization.addChunk(chunk);
    Map<BlockPos, TileEntity> tileEntityMap = chunk.getTileEntityMap();
    for (Map.Entry<BlockPos, TileEntity> tileEntityEntry : tileEntityMap.entrySet()) {
      TileEntity tileEntity = tileEntityEntry.getValue();
      if (tileEntity instanceof MobSpawnerTileEntity) {
        mobSpawnerList.add((MobSpawnerTileEntity) tileEntity);
      }
    }
  }

  @SubscribeEvent
  public static void handleChunkUnloadEvent(ChunkEvent.Unload event) {
    Chunk chunk = (Chunk) event.getChunk();
    ChunkOptimization.removeChunk(chunk);
    Map<BlockPos, TileEntity> tileEntityMap = chunk.getTileEntityMap();
    for (Map.Entry<BlockPos, TileEntity> tileEntityEntry : tileEntityMap.entrySet()) {
      TileEntity tileEntity = tileEntityEntry.getValue();
      if (tileEntity instanceof MobSpawnerTileEntity) {
        mobSpawnerList.remove((MobSpawnerTileEntity) tileEntity);
      }
    }
  }

  public static Set<MobSpawnerTileEntity> getMobSpawner() {
    return mobSpawnerList;
  }
}
