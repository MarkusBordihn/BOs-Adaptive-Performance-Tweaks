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

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import org.apache.logging.log4j.Logger;

import net.minecraft.tileentity.MobSpawnerTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.event.world.ChunkEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.spawn.SpawnerManager;

@EventBusSubscriber
public class ChunkManager extends Manager {

  private static Set<Chunk> chunkList = new LinkedHashSet<>();
  public static Integer maxNumberOfChunks = 1000;
  private static final Logger log = getLogger(ChunkManager.class.getSimpleName());

  @SubscribeEvent
  public static void onServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    chunkList = new LinkedHashSet<>();
  }

  @SubscribeEvent
  public static void handleChunkLoadEvent(ChunkEvent.Load event) {
    if (event.getWorld().isClientSide()) {
      return;
    }
    Chunk chunk = (Chunk) event.getChunk();

    // Check every block if the chunk is not known yet.
    if (!hasChunk(chunk)) {
      addChunk(chunk);
      Map<BlockPos, TileEntity> tileEntityMap = chunk.getBlockEntities();
      for (Map.Entry<BlockPos, TileEntity> tileEntityEntry : tileEntityMap.entrySet()) {
        TileEntity tileEntity = tileEntityEntry.getValue();
        if (tileEntity instanceof MobSpawnerTileEntity) {
          SpawnerManager.addSpawner((MobSpawnerTileEntity) tileEntity);
        }
      }
    }
  }

  @SubscribeEvent
  public static void handleChunkUnloadEvent(ChunkEvent.Unload event) {
    if (event.getWorld().isClientSide()) {
      return;
    }
    Chunk chunk = (Chunk) event.getChunk();

    // Skip additional checks if the chunk is unknown.
    if (hasChunk(chunk)) {
      removeChunk(chunk);
      Map<BlockPos, TileEntity> tileEntityMap = chunk.getBlockEntities();
      for (Map.Entry<BlockPos, TileEntity> tileEntityEntry : tileEntityMap.entrySet()) {
        TileEntity tileEntity = tileEntityEntry.getValue();
        if (tileEntity instanceof MobSpawnerTileEntity) {
          SpawnerManager.removeSpawner((MobSpawnerTileEntity) tileEntity);
        }
      }
    }
  }

  public static void addChunk(Chunk chunk) {
    log.trace("Add chunk {} at {}", chunk, chunk.getPos());
    chunkList.add(chunk);
    if (chunkList.size() > maxNumberOfChunks && log.isTraceEnabled()) {
      log.trace("Should optimize chunks because {} exceeding limit of {}...", chunkList.size(),
          maxNumberOfChunks);
    }
  }

  public static void removeChunk(Chunk chunk) {
    log.trace("Remove chunk {} at {}", chunk, chunk.getPos());
    chunkList.remove(chunk);
  }

  public static boolean hasChunk(Chunk chunk) {
    return chunkList.contains(chunk);
  }
}
