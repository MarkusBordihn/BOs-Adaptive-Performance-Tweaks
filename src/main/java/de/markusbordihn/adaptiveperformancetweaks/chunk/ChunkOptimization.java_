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

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.world.chunk.AbstractChunkProvider;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import de.markusbordihn.adaptiveperformancetweaks.Optimization;
import de.markusbordihn.adaptiveperformancetweaks.server.OptimizationEvent;

@EventBusSubscriber
public class ChunkOptimization extends Optimization {

  private static Set<Chunk> chunkList = new LinkedHashSet<>();
  private static boolean needsOptimization = false;

  @SubscribeEvent
  public static void handleOptimizationEvent(OptimizationEvent event) {
    if (needsOptimization) {
      cleanupChunks();
      needsOptimization = false;
    }
  }

  public static boolean isNeedsOptimization() {
    return needsOptimization;
  }

  public static void setNeedsOptimization(boolean needsOptimization) {
    ChunkOptimization.needsOptimization = needsOptimization;
  }

  public static void cleanupChunks() {
    if (chunkList.isEmpty()) {
      return;
    }
    Integer currentNumberOfChunks = chunkList.size();
    log.debug("Cleanup chunks because {} exceeds limit of {} ...", currentNumberOfChunks,
        ChunkManager.maxNumberOfChunks);
    Iterator<Chunk> chunkIterator = chunkList.iterator();
    while (chunkIterator.hasNext()) {
      Chunk chunk = chunkIterator.next();
      if (currentNumberOfChunks > ChunkManager.maxNumberOfChunks) {
        AbstractChunkProvider chunkProvider = chunk.getLevel().getChunkSource();
        ChunkPos chunkPosition = chunk.getPos();
        if (chunkProvider.isEntityTickingChunk(chunkPosition)) {
          log.debug("Cleanup chunk {} at {} with status {}", chunk, chunkPosition,
              chunk.getStatus());
          chunkProvider.updateChunkForced(chunkPosition, false);
          currentNumberOfChunks--;
        }
      }
    }
  }

}
