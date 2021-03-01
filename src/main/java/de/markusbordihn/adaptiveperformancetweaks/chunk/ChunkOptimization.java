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
import java.util.Set;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.world.chunk.AbstractChunkProvider;
import net.minecraft.world.chunk.Chunk;

import de.markusbordihn.adaptiveperformancetweaks.Optimization;

public class ChunkOptimization extends Optimization {

  private static Integer maxNumberOfChunks = 1000;
  private static Set<Chunk> chunkList = new LinkedHashSet<>();

  public static void addChunk(Chunk chunk) {
    log.trace("Add chunk {}", chunk);
    chunkList.add(chunk);
  }

  public static void removeChunk(Chunk chunk) {
    log.trace("Remove chunk {}", chunk);
    chunkList.remove(chunk);
  }

  public static void cleanupChunks() {
    // Not working ...
    if (chunkList.isEmpty()) {
      return;
    }
    Integer currentNumberOfChunks = chunkList.size();
    for (Chunk chunk : chunkList) {
      if (currentNumberOfChunks > maxNumberOfChunks) {
        AbstractChunkProvider chunkProvider = chunk.getWorld().getChunkProvider();
        ChunkPos chunkPosition = chunk.getPos();
        if (chunkProvider.isChunkLoaded(chunkPosition)) {
          log.info("Cleanup chunk {} at {}", chunk, chunkPosition);
          chunkProvider.forceChunk(chunkPosition, false);
          currentNumberOfChunks--;
        }
      }
    }
  }

}
