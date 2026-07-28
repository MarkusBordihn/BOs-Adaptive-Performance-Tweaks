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

package de.markusbordihn.adaptiveperformancetweaks.neoforge.mixin;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ChunkGenThrottleMixinTest {

  @Test
  @DisplayName("Only the server chunk cache tick path is throttled, not every distance update")
  void targetsServerChunkCacheTickOnly() throws IOException {
    String source = Files.readString(Path.of(
      "src/main/java/de/markusbordihn/adaptiveperformancetweaks/neoforge/mixin/ChunkGenThrottleMixin.java"));
    assertTrue(source.contains("@Mixin(ServerChunkCache.class)"));
    assertTrue(source.contains("method = \"tick(Ljava/util/function/BooleanSupplier;Z)V\""));
    assertTrue(source.contains(
      "target = \"Lnet/minecraft/server/level/ServerChunkCache;runDistanceManagerUpdates()Z\""));
    assertTrue(source.contains("getThrottleDivisor(this.level)"));
    assertFalse(source.contains("@Mixin(ChunkMap.class)"));
    assertFalse(source.contains("@Inject(method = \"runDistanceManagerUpdates\""));
  }
}
