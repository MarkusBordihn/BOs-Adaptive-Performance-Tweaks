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

package de.markusbordihn.adaptiveperformancetweaks.forge.mixin;

import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnManager;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.level.NaturalSpawner;
import net.minecraft.world.level.StructureManager;
import net.minecraft.world.level.biome.MobSpawnSettings;
import net.minecraft.world.level.chunk.ChunkGenerator;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(NaturalSpawner.class)
public class NaturalSpawnerEntityMixin {

  @Shadow
  private static boolean isValidSpawnPostitionForType(
      ServerLevel serverLevel,
      MobCategory mobCategory,
      StructureManager structureManager,
      ChunkGenerator chunkGenerator,
      MobSpawnSettings.SpawnerData spawnerData,
      BlockPos.MutableBlockPos mutableBlockPos,
      double d) {
    return false;
  }

  @Redirect(
      method =
          "spawnCategoryForPosition(Lnet/minecraft/world/entity/MobCategory;Lnet/minecraft/server/level/ServerLevel;Lnet/minecraft/world/level/chunk/ChunkAccess;Lnet/minecraft/core/BlockPos;Lnet/minecraft/world/level/NaturalSpawner$SpawnPredicate;Lnet/minecraft/world/level/NaturalSpawner$AfterSpawnCallback;)V",
      at =
          @At(
              value = "INVOKE",
              target =
                  "Lnet/minecraft/world/level/NaturalSpawner;isValidSpawnPostitionForType(Lnet/minecraft/server/level/ServerLevel;Lnet/minecraft/world/entity/MobCategory;Lnet/minecraft/world/level/StructureManager;Lnet/minecraft/world/level/chunk/ChunkGenerator;Lnet/minecraft/world/level/biome/MobSpawnSettings$SpawnerData;Lnet/minecraft/core/BlockPos$MutableBlockPos;D)Z"))
  private static boolean aptweaks_preCheckBeforeEntityCreate(
      ServerLevel serverLevel,
      MobCategory mobCategory,
      StructureManager structureManager,
      ChunkGenerator chunkGenerator,
      MobSpawnSettings.SpawnerData spawnerData,
      BlockPos.MutableBlockPos mutableBlockPos,
      double distance) {
    if (!isValidSpawnPostitionForType(
        serverLevel,
        mobCategory,
        structureManager,
        chunkGenerator,
        spawnerData,
        mutableBlockPos,
        distance)) {
      return false;
    }
    return !SpawnManager.shouldDenyMobSpawnAt(
        spawnerData.type, serverLevel, mutableBlockPos, MobSpawnType.NATURAL);
  }
}
