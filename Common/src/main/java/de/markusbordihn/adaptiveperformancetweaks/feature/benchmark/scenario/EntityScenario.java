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

package de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario;

import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnConfig;
import java.util.concurrent.ThreadLocalRandom;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.levelgen.Heightmap;
import net.minecraft.world.phys.Vec3;

public final class EntityScenario implements BenchmarkScenario {

  private static final int OVERLOADED_CHUNK_ENTITY_COUNT = 80;
  private static final int SUPPORT_CHUNK_ENTITY_COUNT = 35;
  private static final double CHUNK_ANCHOR_OFFSET = 4.0d;
  private static final double CHUNK_ANCHOR_JITTER = 2.4d;
  private static final double SPAWN_CLEARANCE_Y = 3.0d;
  private static final double SPAWN_Y_VARIATION = 0.6d;
  private static final ResourceLocation ENTITY_TYPE_ID =
    ResourceLocation.tryParse("minecraft:chicken");

  private static Vec3 getChunkCenter(Vec3 position) {
    return new Vec3((Mth.floor(position.x) >> 4 << 4) + 8.0d, position.y,
      (Mth.floor(position.z) >> 4 << 4) + 8.0d);
  }

  private static Vec3[] getChunkAnchors(Vec3 chunkCenter) {
    Vec3 resolvedChunkCenter = getChunkCenter(chunkCenter);
    return new Vec3[]{
      resolvedChunkCenter.add(-CHUNK_ANCHOR_OFFSET, 0.0d, -CHUNK_ANCHOR_OFFSET),
      resolvedChunkCenter.add(CHUNK_ANCHOR_OFFSET, 0.0d, -CHUNK_ANCHOR_OFFSET),
      resolvedChunkCenter.add(-CHUNK_ANCHOR_OFFSET, 0.0d, CHUNK_ANCHOR_OFFSET),
      resolvedChunkCenter.add(CHUNK_ANCHOR_OFFSET, 0.0d, CHUNK_ANCHOR_OFFSET)
    };
  }

  private static void spawnChunk(BenchmarkScenarioContext context, EntityType<?> entityType,
    Vec3 chunkCenter, int amount) {
    ThreadLocalRandom random = ThreadLocalRandom.current();
    Vec3[] anchors = getChunkAnchors(chunkCenter);
    for (int index = 0; index < amount; index++) {
      Entity entity = entityType.create(context.level());
      if (entity == null) {
        continue;
      }

      Vec3 anchor = anchors[random.nextInt(anchors.length)];
      BlockPos surfacePos = context.level()
        .getHeightmapPos(Heightmap.Types.MOTION_BLOCKING_NO_LEAVES,
          BlockPos.containing(anchor.x, 0.0d, anchor.z));
      entity.moveTo(
        anchor.x + random.nextDouble(-CHUNK_ANCHOR_JITTER, CHUNK_ANCHOR_JITTER),
        surfacePos.getY() + SPAWN_CLEARANCE_Y + random.nextDouble(0.0d, SPAWN_Y_VARIATION),
        anchor.z + random.nextDouble(-CHUNK_ANCHOR_JITTER, CHUNK_ANCHOR_JITTER),
        random.nextFloat() * 360.0f,
        0.0f);
      context.tagEntity(entity);
      context.level().addFreshEntity(entity);
    }
  }

  @Override
  public BenchmarkScenarioId id() {
    return BenchmarkScenarioId.ENTITIES;
  }

  @Override
  public double cleanupRadius() {
    return 64.0d;
  }

  @Override
  public Vec3 centerOffset() {
    return new Vec3(16.0d, 0.0d, 0.0d);
  }

  @Override
  public boolean shouldFacePlayerToFocus() {
    return true;
  }

  @Override
  public Vec3 playerFocusOffset() {
    return new Vec3(8.0d, 0.0d, 8.0d);
  }

  @Override
  public void setup(BenchmarkScenarioContext context) {
    if (ENTITY_TYPE_ID == null) {
      return;
    }

    EntityType<?> entityType = BuiltInRegistries.ENTITY_TYPE.get(ENTITY_TYPE_ID);
    if (entityType == null) {
      return;
    }

    Vec3 center = context.center();
    spawnChunk(context, entityType, center, OVERLOADED_CHUNK_ENTITY_COUNT);
    spawnChunk(context, entityType, center.add(16.0d, 0.0d, 0.0d), OVERLOADED_CHUNK_ENTITY_COUNT);
    spawnChunk(context, entityType, center.add(0.0d, 0.0d, 16.0d), OVERLOADED_CHUNK_ENTITY_COUNT);
    spawnChunk(context, entityType, center.add(16.0d, 0.0d, 16.0d), SUPPORT_CHUNK_ENTITY_COUNT);
  }

  @Override
  public void onMeasurementTick(BenchmarkScenarioContext context) {
    ThreadLocalRandom random = ThreadLocalRandom.current();
    if (random.nextInt(4) != 0 || ENTITY_TYPE_ID == null) {
      return;
    }

    EntityType<?> entityType = BuiltInRegistries.ENTITY_TYPE.get(ENTITY_TYPE_ID);
    if (entityType == null) {
      return;
    }

    Vec3 center = context.center();
    spawnChunk(context, entityType, center, random.nextInt(1, 3));
    if (random.nextBoolean()) {
      spawnChunk(context, entityType, center.add(16.0d, 0.0d, 0.0d), random.nextInt(1, 3));
    }
    if (random.nextBoolean()) {
      spawnChunk(context, entityType, center.add(0.0d, 0.0d, 16.0d), random.nextInt(1, 3));
    }
    if (random.nextInt(3) == 0) {
      spawnChunk(context, entityType, center.add(16.0d, 0.0d, 16.0d), 1);
    }

    if (context.activeBlock()) {
      CoreEntityManager.cleanupChunkMobFarms(
        SpawnConfig.entityChunkCleanupPerTypeLimit,
        entity -> entity.getTags().contains(context.scenarioTag()));
    }
  }
}
