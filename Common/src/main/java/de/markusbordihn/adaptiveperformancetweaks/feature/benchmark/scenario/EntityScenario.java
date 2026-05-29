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

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public final class EntityScenario implements BenchmarkScenario {

  private static final int ENTITY_COUNT = 96;
  private static final double ENTITY_RADIUS = 30.0d;
  private static final Identifier ENTITY_TYPE_ID = Identifier.tryParse("minecraft:cow");

  @Override
  public BenchmarkScenarioId id() {
    return BenchmarkScenarioId.ENTITIES;
  }

  @Override
  public double cleanupRadius() {
    return 36.0d;
  }

  @Override
  public Vec3 centerOffset() {
    return new Vec3(16.0d, 0.0d, 0.0d);
  }

  @Override
  public void setup(BenchmarkScenarioContext context) {
    if (ENTITY_TYPE_ID == null) {
      return;
    }

    EntityType<?> entityType = BuiltInRegistries.ENTITY_TYPE.get(ENTITY_TYPE_ID).map(ref -> ref.value()).orElse(null);
    if (entityType == null) {
      return;
    }

    Vec3 center = context.center();
    for (int index = 0; index < ENTITY_COUNT; index++) {
      double angle = (Math.PI * 2.0d * index) / ENTITY_COUNT;
      double ring = 8.0d + (index % 6) * 4.0d;
      double distance = Math.min(ring, ENTITY_RADIUS);
      double spawnX = center.x + Math.cos(angle) * distance;
      double spawnZ = center.z + Math.sin(angle) * distance;
      Entity entity = entityType.create(context.level(), EntitySpawnReason.COMMAND);
      if (entity == null) {
        continue;
      }
      entity.setPos(spawnX, center.y, spawnZ);
      context.tagEntity(entity);
      context.level().addFreshEntity(entity);
    }
  }
}
