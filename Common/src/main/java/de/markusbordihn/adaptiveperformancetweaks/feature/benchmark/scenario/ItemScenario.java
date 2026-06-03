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

import java.util.concurrent.ThreadLocalRandom;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.phys.Vec3;

public final class ItemScenario implements BenchmarkScenario {

  private static final int ITEM_COUNT = 216;
  private static final double ITEM_RADIUS = 5.0d;
  private static final double ITEM_SPACING = 0.45d;
  private static final int GRID_WIDTH = 12;

  private static double randomOffset(ThreadLocalRandom random) {
    double distance = random.nextDouble(1.0d, 2.0d);
    return random.nextBoolean() ? distance : -distance;
  }

  @Override
  public BenchmarkScenarioId id() {
    return BenchmarkScenarioId.ITEMS;
  }

  @Override
  public double cleanupRadius() {
    return 16.0d;
  }

  @Override
  public Vec3 centerOffset() {
    return new Vec3(12.0d, 0.0d, 0.0d);
  }

  @Override
  public void setup(BenchmarkScenarioContext context) {
    Vec3 center = context.center();

    for (int index = 0; index < ITEM_COUNT; index++) {
      double xOffset = ((index % GRID_WIDTH) - (GRID_WIDTH / 2.0d)) * ITEM_SPACING;
      double zOffset = ((index / GRID_WIDTH) - (ITEM_COUNT / GRID_WIDTH / 2.0d)) * ITEM_SPACING;
      ItemEntity itemEntity = new ItemEntity(
        context.level(),
        center.x + Mth.clamp(xOffset, -ITEM_RADIUS, ITEM_RADIUS),
        center.y + 1.0d,
        center.z + Mth.clamp(zOffset, -ITEM_RADIUS, ITEM_RADIUS),
        new ItemStack(Items.COBBLESTONE));
      context.tagEntity(itemEntity);
      context.level().addFreshEntity(itemEntity);
    }
  }

  @Override
  public void onMeasurementTick(BenchmarkScenarioContext context) {
    ThreadLocalRandom random = ThreadLocalRandom.current();
    if (random.nextInt(5) != 0) {
      return;
    }

    Vec3 center = context.center();
    int burstSize = random.nextInt(3, 7);
    for (int index = 0; index < burstSize; index++) {
      ItemEntity itemEntity = new ItemEntity(
        context.level(),
        center.x + randomOffset(random),
        center.y + 1.0d,
        center.z + randomOffset(random),
        new ItemStack(Items.COBBLESTONE, random.nextInt(1, 5)));
      context.tagEntity(itemEntity);
      context.level().addFreshEntity(itemEntity);
    }
  }
}
