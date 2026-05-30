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
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.phys.Vec3;

public final class XpScenario implements BenchmarkScenario {

  private static final int ORB_COUNT = 96;
  private static final double ORB_RADIUS = 4.5d;
  private static final double ORB_SPACING = 0.6d;
  private static final int GRID_WIDTH = 8;

  private static double randomOffset(ThreadLocalRandom random) {
    double distance = random.nextDouble(1.0d, 2.0d);
    return random.nextBoolean() ? distance : -distance;
  }

  @Override
  public BenchmarkScenarioId id() {
    return BenchmarkScenarioId.XP;
  }

  @Override
  public double cleanupRadius() {
    return 14.0d;
  }

  @Override
  public Vec3 centerOffset() {
    return new Vec3(18.0d, 0.0d, 0.0d);
  }

  @Override
  public void setup(BenchmarkScenarioContext context) {
    Vec3 center = context.center();

    for (int index = 0; index < ORB_COUNT; index++) {
      double xOffset = ((index % GRID_WIDTH) - (GRID_WIDTH / 2.0d)) * ORB_SPACING;
      double zOffset = ((index / GRID_WIDTH) - (ORB_COUNT / GRID_WIDTH / 2.0d)) * ORB_SPACING;
      ExperienceOrb experienceOrb = new ExperienceOrb(
        context.level(),
        center.x + Mth.clamp(xOffset, -ORB_RADIUS, ORB_RADIUS),
        center.y + 1.0d,
        center.z + Mth.clamp(zOffset, -ORB_RADIUS, ORB_RADIUS),
        1);
      context.tagEntity(experienceOrb);
      context.level().addFreshEntity(experienceOrb);
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
      ExperienceOrb experienceOrb = new ExperienceOrb(
        context.level(),
        center.x + randomOffset(random),
        center.y + 1.0d,
        center.z + randomOffset(random),
        random.nextInt(1, 4));
      context.tagEntity(experienceOrb);
      context.level().addFreshEntity(experienceOrb);
    }
  }
}
