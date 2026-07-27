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

import net.minecraft.world.phys.Vec3;

public interface BenchmarkScenario {

  BenchmarkScenarioId id();

  default String displayName() {
    return this.id().getDisplayName();
  }

  default boolean usesAutoMove(boolean requestedAutoMove) {
    return requestedAutoMove && this.id().supportsAutoMove();
  }

  default double cleanupRadius() {
    return 0.0d;
  }

  default Vec3 centerOffset() {
    return Vec3.ZERO;
  }

  default boolean shouldFacePlayerToFocus() {
    return false;
  }

  default Vec3 playerFocusOffset() {
    return Vec3.ZERO;
  }

  default void setup(BenchmarkScenarioContext context) {
  }

  default void beforeMeasurement(BenchmarkScenarioContext context) {
  }

  default void onMeasurementTick(BenchmarkScenarioContext context) {
  }

  default void cleanup(BenchmarkScenarioContext context) {
  }
}
