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

import java.util.Locale;

public enum BenchmarkScenarioId {
  GENERAL(3, true),
  EXPLORATION(2, false),
  ITEMS,
  XP("XP"),
  ENTITIES,
  RECOVERY;

  private final int suiteWeight;
  private final boolean supportsAutoMove;
  private final String displayNameOverride;

  BenchmarkScenarioId() {
    this(1, false, null);
  }

  BenchmarkScenarioId(String displayNameOverride) {
    this(1, false, displayNameOverride);
  }

  BenchmarkScenarioId(int suiteWeight, boolean supportsAutoMove) {
    this(suiteWeight, supportsAutoMove, null);
  }

  BenchmarkScenarioId(int suiteWeight, boolean supportsAutoMove, String displayNameOverride) {
    this.suiteWeight = suiteWeight;
    this.supportsAutoMove = supportsAutoMove;
    this.displayNameOverride = displayNameOverride;
  }

  public String getId() {
    return name().toLowerCase(Locale.ROOT);
  }

  public String getDisplayName() {
    if (displayNameOverride != null) {
      return displayNameOverride;
    }
    String n = name();
    return n.charAt(0) + n.substring(1).toLowerCase(Locale.ROOT);
  }

  public int getSuiteWeight() {
    return this.suiteWeight;
  }

  public boolean supportsAutoMove() {
    return this.supportsAutoMove;
  }
}
