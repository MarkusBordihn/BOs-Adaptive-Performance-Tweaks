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

package de.markusbordihn.adaptiveperformancetweaks.feature.spawn;

import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.util.List;
import java.util.Set;

public record SpawnPreset(
  boolean replace,
  String modId,
  List<String> requiredMods,
  int priority,
  DimensionFilter dimensions,
  EntityLimits entities,
  LoadFactors loadFactors,
  boolean excludeFromTracking,
  String notes) {

  public record DimensionFilter(List<String> allow, List<String> deny, List<String> ignore) {

  }

  public record EntityLimits(
    Set<String> allowList,
    Set<String> denyList,
    int perPlayerMax,
    int perWorldMax,
    int perServerMax,
    int perChunkMax) {

  }

  public record LoadFactors(
    double veryLow,
    double low,
    double normal,
    double medium,
    double high,
    double veryHigh) {

    public static LoadFactors defaults() {
      return new LoadFactors(1.0, 1.0, 0.9, 0.7, 0.4, 0.1);
    }

    public double forLevel(ServerLoadLevel level) {
      return switch (level) {
        case VERY_LOW -> this.veryLow;
        case LOW -> this.low;
        case NORMAL -> this.normal;
        case MEDIUM -> this.medium;
        case HIGH -> this.high;
        case VERY_HIGH -> this.veryHigh;
      };
    }
  }

}
