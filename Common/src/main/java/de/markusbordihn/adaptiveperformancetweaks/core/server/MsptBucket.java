/*
 * Copyright 2024 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweaks.core.server;

import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;

public enum MsptBucket {
  UNDER_5_MS(ServerLoadLevel.VERY_LOW),
  FROM_5_TO_10_MS(ServerLoadLevel.VERY_LOW),
  FROM_10_TO_VERY_LOW_MS(ServerLoadLevel.VERY_LOW),
  FROM_VERY_LOW_TO_LOW_MS(ServerLoadLevel.LOW),
  FROM_LOW_TO_NORMAL_MS(ServerLoadLevel.NORMAL),
  FROM_NORMAL_TO_MEDIUM_MS(ServerLoadLevel.MEDIUM),
  FROM_MEDIUM_TO_HIGH_MS(ServerLoadLevel.HIGH),
  FROM_HIGH_TO_100_MS(ServerLoadLevel.VERY_HIGH),
  FROM_100_MS_UP(ServerLoadLevel.VERY_HIGH);

  private final ServerLoadLevel mappedLoadLevel;

  MsptBucket(ServerLoadLevel mappedLoadLevel) {
    this.mappedLoadLevel = mappedLoadLevel;
  }

  public static MsptBucket fromTickTime(double tickTimeMs) {
    if (tickTimeMs <= 5.0) {
      return UNDER_5_MS;
    }
    if (tickTimeMs <= 10.0) {
      return FROM_5_TO_10_MS;
    }
    if (tickTimeMs <= CoreConfig.serverLoadVeryLowThreshold) {
      return FROM_10_TO_VERY_LOW_MS;
    }
    if (tickTimeMs <= CoreConfig.serverLoadLowThreshold) {
      return FROM_VERY_LOW_TO_LOW_MS;
    }
    if (tickTimeMs <= CoreConfig.serverLoadNormalThreshold) {
      return FROM_LOW_TO_NORMAL_MS;
    }
    if (tickTimeMs <= CoreConfig.serverLoadMediumThreshold) {
      return FROM_NORMAL_TO_MEDIUM_MS;
    }
    if (tickTimeMs <= CoreConfig.serverLoadHighThreshold) {
      return FROM_MEDIUM_TO_HIGH_MS;
    }
    if (tickTimeMs < 100.0) {
      return FROM_HIGH_TO_100_MS;
    }
    return FROM_100_MS_UP;
  }

  private static String formatRangeLabel(int minInclusive, int maxExclusive) {
    return minInclusive + "-" + maxExclusive + "ms";
  }

  public String getLabel() {
    return switch (this) {
      case UNDER_5_MS -> "<5ms";
      case FROM_5_TO_10_MS -> "5-10ms";
      case FROM_10_TO_VERY_LOW_MS -> formatRangeLabel(10, CoreConfig.serverLoadVeryLowThreshold);
      case FROM_VERY_LOW_TO_LOW_MS ->
        formatRangeLabel(CoreConfig.serverLoadVeryLowThreshold, CoreConfig.serverLoadLowThreshold);
      case FROM_LOW_TO_NORMAL_MS ->
        formatRangeLabel(CoreConfig.serverLoadLowThreshold, CoreConfig.serverLoadNormalThreshold);
      case FROM_NORMAL_TO_MEDIUM_MS -> formatRangeLabel(CoreConfig.serverLoadNormalThreshold,
        CoreConfig.serverLoadMediumThreshold);
      case FROM_MEDIUM_TO_HIGH_MS ->
        formatRangeLabel(CoreConfig.serverLoadMediumThreshold, CoreConfig.serverLoadHighThreshold);
      case FROM_HIGH_TO_100_MS -> formatRangeLabel(CoreConfig.serverLoadHighThreshold, 100);
      case FROM_100_MS_UP -> "100ms+";
    };
  }

  public ServerLoadLevel getMappedLoadLevel() {
    return this.mappedLoadLevel;
  }
}
