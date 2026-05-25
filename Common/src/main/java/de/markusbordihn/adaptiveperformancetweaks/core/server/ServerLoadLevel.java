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

public enum ServerLoadLevel {
  VERY_LOW,
  LOW,
  NORMAL,
  MEDIUM,
  HIGH,
  VERY_HIGH;

  public static ServerLoadLevel fromAverageTickTime(double averageTickTimeMs) {
    if (averageTickTimeMs <= CoreConfig.serverLoadVeryLowThreshold) {
      return VERY_LOW;
    } else if (averageTickTimeMs <= CoreConfig.serverLoadLowThreshold) {
      return LOW;
    } else if (averageTickTimeMs <= CoreConfig.serverLoadNormalThreshold) {
      return NORMAL;
    } else if (averageTickTimeMs <= CoreConfig.serverLoadMediumThreshold) {
      return MEDIUM;
    } else if (averageTickTimeMs <= CoreConfig.serverLoadHighThreshold) {
      return HIGH;
    }
    return VERY_HIGH;
  }

  public boolean isHigherThan(ServerLoadLevel other) {
    return this.ordinal() > other.ordinal();
  }

  public boolean isHigh() {
    return this == HIGH || this == VERY_HIGH;
  }
}
