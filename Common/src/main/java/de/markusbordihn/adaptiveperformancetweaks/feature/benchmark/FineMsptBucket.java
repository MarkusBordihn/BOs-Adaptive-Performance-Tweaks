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

package de.markusbordihn.adaptiveperformancetweaks.feature.benchmark;

public enum FineMsptBucket {
  UNDER_3_MS("<3ms"),
  FROM_3_TO_5_MS("3-5ms"),
  FROM_5_TO_10_MS("5-10ms"),
  FROM_10_MS_UP("10ms+");

  private final String label;

  FineMsptBucket(String label) {
    this.label = label;
  }

  public static FineMsptBucket fromTickTime(double tickTimeMs) {
    if (tickTimeMs < 3.0d) {
      return UNDER_3_MS;
    }
    if (tickTimeMs < 5.0d) {
      return FROM_3_TO_5_MS;
    }
    if (tickTimeMs < 10.0d) {
      return FROM_5_TO_10_MS;
    }
    return FROM_10_MS_UP;
  }

  public String getLabel() {
    return this.label;
  }
}
