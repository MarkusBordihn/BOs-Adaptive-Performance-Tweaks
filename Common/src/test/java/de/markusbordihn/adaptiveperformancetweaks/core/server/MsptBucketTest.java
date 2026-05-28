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

package de.markusbordihn.adaptiveperformancetweaks.core.server;

import static org.junit.jupiter.api.Assertions.assertEquals;

import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class MsptBucketTest {

  @ParameterizedTest
  @CsvSource({
    "0, UNDER_5_MS, VERY_LOW",
    "5, UNDER_5_MS, VERY_LOW",
    "5.1, FROM_5_TO_10_MS, VERY_LOW",
    "10, FROM_5_TO_10_MS, VERY_LOW",
    "10.1, FROM_10_TO_VERY_LOW_MS, VERY_LOW",
    "20, FROM_10_TO_VERY_LOW_MS, VERY_LOW",
    "20.1, FROM_VERY_LOW_TO_LOW_MS, LOW",
    "40, FROM_VERY_LOW_TO_LOW_MS, LOW",
    "40.1, FROM_LOW_TO_NORMAL_MS, NORMAL",
    "46, FROM_LOW_TO_NORMAL_MS, NORMAL",
    "46.1, FROM_NORMAL_TO_MEDIUM_MS, MEDIUM",
    "49, FROM_NORMAL_TO_MEDIUM_MS, MEDIUM",
    "49.1, FROM_MEDIUM_TO_HIGH_MS, HIGH",
    "55, FROM_MEDIUM_TO_HIGH_MS, HIGH",
    "55.1, FROM_HIGH_TO_100_MS, VERY_HIGH",
    "100, FROM_100_MS_UP, VERY_HIGH"
  })
  void fromTickTimeMapsToExpectedBucketAndLoadLevel(
    double ms, MsptBucket expectedBucket, ServerLoadLevel expectedLevel) {
    MsptBucket bucket = MsptBucket.fromTickTime(ms);
    assertEquals(expectedBucket, bucket);
    assertEquals(expectedLevel, bucket.getMappedLoadLevel());
  }

  @Test
  void labelsReflectConfiguredThresholds() {
    assertEquals("<5ms", MsptBucket.UNDER_5_MS.getLabel());
    assertEquals("5-10ms", MsptBucket.FROM_5_TO_10_MS.getLabel());
    assertEquals("10-" + CoreConfig.serverLoadVeryLowThreshold + "ms",
      MsptBucket.FROM_10_TO_VERY_LOW_MS.getLabel());
    assertEquals(CoreConfig.serverLoadVeryLowThreshold + "-"
        + CoreConfig.serverLoadLowThreshold + "ms",
      MsptBucket.FROM_VERY_LOW_TO_LOW_MS.getLabel());
    assertEquals(CoreConfig.serverLoadLowThreshold + "-"
        + CoreConfig.serverLoadNormalThreshold + "ms",
      MsptBucket.FROM_LOW_TO_NORMAL_MS.getLabel());
    assertEquals(CoreConfig.serverLoadNormalThreshold + "-"
        + CoreConfig.serverLoadMediumThreshold + "ms",
      MsptBucket.FROM_NORMAL_TO_MEDIUM_MS.getLabel());
    assertEquals(CoreConfig.serverLoadMediumThreshold + "-"
        + CoreConfig.serverLoadHighThreshold + "ms",
      MsptBucket.FROM_MEDIUM_TO_HIGH_MS.getLabel());
    assertEquals(CoreConfig.serverLoadHighThreshold + "-100ms",
      MsptBucket.FROM_HIGH_TO_100_MS.getLabel());
    assertEquals("100ms+", MsptBucket.FROM_100_MS_UP.getLabel());
  }
}
