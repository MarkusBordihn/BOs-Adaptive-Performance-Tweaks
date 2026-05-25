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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class ServerLoadLevelTest {

  @ParameterizedTest
  @CsvSource({
    "0,    VERY_LOW",
    "19.9, VERY_LOW",
    "20,   VERY_LOW",
    "20.1, LOW",
    "39.9, LOW",
    "40,   LOW",
    "40.1, NORMAL",
    "45.9, NORMAL",
    "46,   NORMAL",
    "46.1, MEDIUM",
    "48.9, MEDIUM",
    "49,   MEDIUM",
    "49.1, HIGH",
    "54.9, HIGH",
    "55,   HIGH",
    "55.1, VERY_HIGH",
    "100,  VERY_HIGH"
  })
  void fromAverageTickTimeClassification(double ms, ServerLoadLevel expected) {
    assertEquals(expected, ServerLoadLevel.fromAverageTickTime(ms));
  }

  @Test
  void isHighReturnsTrueForHighAndVeryHigh() {
    assertTrue(ServerLoadLevel.HIGH.isHigh());
    assertTrue(ServerLoadLevel.VERY_HIGH.isHigh());
  }

  @Test
  void isHighReturnsFalseForLowerLevels() {
    assertFalse(ServerLoadLevel.VERY_LOW.isHigh());
    assertFalse(ServerLoadLevel.LOW.isHigh());
    assertFalse(ServerLoadLevel.NORMAL.isHigh());
    assertFalse(ServerLoadLevel.MEDIUM.isHigh());
  }

  @Test
  void isHigherThanFollowsOrdinalOrder() {
    assertTrue(ServerLoadLevel.VERY_HIGH.isHigherThan(ServerLoadLevel.HIGH));
    assertTrue(ServerLoadLevel.HIGH.isHigherThan(ServerLoadLevel.MEDIUM));
    assertTrue(ServerLoadLevel.MEDIUM.isHigherThan(ServerLoadLevel.NORMAL));
    assertTrue(ServerLoadLevel.NORMAL.isHigherThan(ServerLoadLevel.LOW));
    assertTrue(ServerLoadLevel.LOW.isHigherThan(ServerLoadLevel.VERY_LOW));
  }

  @Test
  void isHigherThanReturnsFalseForSameLevelAndLower() {
    assertFalse(ServerLoadLevel.NORMAL.isHigherThan(ServerLoadLevel.NORMAL));
    assertFalse(ServerLoadLevel.LOW.isHigherThan(ServerLoadLevel.HIGH));
  }
}
