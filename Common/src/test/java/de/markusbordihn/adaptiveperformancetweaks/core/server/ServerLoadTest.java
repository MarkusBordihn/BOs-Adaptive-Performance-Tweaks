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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class ServerLoadTest {

  @AfterEach
  void cleanup() {
    CoreConfig.logServerLoad = true;
    CoreConfig.serverLoadLogIntervalSeconds = 60;
    CoreConfig.serverLoadLogSignificantChangeSteps = 2;
  }

  @Test
  void oneStepChangeDoesNotLogBeforeInterval() {
    assertFalse(ServerLoad.shouldLogServerLoadChange(
      ServerLoadLevel.LOW, ServerLoadLevel.VERY_LOW, 5_000L, 0L));
  }

  @Test
  void oneStepChangeLogsAfterInterval() {
    assertTrue(ServerLoad.shouldLogServerLoadChange(
      ServerLoadLevel.LOW, ServerLoadLevel.VERY_LOW, 61_000L, 0L));
  }

  @Test
  void significantChangeLogsImmediately() {
    assertTrue(ServerLoad.shouldLogServerLoadChange(
      ServerLoadLevel.VERY_HIGH, ServerLoadLevel.VERY_LOW, 5_000L, 0L));
  }

  @Test
  void disabledLoggingSuppressesChanges() {
    CoreConfig.logServerLoad = false;

    assertFalse(ServerLoad.shouldLogServerLoadChange(
      ServerLoadLevel.VERY_HIGH, ServerLoadLevel.VERY_LOW, 61_000L, 0L));
  }

}
