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

package de.markusbordihn.adaptiveperformancetweaks.core.player;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Field;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PlayerPositionManagerTest {

  private static Object readStaticField(String fieldName) throws Exception {
    Field field = PlayerPositionManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    return field.get(null);
  }

  @Test
  @DisplayName("The configured movement tracking survives the reset on server start")
  void resetKeepsConfiguredMovementTracking() throws Exception {
    try {
      PlayerPositionManager.configureMovementTracking(10, 3);
      PlayerPositionManager.reset();

      assertEquals(10, PlayerPositionManager.getMovementUpdateTick());
      assertEquals(3, readStaticField("playerMovementWindowSamples"));
      assertEquals(10, readStaticField("nextMovementUpdateTick"));
    } finally {
      PlayerPositionManager.configureMovementTracking(20, 5);
      PlayerPositionManager.reset();
    }
  }
}
