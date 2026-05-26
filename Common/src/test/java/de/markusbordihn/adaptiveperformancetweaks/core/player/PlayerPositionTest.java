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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;
import org.junit.jupiter.api.Test;

class PlayerPositionTest {

  @Test
  void movementWindowTracksRecentDistance() {
    PlayerPosition playerPosition = new PlayerPosition(
      "Explorer", UUID.randomUUID(), "minecraft:overworld", 0, 64, 0, 128);

    playerPosition.updateMovement(0.0, 64.0, 0.0, "minecraft:overworld", 20, 3, 20);
    playerPosition.updateMovement(8.0, 64.0, 0.0, "minecraft:overworld", 40, 3, 20);
    playerPosition.updateMovement(16.0, 64.0, 0.0, "minecraft:overworld", 60, 3, 20);
    playerPosition.updateMovement(24.0, 64.0, 0.0, "minecraft:overworld", 80, 3, 20);

    assertTrue(playerPosition.hasCompleteMovementWindow());
    assertTrue(playerPosition.hasRecentMovementDistance(24.0));
    assertEquals(24.0, playerPosition.getMovementWindowDistance(), 0.001);
    assertEquals(1, playerPosition.getLastChunkX());
  }

  @Test
  void stableTicksIncreaseWhenPlayerStopsMoving() {
    PlayerPosition playerPosition = new PlayerPosition(
      "Stable", UUID.randomUUID(), "minecraft:overworld", 0, 64, 0, 128);

    playerPosition.updateMovement(0.0, 64.0, 0.0, "minecraft:overworld", 20, 3, 20);
    playerPosition.updateMovement(0.0, 64.0, 0.0, "minecraft:overworld", 40, 3, 20);
    playerPosition.updateMovement(0.0, 64.0, 0.0, "minecraft:overworld", 60, 3, 20);

    assertTrue(playerPosition.isStableForTicks(40));
    assertFalse(playerPosition.isLoginWarmupActive(100));
    playerPosition.setLoginWarmup(60, 40);
    assertTrue(playerPosition.isLoginWarmupActive(80));
    assertFalse(playerPosition.isLoginWarmupActive(120));
  }
}
