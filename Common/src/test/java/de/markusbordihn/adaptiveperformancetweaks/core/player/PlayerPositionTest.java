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
import org.junit.jupiter.api.DisplayName;
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
    assertEquals(24.0, playerPosition.getMovementWindowDistance(), 0.001);
    assertEquals(8.0, playerPosition.getMovementSpeed(), 0.001);
    assertTrue(playerPosition.hasRecentMovementSpeed(8.0));
    assertFalse(playerPosition.hasRecentMovementSpeed(8.1));
    assertEquals(1, playerPosition.getLastChunkX());
  }

  @Test
  @DisplayName("Vertical movement is ignored, a falling player is not treated as travelling")
  void movementWindowIgnoresVerticalMovement() {
    PlayerPosition playerPosition = new PlayerPosition(
      "Faller", UUID.randomUUID(), "minecraft:overworld", 0, 320, 0, 128);

    playerPosition.updateMovement(0.0, 320.0, 0.0, "minecraft:overworld", 20, 3, 20);
    playerPosition.updateMovement(0.0, 220.0, 0.0, "minecraft:overworld", 40, 3, 20);
    playerPosition.updateMovement(0.0, 120.0, 0.0, "minecraft:overworld", 60, 3, 20);
    playerPosition.updateMovement(0.0, 20.0, 0.0, "minecraft:overworld", 80, 3, 20);

    assertTrue(playerPosition.hasCompleteMovementWindow());
    assertEquals(0.0, playerPosition.getMovementSpeed(), 0.001);
  }

  @Test
  @DisplayName("Speed uses the elapsed ticks, so delayed samples are not read as faster travel")
  void movementSpeedUsesElapsedTicksInsteadOfTheSampleInterval() {
    PlayerPosition playerPosition = new PlayerPosition(
      "Lagging", UUID.randomUUID(), "minecraft:overworld", 0, 64, 0, 128);

    playerPosition.updateMovement(0.0, 64.0, 0.0, "minecraft:overworld", 20, 3, 20);
    playerPosition.updateMovement(8.0, 64.0, 0.0, "minecraft:overworld", 60, 3, 20);
    playerPosition.updateMovement(16.0, 64.0, 0.0, "minecraft:overworld", 100, 3, 20);
    playerPosition.updateMovement(24.0, 64.0, 0.0, "minecraft:overworld", 140, 3, 20);

    assertTrue(playerPosition.hasCompleteMovementWindow());
    assertEquals(4.0, playerPosition.getMovementSpeed(), 0.001);
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
