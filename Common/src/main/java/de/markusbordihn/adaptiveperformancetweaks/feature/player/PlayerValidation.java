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

package de.markusbordihn.adaptiveperformancetweaks.feature.player;

import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class PlayerValidation {

  private final ServerPlayer player;
  private final String username;
  private final Vec3 initialPosition;
  private final double initialYHeadRot;
  private final long startTime = System.currentTimeMillis();
  private final boolean wasInvisible;
  private final boolean wasInvulnerable;

  public PlayerValidation(ServerPlayer player) {
    this.player = player;
    this.username = player.getName().getString();
    this.initialPosition = player.position();
    this.initialYHeadRot = player.getYHeadRot();
    this.wasInvisible = player.isInvisible();
    this.wasInvulnerable = player.isInvulnerable();
  }

  public boolean hasPlayerMoved() {
    return !this.initialPosition.equals(this.player.position())
      || this.initialYHeadRot != this.player.getYHeadRot();
  }

  public String getUsername() {
    return this.username;
  }

  public long getValidationTimeElapsed() {
    return System.currentTimeMillis() - this.startTime;
  }

  public boolean wasInvisible() {
    return this.wasInvisible;
  }

  public boolean wasInvulnerable() {
    return this.wasInvulnerable;
  }
}
