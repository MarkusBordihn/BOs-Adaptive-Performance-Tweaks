/**
 * Copyright 2021 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweaks.player;

import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;

public class PlayerValidation {

  private String username;
  private double posX = 0;
  private double posY = 0;
  private double posZ = 0;
  private double rotationYawHead;
  private long lastValidationTime = System.currentTimeMillis();
  private ServerPlayerEntity player;

  PlayerValidation(ServerPlayerEntity player) {
    this.player = player;
    this.username = player.getName().getString();
    this.posX = player.getPosX();
    this.posY = player.getPosY();
    this.posZ = player.getPosZ();
    this.rotationYawHead = player.getRotationYawHead();
  }

  public boolean hasPlayerMoved() {
    return (
      !this.username.equals(this.player.getName().getString()) ||
      this.posX != this.player.getPosX() ||
      this.posY != this.player.getPosY() ||
      this.posZ != this.player.getPosZ() ||
      this.rotationYawHead != this.player.getRotationYawHead()
    );
  }

  public ServerPlayerEntity getPlayer() {
    return this.player;
  }

  public String getUsername() {
    return this.username;
  }

  public long getValidationTime() {
    return this.lastValidationTime;
  }

  public long getValidationTimeElapsed() {
    return System.currentTimeMillis() - this.lastValidationTime;
  }

  public boolean isPlayerEntity(Entity entity) {
    if (!(entity instanceof PlayerEntity)) {
      return false;
    }
    return (username.equals(entity.getName().getString()));
  }

}
