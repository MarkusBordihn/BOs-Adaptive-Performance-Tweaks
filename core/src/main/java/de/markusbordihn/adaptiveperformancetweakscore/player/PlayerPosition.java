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

package de.markusbordihn.adaptiveperformancetweakscore.player;

import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class PlayerPosition {
  private ServerPlayer player;
  private String playerName = "";
  private String levelName = "";
  private boolean canSeeSky = false;
  private boolean isUnderWater = false;
  private boolean viewAreaCalculated = false;
  private long lastActionTime = 0;
  private int posX = 0;
  private int posY = 0;
  private int posZ = 0;
  private int viewAreaStartX = 0;
  private int viewAreaStartY = 0;
  private int viewAreaStartZ = 0;
  private int viewAreaStopX = 0;
  private int viewAreaStopY = 0;
  private int viewAreaStopZ = 0;
  private int viewDistance = 8;

  private static final int CHUNK_SIZE = 16;
  private static final int MAX_BUILD_HEIGHT = 320;
  private static final int MIN_BUILD_HEIGHT = -64;

  public PlayerPosition(ServerPlayer player, int viewDistance) {
    this.player = player;
    this.playerName = player.getName().getString();
    this.updatePosition(player.getLevel().dimension().location().toString(), viewDistance);
    this.calculateViewArea();
  }

  public boolean update(ServerPlayer player, int viewDistance) {
    return update(player.getLevel().dimension().location().toString(), viewDistance);
  }

  public boolean update(String levelName, int viewDistance) {
    if (!this.levelName.equals(levelName)
        || this.viewDistance != viewDistance || this.lastActionTime != this.player.getLastActionTime()
        || hasChangedPosition()) {
      this.updatePosition(levelName, viewDistance);
      return true;
    }
    return false;
  }

  public void updatePosition(String levelName, int viewDistance) {
    this.lastActionTime = player.getLastActionTime();
    Vec3 position = player.position();
    this.posX = (int) position.x;
    this.posY = (int) position.y;
    this.posZ = (int) position.z;
    this.canSeeSky = this.player.getLevel().canSeeSky(this.player.blockPosition());
    this.isUnderWater = this.player.isUnderWater();
    if (!this.levelName.equals(levelName)) {
      this.levelName = levelName;
    }
    updateViewDistance(viewDistance);
    this.viewAreaCalculated = false;
  }

  public void updateViewDistance(int viewDistance) {
    this.viewDistance = viewDistance;
  }

  public boolean isInsidePlayerViewArea(String levelName, int x, int y, int z) {
    if (!this.levelName.equals(levelName)) {
      return false;
    }
    calculateViewArea();
    return ((this.viewAreaStartX < x && x < this.viewAreaStopX)
        && (this.viewAreaStartY < y && y < this.viewAreaStopY)
        && (this.viewAreaStartZ < z && z < this.viewAreaStopZ));
  }

  public String getPlayerName() {
    return this.playerName;
  }

  public boolean hasChangedPosition() {
    Vec3 currentPosition = this.player.position();
    return Integer.compare(this.posX, (int) currentPosition.x) != 0
        || Integer.compare(this.posY, (int) currentPosition.y) != 0
        || Integer.compare(this.posZ, (int) currentPosition.z) != 0;
  }

  private void calculateViewArea() {
    if (this.viewAreaCalculated) {
      return;
    }

    // Simple calculation for X, Y and Z
    this.viewAreaStartX = this.posX - this.viewDistance * CHUNK_SIZE;
    this.viewAreaStopX = this.posX + this.viewDistance * CHUNK_SIZE;
    this.viewAreaStartY = Math.max(this.posY - this.viewDistance * CHUNK_SIZE, MIN_BUILD_HEIGHT);
    this.viewAreaStopY = Math.min(this.posY + this.viewDistance * CHUNK_SIZE, MAX_BUILD_HEIGHT);
    this.viewAreaStartZ = this.posZ - this.viewDistance * CHUNK_SIZE;
    this.viewAreaStopZ = this.posZ + this.viewDistance * CHUNK_SIZE;

    this.viewAreaCalculated = true;
  }

  public String toString() {
    // Makes sure we get accurate values, if the view area is not calculated yet.
    calculateViewArea();
    return "PlayerPosition[Player{name: '" + this.playerName + "', world: '" + this.levelName
        + "', x:" + this.posX + ", y:" + this.posY + ", z:" + this.posZ + ", viewDistance: "
        + this.viewDistance + "}, Range{x:" + this.viewAreaStartX + " to " + this.viewAreaStopX
        + ", y:" + this.viewAreaStartY + " to " + this.viewAreaStopY + ", z:" + this.viewAreaStartZ
        + " to " + this.viewAreaStopZ + "}, Meta{canSeeSky: " + this.canSeeSky + ", isUnderWater: "
        + this.isUnderWater + "}]";
  }

}
