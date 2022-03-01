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

import java.util.UUID;

import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;

public class PlayerPosition {

  private static final String OVERWORLD = "minecraft:overworld";
  private static final int CHUNK_SIZE = 16;
  private static final int MAX_BUILD_HEIGHT = 320;
  private static final int MIN_BUILD_HEIGHT = -64;
  private static final int OVERGROUND_Y = 63;
  private static final int OVERGROUND_Y_MIN_VIEW = OVERGROUND_Y - 12;
  private static final int WATER_Y_MAX_VIEW = OVERGROUND_Y + 12;

  private ServerPlayer player;
  private String playerName = "";
  private UUID playerUUID;
  private String levelName = "";
  private boolean canSeeSky = false;
  private boolean isUnderWater = false;
  private boolean viewAreaCalculated = false;
  private long lastActionTime = 0;
  private int posX = 0;
  private int posY = 0;
  private int posZ = 0;
  private int simulationDistance = 4;
  private int viewAreaDistance = 8;
  private int viewAreaStartX = 0;
  private int viewAreaStartY = 0;
  private int viewAreaStartZ = 0;
  private int viewAreaStopX = 0;
  private int viewAreaStopY = 0;
  private int viewAreaStopZ = 0;
  private int viewDistance = 8;

  public PlayerPosition(ServerPlayer player, int viewDistance, int simulationDistance) {
    this.player = player;
    this.playerName = player.getName().getString();
    this.playerUUID = player.getUUID();
    this.updatePosition(player.getLevel().dimension().location().toString(), viewDistance,
        simulationDistance);
    this.calculateViewArea();
  }

  public void update(ServerPlayer player, int viewDistance, int simulationDistance) {
    update(player.getLevel().dimension().location().toString(), viewDistance, simulationDistance);
  }

  public void update(String levelName, int viewDistance, int simulationDistance) {
    if (!this.levelName.equals(levelName) || this.viewDistance != viewDistance
        || this.simulationDistance != simulationDistance
        || this.lastActionTime != this.player.getLastActionTime() || hasChangedPosition()) {
      this.updatePosition(levelName, viewDistance, simulationDistance);
    }
  }

  public boolean isInsidePlayerViewArea(String levelName) {
    return this.levelName.equals(levelName);
  }

  public boolean isInsidePlayerViewArea(String levelName, int x, int y, int z) {
    if (!isInsidePlayerViewArea(levelName)) {
      return false;
    }
    calculateViewArea();
    return ((this.viewAreaStartX < x && x < this.viewAreaStopX)
        && (this.viewAreaStartY < y && y < this.viewAreaStopY)
        && (this.viewAreaStartZ < z && z < this.viewAreaStopZ));
  }

  public boolean isInsidePlayerViewArea(Entity entity, String levelName) {
    if (entity == null || entity.isRemoved() || !isInsidePlayerViewArea(levelName)) {
      return false;
    }
    int x = (int) entity.getX();
    int y = (int) entity.getY();
    int z = (int) entity.getZ();
    return isInsidePlayerViewArea(levelName, x, y, z);
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

  private void updatePosition(String levelName, int viewDistance, int simulationDistance) {
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
    this.simulationDistance = simulationDistance;
    this.viewDistance = viewDistance;
    this.viewAreaCalculated = false;
  }

  private void calculateViewArea() {
    if (this.viewAreaCalculated) {
      return;
    }

    // Calculate view area distance in blocks based on surrounding factors
    if (!this.canSeeSky || this.isUnderWater) {
      this.viewAreaDistance = (this.simulationDistance < this.viewDistance ? this.simulationDistance
          : this.simulationDistance - 1) * CHUNK_SIZE;
    } else {
      this.viewAreaDistance = this.viewDistance * CHUNK_SIZE;
    }
    if (this.viewAreaDistance > 240) {
      // This 240x240 (15x15 chunks) is the max. mob spawn radius for any natural spawn
      this.viewAreaDistance = 240;
    }

    // Simple calculation for X, Y and Z
    this.viewAreaStartX = this.posX - this.viewAreaDistance;
    this.viewAreaStopX = this.posX + this.viewAreaDistance;
    this.viewAreaStartY = Math.max(this.posY - this.viewAreaDistance, MIN_BUILD_HEIGHT);
    this.viewAreaStopY = Math.min(this.posY + this.viewAreaDistance, MAX_BUILD_HEIGHT);
    this.viewAreaStartZ = this.posZ - this.viewAreaDistance;
    this.viewAreaStopZ = this.posZ + this.viewAreaDistance;

    // Additional optimization based on the current position, world and other factors like if the
    // player is able to see the sky, if the player is overground or underground.
    if (OVERWORLD.equals(this.levelName)) {

      // Player is on overground and can see Sky
      if (this.posY >= OVERGROUND_Y) {
        this.viewAreaStartY = OVERGROUND_Y_MIN_VIEW;
      }

      // Player is underground and can not see the sky
      else if (!this.canSeeSky) {
        this.viewAreaStopY = OVERGROUND_Y;
      }

      // Player is underground and underwater
      else if (this.isUnderWater) {
        this.viewAreaStopY = WATER_Y_MAX_VIEW;
      }
    }

    this.viewAreaCalculated = true;
  }

  public String toString() {
    // Make sure to calculate view area before displaying results.
    calculateViewArea();

    return "PlayerPosition[Player{name: '" + this.playerName + "', uuid: '" + this.playerUUID
        + "', world: '" + this.levelName + "', x:" + this.posX + ", y:" + this.posY + ", z:"
        + this.posZ + ", viewDistance: " + this.viewDistance + ", simulationDistance: "
        + this.simulationDistance + ", viewAreaDistance: " + this.viewAreaDistance + "}, Range{x:"
        + this.viewAreaStartX + " to " + this.viewAreaStopX + ", y:" + this.viewAreaStartY + " to "
        + this.viewAreaStopY + ", z:" + this.viewAreaStartZ + " to " + this.viewAreaStopZ
        + "}, Meta{canSeeSky: " + this.canSeeSky + ", isUnderWater: " + this.isUnderWater + "}]";
  }

}
