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

  private static final String NETHER = "minecraft:the_nether";
  private static final String OVERWORLD = "minecraft:overworld";
  private static final String THE_END = "minecraft:the_end";
  private static final float NETHER_EXPAND_FACTOR = 1.1f;
  private static final float THE_END_EXPAND_FACTOR = 1.5f;
  private static final int CHUNK_SIZE = 16;
  private static final int MAX_BUILD_HEIGHT = 320;
  private static final int MAX_BUILD_HEIGHT_NETHER = 256;
  private static final int MAX_BUILD_HEIGHT_THE_END = 256;
  private static final int MAX_VIEW_AREA_DISTANCE = 240; // 240x240 = 15x15 chunks max.
  private static final int MIN_BUILD_HEIGHT = -64;
  private static final int OVERGROUND_Y = 63;
  private static final int OVERGROUND_Y_MIN_VIEW = OVERGROUND_Y - 12;
  private static final int WATER_Y_MAX_VIEW = OVERGROUND_Y + 12;

  private ServerPlayer player;
  private String levelName = "";
  private String playerName = "";
  private UUID playerUUID;
  private boolean canSeeSky = false;
  private boolean isNether = false;
  private boolean isOverworld = false;
  private boolean isTheEnd = false;
  private boolean isUnderWater = false;
  private boolean viewAreaCalculated = false;
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
  private long lastActionTime = 0;

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

  public void forceUpdate(ServerPlayer player, int viewDistance, int simulationDistance) {
    this.updatePosition(player, player.getLevel().dimension().location().toString(), viewDistance,
        simulationDistance);
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
    updatePosition(this.player, levelName, viewDistance, simulationDistance);
  }

  private void updatePosition(ServerPlayer serverPlayer, String levelName, int viewDistance,
      int simulationDistance) {
    this.player = serverPlayer;
    this.lastActionTime = serverPlayer.getLastActionTime();
    Vec3 position = serverPlayer.position();
    this.posX = (int) position.x;
    this.posY = (int) position.y;
    this.posZ = (int) position.z;
    if (!this.levelName.equals(levelName)) {
      this.levelName = levelName;
      this.isNether = levelName.equals(NETHER);
      this.isOverworld = levelName.equals(OVERWORLD);
      this.isTheEnd = levelName.equals(THE_END);
    }
    this.canSeeSky =
        !this.isNether && serverPlayer.getLevel().canSeeSky(serverPlayer.blockPosition());
    this.isUnderWater = !this.isNether && serverPlayer.isUnderWater();
    this.simulationDistance = simulationDistance;
    this.viewDistance = viewDistance;
    this.viewAreaCalculated = false;
  }

  private void calculateViewArea() {
    if (this.viewAreaCalculated) {
      return;
    }

    // Calculate view area distance in blocks based on surrounding factors like dimension, player
    // can see sky or is under water.
    if ((!isNether && !isTheEnd && !this.canSeeSky) || this.isUnderWater) {
      this.viewAreaDistance =
          (this.simulationDistance < this.viewDistance - 1 ? this.simulationDistance
              : this.viewDistance - 1) * CHUNK_SIZE;
    } else {
      this.viewAreaDistance = this.viewDistance * CHUNK_SIZE;
    }

    // Limit max view area distance to consider mods which changes the factors.
    if (this.viewAreaDistance > MAX_VIEW_AREA_DISTANCE) {
      this.viewAreaDistance = MAX_VIEW_AREA_DISTANCE;
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
    if (isOverworld) {
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

    // Additional optimization for the Nether to expand the view area in the X and Z direction.
    // This makes sure that the player is able to see the full view distance in the Nether.
    if (isNether) {
      this.viewAreaStartX = Math.round(this.viewAreaStartX * NETHER_EXPAND_FACTOR);
      this.viewAreaStopX = Math.round(this.viewAreaStopX * NETHER_EXPAND_FACTOR);
      this.viewAreaStopY = Math.min(this.viewAreaStopY, MAX_BUILD_HEIGHT_NETHER);
      this.viewAreaStartZ = Math.round(this.viewAreaStartZ * NETHER_EXPAND_FACTOR);
      this.viewAreaStopZ = Math.round(this.viewAreaStopZ * NETHER_EXPAND_FACTOR);
    }

    // Additional optimization for the End to expand the view area in the X and Z direction.
    // This makes sure that the player is able to see the full view distance to fly in the End.
    if (isTheEnd) {
      this.viewAreaStartX = Math.round(this.viewAreaStartX * THE_END_EXPAND_FACTOR);
      this.viewAreaStopX = Math.round(this.viewAreaStopX * THE_END_EXPAND_FACTOR);
      this.viewAreaStartY = Math.round(this.viewAreaStartY * THE_END_EXPAND_FACTOR);
      this.viewAreaStopY = Math.min(Math.round(this.viewAreaStopY * THE_END_EXPAND_FACTOR),
          MAX_BUILD_HEIGHT_THE_END);
      this.viewAreaStartZ = Math.round(this.viewAreaStartZ * THE_END_EXPAND_FACTOR);
      this.viewAreaStopZ = Math.round(this.viewAreaStopZ * THE_END_EXPAND_FACTOR);
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
        + "}, Meta{isOverworld: " + this.isOverworld + ", isNether: " + this.isNether
        + ", canSeeSky: " + this.canSeeSky + ", isUnderWater: " + this.isUnderWater + "}]";
  }

}
