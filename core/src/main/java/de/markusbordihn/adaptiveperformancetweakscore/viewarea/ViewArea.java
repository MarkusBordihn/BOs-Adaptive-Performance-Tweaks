/*
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

package de.markusbordihn.adaptiveperformancetweakscore.viewarea;

import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;

public class ViewArea {

  private static final String NETHER = "minecraft:the_nether";
  private static final String THE_END = "minecraft:the_end";
  private static final float NETHER_EXPAND_FACTOR = 1.1f;
  private static final float THE_END_EXPAND_FACTOR = 1.5f;

  private static final int MAX_BUILD_HEIGHT = 320;
  private static final int MAX_BUILD_HEIGHT_NETHER = 256;
  private static final int MAX_BUILD_HEIGHT_THE_END = 256;
  private static final int MAX_VIEW_AREA_DISTANCE = 240; // 240x240 = 15x15 chunks max.
  private static final int MIN_BUILD_HEIGHT = -64;

  private String levelName;
  private int posX;
  private int posY;
  private int posZ;
  private int startX;
  private int startY;
  private int startZ;
  private int stopX;
  private int stopY;
  private int stopZ;
  private int viewAreaDistance;
  private int blocksViewDistance;

  public ViewArea(ServerPlayer player, int viewAreaDistance) {
    this(
        player.blockPosition().getX(),
        player.blockPosition().getY(),
        player.blockPosition().getZ(),
        viewAreaDistance,
        player.level.dimension().location().toString());
  }

  public ViewArea(int posX, int posY, int posZ, int viewAreaDistance, String levelName) {
    this.update(posX, posY, posZ, viewAreaDistance, levelName);
  }

  public boolean isLevel(String levelName) {
    return this.levelName.equals(levelName);
  }

  public boolean isInside(Entity entity, String levelName) {
    if (entity == null || entity.isRemoved() || !isLevel(levelName)) {
      return false;
    }
    BlockPos blockPos = entity.blockPosition();
    return isInside(blockPos.getX(), blockPos.getY(), blockPos.getZ());
  }

  public boolean isInside(int posX, int posY, int posZ) {
    return (posX >= this.startX
        && posX <= this.stopX
        && posY >= this.startY
        && posY <= this.stopY
        && posZ >= this.startZ
        && posZ <= this.stopZ);
  }

  public boolean update(ServerPlayer serverPlayer, int viewAreaDistance) {
    return this.update(
        serverPlayer.blockPosition().getX(),
        serverPlayer.blockPosition().getY(),
        serverPlayer.blockPosition().getZ(),
        viewAreaDistance,
        serverPlayer.level.dimension().location().toString());
  }

  public boolean update(ServerPlayer serverPlayer, int viewAreaDistance, String levelName) {
    return this.update(
        serverPlayer.blockPosition().getX(),
        serverPlayer.blockPosition().getY(),
        serverPlayer.blockPosition().getZ(),
        viewAreaDistance,
        levelName);
  }

  public boolean update(int posX, int posY, int posZ, int viewAreaDistance, String levelName) {

    // Check if calculation is necessary or has changed.
    if (this.posX == posX
        && this.posY == posY
        && this.posZ == posZ
        && this.viewAreaDistance == viewAreaDistance
        && this.levelName.equals(levelName)) {
      return false;
    }

    // Update position for change detection and debugging purpose.
    this.posX = posX;
    this.posY = posY;
    this.posZ = posZ;

    // Update level for better view area calculation.
    this.levelName = levelName;
    boolean isNether = levelName.equals(NETHER);
    boolean isTheEnd = levelName.equals(THE_END);

    // Limit max view area distance to consider mods which changes these factors.
    this.viewAreaDistance = viewAreaDistance;
    if (viewAreaDistance > MAX_VIEW_AREA_DISTANCE) {
      viewAreaDistance = MAX_VIEW_AREA_DISTANCE;
    }

    // Expand view area distance for Nether and The End.
    if (isNether) {
      viewAreaDistance = (int) (viewAreaDistance * NETHER_EXPAND_FACTOR);
    } else if (isTheEnd) {
      viewAreaDistance = (int) (viewAreaDistance * THE_END_EXPAND_FACTOR);
    }
    this.blocksViewDistance = viewAreaDistance;

    // Simple calculation for X
    this.startX = posX - viewAreaDistance;
    this.stopX = posX + viewAreaDistance;

    // Simple calculation for Y
    if (this.levelName.equals(NETHER)) {
      this.startY = Math.max(posY - viewAreaDistance, MIN_BUILD_HEIGHT);
      this.stopY = Math.min(posY + viewAreaDistance, MAX_BUILD_HEIGHT_NETHER);
    } else if (this.levelName.equals(THE_END)) {
      this.startY = Math.max(posY - viewAreaDistance, MIN_BUILD_HEIGHT);
      this.stopY = Math.min(posY + viewAreaDistance, MAX_BUILD_HEIGHT_THE_END);
    } else {
      this.startY = Math.max(posY - viewAreaDistance, MIN_BUILD_HEIGHT);
      this.stopY = Math.min(posY + viewAreaDistance, MAX_BUILD_HEIGHT);
    }

    // Simple calculation for Z
    this.startZ = posZ - viewAreaDistance;
    this.stopZ = posZ + viewAreaDistance;

    return true;
  }

  public String toString() {
    return "ViewArea[level='"
        + this.levelName
        + "'', pos=("
        + this.posX
        + ","
        + this.posY
        + ","
        + this.posZ
        + "), start=("
        + this.startX
        + ","
        + this.startY
        + ","
        + this.startZ
        + "), stop=("
        + this.stopX
        + ","
        + this.stopY
        + ","
        + this.stopZ
        + "), blockViewDistance="
        + this.blocksViewDistance
        + "]";
  }
}
