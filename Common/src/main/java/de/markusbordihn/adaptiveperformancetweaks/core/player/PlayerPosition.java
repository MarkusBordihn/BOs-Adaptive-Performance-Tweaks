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

import java.util.Arrays;
import java.util.UUID;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;

public class PlayerPosition {

  private static final int CHUNK_SIZE = 16;
  private static final double STATIONARY_DISTANCE_EPSILON = 0.01D;

  private final ViewArea viewArea;
  private final String playerName;
  private final UUID playerUUID;
  private String levelName;
  private double lastPosX;
  private double lastPosY;
  private double lastPosZ;
  private int lastChunkX;
  private int lastChunkZ;
  private int lastMovementSampleTick = -1;
  private double[] movementWindow = new double[0];
  private int movementWindowCount = 0;
  private int movementWindowIndex = 0;
  private double movementWindowDistance = 0.0D;
  private int stableTicks = 0;
  private int loginWarmupUntilTick = 0;

  public PlayerPosition(ServerPlayer player, int viewDistance, int simulationDistance) {
    this(
      player.getName().getString(),
      player.getUUID(),
      player.level().dimension().location().toString(),
      player.blockPosition().getX(),
      player.blockPosition().getY(),
      player.blockPosition().getZ(),
      getViewAreaDistance(player, player.level().dimension().location().toString(),
        viewDistance, simulationDistance));
    this.lastPosX = player.getX();
    this.lastPosY = player.getY();
    this.lastPosZ = player.getZ();
  }

  public PlayerPosition(String playerName, UUID playerUUID, String levelName, int posX, int posY,
    int posZ, int viewAreaDistance) {
    this.playerName = playerName;
    this.playerUUID = playerUUID;
    this.levelName = levelName;
    this.viewArea = new ViewArea(posX, posY, posZ, viewAreaDistance, levelName);
    this.lastPosX = posX;
    this.lastPosY = posY;
    this.lastPosZ = posZ;
    this.lastChunkX = posX >> 4;
    this.lastChunkZ = posZ >> 4;
  }

  public static int getViewAreaDistance(
    ServerPlayer serverPlayer, String levelName, int viewDistance, int simulationDistance) {
    boolean isNether = levelName.equals(ViewArea.NETHER);
    boolean isTheEnd = levelName.equals(ViewArea.THE_END);
    boolean canSeeSky =
      !isNether && serverPlayer.level().canSeeSky(serverPlayer.blockPosition());
    boolean isUnderWater = !isNether && serverPlayer.isUnderWater();

    if ((!isNether && !isTheEnd && !canSeeSky) || isUnderWater) {
      return Math.min(simulationDistance, viewDistance - 1) * CHUNK_SIZE;
    }

    return viewDistance * CHUNK_SIZE;
  }

  public String getPlayerName() {
    return this.playerName;
  }

  public String getLevelName() {
    return this.levelName;
  }

  public double getMovementWindowDistance() {
    return this.movementWindowDistance;
  }

  public int getLastChunkX() {
    return this.lastChunkX;
  }

  public int getLastChunkZ() {
    return this.lastChunkZ;
  }

  public boolean hasCompleteMovementWindow() {
    return this.movementWindow.length > 0 && this.movementWindowCount >= this.movementWindow.length;
  }

  public boolean hasRecentMovementDistance(double thresholdBlocks) {
    return hasCompleteMovementWindow() && this.movementWindowDistance >= thresholdBlocks;
  }

  public boolean isLoginWarmupActive(int currentTick) {
    return currentTick < this.loginWarmupUntilTick;
  }

  public void setLoginWarmup(int currentTick, int warmupTicks) {
    this.loginWarmupUntilTick = Math.max(this.loginWarmupUntilTick,
      currentTick + Math.max(0, warmupTicks));
  }

  public boolean isStableForTicks(int requiredTicks) {
    return this.stableTicks >= Math.max(0, requiredTicks);
  }

  public void updateMovement(ServerPlayer serverPlayer, String levelName, int currentTick,
    int movementWindowSamples, int sampleTicks) {
    updateMovement(serverPlayer.getX(), serverPlayer.getY(), serverPlayer.getZ(), levelName,
      currentTick, movementWindowSamples, sampleTicks);
  }

  public void updateMovement(double posX, double posY, double posZ, String levelName,
    int currentTick, int movementWindowSamples, int sampleTicks) {
    ensureMovementWindowSize(movementWindowSamples);
    if (!levelName.equals(this.levelName)) {
      resetMovementWindow();
      this.levelName = levelName;
      this.lastPosX = posX;
      this.lastPosY = posY;
      this.lastPosZ = posZ;
      this.lastChunkX = ((int) Math.floor(posX)) >> 4;
      this.lastChunkZ = ((int) Math.floor(posZ)) >> 4;
      this.lastMovementSampleTick = currentTick;
      this.stableTicks = 0;
      return;
    }

    if (this.lastMovementSampleTick < 0) {
      this.lastPosX = posX;
      this.lastPosY = posY;
      this.lastPosZ = posZ;
      this.lastChunkX = ((int) Math.floor(posX)) >> 4;
      this.lastChunkZ = ((int) Math.floor(posZ)) >> 4;
      this.lastMovementSampleTick = currentTick;
      return;
    }

    double deltaX = posX - this.lastPosX;
    double deltaY = posY - this.lastPosY;
    double deltaZ = posZ - this.lastPosZ;
    double movementDistance = Math.sqrt(deltaX * deltaX + deltaY * deltaY + deltaZ * deltaZ);
    addMovementDistance(movementDistance);

    int tickDelta = Math.max(sampleTicks, currentTick - this.lastMovementSampleTick);
    if (movementDistance <= STATIONARY_DISTANCE_EPSILON) {
      this.stableTicks += tickDelta;
    } else {
      this.stableTicks = 0;
    }

    this.lastPosX = posX;
    this.lastPosY = posY;
    this.lastPosZ = posZ;
    this.lastChunkX = ((int) Math.floor(posX)) >> 4;
    this.lastChunkZ = ((int) Math.floor(posZ)) >> 4;
    this.lastMovementSampleTick = currentTick;
  }

  public boolean update(
    ServerPlayer serverPlayer, String levelName, int viewDistance, int simulationDistance) {
    return updateViewArea(serverPlayer, levelName, viewDistance, simulationDistance);
  }

  public boolean updateViewArea(
    ServerPlayer serverPlayer, String levelName, int viewDistance, int simulationDistance) {
    this.levelName = levelName;
    int viewAreaDistance =
      getViewAreaDistance(serverPlayer, levelName, viewDistance, simulationDistance);
    return this.viewArea.update(serverPlayer, viewAreaDistance, levelName);
  }

  public boolean isInsidePlayerViewArea(String levelName) {
    return this.levelName.equals(levelName);
  }

  public boolean isInsidePlayerViewArea(String levelName, int x, int y, int z) {
    return this.levelName.equals(levelName) && this.viewArea.isInside(x, y, z);
  }

  public boolean isInsidePlayerViewArea(Entity entity, String levelName) {
    return this.viewArea.isInside(entity, levelName);
  }

  private void ensureMovementWindowSize(int movementWindowSamples) {
    int windowSize = Math.max(1, movementWindowSamples);
    if (this.movementWindow.length == windowSize) {
      return;
    }

    this.movementWindow = new double[windowSize];
    resetMovementWindow();
  }

  private void addMovementDistance(double movementDistance) {
    if (this.movementWindow.length == 0) {
      return;
    }

    if (this.movementWindowCount == this.movementWindow.length) {
      this.movementWindowDistance -= this.movementWindow[this.movementWindowIndex];
    } else {
      this.movementWindowCount++;
    }

    this.movementWindow[this.movementWindowIndex] = movementDistance;
    this.movementWindowDistance += movementDistance;
    this.movementWindowIndex = (this.movementWindowIndex + 1) % this.movementWindow.length;
  }

  private void resetMovementWindow() {
    Arrays.fill(this.movementWindow, 0.0D);
    this.movementWindowCount = 0;
    this.movementWindowIndex = 0;
    this.movementWindowDistance = 0.0D;
  }

  public String toString() {
    return "PlayerPosition[player='"
      + this.playerName
      + "', uuid="
      + this.playerUUID
      + ", level='"
      + this.levelName
      + "', chunk=("
      + this.lastChunkX
      + ","
      + this.lastChunkZ
      + "), movementWindow="
      + String.format("%.1f", this.movementWindowDistance)
      + ", stableTicks="
      + this.stableTicks
      + ", loginWarmup="
      + this.loginWarmupUntilTick
      + ", viewArea="
      + this.viewArea
      + "]";
  }
}
