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

import net.minecraft.entity.player.ServerPlayerEntity;

import de.markusbordihn.adaptiveperformancetweaks.config.CommonConfig;
import de.markusbordihn.adaptiveperformancetweaks.world.WorldViewManager;

public class PlayerPosition {

  private ServerPlayerEntity player;
  private String playerName = "";
  private String worldName = "";
  private boolean viewAreaCalculated = false;
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
  private int viewAreaXFactor = 32;
  private int viewAreaYFactor = 16;
  private int viewAreaZFactor = 32;

  private int viewAreaXFactorBase = CommonConfig.COMMON.viewAreaXFactor.get();
  private int viewAreaYFactorBase = CommonConfig.COMMON.viewAreaYFactor.get();
  private int viewAreaZFactorBase = CommonConfig.COMMON.viewAreaZFactor.get();
  private double viewAreaDistanceFactorBase = CommonConfig.COMMON.viewAreaDistanceFactor.get();

  public PlayerPosition(ServerPlayerEntity player, String worldName) {
    this.player = player;
    this.playerName = player.getName().getString();
    this.posX = (int) player.getPosX();
    this.posY = (int) player.getPosX();
    this.posZ = (int) player.getPosX();
    this.viewDistance = WorldViewManager.getViewDistance(worldName);
    this.worldName = worldName;
    this.calculateViewArea();
  }

  public boolean hasChanged(String worldName) {
    return !this.worldName.equals(worldName)
        || this.viewDistance != WorldViewManager.getViewDistance(worldName)
        || this.posX != (int) this.player.getPosX() || this.posY != (int) this.player.getPosY()
        || this.posZ != (int) this.player.getPosZ();
  }

  public boolean update(String worldName) {
    boolean hasChanged = hasChanged(worldName);
    if (hasChanged) {
      updateViewDistance(WorldViewManager.getViewDistance(worldName));
      this.posX = (int) this.player.getPosX();
      this.posY = (int) this.player.getPosY();
      this.posZ = (int) this.player.getPosZ();
      this.worldName = worldName;
      this.viewAreaCalculated = false;
    }
    return hasChanged;
  }

  public void updateViewDistance(int viewDistance) {
    int viewDistanceFactor = (int) Math.round(this.viewDistance * this.viewAreaDistanceFactorBase);
    this.viewDistance = viewDistance;
    this.viewAreaXFactor =
        Math.max(this.viewAreaXFactorBase, this.viewAreaXFactorBase * viewDistanceFactor);
    this.viewAreaYFactor =
        Math.max(this.viewAreaYFactorBase, this.viewAreaYFactorBase * viewDistanceFactor);
    this.viewAreaZFactor =
        Math.max(this.viewAreaZFactorBase, this.viewAreaZFactorBase * viewDistanceFactor);
  }

  public boolean isInsidePlayerViewArea(String worldName, int x, int y, int z) {
    if (!this.worldName.equals(worldName)) {
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

  private void calculateViewArea() {
    if (this.viewAreaCalculated) {
      return;
    }
    this.viewAreaStartX = this.posX - viewAreaXFactor;
    this.viewAreaStopX = this.posX + viewAreaXFactor;
    this.viewAreaStartY = this.posY - viewAreaYFactor;
    if (this.posY >= 60) {
      this.viewAreaStopY = (int) Math.round(this.posY + viewAreaYFactor * 1.5);
    } else {
      this.viewAreaStopY = this.posY + viewAreaYFactor;
    }
    this.viewAreaStartZ = this.posZ - viewAreaZFactor;
    this.viewAreaStopZ = this.posZ + viewAreaZFactor;
    this.viewAreaCalculated = true;
  }

  public String toString() {
    return String.format(
        "PlayerPosition[Player{name: '%s', x:%s, y:%s, z:%s}, Range{x:%s to %s, y:%s to %s, z:%s to %s}]",
        this.playerName, this.posX, this.posY, this.posZ, this.viewAreaStartX, this.viewAreaStopX,
        this.viewAreaStartY, this.viewAreaStopY, this.viewAreaStartZ, this.viewAreaStopZ);
  }

}
