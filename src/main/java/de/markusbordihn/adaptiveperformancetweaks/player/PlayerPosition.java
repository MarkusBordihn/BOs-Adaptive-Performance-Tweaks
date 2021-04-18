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
  private boolean canSeeSky = false;
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

  private static final int OVERGROUND_Y = 63;
  private static final int OVERGROUND_Y_MIN_VIEW = OVERGROUND_Y - 12;
  private static final int MAX_BUILD_HEIGHT = 320;

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

  public boolean update(String worldName) {
    if (!this.worldName.equals(worldName)
        || this.viewDistance != WorldViewManager.getViewDistance(worldName)
        || this.posX != (int) this.player.getPosX() || this.posY != (int) this.player.getPosY()
        || this.posZ != (int) this.player.getPosZ()) {
      updateViewDistance(WorldViewManager.getViewDistance(worldName));
      this.posX = (int) this.player.getPosX();
      this.posY = (int) this.player.getPosY();
      this.posZ = (int) this.player.getPosZ();
      this.canSeeSky = this.player.getEntityWorld().canSeeSky(this.player.getPosition());
      this.worldName = worldName;
      this.viewAreaCalculated = false;
      return true;
    }
    return false;
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

    // Simple calculation for X and Z
    this.viewAreaStartX = this.posX - viewAreaXFactor;
    this.viewAreaStopX = this.posX + viewAreaXFactor;
    this.viewAreaStartZ = this.posZ - viewAreaZFactor;
    this.viewAreaStopZ = this.posZ + viewAreaZFactor;

    // Optimize Y based on the current position and other factors like if the player is able to
    // see the sky, if the player is overground or underground ...
    if (this.posY >= OVERGROUND_Y && this.canSeeSky) {
      // Player is on overground and can see Sky
      this.viewAreaStartY = OVERGROUND_Y_MIN_VIEW;
      this.viewAreaStopY = MAX_BUILD_HEIGHT;
    } else if (this.posY >= OVERGROUND_Y) {
      // Player is on overground and can not see Sky
      if (this.posY - viewAreaYFactor < OVERGROUND_Y_MIN_VIEW) {
        this.viewAreaStartY = OVERGROUND_Y_MIN_VIEW;
      } else {
        this.viewAreaStartY = this.posY - viewAreaYFactor;
      }
      this.viewAreaStopY = this.posY + viewAreaYFactor;
    } else if (this.canSeeSky) {
      // Player is on underground and can see Sky
      this.viewAreaStartY = this.posY - viewAreaYFactor;
      this.viewAreaStopY = (int) Math.round(this.posY + viewAreaYFactor * 2.0);
    } else {
      // Player is on underground and can not see Sky
      this.viewAreaStartY = this.posY - viewAreaYFactor;
      this.viewAreaStopY = this.posY + viewAreaYFactor;
    }
    this.viewAreaCalculated = true;
  }

  public String toString() {
    return "PlayerPosition[Player{name: '" + this.playerName + "', x:" + this.posX + ", y:"
        + this.posY + ", z:" + this.posZ + "}, Range{x:" + this.viewAreaStartX + " to "
        + this.viewAreaStopX + ", y:" + this.viewAreaStartY + " to " + this.viewAreaStopY + ", z:"
        + this.viewAreaStopZ + " to " + this.viewAreaStopZ + "}, Meta{canSeeSky: " + this.canSeeSky
        + "}]";
  }

}
