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

package de.markusbordihn.adaptiveperformancetweakscore.server;

import net.minecraft.server.level.ServerLevel;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.eventbus.api.Event;

import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLevelLoad.ServerLevelLoadLevel;

public class ServerLevelLoadEvent extends Event {

  private Dist dist;
  private ServerLevel serverLevel;
  private ServerLevelLoadLevel lastServerLevelLoadLevel = ServerLevelLoadLevel.NORMAL;
  private ServerLevelLoadLevel serverLevelLoadLevel = ServerLevelLoadLevel.NORMAL;
  private String serverLevelName;
  private double serverLevelLoadLevelFactor = 0;
  private int playerCount = 0;
  private double avgTickTime = 50.0;
  private double lastAvgTickTime = 45.0;

  public ServerLevelLoadEvent(ServerLevel serverLevel, ServerLevelLoadLevel serverLevelLoadLevel,
      ServerLevelLoadLevel lastServerLevelLoadLevel, double avgTickTime, double lastAvgTickTim,
      Dist dist) {
    super();
    this.avgTickTime = avgTickTime;
    this.dist = dist;
    this.lastAvgTickTime = lastAvgTickTim;
    this.lastServerLevelLoadLevel = lastServerLevelLoadLevel;
    this.playerCount = serverLevel.players().size();
    this.serverLevel = serverLevel;
    this.serverLevelLoadLevel = serverLevelLoadLevel;
    this.serverLevelName = serverLevel.getLevel().dimension().location().toString();
  }

  public boolean isDedicatedServer() {
    return this.dist.isDedicatedServer();
  }

  public boolean isClient() {
    return this.dist.isClient();
  }

  public int getPlayerCount() {
    return this.playerCount;
  }

  public double getAvgTickTime() {
    return this.avgTickTime;
  }

  public double getLastAvgTickTime() {
    return this.lastAvgTickTime;
  }

  public ServerLevel getServerLevel() {
    return this.serverLevel;
  }

  public String getServerLevelName() {
    return this.serverLevelName;
  }

  public ServerLevelLoadLevel getServerLevelLoadLevel() {
    return this.serverLevelLoadLevel;
  }

  public double getServerLevelLoadLevelFactor() {
    if (this.serverLevelLoadLevelFactor == 0) {
      this.serverLevelLoadLevelFactor = calculateServerLevelLoadLevelFactor();
    }
    return this.serverLevelLoadLevelFactor;
  }

  public double calculateServerLevelLoadLevelFactor() {
    if ((this.serverLevelLoadLevel == ServerLevelLoadLevel.VERY_LOW
        || this.serverLevelLoadLevel == ServerLevelLoadLevel.LOW
        || this.serverLevelLoadLevel == ServerLevelLoadLevel.NORMAL)
        && (this.lastServerLevelLoadLevel == ServerLevelLoadLevel.VERY_LOW
            || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.LOW
            || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.NORMAL)) {
      return 1.0;
    }
    if (this.serverLevelLoadLevel == ServerLevelLoadLevel.NORMAL
        && this.lastServerLevelLoadLevel == ServerLevelLoadLevel.MEDIUM) {
      return 0.9;
    }
    if (this.serverLevelLoadLevel == ServerLevelLoadLevel.NORMAL
        && (this.lastServerLevelLoadLevel == ServerLevelLoadLevel.HIGH
            || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.VERY_HIGH)) {
      return 0.8;
    }
    if (this.serverLevelLoadLevel == ServerLevelLoadLevel.MEDIUM
        && (this.lastServerLevelLoadLevel == ServerLevelLoadLevel.MEDIUM
            || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.NORMAL
            || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.VERY_LOW
            || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.LOW)) {
      return 0.7;
    }
    if (this.serverLevelLoadLevel == ServerLevelLoadLevel.MEDIUM
        && (this.lastServerLevelLoadLevel == ServerLevelLoadLevel.HIGH
            || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.VERY_HIGH)) {
      return 0.6;
    }
    if (this.serverLevelLoadLevel == ServerLevelLoadLevel.HIGH
        && this.lastServerLevelLoadLevel != ServerLevelLoadLevel.HIGH
        && this.lastServerLevelLoadLevel != ServerLevelLoadLevel.VERY_HIGH) {
      return 0.5;
    }
    if (this.serverLevelLoadLevel == ServerLevelLoadLevel.HIGH) {
      return 0.4;
    }
    if (this.serverLevelLoadLevel == ServerLevelLoadLevel.VERY_HIGH
        && this.lastServerLevelLoadLevel != ServerLevelLoadLevel.VERY_HIGH) {
      return 0.3;
    }
    if (this.serverLevelLoadLevel == ServerLevelLoadLevel.VERY_HIGH) {
      return 0.2;
    }
    return 1.0;
  }

  public boolean hasHighServerLevelLoad() {
    return (this.serverLevelLoadLevel == ServerLevelLoadLevel.MEDIUM
        || this.serverLevelLoadLevel == ServerLevelLoadLevel.HIGH
        || this.serverLevelLoadLevel == ServerLevelLoadLevel.VERY_HIGH
        || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.HIGH
        || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.VERY_HIGH);
  }

  public boolean hasNormalServerLevelLoad() {
    return (this.serverLevelLoadLevel == ServerLevelLoadLevel.NORMAL)
        && (this.lastServerLevelLoadLevel == ServerLevelLoadLevel.VERY_LOW
            || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.LOW
            || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.NORMAL);
  }

  public boolean hasLowServerLevelLoad() {
    return (this.serverLevelLoadLevel == ServerLevelLoadLevel.VERY_LOW
        || this.serverLevelLoadLevel == ServerLevelLoadLevel.LOW
            && (this.lastServerLevelLoadLevel == ServerLevelLoadLevel.VERY_LOW
                || this.lastServerLevelLoadLevel == ServerLevelLoadLevel.LOW));
  }

  public boolean hasChanged() {
    return this.serverLevelLoadLevel != this.lastServerLevelLoadLevel;
  }
}
