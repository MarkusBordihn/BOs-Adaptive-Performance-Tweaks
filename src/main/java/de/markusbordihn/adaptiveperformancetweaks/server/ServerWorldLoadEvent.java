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

package de.markusbordihn.adaptiveperformancetweaks.server;

import de.markusbordihn.adaptiveperformancetweaks.server.ServerWorldLoad.ServerWorldLoadLevel;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.eventbus.api.Event;

public class ServerWorldLoadEvent extends Event {
  private ServerWorld serverWorld;
  private ServerWorldLoadLevel lastServerWorldLoadLevel = ServerWorldLoadLevel.NORMAL;
  private ServerWorldLoadLevel serverWorldLoadLevel = ServerWorldLoadLevel.NORMAL;
  private String serverWorldName;
  private double serverWorldLoadLevelFactor = 0;
  private int playerCount = 0;

  public ServerWorldLoadEvent(ServerWorld serverWorld, ServerWorldLoadLevel serverWorldLoadLevel,
      ServerWorldLoadLevel lastServerWorldLoadLevel) {
    super();
    this.playerCount = serverWorld.getPlayers().size();
    this.serverWorld = serverWorld;
    this.serverWorldName = serverWorld.getDimensionKey().getLocation().toString();
    this.lastServerWorldLoadLevel = lastServerWorldLoadLevel;
    this.serverWorldLoadLevel = serverWorldLoadLevel;
  }

  public int getPlayerCount() {
    return this.playerCount;
  }

  public ServerWorld getServerWorld() {
    return this.serverWorld;
  }

  public String getServerWorldName() {
    return this.serverWorldName;
  }

  public ServerWorldLoadLevel getServerWorldLoadLevel() {
    return this.serverWorldLoadLevel;
  }

  public double getServerWorldLoadLevelFactor() {
    if (this.serverWorldLoadLevelFactor == 0) {
      this.serverWorldLoadLevelFactor = calculateServerWorldLoadLevelFactor();
    }
    return this.serverWorldLoadLevelFactor;
  }

  public double calculateServerWorldLoadLevelFactor() {
    if (this.serverWorldLoadLevel == ServerWorldLoadLevel.NORMAL
        && (this.lastServerWorldLoadLevel == ServerWorldLoadLevel.NORMAL
            || this.lastServerWorldLoadLevel == ServerWorldLoadLevel.VERY_LOW
            || this.lastServerWorldLoadLevel == ServerWorldLoadLevel.LOW)) {
      return 1.0;
    }
    if (this.serverWorldLoadLevel == ServerWorldLoadLevel.NORMAL
        && this.lastServerWorldLoadLevel == ServerWorldLoadLevel.MEDIUM) {
      return 0.9;
    }
    if (this.serverWorldLoadLevel == ServerWorldLoadLevel.NORMAL
        && (this.lastServerWorldLoadLevel == ServerWorldLoadLevel.HIGH
            || this.lastServerWorldLoadLevel == ServerWorldLoadLevel.VERY_HIGH)) {
      return 0.8;
    }
    if (this.serverWorldLoadLevel == ServerWorldLoadLevel.MEDIUM
        && (this.lastServerWorldLoadLevel == ServerWorldLoadLevel.MEDIUM
            || this.lastServerWorldLoadLevel == ServerWorldLoadLevel.NORMAL
            || this.lastServerWorldLoadLevel == ServerWorldLoadLevel.VERY_LOW
            || this.lastServerWorldLoadLevel == ServerWorldLoadLevel.LOW)) {
      return 0.7;
    }
    if (this.serverWorldLoadLevel == ServerWorldLoadLevel.MEDIUM
        && (this.lastServerWorldLoadLevel == ServerWorldLoadLevel.HIGH
            || this.lastServerWorldLoadLevel == ServerWorldLoadLevel.VERY_HIGH)) {
      return 0.6;
    }
    if (this.serverWorldLoadLevel == ServerWorldLoadLevel.HIGH) {
      return 0.5;
    }
    if (this.serverWorldLoadLevel == ServerWorldLoadLevel.VERY_HIGH) {
      return 0.4;
    }
    return 1.0;
  }

  public double getServerLoadMultiplier() {
    if (hasHighServerWorldLoad()) {
      return 0.5;
    } else if (hasNormalServerWorldLoad()) {
      return 1.0;
    } else if (hasLowServerWorldLoad()) {
      return 1.5;
    }
    return 1.0;
  }

  public boolean hasHighServerWorldLoad() {
    return (this.serverWorldLoadLevel == ServerWorldLoadLevel.MEDIUM
        || this.serverWorldLoadLevel == ServerWorldLoadLevel.HIGH
        || this.serverWorldLoadLevel == ServerWorldLoadLevel.VERY_HIGH);
  }

  public boolean hasNormalServerWorldLoad() {
    return (this.serverWorldLoadLevel == ServerWorldLoadLevel.NORMAL);
  }

  public boolean hasLowServerWorldLoad() {
    return (this.serverWorldLoadLevel == ServerWorldLoadLevel.VERY_LOW
        || this.serverWorldLoadLevel == ServerWorldLoadLevel.LOW);
  }

  public boolean hasChanged() {
    return this.serverWorldLoadLevel == this.lastServerWorldLoadLevel;
  }
}
