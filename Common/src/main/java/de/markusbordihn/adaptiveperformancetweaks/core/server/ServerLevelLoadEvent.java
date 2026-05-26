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

package de.markusbordihn.adaptiveperformancetweaks.core.server;

import net.minecraft.server.level.ServerLevel;

public class ServerLevelLoadEvent {

  private final ServerLevel serverLevel;
  private final ServerLoadLevel loadLevel;
  private final ServerLoadLevel lastLoadLevel;
  private final double avgTickTime;
  private final double lastAvgTickTime;

  public ServerLevelLoadEvent(
    ServerLevel serverLevel,
    ServerLoadLevel loadLevel,
    ServerLoadLevel lastLoadLevel,
    double avgTickTime,
    double lastAvgTickTime) {
    this.serverLevel = serverLevel;
    this.loadLevel = loadLevel;
    this.lastLoadLevel = lastLoadLevel;
    this.avgTickTime = avgTickTime;
    this.lastAvgTickTime = lastAvgTickTime;
  }

  public ServerLevel getServerLevel() {
    return this.serverLevel;
  }

  public ServerLoadLevel getLoadLevel() {
    return this.loadLevel;
  }

  public ServerLoadLevel getLastLoadLevel() {
    return this.lastLoadLevel;
  }

  public double getAvgTickTime() {
    return this.avgTickTime;
  }

  public double getLastAvgTickTime() {
    return this.lastAvgTickTime;
  }

  public boolean hasChanged() {
    return this.loadLevel != this.lastLoadLevel;
  }
}
