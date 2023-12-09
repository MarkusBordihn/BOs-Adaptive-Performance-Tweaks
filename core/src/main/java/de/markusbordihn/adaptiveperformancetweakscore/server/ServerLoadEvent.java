/**
 * Copyright 2021 Markus Bordihn
 *
 * <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * <p>The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * <p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.adaptiveperformancetweakscore.server;

import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLoad.ServerLoadLevel;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.eventbus.api.Event;

public class ServerLoadEvent extends Event {

  private Dist dist;
  private ServerLoadLevel lastServerLoadLevel = ServerLoadLevel.NORMAL;
  private ServerLoadLevel serverLoadLevel = ServerLoadLevel.NORMAL;
  private double avgTickTime = 50.0;
  private double lastAvgTickTime = 45.0;

  public ServerLoadEvent(
      ServerLoadLevel currentServerLoad,
      ServerLoadLevel lastServerLoad,
      double avgTickTime,
      double lastAvgTickTim,
      Dist dist) {
    super();
    this.avgTickTime = avgTickTime;
    this.dist = dist;
    this.lastAvgTickTime = lastAvgTickTim;
    this.lastServerLoadLevel = lastServerLoad;
    this.serverLoadLevel = currentServerLoad;
  }

  public boolean isDedicatedServer() {
    return this.dist.isDedicatedServer();
  }

  public boolean isClient() {
    return this.dist.isClient();
  }

  public double getAvgTickTime() {
    return this.avgTickTime;
  }

  public double getLastAvgTickTime() {
    return this.lastAvgTickTime;
  }

  public ServerLoadLevel getServerLoad() {
    return serverLoadLevel;
  }

  public ServerLoadLevel getLastServerLoad() {
    return lastServerLoadLevel;
  }

  public boolean hasVeryHighServerLoad() {
    return this.serverLoadLevel == ServerLoadLevel.VERY_HIGH;
  }

  public boolean hasHighServerLoad() {
    return (this.serverLoadLevel == ServerLoadLevel.MEDIUM
        || this.serverLoadLevel == ServerLoadLevel.HIGH
        || this.serverLoadLevel == ServerLoadLevel.VERY_HIGH);
  }

  public boolean hasNormalServerLoad() {
    return (this.serverLoadLevel == ServerLoadLevel.NORMAL);
  }

  public boolean hasLowServerLoad() {
    return (this.serverLoadLevel == ServerLoadLevel.VERY_LOW
        || this.serverLoadLevel == ServerLoadLevel.LOW);
  }

  public boolean hasChanged() {
    return this.serverLoadLevel != this.lastServerLoadLevel;
  }
}
