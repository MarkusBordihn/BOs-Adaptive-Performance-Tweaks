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

public class ServerLoadEvent {

  private final ServerLoadLevel serverLoadLevel;
  private final ServerLoadLevel lastServerLoadLevel;
  private final double avgTickTime;
  private final double lastAvgTickTime;

  public ServerLoadEvent(
    ServerLoadLevel serverLoadLevel,
    ServerLoadLevel lastServerLoadLevel,
    double avgTickTime,
    double lastAvgTickTime) {
    this.serverLoadLevel = serverLoadLevel;
    this.lastServerLoadLevel = lastServerLoadLevel;
    this.avgTickTime = avgTickTime;
    this.lastAvgTickTime = lastAvgTickTime;
  }

  public ServerLoadLevel getServerLoadLevel() {
    return this.serverLoadLevel;
  }

  public double getAvgTickTime() {
    return this.avgTickTime;
  }

  public boolean hasChanged() {
    return this.serverLoadLevel != this.lastServerLoadLevel;
  }

  public boolean hasVeryHighServerLoad() {
    return this.serverLoadLevel == ServerLoadLevel.VERY_HIGH;
  }

  public boolean hasHighServerLoad() {
    return this.serverLoadLevel.isHigh();
  }

}
