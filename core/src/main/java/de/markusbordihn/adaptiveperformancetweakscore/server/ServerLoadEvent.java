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

import net.minecraftforge.eventbus.api.Event;

import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLoad.ServerLoadLevel;

public class ServerLoadEvent extends Event {

  private ServerLoadLevel serverLoadLevel = ServerLoadLevel.NORMAL;
  private ServerLoadLevel lastServerLoadLevel = ServerLoadLevel.NORMAL;

  public ServerLoadEvent(ServerLoadLevel currentServerLoad, ServerLoadLevel lastServerLoad) {
    super();
    this.serverLoadLevel = currentServerLoad;
    this.lastServerLoadLevel = lastServerLoad;
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
    return this.serverLoadLevel == this.lastServerLoadLevel;
  }
}
