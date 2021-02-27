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

import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoad.ServerLoadLevel;
import net.minecraftforge.eventbus.api.Event;

public class ServerLoadEvent extends Event {
  private ServerLoadLevel currentServerLoadLevel = ServerLoadLevel.NORMAL;
  private ServerLoadLevel lastServerLoadLevel = ServerLoadLevel.NORMAL;

  public ServerLoadEvent(ServerLoadLevel currentServerLoad, ServerLoadLevel lastServerLoad) {
    super();
    this.currentServerLoadLevel = currentServerLoad;
    this.lastServerLoadLevel = lastServerLoad;
  }

  public ServerLoadLevel getServerLoad() {
    return currentServerLoadLevel;
  }

  public ServerLoadLevel getLastServerLoad() {
    return lastServerLoadLevel;
  }

  public boolean hasHighServerLoad() {
    return (this.currentServerLoadLevel == ServerLoadLevel.MEDIUM
        || this.currentServerLoadLevel == ServerLoadLevel.HIGH
        || this.currentServerLoadLevel == ServerLoadLevel.VERY_HIGH);
  }

  public boolean hasNormalServerLoad() {
    return (this.currentServerLoadLevel == ServerLoadLevel.NORMAL);
  }

  public boolean hasLowServerLoad() {
    return (this.currentServerLoadLevel == ServerLoadLevel.VERY_LOW
        || this.currentServerLoadLevel == ServerLoadLevel.LOW);
  }
}
