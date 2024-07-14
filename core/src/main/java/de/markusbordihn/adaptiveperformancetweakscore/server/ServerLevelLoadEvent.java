/*
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

import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLevelLoad.ServerLevelLoadLevel;
import net.minecraft.server.level.ServerLevel;
import net.minecraftforge.eventbus.api.Event;

public class ServerLevelLoadEvent extends Event {

  private final String serverLevelName;
  private ServerLevelLoadLevel lastServerLevelLoadLevel;
  private ServerLevelLoadLevel serverLevelLoadLevel;

  public ServerLevelLoadEvent(
      ServerLevel serverLevel,
      ServerLevelLoadLevel serverLevelLoadLevel,
      ServerLevelLoadLevel lastServerLevelLoadLevel,
      double avgTickTime,
      double lastAvgTickTim) {
    super();
    this.lastServerLevelLoadLevel = lastServerLevelLoadLevel;
    this.serverLevelLoadLevel = serverLevelLoadLevel;
    this.serverLevelName = serverLevel.getLevel().dimension().location().toString();
  }

  public String getServerLevelName() {
    return this.serverLevelName;
  }

  public ServerLevelLoadLevel getServerLevelLoadLevel() {
    return this.serverLevelLoadLevel;
  }

  public boolean hasChanged() {
    return this.serverLevelLoadLevel != this.lastServerLevelLoadLevel;
  }
}
