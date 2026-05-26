/*
 * Copyright 2024 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweaks.core.debug;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import java.util.Locale;

public enum DebugModule {
  AI(Constants.LOG_NAME_AI, "AI goal throttling for mobs"),
  CHUNKS(Constants.LOG_NAME_CHUNKS, "Chunk generation throttling"),
  CORE(Constants.LOG_NAME, "Core tracking and configuration"),
  DISTANCE(Constants.LOG_NAME_DISTANCE, "Adaptive view/sim distance"),
  ENTITIES(Constants.LOG_NAME_ENTITIES, "Entity join/leave tracking"),
  GAMERULES(Constants.LOG_NAME_GAMERULES, "Dynamic gamerule adjustments"),
  ITEMS(Constants.LOG_NAME_ITEMS, "Item entity and XP orb clustering"),
  PLAYERS(Constants.LOG_NAME_PLAYERS, "Player login protection and damage"),
  SPAWN(Constants.LOG_NAME_SPAWN, "Mob spawn throttling and presets");

  private final String loggerName;
  private final String description;

  DebugModule(String loggerName, String description) {
    this.loggerName = loggerName;
    this.description = description;
  }

  public String getId() {
    return this.name().toLowerCase(Locale.ROOT);
  }

  public String getLoggerName() {
    return this.loggerName;
  }

  public String getDescription() {
    return this.description;
  }
}
