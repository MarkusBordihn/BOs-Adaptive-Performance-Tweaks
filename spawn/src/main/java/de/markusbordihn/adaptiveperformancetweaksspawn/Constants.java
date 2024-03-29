/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweaksspawn;

public final class Constants {

  // General Mod definitions
  public static final String LOG_NAME = "APTweaks(Spawn)";
  public static final String LOG_PREFIX = "[APTweaks:Spawn]";
  public static final String MOD_COMMAND = "aptweaks";
  public static final String MOD_ID = "adaptive_performance_tweaks_spawn";
  public static final String MOD_NAME = "Adaptive Performance Tweaks: Spawn";
  public static final String MODULE_NAME = "Spawn";
  // Config Descriptions
  public static final String CONFIG_LIST_PASSIVE_MOBS =
      "List of passive Mobs to optimize in the format [\"minecraft:bat\", \"minecraft:cat\", ..]";
  public static final String CONFIG_LIST_NEUTRAL_MOBS =
      "List of neutral Mobs to optimize in the format [\"minecraft:bee\", \"minecraft:wolf\", ..]";
  public static final String CONFIG_LIST_HOSTILE_MOBS =
      "List of hostile Mobs to optimize in the format [\"minecraft:blaze\", \"minecraft:cave_spider\", ..]";

  protected Constants() {}
}
