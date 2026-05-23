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

package de.markusbordihn.adaptiveperformancetweaks;

import java.nio.file.Path;
import java.nio.file.Paths;

public final class Constants {

  public static final String MOD_ID = "adaptive_performance_tweaks";
  public static final String MOD_NAME = "Adaptive Performance Tweaks";
  public static final String MOD_COMMAND = "aptweaks";
  public static final String ISSUE_REPORT =
    "https://github.com/MarkusBordihn/BOs-Adaptive-Performance-Tweaks/issues";
  public static final String LOG_NAME = "APTweaks";
  public static final String LOG_NAME_AI = MOD_NAME + ".ai";
  public static final String LOG_NAME_CHUNKS = MOD_NAME + ".chunks";
  public static final String LOG_NAME_DISTANCE = MOD_NAME + ".distance";
  public static final String LOG_NAME_ENTITIES = MOD_NAME + ".entities";
  public static final String LOG_NAME_GAMERULES = MOD_NAME + ".gamerules";
  public static final String LOG_NAME_ITEMS = MOD_NAME + ".items";
  public static final String LOG_NAME_PLAYERS = MOD_NAME + ".players";
  public static final String LOG_NAME_SPAWN = MOD_NAME + ".spawn";
  public static final String LOG_REGISTER_PREFIX = "Register " + MOD_NAME;

  public static Path GAME_DIR = Paths.get("").toAbsolutePath();
  public static Path CONFIG_DIR = GAME_DIR.resolve("config");

  private Constants() {
  }
}
