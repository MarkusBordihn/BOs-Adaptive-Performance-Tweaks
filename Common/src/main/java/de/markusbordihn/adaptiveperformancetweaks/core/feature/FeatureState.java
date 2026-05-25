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

package de.markusbordihn.adaptiveperformancetweaks.core.feature;

import java.util.Locale;

/**
 * Three-way feature state for mod-conflict-aware feature toggling.
 *
 * <ul>
 *   <li>{@link #AUTO} (default) — enable the feature unless a conflicting mod is detected at
 *       startup. When a conflict is found the feature is automatically disabled with a warning.
 *   <li>{@link #ENABLED} — force the feature on; a warning is logged if a conflicting mod is
 *       present so the admin can decide whether to keep both active.
 *   <li>{@link #DISABLED} — force the feature off regardless of installed mods.
 * </ul>
 *
 * <p>Config values accepted (case-insensitive): {@code auto}, {@code true}/{@code enabled}/{@code
 * on}, {@code false}/{@code disabled}/{@code off}.
 */
public enum FeatureState {
  AUTO,
  ENABLED,
  DISABLED;

  public static FeatureState parse(String value) {
    return switch (value.trim().toLowerCase(Locale.ROOT)) {
      case "true", "enabled", "on" -> ENABLED;
      case "false", "disabled", "off" -> DISABLED;
      default -> AUTO;
    };
  }
}
