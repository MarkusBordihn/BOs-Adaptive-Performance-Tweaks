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

package de.markusbordihn.adaptiveperformancetweaks.core.entity;

import java.util.Locale;

public enum TrackingCategory {
  TECHNICAL,
  VEHICLE_STRUCTURE,
  WORLD_EFFECT,
  MANAGED_LIVING,
  STORAGE_NETWORK,
  MANUAL_OVERRIDE,
  UNKNOWN;

  public static TrackingCategory fromSerializedName(String name) {
    if (name == null || name.isBlank()) {
      return UNKNOWN;
    }

    String normalized = name.trim().toLowerCase(Locale.ROOT);
    for (TrackingCategory category : values()) {
      if (category.getSerializedName().equals(normalized)) {
        return category;
      }
    }

    return UNKNOWN;
  }

  public String getSerializedName() {
    return this.name().toLowerCase(Locale.ROOT);
  }
}
