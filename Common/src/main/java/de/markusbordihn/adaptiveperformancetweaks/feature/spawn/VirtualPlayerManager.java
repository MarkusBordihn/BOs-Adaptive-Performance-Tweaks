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

package de.markusbordihn.adaptiveperformancetweaks.feature.spawn;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.phys.Vec3;

public final class VirtualPlayerManager {

  private static final Map<String, List<Vec3>> positions = new HashMap<>();

  private VirtualPlayerManager() {
  }

  public static void add(ServerLevel level, Vec3 position) {
    positions.computeIfAbsent(dimensionId(level), k -> new ArrayList<>()).add(position);
  }

  public static void remove(ServerLevel level, Vec3 position) {
    List<Vec3> list = positions.get(dimensionId(level));
    if (list != null) {
      list.remove(position);
    }
  }

  public static void clearAll() {
    positions.clear();
  }

  public static Vec3 nearest(ServerLevel level, Vec3 origin) {
    List<Vec3> list = positions.getOrDefault(dimensionId(level), Collections.emptyList());
    Vec3 nearest = null;
    double nearestDistSq = Double.MAX_VALUE;
    for (Vec3 pos : list) {
      double distSq = origin.distanceToSqr(pos);
      if (distSq < nearestDistSq) {
        nearestDistSq = distSq;
        nearest = pos;
      }
    }

    return nearest;
  }

  private static String dimensionId(ServerLevel level) {
    return level.dimension().identifier().toString();
  }
}
