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

package de.markusbordihn.adaptiveperformancetweaks.entity;

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Set;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;

public final class EntityJoinInterceptor {

  private static final ThreadLocal<Set<Entity>> PENDING_ENTITIES =
    ThreadLocal.withInitial(() -> Collections.newSetFromMap(new IdentityHashMap<>()));

  private EntityJoinInterceptor() {
  }

  public static boolean denyOrMarkPending(Entity entity, ServerLevel level) {
    if (CommonEntityEventHandler.shouldDenyEntityJoinLevel(entity, level)) {
      return true;
    }
    PENDING_ENTITIES.get().add(entity);
    return false;
  }

  public static boolean consumePending(Entity entity) {
    return PENDING_ENTITIES.get().remove(entity);
  }

  public static void clearPending(Entity entity) {
    PENDING_ENTITIES.get().remove(entity);
  }
}
