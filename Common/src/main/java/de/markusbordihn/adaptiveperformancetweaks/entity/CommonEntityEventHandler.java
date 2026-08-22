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

import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ArrowEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ExperienceOrbManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemEntityManager;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.projectile.arrow.AbstractArrow;
import net.minecraft.world.level.Level;

public final class CommonEntityEventHandler {

  private CommonEntityEventHandler() {
  }

  public static boolean handleEntityJoinLevel(Entity entity, Level level) {
    if (shouldDenyEntityJoinLevel(entity, level)) {
      return true;
    }

    trackEntityJoinLevel(entity, level);

    return false;
  }

  public static boolean shouldDenyEntityJoinLevel(Entity entity, Level level) {
    if (level.isClientSide()) {
      return false;
    }

    if (FeatureToggle.ITEMS.isEnabled() && entity instanceof ItemEntity itemEntity) {
      return ItemEntityManager.handleItemEntityJoinLevel(itemEntity, level);
    } else if (FeatureToggle.EXPERIENCE_ORBS.isEnabled()
      && entity instanceof ExperienceOrb orbEntity) {
      return ExperienceOrbManager.handleExperienceOrbJoinLevel(orbEntity, level);
    } else if (FeatureToggle.ARROWS.isEnabled() && entity instanceof AbstractArrow arrowEntity) {
      ArrowEntityManager.handleArrowJoinLevel(arrowEntity, level);
    }

    return false;
  }

  public static void trackEntityJoinLevel(Entity entity, Level level) {
    if (level.isClientSide()) {
      return;
    }

    CoreEntityManager.handleEntityJoinLevel(entity, false);
  }

  public static void handleEntityLeaveLevel(Entity entity, Level level) {
    if (level.isClientSide()) {
      return;
    }

    if (FeatureToggle.ITEMS.isEnabled() && entity instanceof ItemEntity itemEntity) {
      ItemEntityManager.handleItemEntityLeaveLevel(itemEntity, level);
    } else if (FeatureToggle.EXPERIENCE_ORBS.isEnabled()
      && entity instanceof ExperienceOrb orbEntity) {
      ExperienceOrbManager.handleExperienceOrbLeaveLevel(orbEntity, level);
    } else if (FeatureToggle.ARROWS.isEnabled() && entity instanceof AbstractArrow arrowEntity) {
      ArrowEntityManager.handleArrowLeaveLevel(arrowEntity, level);
    }
    CoreEntityManager.handleEntityLeaveLevel(entity, false);
  }

  public static void handleLivingDeath(Entity entity) {
    if (entity == null || entity.level().isClientSide()) {
      return;
    }

    CoreEntityManager.handleLivingDeath(entity, false);
  }
}
