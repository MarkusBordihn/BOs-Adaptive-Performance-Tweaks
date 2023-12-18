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

package de.markusbordihn.adaptiveperformancetweakscore.entity;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import net.minecraft.world.entity.item.ItemEntity;

public class CoreItemEntityManager {

  protected CoreItemEntityManager() {}

  @SuppressWarnings("java:S1126")
  public static boolean isRelevantItemEntity(ItemEntity itemEntity) {
    if (itemEntity == null || itemEntity.isRemoved() || itemEntity.hasCustomName()) {
      return false;
    }

    // All items has the entity minecraft.item, so we are using the translation key
    // to better distinguish the different types of items and minecraft.item as backup.
    String itemName = itemEntity.getItem().getItem().getDescriptionId();
    if (itemName == null || itemName.isEmpty()) {
      // Use encode id as item name.
      itemName = itemEntity.getEncodeId();
      if (itemName == null || itemName.isEmpty()) {
        return false;
      }
    }

    // Ignore dropped air blocks because these are not used at all by the players.
    // Warning: Removing the air block is a bad idea, because it's used to pre-reserve the space.
    if (itemName.equals("block.minecraft.air") || itemName.equals("minecraft:air")) {
      return false;
    }

    // Ignore specific entities from mods which implements their own spawn handling, logic or
    // using pseudo mobs for interactive blocks.
    if (CoreConstants.CREATE_LOADED && itemName.startsWith(CoreConstants.CREATE_MOD)) {
      return false;
    }

    return true;
  }
}
