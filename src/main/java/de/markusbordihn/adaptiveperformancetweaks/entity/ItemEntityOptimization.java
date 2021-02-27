/**
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

package de.markusbordihn.adaptiveperformancetweaks.entity;

import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

import net.minecraft.entity.item.ItemEntity;

import de.markusbordihn.adaptiveperformancetweaks.Optimization;

public class ItemEntityOptimization extends Optimization {

  private static Integer maxNumberOfItems = COMMON.maxNumberOfItems.get();
  private static Set<ItemEntity> itemEntityList = new LinkedHashSet<>();

  public static void cleanupItems() {
    for (int i = itemEntityList.size(); i > maxNumberOfItems; i--) {
      if (!itemEntityList.isEmpty()) {
        Optional<ItemEntity> firstItemEntity = itemEntityList.stream().findFirst();
        if (firstItemEntity.isPresent()) {
          ItemEntity itemEntity = firstItemEntity.get();
          log.debug("Removing item {}", itemEntity);
          itemEntity.remove(false);
          itemEntityList.remove(itemEntity);
        }
      }
    }
  }

  public static void addItem(ItemEntity itemEntity) {
    itemEntityList.add(itemEntity);
    if (itemEntityList.size() > maxNumberOfItems) {
      cleanupItems();
    }
  }

  public static void removeItem(ItemEntity itemEntity) {
    itemEntityList.remove(itemEntity);
  }

}
