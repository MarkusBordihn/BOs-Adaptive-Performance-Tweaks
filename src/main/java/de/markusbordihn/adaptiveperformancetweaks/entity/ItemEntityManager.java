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

import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.mutable.MutableInt;
import net.minecraft.entity.item.ItemEntity;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;

public class ItemEntityManager extends Manager {

  private static Map<String, MutableInt> itemEntityCounter = new HashMap<>();

  public static void handleItemEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    ItemEntity itemEntity = (ItemEntity) event.getEntity();
    String itemName = itemEntity.getEntityString();
    String worldName = itemEntity.getEntityWorld().getDimensionKey().getLocation().toString();
    incrementCounter(itemEntityCounter, worldName + ':' + itemName);
    ItemEntityOptimization.addItem(itemEntity);
    log.debug("Item {} ({}) joined {}.", itemName, getNumberOfItems(worldName, itemName), worldName);
  }

  public static void handleItemEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    ItemEntity itemEntity = (ItemEntity) event.getEntity();
    String itemName = itemEntity.getEntityString();
    String worldName = itemEntity.getEntityWorld().getDimensionKey().getLocation().toString();
    decrementCounter(itemEntityCounter, worldName + ':' + itemName);
    ItemEntityOptimization.removeItem(itemEntity);
    log.debug("Item {} ({}) leaved {}.", itemName, getNumberOfItems(worldName, itemName), worldName);
  }

  public static Integer getNumberOfItems(String worldName, String itemName) {
    return getCounter(itemEntityCounter, worldName + ':' + itemName);
  }

}
