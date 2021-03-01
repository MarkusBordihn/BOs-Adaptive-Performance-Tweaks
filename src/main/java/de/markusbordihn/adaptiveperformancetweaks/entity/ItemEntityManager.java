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
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import net.minecraft.entity.item.ItemEntity;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoadEvent;

@EventBusSubscriber
public class ItemEntityManager extends Manager {

  private static Map<String, Set<ItemEntity>> itemEntityMap = new HashMap<>();
  private static Integer maxNumberOfItems = COMMON.maxNumberOfItems.get();
  private static boolean hasHighServerLoad = false;

  @SubscribeEvent
  public static void onServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    maxNumberOfItems = COMMON.maxNumberOfItems.get();
    log.info("Max number of Items allowed per world: {}", maxNumberOfItems);
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    hasHighServerLoad = event.hasHighServerLoad();
  }

  public static void handleItemEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    ItemEntity itemEntity = (ItemEntity) event.getEntity();
    String itemName = itemEntity.getEntityString();
    String itemDisplayName = itemEntity.getDisplayName().getString();
    String worldName = itemEntity.getEntityWorld().getDimensionKey().getLocation().toString();
    Set<ItemEntity> itemEntities = itemEntityMap.get(worldName + ':' + itemName);
    if (itemEntities == null) {
      itemEntities = new LinkedHashSet<>();
      itemEntityMap.put(worldName + ':' + itemName, itemEntities);
    }
    itemEntities.add(itemEntity);
    log.debug("Item {} {} ({}) joined {}.", itemName, itemDisplayName, itemEntities.size(),
        worldName);

    // Automatic cleanup if we a reaching the maxNumberOfItems.
    if (itemEntities.size() > maxNumberOfItems) {
      cleanupItems(worldName, itemName);
    }

    // Lower the lifespan by 0.6 of the item, if the server has high server load.
    if (hasHighServerLoad && itemEntity.lifespan > 1) {
      itemEntity.lifespan = (int) Math.round(itemEntity.lifespan * 0.6);
    }
  }

  public static void handleItemEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    ItemEntity itemEntity = (ItemEntity) event.getEntity();
    String itemName = itemEntity.getEntityString();
    String worldName = itemEntity.getEntityWorld().getDimensionKey().getLocation().toString();
    Set<ItemEntity> itemEntities =
        itemEntityMap.getOrDefault(worldName + ':' + itemName, new LinkedHashSet<>());
    itemEntities.remove(itemEntity);
    log.debug("Item {} ({}) leaved {}.", itemName, itemEntities.size(), worldName);
  }

  public static Integer getNumberOfItems(String worldName, String itemName) {
    return itemEntityMap.getOrDefault(worldName + ':' + itemName, new LinkedHashSet<>()).size();
  }

  public static void cleanupItems(String worldName, String itemName) {
    Set<ItemEntity> itemEntities =
        itemEntityMap.getOrDefault(worldName + ':' + itemName, new LinkedHashSet<>());
    if (itemEntities.isEmpty() || itemEntities.size() <=  maxNumberOfItems) {
      return;
    }
    for (int i = itemEntities.size(); i > maxNumberOfItems; i--) {
      Optional<ItemEntity> firstItemEntity = itemEntities.stream().findFirst();
      if (firstItemEntity.isPresent()) {
        ItemEntity itemEntity = firstItemEntity.get();
        log.debug("Removing item {}", itemEntity);
        itemEntity.remove(false);
        itemEntities.remove(itemEntity);
      }
    }
  }
}
