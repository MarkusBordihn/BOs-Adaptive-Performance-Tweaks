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

import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.server.OptimizationEvent;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoadEvent;

@EventBusSubscriber
public class ItemEntityManager extends Manager {

  private static Map<String, Set<ItemEntity>> itemTypeEntityMap = new HashMap<>();
  private static Map<String, Set<ItemEntity>> itemWorldEntityMap = new HashMap<>();
  private static Integer maxNumberOfItems = COMMON.maxNumberOfItems.get();
  private static Integer maxNumberOfItemsPerType = COMMON.maxNumberOfItemsPerType.get();
  private static boolean hasHighServerLoad = false;
  private static boolean needsOptimization = false;

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    maxNumberOfItems = COMMON.maxNumberOfItems.get();
    maxNumberOfItemsPerType = COMMON.maxNumberOfItemsPerType.get();
    if (maxNumberOfItems < maxNumberOfItemsPerType) {
      maxNumberOfItems = maxNumberOfItemsPerType * 2;
      log.error(
          "Max number of items could not be lower than max. number of items per type, using {} for maxNumberOfItems instead!",
          maxNumberOfItems);
    }
    log.info("Max number of Items allowed per world: {} / per type: {}", maxNumberOfItems, maxNumberOfItemsPerType);
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    hasHighServerLoad = event.hasHighServerLoad();
    if (hasHighServerLoad) {
      needsOptimization = true;
    }
  }

  @SubscribeEvent
  public static void handleOptimizationEvent(OptimizationEvent event) {
    if (needsOptimization) {
      optimizeItems();
      needsOptimization = false;
    }
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleItemEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    Entity entity = event.getEntity();
    if (!(entity instanceof ItemEntity)) {
      return;
    }

    // All items has the entity minecraft.item, so we are using the translation key
    // to better
    // distinguish the different types of items and minecraft.item as backup.
    ItemEntity itemEntity = (ItemEntity) entity;
    String itemName = itemEntity.getItem().getTranslationKey();
    if (itemName == null) {
      itemName = itemEntity.getEntityString();
    }

    // Removed dropped air blocks because these are not used at all by the players.
    if (itemName.equals("block.minecraft.air")) {
      itemEntity.remove(false);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Get world name and start processing of data
    String worldName = itemEntity.getEntityWorld().getDimensionKey().getLocation().toString();
    if (log.isDebugEnabled()) {
      log.debug("Item {} {} joined {}.", itemName, itemEntity.getDisplayName().getString(), worldName);
    }

    // Handles items per world
    Set<ItemEntity> itemWorldEntities = itemWorldEntityMap.get(worldName);
    if (itemWorldEntities == null) {
      itemWorldEntities = new LinkedHashSet<>();
      itemWorldEntityMap.put(worldName, itemWorldEntities);
    }
    itemWorldEntities.add(itemEntity);

    int numberOfItemWorldEntities = itemWorldEntities.size();
    if (numberOfItemWorldEntities > maxNumberOfItems) {
      ItemEntity firsItemWorldEntity = itemWorldEntities.iterator().next();
      log.debug("[Item World Limit {}] Removing item {}", numberOfItemWorldEntities, firsItemWorldEntity);
      firsItemWorldEntity.remove(false);
      itemWorldEntities.remove(firsItemWorldEntity);
      itemTypeEntityMap.getOrDefault('[' + worldName + ']' + itemName, new LinkedHashSet<>())
          .remove(firsItemWorldEntity);
    }

    // Handles items per type and world
    Set<ItemEntity> itemTypeEntities = itemTypeEntityMap.get('[' + worldName + ']' + itemName);
    if (itemTypeEntities == null) {
      itemTypeEntities = new LinkedHashSet<>();
      itemTypeEntityMap.put('[' + worldName + ']' + itemName, itemTypeEntities);
    }
    itemTypeEntities.add(itemEntity);

    int numberOfItemEntities = itemTypeEntities.size();
    if (numberOfItemEntities > maxNumberOfItemsPerType) {
      ItemEntity firstItemEntity = itemTypeEntities.iterator().next();
      log.debug("[Item Type Limit {}] Removing item {}", numberOfItemEntities, firstItemEntity);
      firstItemEntity.remove(false);
      itemTypeEntities.remove(firstItemEntity);
      itemWorldEntities.remove(firstItemEntity);
    }

    // Lower the lifespan by 0.6 of the item, if the item has any lifespan and
    // server has high server load.
    if (hasHighServerLoad && itemEntity.lifespan > 1) {
      itemEntity.lifespan = (int) Math.round(itemEntity.lifespan * 0.6);
    }
  }

  @SubscribeEvent
  public static void handleItemEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    Entity entity = event.getEntity();
    if (!(entity instanceof ItemEntity)) {
      return;
    }

    ItemEntity itemEntity = (ItemEntity) entity;
    String itemName = itemEntity.getItem().getTranslationKey();
    if (itemName == null) {
      itemName = itemEntity.getEntityString();
    }
    String worldName = itemEntity.getEntityWorld().getDimensionKey().getLocation().toString();

    // Remove item from world map.
    itemWorldEntityMap.getOrDefault(worldName, new LinkedHashSet<>()).remove(itemEntity);

    // Remove item from world type map.
    itemTypeEntityMap.getOrDefault('[' + worldName + ']' + itemName, new LinkedHashSet<>()).remove(itemEntity);

    if (log.isDebugEnabled()) {
      log.debug("Item {} {} leaved {}.", itemName, itemEntity.getDisplayName().getString(), worldName);
    }
  }

  public static int optimizeItems() {
    log.debug("[Optimize Items] Received request to optimize items ...");
    int numberOfRemovedItems = 0;

    // Optimize Items by World
    int maxNumberOfOptimizedWorldItems = (int) Math.round(maxNumberOfItems * 0.9);
    for (Map.Entry<String, Set<ItemEntity>> itemWorldEntities : itemWorldEntityMap.entrySet()) {
      Set<ItemEntity> itemWorldEntitiesValues = itemWorldEntities.getValue();
      if (itemWorldEntitiesValues.size() > maxNumberOfOptimizedWorldItems) {
        List<ItemEntity> sortedItemWorldEntitiesValues = itemWorldEntitiesValues.stream()
            .sorted(Comparator.comparing(ItemEntity::getEntityId)).collect(Collectors.toList());
        for (int i = 0; i < sortedItemWorldEntitiesValues.size() - maxNumberOfOptimizedWorldItems; i++) {
          ItemEntity itemEntity = sortedItemWorldEntitiesValues.get(i);
          if (itemEntity.isAddedToWorld()) {
            itemEntity.remove(false);
            numberOfRemovedItems++;
          }
        }
      }
    }

    // Optimize Items by Type and World
    int maxNumberOfOptimizedTypeItems = (int) Math.round(maxNumberOfItemsPerType * 0.9);
    for (Map.Entry<String, Set<ItemEntity>> itemTypeEntities : itemTypeEntityMap.entrySet()) {
      Set<ItemEntity> itemTypeEntitiesValues = itemTypeEntities.getValue();
      if (itemTypeEntitiesValues.size() > maxNumberOfOptimizedTypeItems) {
        List<ItemEntity> sortedItemTypeEntitiesValues = itemTypeEntitiesValues.stream()
            .sorted(Comparator.comparing(ItemEntity::getEntityId)).collect(Collectors.toList());
        for (int i = 0; i < sortedItemTypeEntitiesValues.size() - maxNumberOfOptimizedTypeItems; i++) {
          ItemEntity itemEntity = sortedItemTypeEntitiesValues.get(i);
          if (itemEntity.isAddedToWorld()) {
            itemEntity.remove(false);
            numberOfRemovedItems++;
          }
        }
      }
    }
    if (numberOfRemovedItems > 0) {
      log.debug("[Optimized Items] Removed {} items from all worlds!", numberOfRemovedItems);
    } else {
      log.debug("[Optimized Items] Found no relevant items which should be removed!", numberOfRemovedItems);
    }
    return numberOfRemovedItems;
  }

  public static Integer getNumberOfItems(String worldName, String itemName) {
    return itemTypeEntityMap.getOrDefault('[' + worldName + ']' + itemName, new LinkedHashSet<>()).size();
  }

  public static Map<String, Set<ItemEntity>> getItemTypeEntityMap() {
    return itemTypeEntityMap;
  }
}
