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
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoadEvent;

@EventBusSubscriber
public class ItemEntityManager extends Manager {

  private static Map<String, Set<ItemEntity>> itemEntityMap = new HashMap<>();
  private static Integer maxNumberOfItems = COMMON.maxNumberOfItems.get();
  private static Integer maxNumberOfItemsPerType = COMMON.maxNumberOfItemsPerType.get();
  private static Integer itemCounter = 0;
  private static boolean hasHighServerLoad = false;
  private static boolean cleanUpRunning = false;

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
    log.info("Max number of Items allowed per world: {} / per type: {}", maxNumberOfItems,
        maxNumberOfItemsPerType);
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    hasHighServerLoad = event.hasHighServerLoad();
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleItemEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    Entity entity = event.getEntity();
    if (!(entity instanceof ItemEntity)) {
      return;
    }

    // All items has the entity minecraft.item, so we are using the translation key to better
    // distinguish the different types of items and minecraft.item as backup.
    ItemEntity itemEntity = (ItemEntity) entity;
    String itemName = itemEntity.getItem().getTranslationKey();
    if (itemName == null) {
      itemName = itemEntity.getEntityString();
    }
    String worldName = itemEntity.getEntityWorld().getDimensionKey().getLocation().toString();
    Set<ItemEntity> itemEntities = itemEntityMap.get('[' + worldName + ']' + itemName);
    if (itemEntities == null) {
      itemEntities = new LinkedHashSet<>();
      itemEntityMap.put('[' + worldName + ']' + itemName, itemEntities);
    }
    itemEntities.add(itemEntity);
    int numberOfItemEntities = itemEntities.size();

    if (log.isDebugEnabled()) {
      log.debug("Item {} {} ({}) joined {}.", itemName, itemEntity.getDisplayName().getString(),
          numberOfItemEntities, worldName);
    }

    // Automatic cleanup if we a reaching the maxNumberOfItemsPerType.
    if (numberOfItemEntities > maxNumberOfItemsPerType) {
      ItemEntity firstItemEntity = itemEntities.iterator().next();
      log.debug("Removing item {}", firstItemEntity);
      firstItemEntity.remove(false);
      itemEntities.remove(firstItemEntity);
    }

    // Global Item counter to handle global number of items.
    if (itemCounter++ >= maxNumberOfItems) {
      cleanupItems(worldName);
    }

    // Lower the lifespan by 0.6 of the item, if the server has high server load.
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
    Set<ItemEntity> itemEntities =
        itemEntityMap.getOrDefault('[' + worldName + ']' + itemName, new LinkedHashSet<>());
    itemEntities.remove(itemEntity);
    if (itemCounter > 0) {
      itemCounter--;
    }
    if (log.isDebugEnabled()) {
      log.debug("Item {} {} ({}) leaved {}.", itemName, itemEntity.getDisplayName().getString(),
          itemEntities.size(), worldName);
    }
  }

  public static void cleanupItems(String worldName) {
    if (cleanUpRunning) {
      return;
    }
    cleanUpRunning= true;
    String worldNamePrefix = '[' + worldName + ']';
    // 1. Get relevant items for the world
    Set<ItemEntity> relevantItems = new HashSet<>();
    for (Map.Entry<String, Set<ItemEntity>> itemEntities : itemEntityMap.entrySet()) {
      if (itemEntities.getKey().startsWith(worldNamePrefix)) {
        relevantItems.addAll(itemEntities.getValue());
      }
    }
    // 2. Sort list by entityId in a sorted list.
    List<ItemEntity> sortedEntityList = relevantItems.stream()
        .sorted(Comparator.comparing(ItemEntity::getEntityId)).collect(Collectors.toList());
    // 3. Remove last added items
    for (int i = 0; i < sortedEntityList.size() - maxNumberOfItems; i++) {
      ItemEntity itemEntity = sortedEntityList.get(i);
      if (itemEntity != null && itemEntity.isAlive()) {
        log.debug("Removing item {}", itemEntity);
        itemEntity.remove(false);
      }
    }
    cleanUpRunning = false;
  }

  public static Integer getNumberOfItems(String worldName, String itemName) {
    return itemEntityMap.getOrDefault('[' + worldName + ']' + itemName, new LinkedHashSet<>())
        .size();
  }

  public static Map<String, Set<ItemEntity>> getItemEntityMap() {
    return itemEntityMap;
  }
}
