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

package de.markusbordihn.adaptiveperformancetweaksitems.entity;

import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Entity.RemovalReason;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;

import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.event.server.ServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.entity.CoreItemEntityManager;
import de.markusbordihn.adaptiveperformancetweakscore.message.WarnMessages;
import de.markusbordihn.adaptiveperformancetweakscore.server.OptimizationEvent;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaksitems.Constants;
import de.markusbordihn.adaptiveperformancetweaksitems.config.CommonConfig;

@EventBusSubscriber
public class ItemEntityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static Integer maxNumberOfItems = COMMON.maxNumberOfItems.get();
  private static Integer maxNumberOfItemsPerType = COMMON.maxNumberOfItemsPerType.get();
  private static boolean optimizeItems = COMMON.optimizeItems.get();
  private static int itemClusterRange = COMMON.itemsClusterRange.get();

  private static Map<String, Set<ItemEntity>> itemTypeEntityMap = new ConcurrentHashMap<>();
  private static Map<String, Set<ItemEntity>> itemWorldEntityMap = new ConcurrentHashMap<>();
  private static boolean hasHighServerLoad = false;
  private static boolean needsOptimization = false;

  private static short ticks = 0;
  private static final short VERIFICATION_TICK = 30 * 20;

  protected ItemEntityManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    // Reset cache to avoid side effects.
    itemTypeEntityMap = new ConcurrentHashMap<>();
    itemWorldEntityMap = new ConcurrentHashMap<>();

    // Re-load config options.
    itemClusterRange = COMMON.itemsClusterRange.get();
    maxNumberOfItems = COMMON.maxNumberOfItems.get();
    maxNumberOfItemsPerType = COMMON.maxNumberOfItemsPerType.get();
    optimizeItems = COMMON.optimizeItems.get();

    // Show additional messages, if needed.
    if (maxNumberOfItems < maxNumberOfItemsPerType) {
      maxNumberOfItems = maxNumberOfItemsPerType * 2;
      log.error(
          "Max number of items could not be lower than max. number of items per type, using {} for maxNumberOfItems instead!",
          maxNumberOfItems);
    }
    if (optimizeItems) {
      log.info("Max number of Items allowed per world: {} / per type: {}", maxNumberOfItems,
          maxNumberOfItemsPerType);
      log.info("Enable clustering of items with a radius of {} blocks.", itemClusterRange);

      // Additional checks for conflicting mods.
      if (CoreConstants.GET_IT_TOGETHER_LOADED) {
        log.warn(() -> WarnMessages.conflictingFeaturesModWarning(
            CoreConstants.GET_IT_TOGETHER_NAME, "clusters items in a specific radius"));
      }

    } else {
      log.info("Item Optimization is disabled!");
    }
  }

  @SubscribeEvent
  public static void handleClientServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.END && ticks++ >= VERIFICATION_TICK) {
      verifyEntities();
      ticks = 0;
    }
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
      if (optimizeItems) {
        optimizeItems();
      }
      needsOptimization = false;
    }
  }

  @SubscribeEvent(priority = EventPriority.LOW)
  public static void handleItemEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    // Ignore client side world and if event is canceled.
    Level level = event.getWorld();
    if (level.isClientSide || event.isCanceled()) {
      return;
    }

    // Ignore everything else besides Items or items which are removed.
    Entity entity = event.getEntity();
    if (!(entity instanceof ItemEntity) || entity.isRemoved()) {
      return;
    }

    // Make sure the Item is relevant for our use case.
    ItemEntity itemEntity = (ItemEntity) entity;
    if (!CoreItemEntityManager.isRelevantItemEntity(itemEntity)) {
      return;
    }

    // All items has the entity minecraft.item, so we are using the translation key
    // to better distinguish the different types of items and minecraft.item as backup.
    ResourceLocation itemRegistryName = itemEntity.getItem().getItem().getRegistryName();
    String itemName =
        itemRegistryName != null ? itemRegistryName.toString() : itemEntity.getEncodeId();

    // Get world name and start processing of data
    String levelName = level.dimension().location().toString();
    if (log.isDebugEnabled()) {
      log.debug("[Item joined {}] {} {}", levelName, itemName,
          itemEntity.getDisplayName().getString());
    }

    // Check if items could be merged with other items
    String itemTypeEntityMapKey = '[' + levelName + ']' + itemName;
    itemTypeEntityMap.computeIfAbsent(itemTypeEntityMapKey, k -> new LinkedHashSet<>());
    Set<ItemEntity> itemTypeEntities = itemTypeEntityMap.get(itemTypeEntityMapKey);
    if (optimizeItems) {
      ItemStack itemStack = itemEntity.getItem();
      if (itemStack != null && itemStack.isStackable()
          && itemStack.getCount() < itemStack.getMaxStackSize()
          && itemStack.getMaxStackSize() > 1) {
        // Get basic information about the current item.
        int x = (int) itemEntity.getX();
        int y = (int) itemEntity.getY();
        int z = (int) itemEntity.getZ();
        int xStart = x - itemClusterRange;
        int yStart = y - itemClusterRange;
        int zStart = z - itemClusterRange;
        int xEnd = x + itemClusterRange;
        int yEnd = y + itemClusterRange;
        int zEnd = z + itemClusterRange;
        boolean itemCanSeeSky = level.canSeeSky(itemEntity.blockPosition());

        // Compare information with known items.
        Set<ItemEntity> itemEntities = new HashSet<>(itemTypeEntities);
        Iterator<ItemEntity> itemEntitiesIterator = itemEntities.iterator();
        while (itemEntitiesIterator.hasNext()) {
          ItemEntity existingItemEntity = itemEntitiesIterator.next();
          int xSub = (int) existingItemEntity.getX();
          int ySub = (int) existingItemEntity.getY();
          int zSub = (int) existingItemEntity.getZ();
          boolean existingItemCanSeeSky = level.canSeeSky(existingItemEntity.blockPosition());
          ItemStack existingItemStack = existingItemEntity.getItem();

          // Check if they are in an equal position, if both could see the sky, ignore the y values.
          if (itemEntity.getId() != existingItemEntity.getId() && existingItemEntity.isAlive()
              && ItemEntity.areMergable(itemStack, existingItemStack)
              && (xStart < xSub && xSub < xEnd)
              && ((itemCanSeeSky && existingItemCanSeeSky) || (yStart < ySub && ySub < yEnd))
              && (zStart < zSub && zSub < zEnd)) {
            if (log.isDebugEnabled()) {
              int newItemCount = existingItemStack.getCount() + itemStack.getCount();
              log.debug("[Merge Item] {} + {} = {} items", itemEntity, existingItemEntity,
                  newItemCount);
            }
            ItemStack combinedItemStack = ItemEntity.merge(existingItemStack, itemStack, 64);
            existingItemEntity.setItem(combinedItemStack);
            return;
          }
        }
      }
    }

    // Storing items per world regardless of item type
    itemWorldEntityMap.computeIfAbsent(levelName, k -> new LinkedHashSet<>());
    Set<ItemEntity> itemWorldEntities = itemWorldEntityMap.get(levelName);
    itemWorldEntities.add(itemEntity);

    // Optimized items per world regardless of type if they exceeding maxNumberOfItems limit.
    if (optimizeItems) {
      int numberOfItemWorldEntities = itemWorldEntities.size();
      if (numberOfItemWorldEntities > maxNumberOfItems) {
        ItemEntity firsItemWorldEntity = itemWorldEntities.iterator().next();
        log.debug("[Item World Limit {}] Removing item {}", numberOfItemWorldEntities,
            firsItemWorldEntity);
        firsItemWorldEntity.remove(RemovalReason.DISCARDED);
        itemWorldEntities.remove(firsItemWorldEntity);
        Set<ItemEntity> itemEntities = itemTypeEntityMap.get('[' + levelName + ']' + itemName);
        if (itemEntities != null) {
          itemEntities.remove(firsItemWorldEntity);
        }
      }
    }

    // Storing items per type and world
    itemTypeEntities.add(itemEntity);

    // Optimized items per type and world if exceeding numberOfItemsPerType limit.
    if (optimizeItems) {
      int numberOfItemEntities = itemTypeEntities.size();
      if (numberOfItemEntities > maxNumberOfItemsPerType) {
        ItemEntity firstItemEntity = itemTypeEntities.iterator().next();
        log.debug("[Item Type Limit {}] Removing item {}", numberOfItemEntities, firstItemEntity);
        firstItemEntity.remove(RemovalReason.DISCARDED);
        itemTypeEntities.remove(firstItemEntity);
        itemWorldEntities.remove(firstItemEntity);
      }
    }

    // Lower the lifespan by 0.6 of the item, if the item has any lifespan and
    // server has high server load.
    if (optimizeItems && hasHighServerLoad && itemEntity.lifespan > 1) {
      itemEntity.lifespan = (int) Math.round(itemEntity.lifespan * 0.6);
    }
  }

  @SubscribeEvent(priority = EventPriority.LOW)
  public static void handleItemEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    // Ignore client side world.
    Level level = event.getWorld();
    if (level.isClientSide) {
      return;
    }

    Entity entity = event.getEntity();
    if (!(entity instanceof ItemEntity)) {
      return;
    }

    // Make sure the Item is relevant for our use case.
    ItemEntity itemEntity = (ItemEntity) entity;
    if (!CoreItemEntityManager.isRelevantItemEntity(itemEntity)) {
      return;
    }

    // All items has the entity minecraft.item, so we are using the translation key
    // to better distinguish the different types of items and minecraft.item as backup.
    ResourceLocation itemRegistryName = itemEntity.getItem().getItem().getRegistryName();
    String itemName =
        itemRegistryName != null ? itemRegistryName.toString() : itemEntity.getEncodeId();

    // Get world name and start processing of data
    String levelName = level.dimension().location().toString();

    // Remove item from world map.
    Set<ItemEntity> itemWorldEntities = itemWorldEntityMap.get(levelName);
    if (itemWorldEntities != null) {
      itemWorldEntities.remove(itemEntity);
    }

    // Remove item from world type map.
    Set<ItemEntity> itemTypeEntities = itemTypeEntityMap.get('[' + levelName + ']' + itemName);
    if (itemTypeEntities != null) {
      itemTypeEntities.remove(itemEntity);
      if (log.isDebugEnabled()) {
        log.debug("[Item leaved {}] {} {}.", levelName, itemName,
            itemEntity.getDisplayName().getString());
      }
    } else {
      log.warn("Item {} {} in {} was not tracked by item entity manager!", itemName,
          itemEntity.getDisplayName().getString(), levelName);
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
            .sorted(Comparator.comparing(ItemEntity::getId)).toList();
        for (int i = 0; i < sortedItemWorldEntitiesValues.size()
            - maxNumberOfOptimizedWorldItems; i++) {
          ItemEntity itemEntity = sortedItemWorldEntitiesValues.get(i);
          if (itemEntity.isAddedToWorld()) {
            itemEntity.remove(RemovalReason.DISCARDED);
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
            .sorted(Comparator.comparing(ItemEntity::getId)).toList();
        for (int i = 0; i < sortedItemTypeEntitiesValues.size()
            - maxNumberOfOptimizedTypeItems; i++) {
          ItemEntity itemEntity = sortedItemTypeEntitiesValues.get(i);
          if (itemEntity.isAddedToWorld()) {
            itemEntity.remove(RemovalReason.DISCARDED);
            numberOfRemovedItems++;
          }
        }
      }
    }

    if (numberOfRemovedItems > 0) {
      log.debug("[Optimized Items] Removed {} items from all worlds!", numberOfRemovedItems);
    } else {
      log.debug("[Optimized Items] Found no relevant items which should be removed!");
    }
    return numberOfRemovedItems;
  }

  public static Integer getNumberOfItems(String levelName, String itemName) {
    Set<ItemEntity> itemEntities = itemTypeEntityMap.get('[' + levelName + ']' + itemName);
    if (itemEntities == null) {
      return 0;
    }
    return itemEntities.size();
  }

  public static Map<String, Set<ItemEntity>> getItemTypeEntityMap() {
    return itemTypeEntityMap;
  }

  public static void verifyEntities() {
    int removedEntries = 0;

    // Verify Entities in overall overview
    for (Set<ItemEntity> entities : itemTypeEntityMap.values()) {
      Iterator<ItemEntity> entityIterator = entities.iterator();
      while (entityIterator.hasNext()) {
        Entity entity = entityIterator.next();
        if (entity != null && entity.isRemoved()) {
          entityIterator.remove();
          removedEntries++;
        }
      }
    }

    // Verify Entities from world specific overview
    for (Set<ItemEntity> entities : itemWorldEntityMap.values()) {
      Iterator<ItemEntity> entityIterator = entities.iterator();
      while (entityIterator.hasNext()) {
        Entity entity = entityIterator.next();
        if (entity != null && entity.isRemoved()) {
          entityIterator.remove();
          removedEntries++;
        }
      }
    }

    if (removedEntries > 0) {
      log.debug("[Verification] Removed {} entries", removedEntries);
    }
  }
}
