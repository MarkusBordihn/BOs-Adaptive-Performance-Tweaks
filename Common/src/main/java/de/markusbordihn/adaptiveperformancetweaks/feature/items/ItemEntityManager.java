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

package de.markusbordihn.adaptiveperformancetweaks.feature.items;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreItemEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListSet;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Entity.RemovalReason;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class ItemEntityManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_ITEMS);
  private static final int VERIFICATION_TICK = 30 * 20;

  private static Map<String, Set<ItemEntity>> itemTypeEntityMap = new ConcurrentHashMap<>();
  private static Map<String, Set<ItemEntity>> itemWorldEntityMap = new ConcurrentHashMap<>();
  private static boolean hasHighServerLoad = false;
  private static boolean hasItemsAllowList = false;
  private static boolean hasItemsDenyList = false;
  private static short ticks = 0;

  private ItemEntityManager() {
  }

  public static void handleServerAboutToStart() {
    resetState();
    hasItemsAllowList = !ItemsConfig.itemsAllowList.isEmpty();
    hasItemsDenyList = !ItemsConfig.itemsDenyList.isEmpty();

    if (ItemsConfig.optimizeItems) {
      log.info(
        "Item optimization enabled: maxPerType={}, maxPerWorld={}, clusterRange={}",
        ItemsConfig.maxNumberOfItemsPerType,
        ItemsConfig.maxNumberOfItems,
        ItemsConfig.itemsClusterRange);
    }
  }

  public static void handleServerStopping() {
    resetState();
  }

  public static int getTrackedItemEntityCount() {
    int total = 0;
    for (Set<ItemEntity> entities : itemWorldEntityMap.values()) {
      total += entities.size();
    }

    return total;
  }

  private static void resetState() {
    itemTypeEntityMap = new ConcurrentHashMap<>();
    itemWorldEntityMap = new ConcurrentHashMap<>();
    hasHighServerLoad = false;
    hasItemsAllowList = !ItemsConfig.itemsAllowList.isEmpty();
    hasItemsDenyList = !ItemsConfig.itemsDenyList.isEmpty();
    ticks = 0;
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    hasHighServerLoad = event.hasHighServerLoad() || event.hasVeryHighServerLoad();
  }

  public static void handleServerTick() {
    if (++ticks < VERIFICATION_TICK) {
      return;
    }

    ticks = 0;
    verifyEntities();
  }

  public static boolean handleItemEntityJoinLevel(ItemEntity itemEntity, Level level) {
    if (level.isClientSide || itemEntity.isRemoved()) {
      return false;
    }

    if (!CoreItemEntityManager.isRelevantItemEntity(itemEntity)) {
      return false;
    }

    String itemName = BuiltInRegistries.ITEM.getKey(itemEntity.getItem().getItem()).toString();

    if (hasItemsAllowList && !ItemsConfig.itemsAllowList.contains(itemName)) {
      log.debug("[Item Allow List] {} not on allow list, skipping", itemName);
      return false;
    }

    if (hasItemsDenyList && ItemsConfig.itemsDenyList.contains(itemName)) {
      log.debug("[Item Deny List] {} on deny list, skipping", itemName);
      return false;
    }

    String levelName = level.dimension().location().toString();
    String itemTypeKey = '[' + levelName + ']' + itemName;

    itemTypeEntityMap.computeIfAbsent(
      itemTypeKey, ignored -> new ConcurrentSkipListSet<>(Comparator.comparingInt(Entity::getId)));
    Set<ItemEntity> itemTypeEntities = itemTypeEntityMap.get(itemTypeKey);

    if (ItemsConfig.optimizeItems && tryMergeItemEntity(itemEntity, itemTypeEntities, level)) {
      PerformanceStats.itemsMerged++;
      return true;
    }

    itemWorldEntityMap.computeIfAbsent(
      levelName, ignored -> new ConcurrentSkipListSet<>(Comparator.comparingInt(Entity::getId)));
    Set<ItemEntity> itemWorldEntities = itemWorldEntityMap.get(levelName);
    itemWorldEntities.add(itemEntity);

    if (ItemsConfig.optimizeItems) {
      enforceWorldLimit(itemWorldEntities, levelName, itemTypeEntityMap);
    }

    itemTypeEntities.add(itemEntity);

    if (ItemsConfig.optimizeItems) {
      enforceTypeLimit(itemTypeEntities, itemWorldEntities);
    }

    return false;
  }

  public static void handleItemEntityLeaveLevel(ItemEntity itemEntity, Level level) {
    if (level.isClientSide) {
      return;
    }
    String levelName = level.dimension().location().toString();

    Set<ItemEntity> itemWorldEntities = itemWorldEntityMap.get(levelName);
    if (itemWorldEntities != null) {
      itemWorldEntities.remove(itemEntity);
    }

    String itemName = BuiltInRegistries.ITEM.getKey(itemEntity.getItem().getItem()).toString();
    String itemTypeKey = '[' + levelName + ']' + itemName;

    Set<ItemEntity> itemTypeEntities = itemTypeEntityMap.get(itemTypeKey);
    if (itemTypeEntities != null) {
      itemTypeEntities.remove(itemEntity);
    }
  }

  private static boolean tryMergeItemEntity(
    ItemEntity itemEntity, Set<ItemEntity> itemTypeEntities, Level level) {
    if (itemTypeEntities.isEmpty()) {
      return false;
    }

    ItemStack itemStack = itemEntity.getItem();
    if (itemStack == null
      || !itemStack.isStackable()
      || itemStack.getCount() >= itemStack.getMaxStackSize()
      || itemStack.getMaxStackSize() <= 1) {
      return false;
    }

    int itemX = (int) itemEntity.getX();
    int itemY = (int) itemEntity.getY();
    int itemZ = (int) itemEntity.getZ();
    int range = ItemsConfig.itemsClusterRange;
    boolean canSeeSky = level.canSeeSky(itemEntity.blockPosition());

    Set<ItemEntity> snapshot = new HashSet<>(itemTypeEntities);
    for (ItemEntity existing : snapshot) {
      ItemStack existingStack = existing.getItem();
      if (existingStack == null || existingStack.isEmpty()) {
        continue;
      }
      int existingX = (int) existing.getX();
      int existingY = (int) existing.getY();
      int existingZ = (int) existing.getZ();
      boolean existingCanSeeSky = level.canSeeSky(existing.blockPosition());

      boolean inRange = (itemX - range < existingX && existingX < itemX + range)
        && ((canSeeSky && existingCanSeeSky) || (itemY - range < existingY
        && existingY < itemY + range))
        && (itemZ - range < existingZ && existingZ < itemZ + range);

      if (itemEntity.getId() != existing.getId()
        && existing.isAlive()
        && canMergeItemStacks(itemStack, existingStack)
        && existingStack.getCount() < existingStack.getMaxStackSize()
        && inRange) {
        log.debug("[Item Merge] {} x{} → stack at {},{},{}",
          BuiltInRegistries.ITEM.getKey(itemStack.getItem()), itemStack.getCount(),
          itemX, itemY, itemZ);
        mergeItemStacks(existingStack, itemStack);
        existing.setItem(existingStack);
        return true;
      }
    }

    return false;
  }

  private static boolean canMergeItemStacks(ItemStack incomingStack, ItemStack existingStack) {
    return !incomingStack.isEmpty()
      && !existingStack.isEmpty()
      && incomingStack.is(existingStack.getItem())
      && incomingStack.getDamageValue() == existingStack.getDamageValue()
      && incomingStack.getCount() < incomingStack.getMaxStackSize()
      && ItemStack.isSameItemSameTags(incomingStack, existingStack);
  }

  private static void mergeItemStacks(ItemStack target, ItemStack source) {
    int transferAmount = Math.min(source.getCount(), target.getMaxStackSize() - target.getCount());
    if (transferAmount > 0) {
      target.grow(transferAmount);
      source.shrink(transferAmount);
    }
  }

  private static void enforceWorldLimit(
    Set<ItemEntity> worldEntities,
    String levelName,
    Map<String, Set<ItemEntity>> typeMap) {
    int count = worldEntities.size();
    if (count <= ItemsConfig.maxNumberOfItems) {
      return;
    }

    Iterator<ItemEntity> iterator = worldEntities.iterator();
    if (!iterator.hasNext()) {
      return;
    }

    ItemEntity oldestItemEntity = iterator.next();
    String itemName = BuiltInRegistries.ITEM.getKey(oldestItemEntity.getItem().getItem())
      .toString();
    log.debug("[World Limit] {} at {} removed ({}/{})",
      itemName, oldestItemEntity.blockPosition(), count, ItemsConfig.maxNumberOfItems);
    oldestItemEntity.remove(RemovalReason.DISCARDED);
    PerformanceStats.itemsRemoved++;
    iterator.remove();
    String typeKey = '[' + levelName + ']' + itemName;
    Set<ItemEntity> typeEntities = typeMap.get(typeKey);
    if (typeEntities != null) {
      typeEntities.remove(oldestItemEntity);
    }
  }

  private static void enforceTypeLimit(Set<ItemEntity> typeEntities,
    Set<ItemEntity> worldEntities) {
    int count = typeEntities.size();
    if (count <= ItemsConfig.maxNumberOfItemsPerType) {
      return;
    }

    Iterator<ItemEntity> iterator = typeEntities.iterator();
    if (!iterator.hasNext()) {
      return;
    }

    ItemEntity oldestItemEntity = iterator.next();
    log.debug("[Type Limit] {} at {} removed ({}/{})",
      BuiltInRegistries.ITEM.getKey(oldestItemEntity.getItem().getItem()),
      oldestItemEntity.blockPosition(), count, ItemsConfig.maxNumberOfItemsPerType);
    oldestItemEntity.remove(RemovalReason.DISCARDED);
    PerformanceStats.itemsRemoved++;
    iterator.remove();
    worldEntities.remove(oldestItemEntity);
  }

  private static void verifyEntities() {
    for (Map.Entry<String, Set<ItemEntity>> entry : itemTypeEntityMap.entrySet()) {
      entry.getValue()
        .removeIf(entity -> entity == null || entity.isRemoved() || !entity.isAlive());
    }

    itemTypeEntityMap.entrySet().removeIf(entry -> entry.getValue().isEmpty());
    for (Map.Entry<String, Set<ItemEntity>> entry : itemWorldEntityMap.entrySet()) {
      entry.getValue()
        .removeIf(entity -> entity == null || entity.isRemoved() || !entity.isAlive());
    }

    itemWorldEntityMap.entrySet().removeIf(entry -> entry.getValue().isEmpty());
  }
}
