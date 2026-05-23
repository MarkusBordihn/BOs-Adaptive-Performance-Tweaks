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

package de.markusbordihn.adaptiveperformancetweaks.core.entity;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPosition;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.AreaEffectCloud;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.entity.Marker;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.TamableAnimal;
import net.minecraft.world.entity.animal.Bee;
import net.minecraft.world.entity.boss.EnderDragonPart;
import net.minecraft.world.entity.boss.enderdragon.EndCrystal;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.entity.boss.wither.WitherBoss;
import net.minecraft.world.entity.decoration.ArmorStand;
import net.minecraft.world.entity.decoration.HangingEntity;
import net.minecraft.world.entity.item.FallingBlockEntity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.ElderGuardian;
import net.minecraft.world.entity.monster.warden.Warden;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.EvokerFangs;
import net.minecraft.world.entity.projectile.EyeOfEnder;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.entity.raid.Raider;
import net.minecraft.world.entity.vehicle.AbstractMinecart;
import net.minecraft.world.entity.vehicle.Boat;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class CoreEntityManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_ENTITIES);

  private static final int VERIFICATION_TICK = 5 * 60 * 20;
  private static final int VERIFICATION_ADD_OPERATIONS_THRESHOLD = 500;
  private static final ConcurrentHashMap<String, Boolean> entityChunkMap =
    new ConcurrentHashMap<>();
  private static final Object verificationLock = new Object();
  private static volatile Set<String> excludedModNamespaces = Collections.emptySet();
  private static int ticks = 0;
  private static int addOperationCounter = 0;
  private static volatile boolean isVerifying = false;
  private static ConcurrentHashMap<String, Set<Entity>> entityMap = new ConcurrentHashMap<>();
  private static ConcurrentHashMap<String, Set<Entity>> entityMapPerChunk =
    new ConcurrentHashMap<>();
  private static ConcurrentHashMap<String, Set<Entity>> entityMapGlobal =
    new ConcurrentHashMap<>();
  private static ConcurrentHashMap<Entity, String> entityChunkKeyMap = new ConcurrentHashMap<>();

  private CoreEntityManager() {
  }

  public static void setExcludedModNamespaces(Set<String> namespaces) {
    excludedModNamespaces = Set.copyOf(namespaces);
    log.debug("Excluded mod namespaces from entity tracking: {}", namespaces);
  }

  public static boolean isExcludedModNamespace(String entityId) {
    if (entityId == null || excludedModNamespaces.isEmpty()) {
      return false;
    }
    int colonIdx = entityId.indexOf(':');

    return colonIdx > 0 && excludedModNamespaces.contains(entityId.substring(0, colonIdx));
  }

  public static void reset() {
    entityChunkMap.clear();
    entityMap = new ConcurrentHashMap<>();
    entityMapPerChunk = new ConcurrentHashMap<>();
    entityMapGlobal = new ConcurrentHashMap<>();
    entityChunkKeyMap = new ConcurrentHashMap<>();
    ticks = 0;
    addOperationCounter = 0;
  }

  public static void handleServerTick() {
    if (++ticks >= VERIFICATION_TICK) {
      triggerVerificationIfNotRunning("time-based");
      ticks = 0;
    }
  }

  public static void handleEntityJoinLevel(Entity entity, boolean isClientSide) {
    if (isClientSide) {
      return;
    }

    ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
    if (entityKey == null) {
      if (log.isDebugEnabled()) {
        log.debug("[Entity Manager] Skipping unregistered entity {} in {}.", entity,
          entity.level().dimension().location());
      }
      return;
    }

    String entityName = entityKey.toString();
    if (!isRelevantEntity(entity, entityName)) {
      return;
    }

    String levelName = entity.level().dimension().location().toString();
    addEntity(entity, entityName, levelName);
  }

  public static void handleEntityLeaveLevel(Entity entity, boolean isClientSide) {
    if (isClientSide || entity == null) {
      return;
    }

    ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
    if (entityKey == null) {
      return;
    }

    removeEntity(entity, entityKey.toString(), entity.level().dimension().location().toString());
  }

  public static void handleLivingDeath(Entity entity, boolean isClientSide) {
    if (isClientSide || entity == null) {
      return;
    }

    ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
    if (entityKey == null) {
      return;
    }

    removeEntity(entity, entityKey.toString(), entity.level().dimension().location().toString());
  }

  public static void addEntity(Entity entity, String entityName, String levelName) {
    Set<Entity> entities =
      entityMap.computeIfAbsent(
        getEntityMapKey(levelName, entityName), key -> ConcurrentHashMap.newKeySet());
    entities.add(entity);

    String entityChunkKey = getEntityChunkKey(levelName, entity.blockPosition());
    Set<Entity> entitiesPerChunk =
      entityMapPerChunk.computeIfAbsent(entityChunkKey, key -> ConcurrentHashMap.newKeySet());
    entitiesPerChunk.add(entity);
    entityChunkKeyMap.put(entity, entityChunkKey);

    Set<Entity> entitiesGlobal =
      entityMapGlobal.computeIfAbsent(entityName, key -> ConcurrentHashMap.newKeySet());
    entitiesGlobal.add(entity);

    entityChunkMap.put(entityChunkKey, true);

    if (++addOperationCounter >= VERIFICATION_ADD_OPERATIONS_THRESHOLD) {
      triggerVerificationIfNotRunning("operation-based");
    }
  }

  public static void removeEntity(Entity entity, String entityName, String levelName) {
    String mapKey = getEntityMapKey(levelName, entityName);
    Set<Entity> entities = entityMap.get(mapKey);
    boolean wasTracked = entities != null && entities.remove(entity);
    if (!wasTracked) {
      return;
    }
    if (entities.isEmpty()) {
      entityMap.remove(mapKey);
    }

    String originalChunkKey = entityChunkKeyMap.remove(entity);
    if (originalChunkKey != null) {
      Set<Entity> entitiesPerChunk = entityMapPerChunk.get(originalChunkKey);
      if (entitiesPerChunk != null) {
        entitiesPerChunk.remove(entity);
        if (entitiesPerChunk.isEmpty()) {
          entityMapPerChunk.remove(originalChunkKey);
        }
      }
    }

    Set<Entity> entitiesGlobal = entityMapGlobal.get(entityName);
    if (entitiesGlobal != null) {
      entitiesGlobal.remove(entity);
      if (entitiesGlobal.isEmpty()) {
        entityMapGlobal.remove(entityName);
      }
    }
  }

  public static String getEntityMapKey(String levelName, String entityName) {
    return '[' + levelName + ']' + entityName;
  }

  public static String getEntityChunkKey(String levelName, BlockPos blockPos) {
    return '[' + levelName + ':' + (blockPos.getX() >> 4) + 'x' + (blockPos.getZ() >> 4) + ']';
  }

  public static Map<String, Set<Entity>> getEntities() {
    return entityMap;
  }

  public static Map<String, Set<Entity>> getEntitiesPerChunk() {
    return entityMapPerChunk;
  }

  public static Map<String, Set<Entity>> getEntitiesGlobal() {
    return entityMapGlobal;
  }

  public static int getNumberOfEntities(String levelName, String entityName) {
    Set<Entity> entities = entityMap.get(getEntityMapKey(levelName, entityName));
    return entities != null ? entities.size() : 0;
  }

  public static int getNumberOfEntities(String entityName) {
    Set<Entity> entities = entityMapGlobal.get(entityName);
    return entities != null ? entities.size() : 0;
  }

  public static int getNumberOfEntitiesInPlayerPositions(
    String levelName, String entityName, List<PlayerPosition> playerPositions) {
    Set<Entity> rawSet = entityMap.get(getEntityMapKey(levelName, entityName));
    if (rawSet == null) {
      return 0;
    }

    int counter = 0;
    Set<Entity> snapshot = new HashSet<>(rawSet);
    for (Entity entity : snapshot) {
      if (entity == null) {
        continue;
      }

      for (PlayerPosition playerPosition : playerPositions) {
        if (playerPosition.isInsidePlayerViewArea(entity, levelName)) {
          counter++;
          break;
        }
      }
    }
    return counter;
  }

  public static boolean hasEntitySpawnedInChunk(String levelName, BlockPos blockPos) {
    return entityChunkMap.getOrDefault(getEntityChunkKey(levelName, blockPos), false);
  }

  public static boolean isRelevantEntity(Entity entity) {
    if (entity == null) {
      return false;
    }

    return !entity.isRemoved()
      && !entity.isSpectator()
      && !entity.isInvisible()
      && !entity.isInvulnerable()
      && !entity.isVehicle()
      && !entity.isPassenger()
      && !(entity instanceof Player)
      && !(entity instanceof ExperienceOrb)
      && !(entity instanceof Projectile)
      && !(entity instanceof AreaEffectCloud)
      && !(entity instanceof LightningBolt)
      && !(entity instanceof FallingBlockEntity)
      && !(entity instanceof EvokerFangs)
      && !(entity instanceof EyeOfEnder)
      && !(entity instanceof HangingEntity)
      && !(entity instanceof Marker)
      && !(entity instanceof EnderDragonPart)
      && !(entity instanceof EndCrystal)
      && !(entity instanceof AbstractMinecart)
      && !(entity instanceof Boat)
      && !(entity instanceof ArmorStand)
      && !(entity instanceof ItemEntity)
      && !(entity instanceof Npc)
      && !(entity instanceof EnderDragon)
      && !(entity instanceof WitherBoss)
      && !(entity instanceof ElderGuardian)
      && !(entity instanceof Warden)
      && !entity.hasCustomName();
  }

  public static boolean isRelevantEntity(Entity entity, String entityName) {
    if (!isRelevantEntity(entity)) {
      return false;
    }

    if (entity instanceof Mob mob
      && (mob.isLeashed() || mob.isPersistenceRequired() || mob.requiresCustomPersistence())) {
      return false;
    }

    if (entity instanceof TamableAnimal tamableAnimal && tamableAnimal.isTame()) {
      return false;
    }

    if (entity instanceof Bee bee && bee.hasHive()) {
      return false;
    }

    if (entity instanceof Raider raider && raider.hasActiveRaid()) {
      return false;
    }

    if (isExcludedModNamespace(entityName)) {
      return false;
    }

    return true;
  }

  private static void triggerVerificationIfNotRunning(String triggerType) {
    synchronized (verificationLock) {
      if (!isVerifying) {
        isVerifying = true;
        try {
          verifyEntities();
          if ("operation-based".equals(triggerType)) {
            addOperationCounter = 0;
          }
        } finally {
          isVerifying = false;
        }
      }
    }
  }

  private static void verifyEntities() {
    int removedEntries = removeDiscardedEntities(entityMap);
    int removedChunkEntries = removeDiscardedEntities(entityMapPerChunk);
    int removedGlobalEntries = removeDiscardedEntities(entityMapGlobal);

    if (removedEntries > 0 || removedChunkEntries > 0 || removedGlobalEntries > 0) {
      log.debug(
        "[Entity Manager] 🗑 Removed {} from overview, {} from chunk overview, {} from global overview.",
        removedEntries,
        removedChunkEntries,
        removedGlobalEntries);
    }
  }

  private static int removeDiscardedEntities(ConcurrentMap<String, Set<Entity>> entityMapToCheck) {
    if (entityMapToCheck == null || entityMapToCheck.isEmpty()) {
      return 0;
    }

    int removedEntries = 0;
    Iterator<Map.Entry<String, Set<Entity>>> mapIterator =
      entityMapToCheck.entrySet().iterator();

    while (mapIterator.hasNext()) {
      Map.Entry<String, Set<Entity>> entry = mapIterator.next();
      Set<Entity> entities = entry.getValue();

      Iterator<Entity> entityIterator = entities.iterator();
      while (entityIterator.hasNext()) {
        Entity entity = entityIterator.next();
        if (entity == null || entity.isRemoved()) {
          entityIterator.remove();
          removedEntries++;
        }
      }

      if (entities.isEmpty()) {
        mapIterator.remove();
      }
    }

    return removedEntries;
  }
}
