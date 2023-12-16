/*
 * Copyright 2022 Markus Bordihn
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
package de.markusbordihn.adaptiveperformancetweakscore.entity;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;
import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPosition;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
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
import net.minecraft.world.level.Level;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityJoinLevelEvent;
import net.minecraftforge.event.entity.EntityLeaveLevelEvent;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber
public class CoreEntityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final short VERIFICATION_TICK = 25 * 20;
  private static final String ENTITY_OWNER_TAG = "Owner";
  private static final String PERSISTENCE_REQUIRED = "PersistenceRequired";
  // Entity Chunk Map to determine if any mob was spawned in a specific chunk.
  private static final ConcurrentHashMap<String, Boolean> entityChunkMap =
      new ConcurrentHashMap<>();
  private static short ticks = 0;
  // Entity map to store all entities, per chunk, per world and global.
  private static ConcurrentHashMap<String, Set<Entity>> entityMap = new ConcurrentHashMap<>();
  private static ConcurrentHashMap<String, Set<Entity>> entityMapPerChunk =
      new ConcurrentHashMap<>();
  private static ConcurrentHashMap<String, Set<Entity>> entityMapPerWorld =
      new ConcurrentHashMap<>();
  private static ConcurrentHashMap<String, Set<Entity>> entityMapGlobal = new ConcurrentHashMap<>();

  protected CoreEntityManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    entityMap = new ConcurrentHashMap<>();
    entityMapPerChunk = new ConcurrentHashMap<>();
    entityMapPerWorld = new ConcurrentHashMap<>();
    entityMapGlobal = new ConcurrentHashMap<>();
  }

  @SubscribeEvent
  public static void handleClientServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.START) {
      ticks++;
      return;
    }

    // Verify entities to consider removed and unloaded entities.
    if (ticks >= VERIFICATION_TICK && event.haveTime()) {
      verifyEntities();
      ticks = 0;
    }
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityJoinLevelEvent(EntityJoinLevelEvent event) {
    // Ignore client side world and if event is canceled.
    Level level = event.getLevel();
    if (level.isClientSide || event.isCanceled()) {
      return;
    }

    // Entity instance checks to ignore specific and short living entities like projectiles.
    Entity entity = event.getEntity();
    if (!isRelevantEntity(entity)) {
      return;
    }

    String entityName = entity.getEncodeId();
    String levelName = level.dimension().location().toString();

    // Skip other checks if unknown entity name, multi-part or custom entity
    if (entityName == null) {
      String entityType = entity.getType().getDescriptionId();

      // Only if debug is enabled we want to know more about the details.
      if (log.isDebugEnabled()) {
        if ((CoreConstants.ADHOOKS_LOADED && entityType.startsWith("entity.adhooks."))
            || (CoreConstants.COFH_CORE_LOADED && entityType.startsWith("entity.cofh_core."))
            || (CoreConstants.WEATHER_STORMS_TORNADOES_LOADED
                && entityType.startsWith("entity.weather2."))) {
          log.debug("[Entity Manager] Ignore modded entity {} in {}", entity, levelName);
        } else if (CoreConstants.MANA_AND_ARTIFICE_LOADED
            && entityType.startsWith("entity.mana-and-artifice.")) {
          log.debug(
              "[Entity Manager] Ignore {} entity {} in {}",
              CoreConstants.MANA_AND_ARTIFICE_NAME,
              entity,
              levelName);
        } else if (entity.isMultipartEntity() || entityType.contains("body_part")) {
          log.debug("[Entity Manager] Ignore multipart entity {} in {}.", entity, levelName);
        } else if (entity.hasCustomName()) {
          Component component = entity.getCustomName();
          log.debug(
              "[Entity Manager] Unknown entity name for entity {} ({}) with custom name {} in {}.",
              entity,
              entityType,
              component != null ? component.getString() : "",
              levelName);
        } else {
          log.warn(
              "[Entity Manager] Unknown entity name for entity {} ({}) in {}. Please report this issue under {}!",
              entity,
              entityType,
              levelName,
              CoreConstants.ISSUE_REPORT);
        }
      }
      return;
    }

    addEntity(entity, entityName, levelName);
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityLeaveLevelEvent(EntityLeaveLevelEvent event) {
    // Ignore client side world.
    Level level = event.getLevel();
    if (level.isClientSide) {
      return;
    }

    // Ignore entities which are handled by other instances or not relevant.
    Entity entity = event.getEntity();
    if (!isRelevantEntity(entity)) {
      return;
    }

    // Skip other checks if unknown entity name.
    String entityName = entity.getEncodeId();
    if (entityName == null) {
      return;
    }
    String levelName = level.dimension().location().toString();
    removeEntity(entity, entityName, levelName);
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleLivingDeathEvent(LivingDeathEvent event) {

    // Ignore entities which are handled by other instances or not relevant.
    Entity entity = event.getEntity();
    if (!isRelevantEntity(entity)) {
      return;
    }

    // Ignore client side world.
    Level level = entity.getLevel();
    if (level.isClientSide) {
      return;
    }

    // Skip if unknown entity name.
    String entityName = entity.getEncodeId();
    if (entityName == null) {
      return;
    }
    String levelName = level.dimension().location().toString();
    removeEntity(entity, entityName, levelName);
  }

  public static void addEntity(Entity entity, Level level) {
    String entityName = entity.getEncodeId();
    String levelName = level.dimension().location().toString();
    addEntity(entity, entityName, levelName);
  }

  public static void addEntity(Entity entity, String entityName, String levelName) {

    // Store entities per type and world.
    Set<Entity> entities =
        entityMap.computeIfAbsent(
            getEntityMapKey(levelName, entityName), key -> ConcurrentHashMap.newKeySet());
    entities.add(entity);

    // Store entities per chunk and world.
    String entityChunkKey = getEntityChunkKey(levelName, entity.blockPosition());
    Set<Entity> entitiesPerChunk =
        entityMapPerChunk.computeIfAbsent(entityChunkKey, key -> ConcurrentHashMap.newKeySet());
    entitiesPerChunk.add(entity);

    // Store entities per world.
    Set<Entity> entitiesPerWorld =
        entityMapPerWorld.computeIfAbsent(levelName, key -> ConcurrentHashMap.newKeySet());
    entitiesPerWorld.add(entity);

    // Store entities global.
    Set<Entity> entitiesGlobal =
        entityMapGlobal.computeIfAbsent(entityName, key -> ConcurrentHashMap.newKeySet());
    entitiesGlobal.add(entity);

    // Update entity chunk map.
    entityChunkMap.put(entityChunkKey, true);

    log.debug("[Joined] Entity {} ({}) joined {}.", entityName, entity, levelName);
  }

  public static void removeEntity(Entity entity, Level level) {
    String entityName = entity.getEncodeId();
    String levelName = level.dimension().location().toString();
    removeEntity(entity, entityName, levelName);
  }

  public static void removeEntity(Entity entity, String entityName, String levelName) {

    // Remove entity from per type and world map.
    Set<Entity> entities = entityMap.get(getEntityMapKey(levelName, entityName));
    if (entities != null) {
      entities.remove(entity);
    }

    // Remove entity from per chunk and world map.
    Set<Entity> entitiesPerChunk =
        entityMapPerChunk.get(getEntityChunkKey(levelName, entity.blockPosition()));
    if (entitiesPerChunk != null) {
      entitiesPerChunk.remove(entity);
    }

    // Remove entity from per world map
    Set<Entity> entitiesPerWorld = entityMapPerWorld.get(levelName);
    if (entitiesPerWorld != null) {
      entitiesPerWorld.remove(entity);
    }

    // Remove entity from global map
    Set<Entity> entitiesGlobal = entityMapGlobal.get(levelName);
    if (entitiesGlobal != null) {
      entitiesGlobal.remove(entity);
    }

    // Entity chunk map will not be updated here, because we want to keep the chunk in the map to
    // determine if any mob was spawned in a specific chunk over time.

    log.debug("[Left] Entity {} ({}) leaved {}.", entityName, entity, levelName);
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

  public static Map<String, Set<Entity>> getEntitiesPerWorld() {
    return entityMapPerWorld;
  }

  public static Map<String, Set<Entity>> getEntitiesGlobal() {
    return entityMapGlobal;
  }

  public static Map<String, Set<Entity>> getEntities(String dimensionName) {
    ConcurrentHashMap<String, Set<Entity>> entityResultMap = new ConcurrentHashMap<>();
    Set<Map.Entry<String, Set<Entity>>> entities = entityMap.entrySet();
    Iterator<Map.Entry<String, Set<Entity>>> entitiesIterator = entities.iterator();
    String levelName = '[' + dimensionName + ']';

    while (entitiesIterator.hasNext()) {
      Map.Entry<String, Set<Entity>> entity = entitiesIterator.next();
      String key = entity.getKey();
      if (key.startsWith(levelName)) {
        entityResultMap.put(key, entity.getValue());
      }
    }
    return entityResultMap;
  }

  public static Integer getNumberOfEntities(String levelName, String entityName) {
    Set<Entity> entities = entityMap.get(getEntityMapKey(levelName, entityName));
    if (entities == null) {
      return 0;
    }
    return entities.size();
  }

  public static Integer getNumberOfEntitiesPerChunk(String levelName, BlockPos blockPos) {
    Set<Entity> entities = entityMapPerChunk.get(getEntityChunkKey(levelName, blockPos));
    if (entities == null) {
      return 0;
    }
    return entities.size();
  }

  public static Integer getNumberOfEntitiesPerWorld(String levelName) {
    Set<Entity> entities = entityMapPerWorld.get(levelName);
    if (entities == null) {
      return 0;
    }
    return entities.size();
  }

  public static Integer getNumberOfEntities(String entityName) {
    Set<Entity> entities = entityMapGlobal.get(entityName);
    if (entities == null) {
      return 0;
    }
    return entities.size();
  }

  public static Integer getNumberOfEntitiesInPlayerPositions(
      String levelName, String entityName, List<PlayerPosition> playerPositions) {
    String entityMapKey = getEntityMapKey(levelName, entityName);
    if (!entityMap.containsKey(entityMapKey)) {
      return 0;
    }
    int counter = 0;
    Set<Entity> entities = new HashSet<>(entityMap.get(entityMapKey));
    for (Entity entity : entities) {
      if (entity != null) {
        for (PlayerPosition playerPosition : playerPositions) {
          if (playerPosition.isInsidePlayerViewArea(entity, levelName)) {
            counter++;
          }
        }
      }
    }
    return counter;
  }

  public static boolean hasEntitySpawnedInChunk(String levelName, BlockPos blockPos) {
    return entityChunkMap.getOrDefault(getEntityChunkKey(levelName, blockPos), false);
  }

  private static void verifyEntities() {
    int removedEntries = 0;
    int removedChunkEntries = 0;
    int removedWorldEntries = 0;
    int removedGlobalEntries = 0;

    // Verify Entities in overall overview
    removedEntries += removeDiscardedEntities(entityMap);

    // Verify Entities from chunk specific overview
    removedChunkEntries += removeDiscardedEntities(entityMapPerChunk);

    // Verify Entities from world specific overview
    removedWorldEntries += removeDiscardedEntities(entityMapPerWorld);

    // Verify Entities from global overview
    removedGlobalEntries += removeDiscardedEntities(entityMapGlobal);

    if (removedEntries > 0
        || removedChunkEntries > 0
        || removedWorldEntries > 0
        || removedGlobalEntries > 0) {
      log.debug(
          "[Entity Manager] 🗑 Removed {} entities from overview, {} from chunk overview, {} from world "
              + "overview and {} from global overview.",
          removedEntries,
          removedChunkEntries,
          removedWorldEntries,
          removedGlobalEntries);
    }
  }

  private static int removeDiscardedEntities(ConcurrentMap<String, Set<Entity>> entityMapToCheck) {
    int removedEntries = 0;
    if (entityMapToCheck != null && !entityMapToCheck.isEmpty()) {
      // Remove entities which are no longer valid like removed onces.
      for (Set<Entity> entities : entityMapToCheck.values()) {
        Iterator<Entity> entityIterator = entities.iterator();
        while (entityIterator.hasNext()) {
          // Check if the entity is still valid.
          Entity entity = entityIterator.next();
          if (entity == null || entity.isRemoved()) {
            entityIterator.remove();
            removedEntries++;
          }
        }
      }
    }
    return removedEntries;
  }

  public static boolean isRelevantEntity(Entity entity) {
    return !(entity == null
        || entity.isRemoved()
        || entity instanceof ExperienceOrb
        || entity instanceof ItemEntity
        || entity instanceof LightningBolt
        || entity instanceof FallingBlockEntity
        || entity instanceof Projectile
        || entity instanceof EvokerFangs
        || entity instanceof EyeOfEnder
        || entity instanceof AbstractMinecart
        || entity instanceof Player
        || entity instanceof Boat
        || entity instanceof ArmorStand
        || entity instanceof AreaEffectCloud
        || entity instanceof EndCrystal
        || entity instanceof Marker
        || entity instanceof HangingEntity
        || entity instanceof Npc
        || entity instanceof EnderDragon
        || entity instanceof EnderDragonPart
        || entity instanceof Warden
        || entity instanceof WitherBoss
        || entity instanceof ElderGuardian
        || entity.isSpectator()
        || entity.isInvisible()
        || entity.isInvulnerable()
        || entity.isVehicle()
        || entity.isPassenger()
        || (entity instanceof Raider raider && raider.hasActiveRaid())
        || (entity instanceof TamableAnimal tamableAnimal
            && (tamableAnimal.getOwner() != null || tamableAnimal.getOwnerUUID() != null))
        || (entity instanceof Mob mob
            && (mob.isLeashed()
                || mob.isPersistenceRequired()
                || mob.requiresCustomPersistence()
                || !mob.removeWhenFarAway(512)))
        || (entity instanceof Bee bee && bee.hasHive())
        || entity.hasCustomName());
  }

  @SuppressWarnings("java:S1126")
  public static boolean isRelevantEntity(Entity entity, String entityName) {

    // Entity instance checks to ignore specific and short living entities like projectiles.
    if (!isRelevantEntity(entity)) {
      return false;
    }

    // Skip other checks if unknown entity name.
    if (entityName == null) {
      return false;
    }

    // Skip checks for known default entities starting with "minecraft:".
    if (entityName.startsWith("minecraft:")) {
      return true;
    }

    // Ignore specific entities from other mods which are not extending the right classes or using
    // some custom definitions which could not be easily checked.
    if (CoreConstants.MANA_AND_ARTIFICE_LOADED
        && entityName.equals("mana-and-artifice:residual_magic")) {
      return false;
    }

    // Ignore specific entities from mods which implements their own spawn handling, logic or using
    // pseudo mobs for interactive blocks.
    if ((CoreConstants.ARS_NOUVEAU_LOADED && entityName.startsWith(CoreConstants.ARS_NOUVEAU_MOD))
        || (CoreConstants.APPLIED_ENERGISTICS_2_LOADED
            && entityName.startsWith(CoreConstants.APPLIED_ENERGISTICS_2_MOD))
        || (CoreConstants.BIGGER_REACTORS_LOADED
            && entityName.startsWith(CoreConstants.BIGGER_REACTORS_MOD))
        || (CoreConstants.BOTANIA_LOADED && entityName.startsWith(CoreConstants.BOTANIA_MOD))
        || (CoreConstants.CREATE_LOADED && entityName.startsWith(CoreConstants.CREATE_MOD))
        || (CoreConstants.EASY_NPC_LOADED && entityName.startsWith(CoreConstants.EASY_NPC_MOD))
        || (CoreConstants.FLUX_NETWORKS_LOADED
            && entityName.startsWith(CoreConstants.FLUX_NETWORKS_MOD))
        || (CoreConstants.GUARD_VILLAGERS_LOADED
            && entityName.startsWith(CoreConstants.GUARD_VILLAGERS_MOD))
        || (CoreConstants.HUMAN_COMPANIONS_LOADED
            && entityName.startsWith(CoreConstants.HUMAN_COMPANIONS_MOD))
        || (CoreConstants.INDUSTRIAL_FOREGOING_LOADED
            && entityName.startsWith(CoreConstants.INDUSTRIAL_FOREGOING_MOD))
        || (CoreConstants.IMMERSIVE_ENGINEERING_LOADED
            && entityName.startsWith(CoreConstants.IMMERSIVE_ENGINEERING_MOD))
        || (CoreConstants.LOOTR_LOADED && entityName.startsWith(CoreConstants.LOOTR_MOD))
        || (CoreConstants.MEKANISM_LOADED && entityName.startsWith(CoreConstants.MEKANISM_FILTER))
        || (CoreConstants.MODULAR_ROUTERS_LOADED
            && entityName.startsWith(CoreConstants.MODULAR_ROUTERS_MOD))
        || (CoreConstants.MINECOLONIES_LOADED
            && entityName.startsWith(CoreConstants.MINECOLONIES_MOD))
        || (CoreConstants.PIPEZ_LOADED && entityName.startsWith(CoreConstants.PIPEZ_MOD))
        || (CoreConstants.POKECUBE_AIO_LOADED
            && entityName.startsWith(CoreConstants.POKECUBE_AIO_MOD))
        || (CoreConstants.STORAGE_DRAWERS_LOADED
            && entityName.startsWith(CoreConstants.STORAGE_DRAWERS_MOD))
        || (CoreConstants.REFINED_STORAGE_LOADED
            && entityName.startsWith(CoreConstants.REFINED_STORAGE_MOD))
        || (CoreConstants.ULTIMATE_CAR_LOADED
            && entityName.startsWith(CoreConstants.ULTIMATE_CAR_MOD))
        || (CoreConstants.VIESCRAFT_MACHINES_LOADED
            && entityName.startsWith(CoreConstants.VIESCRAFT_MACHINES_MOD))
        || (CoreConstants.WEATHER_STORMS_TORNADOES_LOADED
            && entityName.startsWith(CoreConstants.WEATHER_STORMS_TORNADOES_MOD))
        || (CoreConstants.XNET_LOADED && entityName.startsWith(CoreConstants.XNET_MOD))) {
      return false;
    }

    // Checking entity NBT data to catch custom entities which are not extending the right classes
    // or using some custom definitions which could not be easily checked.
    CompoundTag compoundTag = entity.getPersistentData();
    if (compoundTag.contains(ENTITY_OWNER_TAG) && compoundTag.get(ENTITY_OWNER_TAG) != null) {
      return false;
    }
    if (compoundTag.contains(PERSISTENCE_REQUIRED)
        && compoundTag.get(PERSISTENCE_REQUIRED) != null
        && compoundTag.getBoolean(PERSISTENCE_REQUIRED)) {
      return false;
    }

    return true;
  }
}
