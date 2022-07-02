/**
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

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.math.Vector3d;
import net.minecraft.world.entity.AreaEffectCloud;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.entity.Marker;
import net.minecraft.world.entity.boss.enderdragon.EndCrystal;
import net.minecraft.world.entity.decoration.ArmorStand;
import net.minecraft.world.entity.decoration.HangingEntity;
import net.minecraft.world.entity.item.FallingBlockEntity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.entity.vehicle.AbstractMinecartContainer;
import net.minecraft.world.entity.vehicle.Boat;
import net.minecraft.world.entity.vehicle.MinecartChest;
import net.minecraft.world.level.Level;

import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweakscore.Constants;
import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPosition;

@EventBusSubscriber
public class EntityManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static short ticks = 0;
  private static final short VERIFICATION_TICK = 25 * 20;

  private static ConcurrentHashMap<String, Set<Entity>> entityMap = new ConcurrentHashMap<>();
  private static ConcurrentHashMap<String, Set<Entity>> entityMapPerWorld =
      new ConcurrentHashMap<>();

  protected EntityManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    entityMap = new ConcurrentHashMap<>();
    entityMapPerWorld = new ConcurrentHashMap<>();
  }

  @SubscribeEvent
  public static void handleClientServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.START) {
      ticks++;
      return;
    }

    // Verify entities to consider removed and unloaded entities.
    if (ticks >= VERIFICATION_TICK) {
      verifyEntities();
      ticks = 0;
    }
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    // Ignore client side world and if event is canceled.
    Level level = event.getWorld();
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
      if ((CoreConstants.ADHOOKS_LOADED && entityType.startsWith("entity.adhooks."))
          || (CoreConstants.COFH_CORE_LOADED && entityType.startsWith("entity.cofh_core."))) {
        log.debug("Ignore modded entity {} in {}", entity, levelName);
      } else if (CoreConstants.MANA_AND_ARTIFICE_LOADED
          && entityType.startsWith("entity.mana-and-artifice.")) {
        log.debug("Ignore {} entity {} in {}", CoreConstants.MANA_AND_ARTIFICE_NAME, entity,
            levelName);
      } else if (entity.isMultipartEntity() || entityType.contains("body_part")) {
        log.debug("Ignore multipart entity {} in {}.", entity, levelName);
      } else if (entity.hasCustomName()) {
        log.debug("Unknown entity name for entity {} ({}) with custom name {} in {}.", entity,
            entityType, entity.getCustomName().getString(), levelName);
      } else {
        log.warn("Unknown entity name for entity {} ({}) in {}. Please report this issue under {}!",
            entity, entityType, levelName, CoreConstants.ISSUE_REPORT);
      }
      return;
    }

    // Ignore entity with custom name
    if (entity.hasCustomName()) {
      log.debug("Ignore custom entity {} with name {} in {}", entityName,
          entity.getCustomName().getString(), levelName);
      return;
    }
    addEntity(entity, entityName, levelName);
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    // Ignore client side world.
    Level level = event.getWorld();
    if (level.isClientSide) {
      return;
    }

    // Ignore entities which are handled by other instances or not relevant.
    Entity entity = event.getEntity();
    if (!isRelevantEntity(entity)) {
      return;
    }

    // Skip other checks if unknown entity name
    String entityName = entity.getEncodeId();
    if (entityName == null) {
      return;
    }
    String levelName = level.dimension().location().toString();
    removeEntity(entity, entityName, levelName);
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleLivingDeathEvent(LivingDeathEvent event) {
    Entity entity = event.getEntity();
    Level level = entity.getLevel();
    if (level.isClientSide) {
      return;
    }
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
    Set<Entity> entities = entityMap.computeIfAbsent(getEntityMapKey(levelName, entityName),
        key -> ConcurrentHashMap.newKeySet());
    entities.add(entity);

    // Store entities per world.
    Set<Entity> entitiesPerWorld =
        entityMapPerWorld.computeIfAbsent(levelName, key -> ConcurrentHashMap.newKeySet());
    entitiesPerWorld.add(entity);

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

    // Remove entity from per world map
    Set<Entity> entitiesPerWorld = entityMapPerWorld.get(levelName);
    if (entitiesPerWorld != null) {
      entitiesPerWorld.remove(entity);
    }

    log.debug("[Left] Entity {} ({}) leaved {}.", entityName, entity, levelName);
  }

  public static String getEntityMapKey(String levelName, String entityName) {
    return '[' + levelName + ']' + entityName;
  }

  public static Map<String, Set<Entity>> getEntities() {
    return entityMap;
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

  public static Integer getNumberOfEntitiesPerWorld(String levelName) {
    Set<Entity> entities = entityMapPerWorld.get(levelName);
    if (entities == null) {
      return 0;
    }
    return entities.size();
  }

  public static Integer getNumberOfEntitiesInPlayerPositions(String levelName, String entityName,
      List<PlayerPosition> playerPositions) {
    String entityMapKey = getEntityMapKey(levelName, entityName);
    if (!entityMap.containsKey(entityMapKey)) {
      return 0;
    }
    int counter = 0;
    Set<Entity> entities = new HashSet<>(entityMap.get(entityMapKey));
    Iterator<Entity> entityIterator = entities.iterator();
    while (entityIterator.hasNext()) {
      Entity entity = entityIterator.next();
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

  public static Integer getNumberOfEntitiesInChunkPosition(String levelName, Vector3d position) {
    if (!entityMapPerWorld.containsKey(levelName)) {
      return 0;
    }
    int counter = 0;
    int chunkX = (int) position.x >> 4;
    int chunkZ = (int) position.z >> 4;
    Set<Entity> entities = new HashSet<>(entityMapPerWorld.get(levelName));
    Iterator<Entity> entityIterator = entities.iterator();
    while (entityIterator.hasNext()) {
      Entity entity = entityIterator.next();
      if (entity != null) {
        int entityChunkX = (int) entity.getX() >> 4;
        int entityChunkZ = (int) entity.getY() >> 4;
        if (chunkX == entityChunkX && chunkZ == entityChunkZ) {
          counter++;
        }
      }
    }
    return counter;
  }

  private static void verifyEntities() {
    int removedEntries = 0;

    // Verify Entities in overall overview
    removedEntries += removeDiscardedEntities(entityMap);

    // Verify Entities from world specific overview
    removedEntries += removeDiscardedEntities(entityMapPerWorld);

    if (removedEntries > 0) {
      log.debug("Removed {} entries during the verification", removedEntries);
    }
  }

  private static int removeDiscardedEntities(ConcurrentMap<String, Set<Entity>> entityMapToCheck) {
    int removedEntries = 0;
    if (entityMapToCheck != null && entityMapToCheck.size() > 0) {
      // Remove entities which are no longer valid liked removed onces.
      for (Set<Entity> entities : entityMapToCheck.values()) {
        Iterator<Entity> entityIterator = entities.iterator();
        while (entityIterator.hasNext()) {
          Entity entity = entityIterator.next();
          if (entity != null && entity.isRemoved()) {
            entityIterator.remove();
            removedEntries++;
          }
        }
      }
    }
    return removedEntries;
  }

  public static boolean isRelevantEntity(Entity entity) {
    return !(entity instanceof ExperienceOrb || entity instanceof ItemEntity
        || entity instanceof LightningBolt || entity instanceof FallingBlockEntity
        || entity instanceof Projectile || entity instanceof MinecartChest
        || entity instanceof AbstractMinecartContainer || entity instanceof Player
        || entity instanceof Boat || entity instanceof ArmorStand
        || entity instanceof AreaEffectCloud || entity instanceof EndCrystal
        || entity instanceof Marker || entity instanceof HangingEntity || entity instanceof Npc
        || entity.isRemoved());
  }

}
