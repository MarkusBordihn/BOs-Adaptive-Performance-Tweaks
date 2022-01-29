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
import java.util.Map;
import java.util.Set;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.math.Vector3d;

import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.entity.item.FallingBlockEntity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.entity.vehicle.AbstractMinecartContainer;
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

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static short ticks = 0;
  private static final short VERIFICATION_TICK = 30 * 20;

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
    if (event.phase == TickEvent.Phase.END && ticks++ >= VERIFICATION_TICK) {
      verifyEntities();
      ticks = 0;
    }
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    // Ignore client side world.
    Level level = event.getWorld();
    if (level.isClientSide) {
      return;
    }

    // Entity instance checks to ignore specific and short living entities like projectiles.
    Entity entity = event.getEntity();
    if (!isRelevantEntity(entity)) {
      return;
    }

    String entityName = entity.getEncodeId();
    String levelName = level.dimension().location().toString();

    // Skip other checks if unknown entity name
    if (entityName == null) {
      if (entity.isMultipartEntity() || entity.getType().toString().contains("body_part")) {
        log.debug("Ignore multipart entity {} in {}.", entity, levelName);
      } else if (entity.hasCustomName()) {
        log.debug("Unknown entity name for entity {} ({}) with custom name {} in {}.", entity,
            entity.getType(), entity.getCustomName().getString(), levelName);
      } else {
        log.warn("Unknown entity name for entity {} ({}) in {}. Please report this issue under {}!",
            entity, entity.getType(), levelName, CoreConstants.ISSUE_REPORT);
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
    String entityMapKey = '[' + levelName + ']' + entityName;
    Set<Entity> entities =
        entityMap.computeIfAbsent(entityMapKey, key -> ConcurrentHashMap.newKeySet());
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
    Set<Entity> entities = entityMap.get('[' + levelName + ']' + entityName);
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

  public static Map<String, Set<Entity>> getEntities() {
    return entityMap;
  }

  public static Integer getNumberOfEntities(String levelName, String entityName) {
    Set<Entity> entities = entityMap.get('[' + levelName + ']' + entityName);
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
    if (!entityMap.contains('[' + levelName + ']' + entityName)) {
      return 0;
    }
    int counter = 0;
    Set<Entity> entities = new HashSet<>(entityMap.get('[' + levelName + ']' + entityName));
    Iterator<Entity> entityIterator = entities.iterator();
    while (entityIterator.hasNext()) {
      Entity entity = entityIterator.next();
      if (entity != null) {
        int x = (int) entity.getX();
        int y = (int) entity.getY();
        int z = (int) entity.getZ();
        for (PlayerPosition playerPosition : playerPositions) {
          if (playerPosition.isInsidePlayerViewArea(levelName, x, y, z)) {
            counter++;
          }
        }
      }
    }
    return counter;
  }

  public static Integer getNumberOfEntitiesInChunkPosition(String levelName, Vector3d position) {
    if (!entityMap.contains(levelName)) {
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

  public static void verifyEntities() {
    int removedEntries = 0;

    // Verify Entities in overall overview
    for (Set<Entity> entities : entityMap.values()) {
      Iterator<Entity> entityIterator = entities.iterator();
      while (entityIterator.hasNext()) {
        Entity entity = entityIterator.next();
        if (entity != null && entity.isRemoved()) {
          entityIterator.remove();
          removedEntries++;
        }
      }
    }

    // Verify Entities from world specific overview
    for (Set<Entity> entities : entityMapPerWorld.values()) {
      Iterator<Entity> entityIterator = entities.iterator();
      while (entityIterator.hasNext()) {
        Entity entity = entityIterator.next();
        if (entity != null && entity.isRemoved()) {
          entityIterator.remove();
          removedEntries++;
        }
      }
    }

    if (removedEntries > 0) {
      log.debug("Removed {} entries during the verification", removedEntries);
    }
  }

  public static boolean isRelevantEntity(Entity entity) {
    return !(entity instanceof ExperienceOrb || entity instanceof ItemEntity
        || entity instanceof LightningBolt || entity instanceof FallingBlockEntity
        || entity instanceof Projectile || entity instanceof MinecartChest
        || entity instanceof AbstractMinecartContainer || entity instanceof Player
        || entity instanceof Npc || entity.isRemoved());
  }

}
