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

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.logging.log4j.Logger;

import net.minecraft.entity.Entity;
import net.minecraft.entity.effect.LightningBoltEntity;
import net.minecraft.entity.item.ExperienceOrbEntity;
import net.minecraft.entity.item.FallingBlockEntity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.projectile.ProjectileEntity;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.player.PlayerPosition;

@EventBusSubscriber
public class EntityManager extends Manager {

  private static ConcurrentHashMap<String, Set<Entity>> entityMap = new ConcurrentHashMap<>();
  private static ConcurrentHashMap<String, Set<Entity>> entityMapPerWorld =
      new ConcurrentHashMap<>();
  private static Set<String> allowList = new HashSet<>(COMMON.spawnAllowList.get());
  private static Set<String> denyList = new HashSet<>(COMMON.spawnDenyList.get());

  public static final String LOG_NAME = EntityManager.class.getSimpleName();
  private static final Logger log = getLogger(LOG_NAME);

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    entityMap = new ConcurrentHashMap<>();
    entityMapPerWorld = new ConcurrentHashMap<>();
    allowList = new HashSet<>(COMMON.spawnAllowList.get());
    denyList = new HashSet<>(COMMON.spawnDenyList.get());
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    // Ignore client side world.
    World world = event.getWorld();
    if (world.isClientSide) {
      return;
    }

    // Ignore entities which are handled by other instances or not relevant.
    Entity entity = event.getEntity();
    if (entity instanceof ExperienceOrbEntity || entity instanceof ItemEntity
        || entity instanceof LightningBoltEntity || entity instanceof FallingBlockEntity
        || entity instanceof ProjectileEntity) {
      return;
    } else if (entity instanceof PlayerEntity) {
      log.debug("Player {} joined world.", entity);
      return;
    }
    String entityName = entity.getEncodeId();
    String worldName = world.dimension().location().toString();

    // Skip other checks if unknown entity name
    if (entityName == null) {
      if (entity.isMultipartEntity() || entity.getType().toString().contains("body_part")) {
        log.debug("Ignore multipart entity {} in {}.", entity, worldName);
      } else if (entity.hasCustomName()) {
        log.debug("Unknown entity name for entity {} ({}) with custom name {} in {}.", entity,
            entity.getType(), entity.getCustomName().getString(), worldName);
      } else {
        log.warn("Unknown entity name for entity {} ({}) in {}. Please report this issue under {}!",
            entity, entity.getType(), worldName, Constants.ISSUE_REPORT);
      }
      return;
    }

    if (denyList.contains(entityName)) {
      log.debug("Removing denied entity {} in {}", entityName, worldName);
      entity.remove();
      event.setCanceled(true);
      return;
    }

    if (allowList.contains(entityName)) {
      log.debug("Ignore allowed entity {} in {}", entityName, worldName);
      return;
    } else if (entity.hasCustomName()) {
      log.debug("Ignore custom entity {} with name {} in {}", entityName,
          entity.getCustomName().getString(), worldName);
      return;
    } else if (entity instanceof MonsterEntity) {
      MonsterEntityManager.handleMonsterEntityJoinWorldEvent(event);
    }

    addEntity(entity, world);
  }

  @SubscribeEvent
  public static void handleEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    // Ignore client side world.
    World world = event.getWorld();
    if (world.isClientSide) {
      return;
    }

    // Ignore entities which are handled by other instances or not relevant.
    Entity entity = event.getEntity();
    if (entity instanceof ExperienceOrbEntity || entity instanceof ItemEntity
        || entity instanceof LightningBoltEntity || entity instanceof FallingBlockEntity
        || entity instanceof ProjectileEntity) {
      return;
    } else if (entity instanceof PlayerEntity) {
      log.debug("Player {} leaved world.", entity);
      return;
    }

    // Skip other checks if unknown entity name
    String entityName = entity.getEncodeId();
    String worldName = world.dimension().location().toString();
    if (entityName == null) {
      return;
    }

    if (entity.hasCustomName()) {
      log.debug("Ignore custom entity {} with name {} in {}", entityName,
          entity.getCustomName().getString(), worldName);
    } else if (entity instanceof MonsterEntity) {
      MonsterEntityManager.handleMonsterEntityLeaveWorldEvent(event);
    }

    removeEntity(entity, world);
  }

  public static void addEntity(Entity entity, World world) {
    String entityName = entity.getEncodeId();
    String worldName = world.dimension().location().toString();

    // Store entities per type and world.
    String entityMapKey = '[' + worldName + ']' + entityName;
    entityMap.computeIfAbsent(entityMapKey, k -> new HashSet<>());
    Set<Entity> entities = entityMap.get(entityMapKey);
    entities.add(entity);

    // Store entities per world.
    entityMapPerWorld.computeIfAbsent(worldName, k -> new HashSet<>());
    Set<Entity> entitiesPerWorld = entityMapPerWorld.get(worldName);
    entitiesPerWorld.add(entity);

    log.debug("Entity {} ({}) joined {}.", entityName, entity, worldName);
  }

  public static void removeEntity(Entity entity, World world) {
    String entityName = entity.getEncodeId();
    String worldName = world.dimension().location().toString();

    // Remove entity from per type and world map.
    Set<Entity> entities = entityMap.get('[' + worldName + ']' + entityName);
    if (entities != null) {
      entities.remove(entity);
    }

    // Remove entity from per world map
    Set<Entity> entitiesPerWorld = entityMapPerWorld.get(worldName);
    if (entitiesPerWorld != null) {
      entitiesPerWorld.remove(entity);
    }

    log.debug("Entity {} ({}) leaved {}.", entityName, entity, worldName);
  }

  public static Map<String, Set<Entity>> getEntities() {
    return entityMap;
  }

  public static Integer getNumberOfEntities(String worldName, String entityName) {
    Set<Entity> entities = entityMap.get('[' + worldName + ']' + entityName);
    if (entities == null) {
      return 0;
    }
    return entities.size();
  }

  public static Integer getNumberOfEntitiesPerWorld(String worldName) {
    Set<Entity> entities = entityMapPerWorld.get(worldName);
    if (entities == null) {
      return 0;
    }
    return entities.size();
  }

  public static Integer getNumberOfEntitiesInPlayerPositions(String worldName, String entityName,
      List<PlayerPosition> playerPositions) {
    if (!entityMap.contains('[' + worldName + ']' + entityName)) {
      return 0;
    }
    int counter = 0;
    Set<Entity> entities = new HashSet<>(entityMap.get('[' + worldName + ']' + entityName));
    Iterator<Entity> entityIterator = entities.iterator();
    while (entityIterator.hasNext()) {
      Entity entity = entityIterator.next();
      if (entity != null) {
        int x = (int) entity.getX();
        int y = (int) entity.getY();
        int z = (int) entity.getZ();
        for (PlayerPosition playerPosition : playerPositions) {
          if (playerPosition.isInsidePlayerViewArea(worldName, x, y, z)) {
            counter++;
          }
        }
      }
    }
    return counter;
  }

  public static Integer getNumberOfEntitiesInChunkPosition(String worldName, Vector3d position) {
    if (!entityMap.contains(worldName)) {
      return 0;
    }
    int counter = 0;
    int chunkX = (int) position.x >> 4;
    int chunkZ = (int) position.z >> 4;
    Set<Entity> entities = new HashSet<>(entityMapPerWorld.get(worldName));
    Iterator<Entity> entityIterator = entities.iterator();
    while (entityIterator.hasNext()) {
      Entity entity = entityIterator.next();
      if (entity != null) {
        int entityChunkX = entity.xChunk;
        int entityChunkZ = entity.zChunk;
        if (chunkX == entityChunkX && chunkZ == entityChunkZ) {
          counter++;
        }
      }
    }
    return counter;
  }

}
