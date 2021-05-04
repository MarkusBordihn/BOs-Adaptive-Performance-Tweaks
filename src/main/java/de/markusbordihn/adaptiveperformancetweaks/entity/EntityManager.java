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

import net.minecraft.entity.Entity;
import net.minecraft.entity.effect.LightningBoltEntity;
import net.minecraft.entity.item.FallingBlockEntity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.player.PlayerPosition;

@EventBusSubscriber
public class EntityManager extends Manager {

  private static ConcurrentHashMap<String, Set<Entity>> entityMap = new ConcurrentHashMap<>();
  private static Set<String> allowList = new HashSet<>(COMMON.spawnAllowList.get());
  private static Set<String> denyList = new HashSet<>(COMMON.spawnDenyList.get());

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    allowList = new HashSet<>(COMMON.spawnAllowList.get());
    denyList = new HashSet<>(COMMON.spawnDenyList.get());
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    Entity entity = event.getEntity();

    // Ignore entities which are handled by other instances or not relevant.
    if (entity instanceof ItemEntity || entity instanceof LightningBoltEntity
        || entity instanceof FallingBlockEntity) {
      return;
    }

    String entityName = entity.getEntityString();
    if (denyList.contains(entityName)) {
      log.debug("Removing denied entity {}", entityName);
      entity.remove();
      event.setCanceled(true);
      return;
    }

    if (allowList.contains(entityName)) {
      log.debug("Ignore allowed entity {}", entityName);
    } else if (entity.hasCustomName()) {
      log.debug("Ignore custom entity {} with name {}", entityName,
          entity.getCustomName().getString());
    } else {
      if (entity instanceof MonsterEntity) {
        MonsterEntityManager.handleMonsterEntityJoinWorldEvent(event);
      }

      if (entity instanceof PlayerEntity) {
        log.debug("Player {} joined world.", entity);
        return;
      }
    }

    String worldName = entity.getEntityWorld().getDimensionKey().getLocation().toString();
    Set<Entity> entities = entityMap.get('[' + worldName + ']' + entityName);
    if (entities != null) {
      entities.add(entity);
    } else {
      entities = new HashSet<>();
      entities.add(entity);
      entityMap.put('[' + worldName + ']' + entityName, entities);
    }
    log.debug("Entity {} ({}) {} joined.", entityName, entities.size(), entity);
  }

  @SubscribeEvent
  public static void handleEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    Entity entity = event.getEntity();

    // Ignore entities which are handled by other instances or not relevant.
    if (entity instanceof ItemEntity || entity instanceof LightningBoltEntity
        || entity instanceof FallingBlockEntity) {
      return;
    }

    if (entity instanceof MonsterEntity) {
      MonsterEntityManager.handleMonsterEntityLeaveWorldEvent(event);
    }

    if (entity instanceof PlayerEntity) {
      log.debug("Player {} leaved world.", entity);
      return;
    }

    String entityName = entity.getEntityString();
    String worldName = entity.getEntityWorld().getDimensionKey().getLocation().toString();
    Set<Entity> entities = entityMap.get('[' + worldName + ']' + entityName);
    if (entities != null) {
      entities.remove(entity);
    } else {
      entities = new HashSet<>();
    }
    log.debug("Entity {} ({}) {} leaved.", entityName, entities.size(), entity);
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

  public static Integer getNumberOfEntitiesInPlayerPositions(String worldName, String entityName,
      List<PlayerPosition> playerPositions) {
    Set<Entity> entities = entityMap.get('[' + worldName + ']' + entityName);
    if (entities == null) {
      return 0;
    }
    int counter = 0;
    Iterator<Entity> entityIterator = entities.iterator();
    while (entityIterator.hasNext()) {
      Entity entity = entityIterator.next();
      int x = (int) entity.getPosX();
      int y = (int) entity.getPosY();
      int z = (int) entity.getPosZ();
      for (PlayerPosition playerPosition : playerPositions) {
        if (playerPosition.isInsidePlayerViewArea(worldName, x, y, z)) {
          counter++;
        }
      }
    }
    return counter;
  }

}
