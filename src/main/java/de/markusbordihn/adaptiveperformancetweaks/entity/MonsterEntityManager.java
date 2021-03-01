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

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import net.minecraft.entity.monster.CreeperEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoadEvent;

@EventBusSubscriber
public class MonsterEntityManager extends Manager {

  private static Map<String, Set<MonsterEntity>> monsterEntityMap = new HashMap<>();
  private static boolean hasHighServerLoad = false;

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    hasHighServerLoad = event.hasHighServerLoad();
    if (hasHighServerLoad) {
      cleanupMonster();
    }
  }

  public static void handleMonsterEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    MonsterEntity monsterEntity = (MonsterEntity) event.getEntity();
    String monsterName = monsterEntity.getEntityString();
    String monsterDisplayName = monsterEntity.getDisplayName().getString();
    String worldName = monsterEntity.getEntityWorld().getDimensionKey().getLocation().toString();
    Set<MonsterEntity> monsterEntities = monsterEntityMap.get('[' + worldName + ']' + monsterName);
    if (monsterEntities == null) {
      monsterEntities = new LinkedHashSet<>();
      monsterEntityMap.put('[' + worldName + ']' + monsterName, monsterEntities);
    }
    monsterEntities.add(monsterEntity);
    log.debug("Monster {} {} joined {}.", monsterName, monsterDisplayName, worldName);
    cleanupMonster();
  }

  public static void handleMonsterEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    MonsterEntity monsterEntity = (MonsterEntity) event.getEntity();
    String monsterName = monsterEntity.getEntityString();
    String monsterDisplayName = monsterEntity.getDisplayName().getString();
    String worldName = monsterEntity.getEntityWorld().getDimensionKey().getLocation().toString();
    Set<MonsterEntity> monsterEntities =
        monsterEntityMap.getOrDefault('[' + worldName + ']' + monsterName, new LinkedHashSet<>());
    monsterEntities.remove(monsterEntity);
    log.debug("Monster {} {} leaved {}.", monsterName, monsterDisplayName, worldName);
  }

  public static void cleanupMonster() {
    for (Map.Entry<String, Set<MonsterEntity>> monsterEntities : monsterEntityMap.entrySet()) {
      for (MonsterEntity monsterEntity : monsterEntities.getValue()) {
        World entityWorld = monsterEntity.getEntityWorld();
        // Burn specific Monster during days to control population
        if (entityWorld.isDaytime()) {
          if (monsterEntity instanceof CreeperEntity) {
            if (entityWorld.canSeeSky(monsterEntity.getPosition())) {
              monsterEntity.setFire(60);
            }
          }
        }
      }
    }
  }

  public static Map<String, Set<MonsterEntity>> getMonsterEntityMap() {
    return monsterEntityMap;
  }
}
