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

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.logging.log4j.Logger;

import net.minecraft.entity.monster.CreeperEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerLoadEvent;

@EventBusSubscriber
public class MonsterEntityManager extends Manager {

  private static Map<String, Set<MonsterEntity>> monsterEntityMap = new ConcurrentHashMap<>();
  private static boolean burnCreeperDuringDaylight = COMMON.burnCreeperDuringDaylight.get();
  private static boolean modDungeonsmodOptimizeWhirlwind =
      COMMON.modDungeonsmodOptimizeWhirlwind.get();
  private static boolean runCleanup = false;

  public static final String LOG_NAME = MonsterEntityManager.class.getSimpleName();
  private static final Logger log = getLogger(LOG_NAME);

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    monsterEntityMap = new ConcurrentHashMap<>();
    burnCreeperDuringDaylight = COMMON.burnCreeperDuringDaylight.get();
    modDungeonsmodOptimizeWhirlwind = COMMON.modDungeonsmodOptimizeWhirlwind.get();
    runCleanup = burnCreeperDuringDaylight || modDungeonsmodOptimizeWhirlwind;
  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (event.hasHighServerLoad()) {
      cleanupMonster();
    }
  }

  public static void handleMonsterEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    MonsterEntity monsterEntity = (MonsterEntity) event.getEntity();
    String monsterName = monsterEntity.getEncodeId();
    String monsterDisplayName = monsterEntity.getDisplayName().getString();
    String worldName = monsterEntity.level.dimension().location().toString();
    String monsterEntityMapKey = '[' + worldName + ']' + monsterName;
    monsterEntityMap.computeIfAbsent(monsterEntityMapKey, k -> new LinkedHashSet<>());
    Set<MonsterEntity> monsterEntities = monsterEntityMap.get(monsterEntityMapKey);
    monsterEntities.add(monsterEntity);
    log.debug("Monster {} {} joined {}.", monsterName, monsterDisplayName, worldName);
  }

  public static void handleMonsterEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {
    MonsterEntity monsterEntity = (MonsterEntity) event.getEntity();
    String monsterName = monsterEntity.getEncodeId();
    String monsterDisplayName = monsterEntity.getDisplayName().getString();
    String worldName = monsterEntity.level.dimension().location().toString();
    Set<MonsterEntity> monsterEntities = monsterEntityMap.get('[' + worldName + ']' + monsterName);
    if (monsterEntities != null) {
      monsterEntities.remove(monsterEntity);
      log.debug("Monster {} {} leaved {}.", monsterName, monsterDisplayName, worldName);
    } else {
      log.warn("Monster {} {} in {} was not tracked by monster entity manager!", monsterName,
          monsterDisplayName, worldName);
    }
  }

  public static void cleanupMonster() {
    if (!runCleanup) {
      return;
    }
    for (Map.Entry<String, Set<MonsterEntity>> monsterEntities : monsterEntityMap.entrySet()) {
      for (MonsterEntity monsterEntity : monsterEntities.getValue()) {
        World entityWorld = monsterEntity.level;
        String monsterName = monsterEntity.getEncodeId();

        // Cleanup specific Monsters during daytime
        if (entityWorld.isDay()) {
          // Burn Crepper during days to control population
          if (burnCreeperDuringDaylight && monsterEntity instanceof CreeperEntity
              && entityWorld.canSeeSky(monsterEntity.blockPosition())) {
            CreeperEntity creeperEntity = (CreeperEntity) monsterEntities;
            creeperEntity.ignite();
          }
          // Remove whirlwind during day
          if (modDungeonsmodOptimizeWhirlwind && "dungeonsmod:whirlwind".equals(monsterName)) {
            monsterEntity.remove();
          }
        }
      }
    }
  }

  public static Map<String, Set<MonsterEntity>> getMonsterEntityMap() {
    return monsterEntityMap;
  }
}
