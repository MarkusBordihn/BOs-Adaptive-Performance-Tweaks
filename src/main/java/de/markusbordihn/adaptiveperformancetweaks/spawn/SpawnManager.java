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

package de.markusbordihn.adaptiveperformancetweaks.spawn;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.minecraft.entity.CreatureEntity;
import net.minecraft.entity.Entity;
import net.minecraft.entity.merchant.villager.VillagerEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.passive.TameableEntity;
import net.minecraft.entity.passive.WaterMobEntity;
import net.minecraft.entity.projectile.ProjectileEntity;
import net.minecraft.world.Difficulty;
import net.minecraftforge.event.DifficultyChangeEvent;
import net.minecraftforge.event.entity.living.LivingSpawnEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.server.ServerLifecycleHooks;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.config.SpawnConfigManager;
import de.markusbordihn.adaptiveperformancetweaks.entity.EntityManager;
import de.markusbordihn.adaptiveperformancetweaks.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweaks.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerWorldLoadEvent;

@EventBusSubscriber
public class SpawnManager extends Manager {

  private static Map<String, Double> serverWorldLoadFactorMap = new HashMap<>();
  private static Set<String> allowList = new HashSet<>(COMMON.spawnAllowList.get());
  private static Set<String> denyList = new HashSet<>(COMMON.spawnDenyList.get());
  private static boolean optimizeHostileMobs = COMMON.optimizeHostileMobs.get();
  private static double difficultyFactor = 1;

  @SubscribeEvent
  public static void onServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    allowList = new HashSet<>(COMMON.spawnAllowList.get());
    denyList = new HashSet<>(COMMON.spawnDenyList.get());
    optimizeHostileMobs = COMMON.optimizeHostileMobs.get();
  }

  @SubscribeEvent
  public static void onServerStarting(FMLServerStartingEvent event) {
    updateGameDifficulty(
        ServerLifecycleHooks.getCurrentServer().getServerConfiguration().getDifficulty());
    log.info("Allow List: {}", allowList);
    log.info("Deny List: {}", denyList);
  }

  @SubscribeEvent
  public static void handleServerWorldLoadEvent(ServerWorldLoadEvent event) {
    if (event.hasChanged()) {
      serverWorldLoadFactorMap.put(event.getServerWorldName(),
          event.getServerWorldLoadLevelFactor());
    }
  }

  @SubscribeEvent
  public static void handleDifficultyChangeEvent(DifficultyChangeEvent event) {
    updateGameDifficulty(event.getDifficulty());
  }

  @SubscribeEvent
  public static void handleLivingCheckSpawnEvent(LivingSpawnEvent.CheckSpawn event) {
    Entity entity = event.getEntity();
    String entityName = entity.getEntityString();
    String worldName = entity.getEntityWorld().getDimensionKey().getLocation().toString();

    // Pre-checks for allowed and defined to avoid expensive calculations
    if (allowList.contains(entityName)) {
      log.debug("[Allowed Entity] Allow spawn event for {} in {} ", entity, worldName);
      event.setResult(Event.Result.DEFAULT);
      return;
    }

    if (denyList.contains(entityName)) {
      log.debug("[Denied Entity] Denied spawn event for {} in {} ", entity, worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Ignore Animals with custom name (e.g. name tags)
    if ((entity instanceof AnimalEntity || entity instanceof TameableEntity)
        && entity.hasCustomName()) {
      log.debug("[Custom Entity] Skip spawn event for {} in {} ", entity, worldName);
      event.setResult(Event.Result.DEFAULT);
      return;
    }

    // Entity instance checks to ignore specific and short living entities like projectiles.
    if (entity instanceof ProjectileEntity) {
      log.trace("Projectile {}", entity);
      return;
    }

    if (entity instanceof MonsterEntity) {
      if (!optimizeHostileMobs) {
        return;
      }
      log.trace("Monster {}", entity);
    }

    if (entity instanceof AnimalEntity) {
      log.trace("Animal {}", entity);
    }

    if (entity instanceof TameableEntity) {
      log.trace("Tameable {}", entity);
    }

    if (entity instanceof WaterMobEntity) {
      log.trace("Water Mob {}", entity);
    }

    if (entity instanceof VillagerEntity) {
      log.trace("Villager {}", entity);
    }

    if (entity instanceof CreatureEntity) {
      log.trace("Creature {}", entity);
    }

    List<PlayerPosition> playersPositionsInsideViewArea =
        PlayerPositionManager.getPlayerPositionsInsideViewArea(worldName, (int) event.getX(),
            (int) event.getY(), (int) event.getZ());

    // Limit spawns to optimized players view area.
    Integer numOfPlayersInsideViewArea = playersPositionsInsideViewArea.size();
    if (numOfPlayersInsideViewArea == 0) {
      log.debug("[View Area Visibility] Blocked spawn event for {} in {}.", entity, worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Defines the spawn factor based on difficulty setting and world load.
    double spawnFactor = (entity instanceof MonsterEntity)
        ? serverWorldLoadFactorMap.getOrDefault(worldName, 1.0) * difficultyFactor
        : serverWorldLoadFactorMap.getOrDefault(worldName, 1.0);

    // Limit spawn based on world limits.
    int spawnLimitPerWorld = SpawnConfigManager.getSpawnLimitPerWorld(entityName);
    int numberOfEntities = EntityManager.getNumberOfEntities(worldName, entityName);
    if (numberOfEntities >= spawnLimitPerWorld * spawnFactor) {
      log.debug("[World limit] Blocked spawn event for {} ({} >= {}) in {}", entityName,
          numberOfEntities, spawnLimitPerWorld, worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Cheap and fast calculation to limit spawn based on possible entities within player limits.
    int spawnLimitPerPlayer = SpawnConfigManager.getSpawnLimitPerPlayer(entityName);
    if (numberOfEntities >= spawnLimitPerPlayer * numOfPlayersInsideViewArea * spawnFactor) {
      log.debug("[Player limit] Blocked spawn event for {} ({} >= {}) in {}", entityName,
          numberOfEntities, spawnLimitPerPlayer, worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Expensive calculation to Limit spawn based on real entities within player position.
    int numberOfEntitiesInsideViewArea = EntityManager.getNumberOfEntitiesInPlayerPositions(
        worldName, entityName, playersPositionsInsideViewArea);
    if (numberOfEntitiesInsideViewArea >= spawnLimitPerPlayer * numOfPlayersInsideViewArea
        * spawnFactor) {
      log.debug("[View Area Limit] Blocked spawn event for {} ({} >= {}) in {}", entityName,
          numberOfEntities, spawnLimitPerPlayer, worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    log.debug("[Check Spawn] Allow spawn event for {} in {}", entityName, worldName);
    event.setResult(Event.Result.DEFAULT);
  }

  @SubscribeEvent
  public static void handleLivingSpecialSpawnEvent(LivingSpawnEvent.SpecialSpawn event) {
    Entity entity = event.getEntity();
    String entityName = entity.getEntityString();
    String worldName = entity.getEntityWorld().getDimensionKey().getLocation().toString();

    if (allowList.contains(entityName)) {
      log.debug("[Allowed Entity] Allow spawn event for {} in {} ", entity, worldName);
      event.setResult(Event.Result.DEFAULT);
      return;
    }

    if (denyList.contains(entityName)) {
      log.debug("[Denied Entity] Denied spawn event for {} in {}", entityName, worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    log.debug("[Special Spawn] Allow spawn Event {}", event.getEntity());
  }

  public static void updateGameDifficulty(Difficulty difficulty) {
    log.debug("Changed game difficulty to {}", difficulty);
    switch (difficulty) {
      case EASY:
        difficultyFactor = 0.75;
        break;
      case NORMAL:
      case PEACEFUL:
        difficultyFactor = 1;
        break;
      case HARD:
        difficultyFactor = 1.5;
        break;
      default:
        difficultyFactor = 1;
    }
  }

}
