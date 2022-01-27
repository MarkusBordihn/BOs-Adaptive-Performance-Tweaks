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

package de.markusbordihn.adaptiveperformancetweaksspawn.spawn;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.Level;

import net.minecraftforge.event.entity.living.LivingSpawnEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweakscore.entity.EntityManager;
import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.CommonConfig;

@EventBusSubscriber
public class SpawnManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static Set<String> allowList = new HashSet<>(COMMON.spawnAllowList.get());
  private static Set<String> denyList = new HashSet<>(COMMON.spawnDenyList.get());
  private static boolean spawnLimitationEnabled = COMMON.spawnLimitationEnabled.get();
  private static int spawnLimitationLimiter = COMMON.spawnLimitationLimiter.get();
  private static int spawnLimitationMaxMobsPerPlayer = COMMON.spawnLimitationMaxMobsPerPlayer.get();
  private static int spawnLimitationMaxMobsPerWorld = COMMON.spawnLimitationMaxMobsPerWorld.get();

  private static int spawnLimiter = 0;

  protected SpawnManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    allowList = new HashSet<>(COMMON.spawnAllowList.get());
    denyList = new HashSet<>(COMMON.spawnDenyList.get());
    spawnLimitationEnabled = COMMON.spawnLimitationEnabled.get();
    spawnLimitationLimiter = COMMON.spawnLimitationLimiter.get();
    spawnLimitationMaxMobsPerPlayer = COMMON.spawnLimitationMaxMobsPerPlayer.get();
    spawnLimitationMaxMobsPerWorld = COMMON.spawnLimitationMaxMobsPerWorld.get();
  }

  @SubscribeEvent
  public static void handleServerStarting(ServerStartingEvent event) {
    if (!allowList.isEmpty()) {
      log.info("Spawn allow List: {}", allowList);
    }
    if (!denyList.isEmpty()) {
      log.info("Spawn deny List: {}", denyList);
    }
    if (spawnLimitationEnabled) {
      if (spawnLimitationLimiter > 0) {
        log.info("\u2713 Enable limiter and block every {} mob from spawning ...",
            spawnLimitationLimiter);
      }
      log.info("\u2713 Enable spawn rate control with maxPerPlayer:{} and maxPerWorld:{} ...",
          spawnLimitationMaxMobsPerPlayer, spawnLimitationMaxMobsPerWorld);
    }
  }

  @SubscribeEvent(priority = EventPriority.HIGHEST)
  public static void handleLivingCheckSpawnEvent(LivingSpawnEvent.CheckSpawn event) {
    handleSpawnEvent(event);
  }

  @SubscribeEvent(priority = EventPriority.HIGHEST)
  public static void handleLivingSpecialSpawnEvent(LivingSpawnEvent.SpecialSpawn event) {
    handleSpawnEvent(event);
  }

  private static void handleSpawnEvent(LivingSpawnEvent event) {
    Entity entity = event.getEntity();
    String entityName = entity.getEncodeId();
    Level level = entity.level;
    String levelName = level.dimension().location().toString();

    // Pre-check for relevant entities.
    if (!isRelevantEntity(entity, entityName, levelName)) {
      return;
    }

    // Pre-check for denied entities to avoid expensive calculations.
    if (denyList.contains(entityName)) {
      log.debug("[Denied Entity] Denied spawn event for {} in {} ", entity, levelName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Ignore entities with custom name (e.g. name tags) regardless of type
    if (entity.hasCustomName()) {
      log.debug("[Custom Entity] Skip spawn event for {} in {} ", entity, levelName);
      return;
    }

    // Get current players positions for later calculations
    List<PlayerPosition> playersPositionsInsideViewArea =
        PlayerPositionManager.getPlayerPositionsInsideViewArea(levelName, (int) event.getX(),
            (int) event.getY(), (int) event.getZ());

    // Limit spawns to optimized players view area for supported mods.
    Integer numOfPlayersInsideViewArea = playersPositionsInsideViewArea.size();
    if (numOfPlayersInsideViewArea == 0) {
      log.debug("[View Area Visibility] Blocked spawn event for {} in {}.", entity, levelName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Limit spawns randomly every x times.
    if (spawnLimitationLimiter > 0 && spawnLimiter++ == spawnLimitationLimiter) {
      log.debug("[Spawn Limiter] Blocked spawn event for {} in {}.", entity, levelName);
      event.setResult(Event.Result.DENY);
      spawnLimiter = 0;
      return;
    }

    // Get the number of current entities for this world.
    int numberOfEntities = EntityManager.getNumberOfEntities(levelName, entityName);

    // Limit spawn based on world limits.
    if (spawnLimitationMaxMobsPerWorld > 0 && numberOfEntities >= spawnLimitationMaxMobsPerWorld) {
      log.debug("[World limit] Blocked spawn event for {} ({} >= {}) in {}", entityName,
          numberOfEntities, spawnLimitationMaxMobsPerWorld, levelName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Cheap and fast calculation to limit spawn based on possible entities within player limits.
    if (spawnLimitationMaxMobsPerPlayer > 0
        && numberOfEntities >= spawnLimitationMaxMobsPerPlayer * numOfPlayersInsideViewArea) {
      log.debug("[Player limit] Blocked spawn event for {} ({} >= {} * {}) in {}", entityName,
          numberOfEntities, spawnLimitationMaxMobsPerPlayer, levelName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Expensive calculation to Limit spawn based on real entities within player position.
    int numberOfEntitiesInsideViewArea = EntityManager.getNumberOfEntitiesInPlayerPositions(
        levelName, entityName, playersPositionsInsideViewArea);
    if (spawnLimitationMaxMobsPerPlayer > 0
        && numberOfEntitiesInsideViewArea >= spawnLimitationMaxMobsPerPlayer
            * numOfPlayersInsideViewArea) {
      log.debug("[View Area Limit] Blocked spawn event for {} ({} >= {} * {}) in {}", entityName,
          numberOfEntitiesInsideViewArea, spawnLimitationMaxMobsPerPlayer, levelName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Allow spawn is no rule is matching
    log.debug("[Allow Spawn] For {} in {} with {} in view and {} in world", entityName, levelName,
        numberOfEntitiesInsideViewArea, numberOfEntities);
  }

  private static boolean isRelevantEntity(Entity entity, String entityName, String levelName) {

    // Skip other checks if unknown entity name
    if (entityName == null) {
      if (entity.isMultipartEntity() || entity.getType().toString().contains("body_part")) {
        log.debug("[Multipart Entity] Allow spawn event for {} in {}", entity, levelName);
      } else {
        log.warn(
            "[Unknown Entity] Name for spawn entity {} ({}) in {} is unknown. Please report this issue under {}]!",
            entity, entity.getType(), levelName, Constants.ISSUE_REPORT);
      }
      return false;
    }

    // Pre-check for allowed entities to avoid expensive calculations
    if (allowList.contains(entityName)) {
      log.debug("[Allowed Entity] Allow spawn event for {} in {} ", entity, levelName);
      return false;
    }

    // Entity instance checks to ignore specific and short living entities like projectiles.
    if (entity instanceof Projectile) {
      return false;
    }

    return true;
  }

}
