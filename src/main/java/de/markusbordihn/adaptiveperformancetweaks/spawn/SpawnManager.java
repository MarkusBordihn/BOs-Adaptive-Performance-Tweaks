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

import org.apache.logging.log4j.Logger;

import net.minecraft.entity.Entity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.projectile.DamagingProjectileEntity;
import net.minecraft.entity.projectile.ProjectileEntity;
import net.minecraft.world.Difficulty;
import net.minecraft.world.World;
import net.minecraft.world.spawner.AbstractSpawner;

import net.minecraftforge.event.DifficultyChangeEvent;
import net.minecraftforge.event.entity.living.LivingSpawnEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.server.ServerLifecycleHooks;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.config.SpawnConfigManager;
import de.markusbordihn.adaptiveperformancetweaks.entity.EntityManager;
import de.markusbordihn.adaptiveperformancetweaks.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweaks.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerWorldLoadEvent;

@EventBusSubscriber
public class SpawnManager extends Manager {

  private static Boolean isUntamedWildModLoaded =
      ModList.get().isLoaded(Constants.UNTAMEDWILDS_MOD);
  private static Map<String, Boolean> serverWorldLoadMap = new HashMap<>();
  private static Map<String, Double> serverWorldLoadFactorMap = new HashMap<>();
  private static Map<String, Integer> spawnConfigPerPlayer =
      SpawnConfigManager.getSpawnConfigPerPlayer();
  private static Map<String, Integer> spawnConfigPerWorld =
      SpawnConfigManager.getSpawnConfigPerWorld();
  private static Map<String, Integer> spawnConfigSpecial =
      SpawnConfigManager.getSpawnConfigSpecial();
  private static Set<String> spawnConfigEntity = SpawnConfigManager.getSpawnConfigEntity();
  private static String lastBlockedSpawnEntityByPlayerLimit = "";
  private static String lastBlockedSpawnEntityByViewArea = "";
  private static String lastBlockedSpawnEntityByWorldLimit = "";
  private static double difficultyFactor = 1;
  private static int spawnLimiter = 0;

  private static Set<String> allowList = new HashSet<>(COMMON.spawnAllowList.get());
  private static Set<String> denyList = new HashSet<>(COMMON.spawnDenyList.get());
  private static boolean spawnLimitationEnabled = COMMON.spawnLimitationEnabled.get();
  private static int spawnLimitationLimiter = COMMON.spawnLimitationLimiter.get();
  private static int spawnLimitationMaxMobsPerPlayer = COMMON.spawnLimitationMaxMobsPerPlayer.get();
  private static int spawnLimitationMaxMobsPerWorld = COMMON.spawnLimitationMaxMobsPerWorld.get();

  public static final String LOG_NAME = SpawnManager.class.getSimpleName();
  private static final Logger log = getLogger(LOG_NAME);

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    serverWorldLoadMap = new HashMap<>();
    serverWorldLoadFactorMap = new HashMap<>();
    allowList = new HashSet<>(COMMON.spawnAllowList.get());
    denyList = new HashSet<>(COMMON.spawnDenyList.get());
    spawnLimitationEnabled = COMMON.spawnLimitationEnabled.get();
    spawnLimitationLimiter = COMMON.spawnLimitationLimiter.get();
    spawnLimitationMaxMobsPerPlayer = COMMON.spawnLimitationMaxMobsPerPlayer.get();
    spawnLimitationMaxMobsPerWorld = COMMON.spawnLimitationMaxMobsPerWorld.get();
  }

  @SubscribeEvent
  public static void handleServerStarting(FMLServerStartingEvent event) {
    updateGameDifficulty(ServerLifecycleHooks.getCurrentServer().getWorldData().getDifficulty());
    if (!allowList.isEmpty()) {
      log.info("Allow List: {}", allowList);
    }
    if (!denyList.isEmpty()) {
      log.info("Deny List: {}", denyList);
    }

    // Spawn Limitations Info
    if (spawnLimitationEnabled) {
      if (spawnLimitationLimiter > 0) {
        log.info("\u2713 Enable limiter and block randomly every {} unknown mob from spawning ...",
            spawnLimitationLimiter);
      }
      if (spawnLimitationMaxMobsPerWorld > 0) {
        log.info("\u2713 Enable spawn rate control with max {} per world ...",
            spawnLimitationMaxMobsPerWorld);
      }
      if (spawnLimitationMaxMobsPerPlayer > 0) {
        log.info("\u2713 Enable spawn rate control with max {} per player ...",
            spawnLimitationMaxMobsPerPlayer);
      }
    }
  }

  @SubscribeEvent
  public static void handleServerWorldLoadEvent(ServerWorldLoadEvent event) {
    if (event.hasChanged()) {
      String worldName = event.getServerWorldName();
      serverWorldLoadFactorMap.put(worldName, event.getServerWorldLoadLevelFactor());
      serverWorldLoadMap.put(worldName, event.hasLowServerWorldLoad());
    }
  }

  @SubscribeEvent
  public static void handleDifficultyChangeEvent(DifficultyChangeEvent event) {
    updateGameDifficulty(event.getDifficulty());
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
    World world = entity.level;
    String worldName = world.dimension().location().toString();

    // Skip other checks if unknown entity name
    if (entityName == null) {
      if (entity.isMultipartEntity() || entity.getType().toString().contains("body_part")) {
        log.debug("[Multipart Entity] Allow spawn event for {} in {}", entity, worldName);
      } else {
        log.warn(
            "Unknown entity name for spawn entity {} ({}) in {}. Please report this issue under {}]!",
            entity, entity.getType(), worldName, Constants.ISSUE_REPORT);
      }
      event.setResult(Event.Result.DEFAULT);
      return;
    }

    // Pre-check for allowed entities to avoid expensive calculations
    if (allowList.contains(entityName)) {
      log.debug("[Allowed Entity] Allow spawn event for {} in {} ", entity, worldName);
      event.setResult(Event.Result.DEFAULT);
      return;
    }

    // Pre-check for denied entities to avoid expensive calculations
    if (denyList.contains(entityName)) {
      log.debug("[Denied Entity] Denied spawn event for {} in {} ", entity, worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Ignore entities with custom name (e.g. name tags) regardless of type
    if (entity.hasCustomName()) {
      log.debug("[Custom Entity] Skip spawn event for {} in {} ", entity, worldName);
      event.setResult(Event.Result.DEFAULT);
      return;
    }

    // Entity instance checks to ignore specific and short living entities like projectiles.
    if (entity instanceof ProjectileEntity || entity instanceof DamagingProjectileEntity) {
      return;
    }

    // Get current players positions for later calculations
    List<PlayerPosition> playersPositionsInsideViewArea =
        PlayerPositionManager.getPlayerPositionsInsideViewArea(worldName, (int) event.getX(),
            (int) event.getY(), (int) event.getZ());

    // Limit spawns to optimized players view area for supported mods.
    Integer numOfPlayersInsideViewArea = playersPositionsInsideViewArea.size();
    if (numOfPlayersInsideViewArea == 0) {
      log.debug("[View Area Visibility] Blocked spawn event for {} in {}.", entity, worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Check if entity should be optimized otherwise ignore entity for advanced calculations.
    if (!spawnConfigEntity.contains(entityName)
        && !spawnConfigEntity.contains(worldName + ':' + entityName)) {

      // Perform basic spawn optimization if spawn limitations are enabled.
      if (spawnLimitationEnabled) {

        // Limited spawn randomly for all "unknown" entities.
        if (spawnLimitationLimiter > 0 && spawnLimiter++ >= spawnLimitationLimiter) {
          log.debug("[Spawn Limiter {}] Blocked spawn event for unknown {} in {}.",
              spawnLimitationLimiter, entity, worldName);
          event.setResult(Event.Result.DENY);
          spawnLimiter = 0;
          return;
        }

        // Limit spawn per world, if enabled
        int numberOfEntities = EntityManager.getNumberOfEntities(worldName, entityName);
        if (spawnLimitationMaxMobsPerWorld > 0
            && numberOfEntities >= spawnLimitationMaxMobsPerWorld) {
          log.debug("[World limit] Blocked spawn event for unknown {} ({} >= {}) in {}", entityName,
              numberOfEntities, spawnLimitationMaxMobsPerWorld, worldName);
          event.setResult(Event.Result.DENY);
          return;
        }

        // Limit spawn per player, if enabled
        if (spawnLimitationMaxMobsPerPlayer > 0
            && numberOfEntities >= spawnLimitationMaxMobsPerPlayer * numOfPlayersInsideViewArea) {
          log.debug("[Player limit] Blocked spawn event for unknown {} ({} >= {} * {}) in {}",
              entityName, numberOfEntities, spawnLimitationMaxMobsPerPlayer, worldName);
          event.setResult(Event.Result.DENY);
          return;
        }

        // Expensive calculation to Limit spawn based on real entities within player position.
        int numberOfEntitiesInsideViewArea = EntityManager.getNumberOfEntitiesInPlayerPositions(
            worldName, entityName, playersPositionsInsideViewArea);
        if (spawnLimitationMaxMobsPerPlayer > 0
            && numberOfEntitiesInsideViewArea >= spawnLimitationMaxMobsPerPlayer
                * numOfPlayersInsideViewArea) {
          log.debug("[View Area Limit] Blocked spawn event for unknown {} ({} >= {} * {}) in {}",
              entityName, numberOfEntitiesInsideViewArea, spawnLimitationMaxMobsPerPlayer,
              worldName);
          event.setResult(Event.Result.DENY);
          return;
        }
      }

      log.debug("[Untracked Entity] Skip spawn event for {} in {}", entityName, worldName);
      event.setResult(Event.Result.DEFAULT);
      return;
    }

    // Get current world server load for later calculation.
    boolean hasLowWorldLoad = serverWorldLoadMap.getOrDefault(worldName, false);

    // Check if entity is from spawner ignore these as long the server load is low.
    AbstractSpawner spawner = null;
    if (event instanceof LivingSpawnEvent.CheckSpawn) {
      spawner = ((LivingSpawnEvent.CheckSpawn) event).getSpawner();
      if (spawner != null && hasLowWorldLoad) {
        log.debug("[Spawner Entity] Ignore spawn event for {} in {}", entityName, worldName);
        return;
      }
    }

    // Defines the spawn factor based on difficulty setting and world load.
    double serverWorldLoadFactor = serverWorldLoadFactorMap.getOrDefault(worldName, 1.0);
    double spawnFactor =
        (entity instanceof MonsterEntity) ? serverWorldLoadFactor * difficultyFactor
            : serverWorldLoadFactor;

    // Get the number of current entities for this world.
    int numberOfEntities = EntityManager.getNumberOfEntities(worldName, entityName);

    // Check for special spawn limits for specific dimensions like the_end and nether.
    int spawnLimitSpecial = spawnConfigSpecial.getOrDefault(worldName + ':' + entityName, -1);
    if (spawnLimitSpecial >= 0) {
      if (numberOfEntities >= spawnLimitSpecial * spawnFactor) {
        log.debug("[Special limit] Blocked spawn event for {} ({} >= {} * {}) in {}", entityName,
            numberOfEntities, spawnLimitSpecial, spawnFactor, worldName);
        event.setResult(Event.Result.DENY);
        return;
      } else {
        log.debug("[Allow Special Spawn] For {} ({}) in {}", entityName, numberOfEntities,
            worldName);
        event.setResult(Event.Result.DEFAULT);
        return;
      }
    }

    // Limit spawn based on world limits.
    int spawnLimitPerWorld = spawnConfigPerWorld.getOrDefault(entityName, -1);
    if (spawnLimitPerWorld >= 0 && numberOfEntities >= spawnLimitPerWorld * spawnFactor) {
      if (!lastBlockedSpawnEntityByWorldLimit.equals(entityName)) {
        lastBlockedSpawnEntityByWorldLimit = entityName;
        log.debug("[World limit] Blocked spawn event for {} ({} >= {} * {}) in {}", entityName,
            numberOfEntities, spawnLimitPerWorld, spawnFactor, worldName);
      }
      event.setResult(Event.Result.DENY);
      return;
    }

    // Cheap and fast calculation to limit spawn based on possible entities within
    // player limits.
    int spawnLimitPerPlayer = spawnConfigPerPlayer.getOrDefault(entityName, -1);
    if (spawnLimitPerPlayer >= 0
        && numberOfEntities >= spawnLimitPerPlayer * numOfPlayersInsideViewArea * spawnFactor) {
      if (!lastBlockedSpawnEntityByPlayerLimit.equals(entityName)) {
        lastBlockedSpawnEntityByPlayerLimit = entityName;
        log.debug("[Player limit] Blocked spawn event for {} ({} >= {} * {}) in {}", entityName,
            numberOfEntities, spawnLimitPerPlayer, spawnFactor, worldName);
      }
      event.setResult(Event.Result.DENY);
      return;
    }

    // Check if we should perform an expensive view area check, not all mods supporting a limited
    // view area and so we need to exclude some specific mods.
    boolean runViewAreaCheck = true;
    if (Boolean.TRUE.equals(isUntamedWildModLoaded) && entityName.startsWith("untamedwilds:")) {
      runViewAreaCheck = false;
    }

    // Expensive calculation to Limit spawn based on real entities within player
    // position.
    int numberOfEntitiesInsideViewArea = EntityManager.getNumberOfEntitiesInPlayerPositions(
        worldName, entityName, playersPositionsInsideViewArea);
    if (runViewAreaCheck && numberOfEntitiesInsideViewArea >= spawnLimitPerPlayer
        * numOfPlayersInsideViewArea * spawnFactor) {
      if (!lastBlockedSpawnEntityByViewArea.equals(entityName)) {
        lastBlockedSpawnEntityByViewArea = entityName;
        log.debug("[View Area Limit] Blocked spawn event for {} ({} >= {} * {}) in {}", entityName,
            numberOfEntitiesInsideViewArea, spawnLimitPerPlayer, spawnFactor, worldName);
      }
      event.setResult(Event.Result.DENY);
      return;
    }

    // Allow spawn is no rule is matching and entity is not from spawner to avoid log spawn.
    if (spawner == null) {
      if (event instanceof LivingSpawnEvent.SpecialSpawn) {
        log.debug("[Special Spawn Event] Allow special spawn Event {}", entity);
      } else {
        log.debug("[Allow Spawn] For {} in {} with {} in view and {} in world", entityName,
            worldName, numberOfEntitiesInsideViewArea, numberOfEntities);
        event.setResult(Event.Result.DEFAULT);
      }
    }
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
