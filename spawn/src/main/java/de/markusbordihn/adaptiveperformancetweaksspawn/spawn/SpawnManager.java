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
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.Level;

import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.living.LivingSpawnEvent;
import net.minecraftforge.event.entity.living.LivingSpawnEvent.SpecialSpawn;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweakscore.message.WarnMessages;
import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLevelLoadEvent;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLevelLoad.ServerLevelLoadLevel;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.CommonConfig;

@EventBusSubscriber
public class SpawnManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static Map<String, ServerLevelLoadLevel> serverLevelLoadLevel = new ConcurrentHashMap<>();
  private static Set<String> allowList = new HashSet<>(COMMON.spawnAllowList.get());
  private static Set<String> denyList = new HashSet<>(COMMON.spawnDenyList.get());
  private static Set<String> ignoreDimensionList =
      new HashSet<>(COMMON.spawnIgnoreDimensionList.get());
  private static boolean spawnLimitationEnabled = COMMON.spawnLimitationEnabled.get();
  private static int spawnLimitationLimiter = COMMON.spawnLimitationLimiter.get();
  private static int spawnLimitationMaxMobsPerPlayer = COMMON.spawnLimitationMaxMobsPerPlayer.get();
  private static int spawnLimitationMaxMobsPerWorld = COMMON.spawnLimitationMaxMobsPerWorld.get();
  private static int spawnLimitationMaxMobsPerServer = COMMON.spawnLimitationMaxMobsPerServer.get();

  private static int spawnLimiter = 0;
  private static boolean hasHighServerLoad = false;
  private static Entity lastAllowedSpawnEntity;

  protected SpawnManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    allowList = new HashSet<>(COMMON.spawnAllowList.get());
    denyList = new HashSet<>(COMMON.spawnDenyList.get());
    ignoreDimensionList = new HashSet<>(COMMON.spawnIgnoreDimensionList.get());
    spawnLimitationEnabled = COMMON.spawnLimitationEnabled.get();
    spawnLimitationLimiter = COMMON.spawnLimitationLimiter.get();
    spawnLimitationMaxMobsPerPlayer = COMMON.spawnLimitationMaxMobsPerPlayer.get();
    spawnLimitationMaxMobsPerWorld = COMMON.spawnLimitationMaxMobsPerWorld.get();
    spawnLimitationMaxMobsPerServer = COMMON.spawnLimitationMaxMobsPerServer.get();
  }

  @SubscribeEvent
  public static void handleServerStarting(ServerStartingEvent event) {
    if (!allowList.isEmpty()) {
      log.info("{} Spawn allow list: {}", Constants.LOG_PREFIX, allowList);
    }
    if (!denyList.isEmpty()) {
      log.info("{} Spawn deny list: {}", Constants.LOG_PREFIX, denyList);
    }
    if (!ignoreDimensionList.isEmpty()) {
      log.info("{} Ignore dimension list: {}", Constants.LOG_PREFIX, ignoreDimensionList);
    }
    if (Boolean.TRUE.equals(COMMON.spawnAggressiveMode.get())) {
      log.warn("Enable more aggressive spawn optimizations!");
    } else {
      log.info(
          "If you want to use a more aggressive spawn optimization, please set 'spawnAggressiveMode' to 'true'");
    }
    if (Boolean.FALSE.equals(COMMON.viewAreaEnabled.get())) {
      log.info("Disable view area optimizations!");
    }
    if (spawnLimitationEnabled) {
      if (spawnLimitationLimiter > 0) {
        log.info("{} \u2713 Enable limiter and block randomly every {} mob from spawning ...",
            Constants.LOG_PREFIX, spawnLimitationLimiter);
      }
      if (spawnLimitationMaxMobsPerPlayer > 0) {
        log.info("{} \u2713 Enable spawn rate control with max {} per player ...",
            Constants.LOG_PREFIX, spawnLimitationMaxMobsPerPlayer);
      }
      if (spawnLimitationMaxMobsPerServer > 0) {
        log.info("{} \u2713 Enable spawn rate control with max {} per server ...",
            Constants.LOG_PREFIX, spawnLimitationMaxMobsPerServer);
      }
    }

    // Added optimization warning for specific Mods
    if (CoreConstants.PERFORMANT_LOADED) {
      log.warn(() -> WarnMessages.coreModWarning(CoreConstants.PERFORMANT_NAME));
    }
    if (CoreConstants.POKECUBE_AIO_LOADED) {
      log.warn(() -> WarnMessages.knownIssuesSpawnModWarning(CoreConstants.POKECUBE_AIO_NAME));
    }
    if (CoreConstants.SODIUM_LOADED) {
      log.error(() -> WarnMessages.coreModWarning(CoreConstants.SODIUM_NAME));
    }
    if (CoreConstants.RUBIDIUM_LOADED) {
      log.error(() -> WarnMessages.coreModWarning(CoreConstants.RUBIDIUM_NAME));
    }
    if (CoreConstants.INCONTROL_LOADED) {
      log.warn(() -> WarnMessages.conflictingFeaturesModWarning(CoreConstants.INCONTROL_NAME,
          "controls the mob spawns and entity spawns"));
    }

    // Disabled optimization warning for specific Mods
    if (CoreConstants.CREATE_LOADED) {
      log.warn(() -> WarnMessages.disabledOptimizationModWarning(CoreConstants.CREATE_NAME));
    }
    if (CoreConstants.BIGGER_REACTORS_LOADED) {
      log.warn(
          () -> WarnMessages.disabledOptimizationModWarning(CoreConstants.BIGGER_REACTORS_NAME));
    }
    if (CoreConstants.BOTANIA_LOADED) {
      log.warn(() -> WarnMessages.disabledOptimizationModWarning(CoreConstants.BOTANIA_NAME));
    }
    if (CoreConstants.CREATE_LOADED) {
      log.warn(() -> WarnMessages.disabledOptimizationModWarning(CoreConstants.CREATE_NAME));
    }
    if (CoreConstants.INDUSTRIAL_FOREGOING_LOADED) {
      log.warn(() -> WarnMessages
          .disabledOptimizationModWarning(CoreConstants.INDUSTRIAL_FOREGOING_NAME));
    }
    if (CoreConstants.MEKANISM_LOADED) {
      log.warn(() -> WarnMessages.disabledOptimizationModWarning(CoreConstants.MEKANISM_NAME));
    }
    if (CoreConstants.PIPEZ_LOADED) {
      log.warn(() -> WarnMessages.disabledOptimizationModWarning(CoreConstants.PIPEZ_NAME));
    }
    if (CoreConstants.POKECUBE_AIO_LOADED) {
      log.warn(() -> WarnMessages.disabledOptimizationModWarning(CoreConstants.POKECUBE_AIO_NAME));
    }
    if (CoreConstants.REFINED_STORAGE_LOADED) {
      log.warn(
          () -> WarnMessages.disabledOptimizationModWarning(CoreConstants.REFINED_STORAGE_NAME));
    }
    if (CoreConstants.ULTIMATE_CAR_LOADED) {
      log.warn(() -> WarnMessages.disabledOptimizationModWarning(CoreConstants.ULTIMATE_CAR_NAME));
    }
    if (CoreConstants.VIESCRAFT_MACHINES_LOADED) {
      log.warn(
          () -> WarnMessages.disabledOptimizationModWarning(CoreConstants.VIESCRAFT_MACHINES_NAME));
    }
    if (CoreConstants.XNET_LOADED) {
      log.warn(() -> WarnMessages.disabledOptimizationModWarning(CoreConstants.XNET_NAME));
    }

  }

  @SubscribeEvent
  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (event.hasChanged()) {
      hasHighServerLoad = event.hasHighServerLoad();
    }
  }

  @SubscribeEvent
  public static void handleServerLevelLoadEvent(ServerLevelLoadEvent event) {
    if (event.hasChanged()) {
      serverLevelLoadLevel.put(event.getServerLevelName(), event.getServerLevelLoadLevel());
    }
  }

  // CheckSpawn is fired when an Entity is about to be spawned.
  @SubscribeEvent(priority = EventPriority.HIGHEST)
  public static void handleLivingCheckSpawnEvent(LivingSpawnEvent.CheckSpawn event) {
    handleSpawnEvent(event);
  }

  // SpecialSpawn is fired when an Entity is to be spawned.
  @SubscribeEvent(priority = EventPriority.HIGHEST)
  public static void handleLivingSpecialSpawnEvent(LivingSpawnEvent.SpecialSpawn event) {
    handleSpawnEvent(event);
  }

  // EntityJoinWorldEvent is fired when an Entity joins the world.
  // This event is cancelable and does not have a result.
  @SubscribeEvent(priority = EventPriority.HIGHEST)
  public static void handleEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    handleSpawnEvent(event);
  }

  private static void handleSpawnEvent(EntityEvent event) {
    // Ignore events which are already canceled or denied.
    if (event.isCanceled() || event.getResult() == Event.Result.DENY) {
      log.debug("[Canceled / denied Spawn Event] Ignore spawn event {}!", event);
      return;
    }

    // Ignore client side events.
    Entity entity = event.getEntity();
    Level level = entity.level;
    if (level.isClientSide()) {
      return;
    }

    String entityName = entity.getEncodeId();
    String levelName = level.dimension().location().toString();
    String eventType = event instanceof LivingSpawnEvent ? "spawn" : "join";

    // Pre-check for ignored dimension to avoid further checks
    if (ignoreDimensionList.contains(levelName)) {
      log.debug("[Ignored Dimension] Allow spawn event for {} in {}", entity, levelName);
      return;
    }

    // Entity instance checks to ignore specific and short living entities like projectiles.
    if (!CoreEntityManager.isRelevantEntity(entity, entityName)) {
      return;
    }

    // Pre-check for allowed entities to avoid expensive calculations
    if (allowList.contains(entityName)) {
      log.debug("[Allowed Entity] Allow spawn event for {} in {}", entity, levelName);
      return;
    }

    // Skip already checked entities.
    if (lastAllowedSpawnEntity == entity) {
      return;
    }

    // Pre-check for denied entities to avoid expensive calculations.
    if (denyList.contains(entityName)) {
      log.debug("[Denied Entity] Denied {} event for {} in {}", eventType, entity, levelName);
      cancelSpawnEvent(event);
      return;
    }

    // Ignore entities with custom name (e.g. name tags) regardless of type
    if (entity.hasCustomName()) {
      log.debug("[Custom Entity] Skip {} event for {} in {}", eventType, entity, levelName);
      return;
    }

    // Spawn Limitations: Max mobs per Server
    int numberOfEntities = CoreEntityManager.getNumberOfEntities(entityName);
    if (spawnLimitationMaxMobsPerServer > 0
        && numberOfEntities >= spawnLimitationMaxMobsPerServer) {
      log.debug(
          "[Spawn Limitations Server] Blocked {} event for {} with {} entities of max {} in {}.",
          eventType, entity, numberOfEntities, levelName);
      cancelSpawnEvent(event);
      return;
    }

    // Spawn Limitations: Max mobs per World
    int numberOfEntitiesPerWorld = CoreEntityManager.getNumberOfEntities(levelName, entityName);
    if (spawnLimitationMaxMobsPerWorld > 0
        && numberOfEntitiesPerWorld >= spawnLimitationMaxMobsPerWorld) {
      log.debug(
          "[Spawn Limitations World] Blocked {} event for {} with {} entities of max {} in {}.",
          eventType, entity, numberOfEntitiesPerWorld, numberOfEntitiesPerWorld, levelName);
      cancelSpawnEvent(event);
      return;
    }

    // Get current players positions for later calculations
    List<PlayerPosition> playersPositionsInsideViewArea = null;
    Integer numOfPlayersInsideViewArea = 0;
    if (event instanceof LivingSpawnEvent livingSpawnEvent
        && Boolean.TRUE.equals(COMMON.viewAreaEnabled.get())) {
      playersPositionsInsideViewArea = PlayerPositionManager.getPlayerPositionsInsideViewArea(
          levelName, (int) livingSpawnEvent.getX(), (int) livingSpawnEvent.getY(),
          (int) livingSpawnEvent.getZ());

      // Limit spawns to optimized players view area for all mods.
      numOfPlayersInsideViewArea = playersPositionsInsideViewArea.size();
      if (numOfPlayersInsideViewArea == 0) {
        log.debug(
            "[View Area Visibility] Blocked {} event for {} with {} entities and {} players in {}.",
            eventType, entity, numberOfEntities, numOfPlayersInsideViewArea, levelName);
        cancelSpawnEvent(event);
        return;
      }
    }

    // Limit spawns randomly every x times.
    if (spawnLimitationLimiter > 0 && spawnLimiter++ >= spawnLimitationLimiter) {
      log.debug("[Spawn Limiter {}] Blocked {} event for {} with {} entities in {}.",
          spawnLimitationLimiter, eventType, entity, numberOfEntities, levelName);
      cancelSpawnEvent(event);
      spawnLimiter = 0;
      return;
    }

    // Spawn Limitations: Max mobs per player
    int numberOfEntitiesInsideViewArea = 0;
    if (playersPositionsInsideViewArea != null) {
      numberOfEntitiesInsideViewArea = CoreEntityManager.getNumberOfEntitiesInPlayerPositions(
          levelName, entityName, playersPositionsInsideViewArea);
      if (spawnLimitationMaxMobsPerPlayer > 0
          && numberOfEntitiesInsideViewArea >= spawnLimitationMaxMobsPerPlayer) {
        log.debug(
            "[Spawn Limitations Player] Blocked {} event for {} with {} entities of max {} and {} players in {}.",
            eventType, entity, numberOfEntitiesInsideViewArea, spawnLimitationMaxMobsPerPlayer,
            numOfPlayersInsideViewArea, levelName);
        cancelSpawnEvent(event);
        return;
      }
    }

    // Get the current level load for supporting less aggressive mode.
    ServerLevelLoadLevel levelLoadLevel =
        serverLevelLoadLevel.computeIfAbsent(levelName, key -> ServerLevelLoad
            .getLevelNameLoadLevel().getOrDefault(key, ServerLevelLoadLevel.NORMAL));

    // Use more aggressive spawn limitation in the case user has enabled aggressive mode or
    // if the general server load or the specific level load of the entity is high.
    boolean aggressiveMode = Boolean.TRUE.equals(COMMON.spawnAggressiveMode.get());
    boolean limitSpawnPerLimits = Boolean.TRUE.equals(
        hasHighServerLoad || ServerLevelLoad.hasHighLevelLoad(levelLoadLevel) || aggressiveMode);
    if (limitSpawnPerLimits) {
      // Get current game difficult to define spawn factor, if not in aggressive mode.
      double spawnFactor = aggressiveMode ? 1 : ServerManager.getGameDifficultyFactor();

      // Get limits per player, world and number of current players.
      int limitPerWorld = SpawnConfigManager.getSpawnLimitPerWorld(entityName);
      int limitPerPlayer = SpawnConfigManager.getSpawnLimitPerPlayer(entityName);
      int limitPerServer = SpawnConfigManager.getSpawnLimitPerServer(entityName);
      int numberOfPlayers = ServerManager.getNumberOfPlayers();

      // Limit spawn based on server limits, but consider current number of players and limits per
      // player.
      if (limitPerServer > 0 && (numberOfPlayers * limitPerPlayer <= limitPerServer)
          && numberOfEntities >= limitPerServer * spawnFactor) {
        log.debug("[Server limit!] Blocked {} event for {} with {} entities of max {} in {}",
            eventType, entityName, numberOfEntities, limitPerServer, levelName);
        cancelSpawnEvent(event);
        return;
      }

      // Limit spawn based on world limits, but consider current number of players and limits per
      // player.
      if (limitPerWorld > 0 && (numberOfPlayers * limitPerPlayer <= limitPerWorld)
          && numberOfEntitiesPerWorld >= limitPerWorld * spawnFactor) {
        log.debug("[World limit!] Blocked {} event for {} with {} entities of max {} in {}",
            eventType, entityName, numberOfEntitiesPerWorld, limitPerWorld, levelName);
        cancelSpawnEvent(event);
        return;
      }

      // Cheap and fast calculation to limit spawn based on possible entities within player limits
      // for high server load.
      if (hasHighServerLoad && limitPerPlayer > 0 && numOfPlayersInsideViewArea > 0
          && numberOfEntitiesPerWorld >= limitPerPlayer * limitPerPlayer
              * numOfPlayersInsideViewArea * spawnFactor) {
        log.debug(
            "[High Server Load!] Blocked {} event for {} with {} entities of max {} and {} players in {}",
            eventType, entityName, numberOfEntitiesPerWorld, limitPerPlayer,
            numOfPlayersInsideViewArea, levelName);
        cancelSpawnEvent(event);
        return;
      }

      // Expensive calculation to Limit spawn based on real entities within player position.
      if (limitPerPlayer > 0 && numberOfEntitiesInsideViewArea > 0
          && numberOfEntitiesInsideViewArea >= limitPerPlayer * numOfPlayersInsideViewArea
              * spawnFactor) {
        log.debug(
            "[View Area Limit!] Blocked {} event for {} with {} entities of max {} and {} players in {}",
            eventType, entityName, numberOfEntitiesInsideViewArea, limitPerPlayer,
            numOfPlayersInsideViewArea, levelName);
        cancelSpawnEvent(event);
        return;
      }
    }

    // Debug messages
    if (!limitSpawnPerLimits) {
      log.debug("[Allow {} (low load)] For {} in {} and {} in world and {} global", eventType,
          entity, levelName, numberOfEntitiesPerWorld, numberOfEntities);
    } else if (numberOfEntitiesInsideViewArea > 0) {
      log.debug("[Allow {}] For {} in {} with {} in view area and {} in world and {} global",
          eventType, entity, levelName, numberOfEntitiesInsideViewArea, numberOfEntitiesPerWorld,
          numberOfEntities);
    } else {
      log.debug("[Allow {}] For {} in {} with {} in world and {} global", eventType, entity,
          levelName, numberOfEntitiesPerWorld, numberOfEntities);
    }

    // Cache result for avoid duplicated checks.
    lastAllowedSpawnEntity = entity;
  }

  // Cancel spawn event.
  private static void cancelSpawnEvent(EntityEvent event) {
    if (event instanceof LivingSpawnEvent && !(event instanceof SpecialSpawn)) {
      event.setResult(Event.Result.DENY);
    } else {
      event.setCanceled(true);
    }
  }

}
