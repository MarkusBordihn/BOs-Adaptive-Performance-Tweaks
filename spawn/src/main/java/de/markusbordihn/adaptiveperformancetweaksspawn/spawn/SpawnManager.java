/*
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

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweakscore.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweakscore.message.WarnMessages;
import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweakscore.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLevelLoad.ServerLevelLoadLevel;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLevelLoadEvent;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweakscore.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;
import de.markusbordihn.adaptiveperformancetweaksspawn.config.CommonConfig;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.Level;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityEvent;
import net.minecraftforge.event.entity.EntityJoinLevelEvent;
import net.minecraftforge.event.entity.living.LivingConversionEvent;
import net.minecraftforge.event.entity.living.MobSpawnEvent;
import net.minecraftforge.event.entity.living.MobSpawnEvent.FinalizeSpawn;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartedEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber
public class SpawnManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  private static final int SERVER_STARTED_DELAY_TICKS = 20 * 20; // startup delay 20 seconds

  private static final Map<String, ServerLevelLoadLevel> serverLevelLoadLevel =
      new ConcurrentHashMap<>();
  private static Set<String> allowList = new HashSet<>();
  private static Set<String> denyList = new HashSet<>();
  private static Set<String> ignoreDimensionList = new HashSet<>();

  // Cache
  private static boolean allowZombieVillagerConversion = false;
  private static boolean hasHighServerLoad = false;
  private static boolean serverStarted = false;
  private static boolean serverStartedDelay = false;
  private static Entity lastAllowedSpawnEntity;
  private static Entity lastBlockedSpawnEntity;
  private static int friendlyChunkCounter = 0;
  private static int serverStartedDelayTicks = 0;
  private static int spawnLimiter = 0;

  protected SpawnManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    // Reset start delay
    serverStarted = false;
    serverStartedDelay = false;
    serverStartedDelayTicks = 0;

    // Reset cache
    allowList = new HashSet<>(COMMON.spawnAllowList.get());
    denyList = new HashSet<>(COMMON.spawnDenyList.get());
    ignoreDimensionList = new HashSet<>(COMMON.spawnIgnoreDimensionList.get());
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
    if (Boolean.TRUE.equals(COMMON.spawnLimitationEnabled.get())) {
      if (COMMON.spawnLimitationLimiter.get() > 0) {
        log.info(
            "{} ✓ Enable limiter and block randomly every {} mob from spawning ...",
            Constants.LOG_PREFIX,
            COMMON.spawnLimitationLimiter.get());
      }
      if (COMMON.spawnLimitationMaxMobsPerPlayer.get() > 0) {
        log.info(
            "{} ✓ Enable spawn rate control with max {} per player ...",
            Constants.LOG_PREFIX,
            COMMON.spawnLimitationMaxMobsPerPlayer.get());
      }
      if (COMMON.spawnLimitationMaxMobsPerWorld.get() > 0) {
        log.info(
            "{} ✓ Enable spawn rate control with max {} per world ...",
            Constants.LOG_PREFIX,
            COMMON.spawnLimitationMaxMobsPerWorld.get());
      }
      if (COMMON.spawnLimitationMaxMobsPerServer.get() > 0) {
        log.info(
            "{} ✓ Enable spawn rate control with max {} per server ...",
            Constants.LOG_PREFIX,
            COMMON.spawnLimitationMaxMobsPerServer.get());
      }
    }

    // Added warning for chunk optimization Mods
    if (CoreConstants.CHUNK_PREGEN_LOADED) {
      log.warn(() -> WarnMessages.chunkPregeneratorModWarning(CoreConstants.CHUNK_PREGEN_NAME));
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
      log.warn(
          () ->
              WarnMessages.conflictingFeaturesModWarning(
                  CoreConstants.INCONTROL_NAME, "controls the mob spawns and entity spawns"));
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
      log.warn(
          () ->
              WarnMessages.disabledOptimizationModWarning(CoreConstants.INDUSTRIAL_FOREGOING_NAME));
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
  public static void handleServerStarted(ServerStartedEvent event) {
    serverStarted = true;
  }

  @SubscribeEvent
  public static void handleServerTickEvent(TickEvent.ServerTickEvent event) {
    if (serverStarted && !serverStartedDelay) {
      if (event.phase == TickEvent.Phase.START) {
        if (serverStartedDelayTicks >= SERVER_STARTED_DELAY_TICKS) {
          serverStartedDelay = true;
        }
      } else {
        serverStartedDelayTicks++;
      }
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

  // FinalizeSpawn is fired before a entity is finalized.
  // This event is cancelable and does not have a result.
  @SubscribeEvent(priority = EventPriority.HIGHEST)
  public static void handleFinalizeSpawnEvent(FinalizeSpawn event) {
    if (!event.isSpawnCancelled()) {
      handleSpawnEvent(event);
    }
  }

  // EntityJoinWorldEvent is fired when an Entity joins the world.
  // This event is cancelable and does not have a result.
  @SubscribeEvent(priority = EventPriority.HIGHEST)
  public static void handleEntityJoinWorldEvent(EntityJoinLevelEvent event) {
    handleSpawnEvent(event);
  }

  // LivingConversionEvent is fired when an Entity is converted from one type to another.
  // This event is used to detect villagers that are converted to zombies.
  @SubscribeEvent(priority = EventPriority.HIGHEST)
  public static void handleLivingConversionEvent(LivingConversionEvent.Pre event) {
    handleConversionEvent(event);
  }

  private static void handleConversionEvent(LivingConversionEvent.Pre event) {
    if (event.getEntity() != null
        && event.getEntity().getType() == EntityType.VILLAGER
        && event.getOutcome() == EntityType.ZOMBIE_VILLAGER) {
      log.debug(
          "[Zombie Villager Conversion] Convert {} to {}", event.getEntity(), event.getOutcome());
      allowZombieVillagerConversion = true;
    }
  }

  private static void handleSpawnEvent(EntityEvent event) {

    // Ignore events which are already canceled or denied.
    if (event.isCanceled() || event.getResult() == Event.Result.DENY) {
      log.debug("[Canceled / denied Spawn Event] Ignore spawn event {}!", event);
      return;
    }

    // Ignore client side events.
    Entity entity = event.getEntity();
    Level level = entity.level();
    if (level.isClientSide()) {
      return;
    }

    // Ignore spawn events before server has started.
    if (!serverStarted) {
      log.debug("[Server not started] Ignore spawn event {}!", event);
      return;
    } else if (!serverStartedDelay) {
      log.debug(
          "[Server start delay ({}/{})] Ignore spawn event {}!",
          serverStartedDelayTicks,
          SERVER_STARTED_DELAY_TICKS,
          event);
      return;
    }

    String entityName = entity.getEncodeId();
    String levelName = level.dimension().location().toString();
    String eventType = event instanceof FinalizeSpawn ? "spawn" : "join";

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
    if (lastAllowedSpawnEntity == entity || lastBlockedSpawnEntity == entity) {
      return;
    }

    // Skip zombie villager conversion if allowed.
    if (allowZombieVillagerConversion && "minecraft:zombie_villager".equals(entityName)) {
      log.debug(
          "[Zombie Villager Conversion] Allow {} event for {} in {}", eventType, entity, levelName);
      allowZombieVillagerConversion = false;
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

    // Get possible spawn position for later calculations
    int entitySpawnX = 0;
    int entitySpawnY = 0;
    int entitySpawnZ = 0;
    boolean hasSpawnPosition = false;

    if (event instanceof MobSpawnEvent mobSpawnEvent) {
      entitySpawnX = (int) mobSpawnEvent.getX();
      entitySpawnY = (int) mobSpawnEvent.getY();
      entitySpawnZ = (int) mobSpawnEvent.getZ();
      hasSpawnPosition = true;
    }

    // Spawn Limitations: Max mobs per Server
    int numberOfEntities = CoreEntityManager.getNumberOfEntities(entityName);
    int spawnLimitationMaxMobsPerServer = COMMON.spawnLimitationMaxMobsPerServer.get();
    if (spawnLimitationMaxMobsPerServer > 0
        && numberOfEntities >= spawnLimitationMaxMobsPerServer) {
      log.debug(
          "[Spawn Limitations Server: {}] Blocked {} event for {} in {}.",
          numberOfEntities,
          eventType,
          entity,
          levelName);
      cancelSpawnEvent(event);
      return;
    }

    // Spawn Limitations: Max mobs per World
    int numberOfEntitiesPerWorld = CoreEntityManager.getNumberOfEntities(levelName, entityName);
    int spawnLimitationMaxMobsPerWorld = COMMON.spawnLimitationMaxMobsPerWorld.get();
    if (spawnLimitationMaxMobsPerWorld > 0
        && numberOfEntitiesPerWorld >= spawnLimitationMaxMobsPerWorld) {
      log.debug(
          "[Spawn Limitations World: {}] Blocked {} event for {} in {}.",
          numberOfEntitiesPerWorld,
          eventType,
          entity,
          levelName);
      cancelSpawnEvent(event);
      return;
    }

    // Limit spawns randomly every x times.
    int spawnLimitationLimiter = COMMON.spawnLimitationLimiter.get();
    if (spawnLimitationLimiter > 0 && spawnLimiter++ >= spawnLimitationLimiter) {
      log.debug(
          "[Spawn Limiter {}] Blocked {} event for {} with {} entities in {}.",
          spawnLimitationLimiter,
          eventType,
          entity,
          numberOfEntities,
          levelName);
      cancelSpawnEvent(event);
      spawnLimiter = 0;
      return;
    }

    // Allow at least one type mob per x chunk to spawn to avoid empty chunks.
    // We still consider the spawn limitation per world and server to avoid over population.
    BlockPos chunkSpawnPosition =
        hasSpawnPosition ? new BlockPos(entitySpawnX, entitySpawnY, entitySpawnZ) : null;
    boolean hasEntitySpawnedInChunk =
        hasSpawnPosition
            && CoreEntityManager.hasEntitySpawnedInChunk(levelName, chunkSpawnPosition);
    int friendlyChunkSpawnRate = COMMON.friendlyChunkSpawnRate.get();
    if (!hasEntitySpawnedInChunk
        && friendlyChunkSpawnRate > 0
        && friendlyChunkCounter++ > friendlyChunkSpawnRate) {
      if (numberOfEntities < SpawnConfigManager.getSpawnLimitPerServer(entityName)
          && numberOfEntities < SpawnConfigManager.getSpawnLimitPerWorld(entityName)) {
        log.debug(
            "[Friendly Spawn per {} x Chunk] Allow {} event for {} in {}",
            friendlyChunkSpawnRate,
            eventType,
            entity,
            levelName);
        lastAllowedSpawnEntity = entity;
        friendlyChunkCounter = 0;
        return;
      } else {
        log.debug(
            "[Friendly Spawn per {} x Chunk] Ignore {} event for {} in {} exceeding world / server limits",
            friendlyChunkSpawnRate,
            eventType,
            entity,
            levelName);
      }
    }

    // Get current players positions for later calculations
    List<PlayerPosition> playersPositionsInsideViewArea = null;
    int numOfPlayersInsideViewArea = 0;
    if (Boolean.TRUE.equals(COMMON.viewAreaEnabled.get()) && hasSpawnPosition) {
      playersPositionsInsideViewArea =
          PlayerPositionManager.getPlayerPositionsInsideViewArea(
              levelName, entitySpawnX, entitySpawnY, entitySpawnZ);

      // Limit spawns to optimized players view area for all mods.
      numOfPlayersInsideViewArea = playersPositionsInsideViewArea.size();
      if (numOfPlayersInsideViewArea == 0) {
        log.debug(
            "[View Area Visibility] Blocked {} event for {} with {} entities and {} players in {}.",
            eventType,
            entity,
            numberOfEntities,
            numOfPlayersInsideViewArea,
            levelName);
        cancelSpawnEvent(event);
        return;
      }
    }

    // Spawn Limitations: Max mobs per player
    int numberOfEntitiesInsideViewArea = 0;
    int spawnLimitationMaxMobsPerPlayer = COMMON.spawnLimitationMaxMobsPerPlayer.get();
    if (playersPositionsInsideViewArea != null) {
      numberOfEntitiesInsideViewArea =
          CoreEntityManager.getNumberOfEntitiesInPlayerPositions(
              levelName, entityName, playersPositionsInsideViewArea);
      if (spawnLimitationMaxMobsPerPlayer > 0
          && numberOfEntitiesInsideViewArea >= spawnLimitationMaxMobsPerPlayer) {
        log.debug(
            "[Spawn Limitations Player] Blocked {} event for {} with {} entities of max {} and {} players in {}.",
            eventType,
            entity,
            numberOfEntitiesInsideViewArea,
            spawnLimitationMaxMobsPerPlayer,
            numOfPlayersInsideViewArea,
            levelName);
        cancelSpawnEvent(event);
        return;
      }
    }

    // Get the current level load for supporting less aggressive mode.
    ServerLevelLoadLevel levelLoadLevel =
        serverLevelLoadLevel.computeIfAbsent(
            levelName,
            key ->
                ServerLevelLoad.getLevelNameLoadLevel()
                    .getOrDefault(key, ServerLevelLoadLevel.NORMAL));

    // Use more aggressive spawn limitation in the case user has enabled aggressive mode or
    // if the general server load or the specific level load of the entity is high.
    boolean aggressiveMode = Boolean.TRUE.equals(COMMON.spawnAggressiveMode.get());
    boolean limitSpawnPerLimits =
        Boolean.TRUE.equals(
            hasHighServerLoad
                || ServerLevelLoad.hasHighLevelLoad(levelLoadLevel)
                || aggressiveMode);
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
      if (limitPerServer > 0
          && (numberOfPlayers * limitPerPlayer <= limitPerServer)
          && numberOfEntities >= limitPerServer * spawnFactor) {
        log.debug(
            "[Server limit!] Blocked {} event for {} with {} entities of max {} in {}",
            eventType,
            entity,
            numberOfEntities,
            limitPerServer,
            levelName);
        cancelSpawnEvent(event);
        return;
      }

      // Limit spawn based on world limits, but consider current number of players and limits per
      // player.
      if (limitPerWorld > 0
          && (numberOfPlayers * limitPerPlayer <= limitPerWorld)
          && numberOfEntitiesPerWorld >= limitPerWorld * spawnFactor) {
        log.debug(
            "[World limit!] Blocked {} event for {} with {} entities of max {} in {}",
            eventType,
            entity,
            numberOfEntitiesPerWorld,
            limitPerWorld,
            levelName);
        cancelSpawnEvent(event);
        return;
      }

      // Cheap and fast calculation to limit spawn based on possible entities within player limits
      // for high server load.
      if (hasHighServerLoad
          && limitPerPlayer > 0
          && numOfPlayersInsideViewArea > 0
          && numberOfEntitiesPerWorld
              >= limitPerPlayer * limitPerPlayer * numOfPlayersInsideViewArea * spawnFactor) {
        log.debug(
            "[High Server Load!] Blocked {} event for {} with {} entities of max {} and {} players in {}",
            eventType,
            entity,
            numberOfEntitiesPerWorld,
            limitPerPlayer,
            numOfPlayersInsideViewArea,
            levelName);
        cancelSpawnEvent(event);
        return;
      }

      // Expensive calculation to Limit spawn based on real entities within player position.
      if (limitPerPlayer > 0
          && numberOfEntitiesInsideViewArea > 0
          && numberOfEntitiesInsideViewArea
              >= limitPerPlayer * numOfPlayersInsideViewArea * spawnFactor) {
        log.debug(
            "[View Area Limit!] Blocked {} event for {} with {} entities of max {} and {} players in {}",
            eventType,
            entity,
            numberOfEntitiesInsideViewArea,
            limitPerPlayer,
            numOfPlayersInsideViewArea,
            levelName);
        cancelSpawnEvent(event);
        return;
      }
    }

    // Debug messages
    if (!limitSpawnPerLimits) {
      log.debug(
          "[Allow {} (low load)] For {} in {} and {} in world and {} global",
          eventType,
          entity,
          levelName,
          numberOfEntitiesPerWorld,
          numberOfEntities);
    } else if (numberOfEntitiesInsideViewArea > 0) {
      log.debug(
          "[Allow {}] For {} in {} with {} in view area and {} in world and {} global",
          eventType,
          entity,
          levelName,
          numberOfEntitiesInsideViewArea,
          numberOfEntitiesPerWorld,
          numberOfEntities);
    } else {
      log.debug(
          "[Allow {}] For {} in {} with {} in world and {} global",
          eventType,
          entity,
          levelName,
          numberOfEntitiesPerWorld,
          numberOfEntities);
    }

    // Cache result for avoid duplicated checks.
    lastAllowedSpawnEntity = entity;
  }

  // Cancel spawn event.
  private static void cancelSpawnEvent(EntityEvent event) {
    if (event.isCanceled() || !event.isCancelable()) {
      return;
    }
    event.setCanceled(true);
    lastBlockedSpawnEntity = event.getEntity();
  }
}
