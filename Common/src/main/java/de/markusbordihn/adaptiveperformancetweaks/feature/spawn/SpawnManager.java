/*
 * Copyright 2024 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweaks.feature.spawn;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SpawnManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_SPAWN);
  private static final int SERVER_STARTED_DELAY_TICKS = 20 * 20;
  private static final double VIEW_AREA_DISTANCE = 64.0;
  private static final double DEFAULT_NEAR_PLAYER_DISTANCE = 32.0;
  private static final Map<ResourceLocation, Map<EntityType<?>, Integer>> worldCountDelta =
    new HashMap<>();
  private static final Map<ChunkCacheKey, Integer> chunkCountDelta = new HashMap<>();
  private static final Map<NearPlayerCacheKey, Integer> nearPlayerCountDelta = new HashMap<>();
  private static final Map<EntityType<?>, Integer> serverCountDelta = new HashMap<>();
  private static final Map<ChunkCacheKey, Integer> tickChunkEntityCountCache = new HashMap<>();
  private static final Map<NearPlayerCacheKey, Integer> tickNearPlayerEntityCountCache =
    new HashMap<>();
  private static final Map<WorldCacheKey, Integer> tickWorldEntityCountCache = new HashMap<>();
  private static final Map<EntityType<?>, Integer> tickServerEntityCountCache = new HashMap<>();
  private static final Map<AnchorCacheKey, Vec3> playerAnchorCache = new HashMap<>();
  private static final Set<AnchorCacheKey> missingPlayerAnchorCache = new HashSet<>();
  private static volatile ServerLoadLevel currentLoadLevel = ServerLoadLevel.NORMAL;
  private static volatile boolean serverStarted = false;
  private static volatile boolean serverStartedDelay = false;
  private static int serverStartedDelayTicks = 0;
  private static int friendlyChunkCounter = 0;
  private static int entityChunkCleanupTicks = 0;

  private SpawnManager() {
  }

  public static void handleServerAboutToStart() {
    currentLoadLevel = ServerLoadLevel.NORMAL;
    serverStarted = false;
    serverStartedDelay = false;
    serverStartedDelayTicks = 0;
    friendlyChunkCounter = 0;
    entityChunkCleanupTicks = 0;
    worldCountDelta.clear();
    chunkCountDelta.clear();
    nearPlayerCountDelta.clear();
    serverCountDelta.clear();
    tickChunkEntityCountCache.clear();
    tickNearPlayerEntityCountCache.clear();
    tickWorldEntityCountCache.clear();
    tickServerEntityCountCache.clear();
    playerAnchorCache.clear();
    missingPlayerAnchorCache.clear();
  }

  public static void handleServerStopping() {
    handleServerAboutToStart();
    VirtualPlayerManager.clearAll();
  }

  public static void handleServerStarted() {
    serverStarted = true;
    log.info(
      "Spawn manager: server started, waiting {}s before applying limits.",
      SERVER_STARTED_DELAY_TICKS / 20);
  }

  public static void handleServerTick() {
    worldCountDelta.clear();
    chunkCountDelta.clear();
    nearPlayerCountDelta.clear();
    serverCountDelta.clear();
    tickChunkEntityCountCache.clear();
    tickNearPlayerEntityCountCache.clear();
    tickWorldEntityCountCache.clear();
    tickServerEntityCountCache.clear();
    playerAnchorCache.clear();
    missingPlayerAnchorCache.clear();
    if (serverStarted && !serverStartedDelay) {
      if (++serverStartedDelayTicks >= SERVER_STARTED_DELAY_TICKS) {
        serverStartedDelay = true;
        log.info("Spawn limits are now active.");
      }
    }

    if (!SpawnConfig.entityChunkCleanupEnabled
      || !currentLoadLevel.isAtLeast(SpawnConfig.minOptimizationLoadLevel)) {
      entityChunkCleanupTicks = 0;
      return;
    }

    if (++entityChunkCleanupTicks >= Math.max(20, SpawnConfig.entityChunkCleanupIntervalTicks)) {
      entityChunkCleanupTicks = 0;
      CoreEntityManager.cleanupChunkMobFarms(SpawnConfig.entityChunkCleanupPerTypeLimit);
    }
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!FeatureToggle.SPAWN.isEnabled()) {
      return;
    }

    currentLoadLevel = event.getServerLoadLevel();
  }

  public static boolean shouldDenyNaturalSpawn(
    MobCategory category, ServerLevel level, BlockPos pos) {
    if (!SpawnConfig.naturalSpawnLimitationEnabled || !serverStartedDelay
      || !currentLoadLevel.isAtLeast(SpawnConfig.minOptimizationLoadLevel)) {
      return false;
    }

    ServerLoadLevel loadLevel = getLoadLevel(level);
    double effectivePassRate = getNaturalSpawnPassRate(loadLevel);
    if (SpawnConfig.naturalSpawnPrioritizeByTimeOfDay
      && !level.dimensionType().hasFixedTime()
      && level.isNight()) {
      if (category == MobCategory.MONSTER) {
        effectivePassRate =
          Math.min(1.0, effectivePassRate + SpawnConfig.naturalSpawnNightMonsterBonus);
      } else {
        effectivePassRate =
          Math.max(0.0, effectivePassRate - SpawnConfig.naturalSpawnNightPassivePenalty);
      }
    }
    boolean denied = ThreadLocalRandom.current().nextDouble() > effectivePassRate;
    PerformanceStats.naturalSpawnChecks++;
    if (denied) {
      PerformanceStats.naturalSpawnsDenied++;
      if (log.isDebugEnabled()) {
        log.debug(
          "[Natural Spawn] Denied {} at {},{} in {} - load={} passRate={}",
          category,
          pos.getX(),
          pos.getZ(),
          level.dimension().location(),
          loadLevel,
          String.format("%.2f", effectivePassRate));
      }
    }

    return denied;
  }

  public static boolean shouldDenyMobSpawn(Mob mob, ServerLevel level, MobSpawnType spawnType) {
    return shouldDenyMobSpawnAt(mob.getType(), level, mob.blockPosition(), spawnType);
  }

  public static boolean shouldDenyMobSpawnAt(
    EntityType<?> entityType, ServerLevel level, BlockPos pos, MobSpawnType spawnType) {
    if (!currentLoadLevel.isAtLeast(SpawnConfig.minOptimizationLoadLevel)) {
      return false;
    }
    String entityId = BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString();
    ResourceLocation dimensionId = level.dimension().location();
    boolean deny = evaluateDenyMobSpawn(entityType, level, pos, spawnType, entityId, dimensionId);
    if (deny) {
      PerformanceStats.mobSpawnsDenied++;
    } else {
      trackAllowedSpawn(entityType, level, pos, dimensionId);
    }
    return deny;
  }

  private static boolean evaluateDenyMobSpawn(EntityType<?> entityType, ServerLevel level,
    BlockPos pos, MobSpawnType spawnType, String entityId, ResourceLocation dimensionId) {
    if (!FeatureToggle.SPAWN.isEnabled() || !SpawnConfig.spawnLimitationEnabled
      || !serverStartedDelay) {
      PerformanceStats.mobSpawnsExcluded++;
      return false;
    }

    if (SpawnConfig.spawnEggBypassLimitations && spawnType == MobSpawnType.SPAWN_EGG) {
      log.debug("[Spawn Egg] Allow {} in {}", entityId, dimensionId);
      PerformanceStats.mobSpawnsExcluded++;
      return false;
    }

    SpawnDecision decision = SpawnPresetRegistry.evaluate(entityType, dimensionId);
    if (decision == SpawnDecision.IGNORE_DIMENSION) {
      log.debug("[Ignored Dimension] Allow {} in {}", entityId, dimensionId);
      PerformanceStats.mobSpawnsExcluded++;
      return false;
    }

    PerformanceStats.mobSpawnChecks++;
    if (decision == SpawnDecision.DENY) {
      log.debug("[Denied Entity] Deny {} at {} in {}", entityId, pos, dimensionId);
      return true;
    }

    ServerLoadLevel loadLevel = getLoadLevel(level);
    SpecialSpawnBonus specialSpawnBonus = getSpecialSpawnBonus(spawnType, loadLevel);
    boolean bonusUsed = false;
    int perChunkMax = SpawnPresetRegistry.getEffectivePerChunkMax(entityType, dimensionId,
      loadLevel);
    if (perChunkMax >= 0) {
      int effectivePerChunkMax = applyBonus(perChunkMax, specialSpawnBonus.perChunkBonus());
      int inChunk = countInChunk(entityType, pos, level, dimensionId);
      if (inChunk >= effectivePerChunkMax) {
        log.debug("[Per-Chunk Limit] Deny {} at {} in {} - {}/{}", entityId, pos,
          dimensionId, inChunk, effectivePerChunkMax);
        return true;
      }

      if (effectivePerChunkMax > perChunkMax && inChunk >= perChunkMax) {
        bonusUsed = true;
      }
    }

    int perPlayerMax = SpawnPresetRegistry.getEffectivePerPlayerMax(entityType, dimensionId,
      loadLevel);
    if (perPlayerMax >= 0) {
      int effectivePerPlayerMax = applyBonus(perPlayerMax, specialSpawnBonus.perPlayerBonus());
      int nearPlayer = countNearPlayer(entityType, Vec3.atCenterOf(pos), level, dimensionId);
      if (nearPlayer >= effectivePerPlayerMax) {
        log.debug("[Per-Player Limit] Deny {} at {} in {} - {}/{}", entityId, pos,
          dimensionId, nearPlayer, effectivePerPlayerMax);
        return true;
      }

      if (effectivePerPlayerMax > perPlayerMax && nearPlayer >= perPlayerMax) {
        bonusUsed = true;
      }
    }

    int perWorldMax = SpawnPresetRegistry.getEffectivePerWorldMax(entityType, dimensionId,
      loadLevel);
    if (perWorldMax >= 0) {
      int effectivePerWorldMax = applyBonus(perWorldMax, specialSpawnBonus.perWorldBonus());
      int inWorld = countInWorld(entityType, level, dimensionId);
      if (inWorld >= effectivePerWorldMax) {
        if (isFriendlyChunkSpawn(entityType, pos, level)) {
          log.debug("[Friendly Chunk Spawn] Allow {} in {} - world limit {}/{} but chunk empty",
            entityId, dimensionId, inWorld, effectivePerWorldMax);
          return false;
        }
        log.debug("[Per-World Limit] Deny {} at {} in {} - {}/{}", entityId, pos,
          dimensionId, inWorld, effectivePerWorldMax);
        return true;
      }

      if (effectivePerWorldMax > perWorldMax && inWorld >= perWorldMax) {
        bonusUsed = true;
      }
    }

    int perServerMax = SpawnPresetRegistry.getEffectivePerServerMax(entityType, dimensionId,
      loadLevel);
    if (perServerMax >= 0) {
      int effectivePerServerMax = applyBonus(perServerMax, specialSpawnBonus.perServerBonus());
      int onServer = countOnServer(entityType);
      if (onServer >= effectivePerServerMax) {
        if (isFriendlyChunkSpawn(entityType, pos, level)) {
          log.debug("[Friendly Chunk Spawn] Allow {} in {} - server limit {}/{} but chunk empty",
            entityId, dimensionId, onServer, effectivePerServerMax);
          return false;
        }
        log.debug("[Per-Server Limit] Deny {} at {} in {} - {}/{}", entityId, pos,
          dimensionId, onServer, effectivePerServerMax);
        return true;
      }

      if (effectivePerServerMax > perServerMax && onServer >= perServerMax) {
        bonusUsed = true;
      }
    }

    if (bonusUsed) {
      PerformanceStats.specialSpawnBonusesApplied++;
    }

    return false;
  }

  private static int applyBonus(int limit, int bonus) {
    return limit < 0 || bonus <= 0 ? limit : limit + bonus;
  }

  private static SpecialSpawnBonus getSpecialSpawnBonus(
    MobSpawnType spawnType, ServerLoadLevel loadLevel) {
    if (!SpawnConfig.specialSpawnTypeBonusEnabled
      || spawnType == null
      || loadLevel.isHigherThan(SpawnConfig.specialSpawnBonusMaxLoadLevel)) {
      return SpecialSpawnBonus.NONE;
    }

    String spawnTypeName = spawnType.name().toLowerCase(Locale.ROOT);
    if (!SpawnConfig.specialSpawnBonusTypes.contains(spawnTypeName)) {
      return SpecialSpawnBonus.NONE;
    }

    return new SpecialSpawnBonus(
      SpawnConfig.specialSpawnBonusPerPlayer,
      SpawnConfig.specialSpawnBonusPerChunk,
      SpawnConfig.specialSpawnBonusPerWorld,
      SpawnConfig.specialSpawnBonusPerServer);
  }

  private static void trackAllowedSpawn(EntityType<?> entityType, ServerLevel level, BlockPos pos,
    ResourceLocation dimensionId) {
    incrementCount(worldCountDelta.computeIfAbsent(dimensionId, ignored -> new HashMap<>()),
      entityType);
    incrementCount(chunkCountDelta, new ChunkCacheKey(dimensionId, pos.getX() >> 4, pos.getZ() >> 4,
      entityType));
    incrementCount(serverCountDelta, entityType);

    Vec3 anchorPos = resolvePlayerAnchor(Vec3.atCenterOf(pos), level, dimensionId);
    if (anchorPos != null) {
      incrementCount(nearPlayerCountDelta,
        new NearPlayerCacheKey(dimensionId, ((int) anchorPos.x) >> 4, ((int) anchorPos.z) >> 4,
          entityType));
    }
  }

  private static boolean isFriendlyChunkSpawn(
    EntityType<?> entityType, BlockPos pos, ServerLevel level) {
    int rate = SpawnConfig.friendlyChunkSpawnRate;
    if (rate <= 0) {
      return false;
    }

    ServerLoadLevel loadLevel = getLoadLevel(level);
    if (loadLevel != ServerLoadLevel.VERY_LOW && loadLevel != ServerLoadLevel.LOW) {
      return false;
    }

    ResourceLocation dimensionId = level.dimension().location();
    if (CoreEntityManager.getTrackedEntityCountInChunk(dimensionId, pos) > 0) {
      return false;
    }

    if (++friendlyChunkCounter < rate) {
      return false;
    }

    friendlyChunkCounter = 0;

    return true;
  }

  public static boolean shouldThrottleSpawner(ServerLevel level) {
    return SpawnConfig.spawnLimitationEnabled && getLoadLevel(level) == ServerLoadLevel.VERY_HIGH;
  }

  public static void handleEntityConversionStart(Entity entity) {
    if (entity == null || entity.level() == null || entity.level().isClientSide()) {
      return;
    }

    ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
    if (entityKey == null) {
      return;
    }

    log.debug("[Entity Conversion] Removing old entity from tracking: {}", entity);
    CoreEntityManager.removeEntity(
      entity, entityKey.toString(), entity.level().dimension().location());
  }

  public static void handleEntityConversionEnd(Entity newEntity) {
    if (newEntity == null) {
      return;
    }

    log.debug("[Entity Conversion] Protecting new entity from cleanup: {}", newEntity);
    CoreEntityManager.registerConversionProtection(newEntity.getUUID());
  }

  private static int countInChunk(EntityType<?> entityType, BlockPos pos, ServerLevel level,
    ResourceLocation dimensionId) {
    ChunkCacheKey cacheKey = new ChunkCacheKey(dimensionId, pos.getX() >> 4, pos.getZ() >> 4,
      entityType);
    int base = tickChunkEntityCountCache.computeIfAbsent(cacheKey,
      key -> CoreEntityManager.getNumberOfEntitiesInChunk(dimensionId, entityType, pos));
    return base + chunkCountDelta.getOrDefault(cacheKey, 0);
  }

  private static int countNearPlayer(EntityType<?> entityType, Vec3 spawnPos, ServerLevel level,
    ResourceLocation dimensionId) {
    double viewDistance =
      SpawnConfig.viewAreaEnabled ? VIEW_AREA_DISTANCE : DEFAULT_NEAR_PLAYER_DISTANCE;
    Vec3 anchorPos = resolvePlayerAnchor(spawnPos, level, dimensionId);
    if (anchorPos == null) {
      return 0;
    }

    NearPlayerCacheKey cacheKey = new NearPlayerCacheKey(
      dimensionId, ((int) anchorPos.x) >> 4, ((int) anchorPos.z) >> 4, entityType);
    final Vec3 finalAnchorPos = anchorPos;
    int base = tickNearPlayerEntityCountCache.computeIfAbsent(cacheKey,
      key -> CoreEntityManager.getNumberOfEntitiesNearPosition(dimensionId, entityType,
        finalAnchorPos, viewDistance));
    return base + nearPlayerCountDelta.getOrDefault(cacheKey, 0);
  }

  private static int countInWorld(EntityType<?> entityType, ServerLevel level,
    ResourceLocation dimensionId) {
    WorldCacheKey cacheKey = new WorldCacheKey(dimensionId, entityType);
    int base = tickWorldEntityCountCache.computeIfAbsent(cacheKey,
      key -> CoreEntityManager.getNumberOfEntities(dimensionId, entityType));
    Map<EntityType<?>, Integer> dimensionDelta = worldCountDelta.get(dimensionId);
    int delta = dimensionDelta != null ? dimensionDelta.getOrDefault(entityType, 0) : 0;
    return base + delta;
  }

  private static int countOnServer(EntityType<?> entityType) {
    if (ServerManager.getMinecraftServer() == null) {
      return 0;
    }

    int base = tickServerEntityCountCache.computeIfAbsent(entityType,
      CoreEntityManager::getNumberOfEntities);
    return base + serverCountDelta.getOrDefault(entityType, 0);
  }

  private static double getNaturalSpawnPassRate(ServerLoadLevel level) {
    return switch (level) {
      case VERY_LOW -> SpawnConfig.naturalSpawnPassRateVeryLow;
      case LOW -> SpawnConfig.naturalSpawnPassRateLow;
      case NORMAL -> SpawnConfig.naturalSpawnPassRateNormal;
      case MEDIUM -> SpawnConfig.naturalSpawnPassRateMedium;
      case HIGH -> SpawnConfig.naturalSpawnPassRateHigh;
      case VERY_HIGH -> SpawnConfig.naturalSpawnPassRateVeryHigh;
    };
  }

  private static ServerLoadLevel getLoadLevel(ServerLevel level) {
    return ServerLevelLoad.hasMeasuredLoad(level)
      ? ServerLevelLoad.getLevelLoad(level)
      : currentLoadLevel;
  }

  private static Vec3 resolvePlayerAnchor(Vec3 spawnPos, ServerLevel level,
    ResourceLocation dimensionId) {
    AnchorCacheKey cacheKey = new AnchorCacheKey(
      dimensionId, ((int) spawnPos.x) >> 4, ((int) spawnPos.z) >> 4);
    Vec3 cachedAnchorPos = playerAnchorCache.get(cacheKey);
    if (cachedAnchorPos != null) {
      return cachedAnchorPos;
    }

    if (missingPlayerAnchorCache.contains(cacheKey)) {
      return null;
    }

    Player realPlayer = level.getNearestPlayer(spawnPos.x, spawnPos.y, spawnPos.z, -1, false);
    Vec3 virtualPos = VirtualPlayerManager.nearest(level, spawnPos);
    if (realPlayer == null && virtualPos == null) {
      missingPlayerAnchorCache.add(cacheKey);
      return null;
    }

    if (virtualPos != null && (realPlayer == null
      || spawnPos.distanceToSqr(virtualPos) < spawnPos.distanceToSqr(realPlayer.position()))) {
      playerAnchorCache.put(cacheKey, virtualPos);
      return virtualPos;
    }

    Vec3 anchorPos = realPlayer.position();
    playerAnchorCache.put(cacheKey, anchorPos);
    return anchorPos;
  }

  private static <K> void incrementCount(Map<K, Integer> countMap, K key) {
    countMap.put(key, countMap.getOrDefault(key, 0) + 1);
  }

  private record AnchorCacheKey(
    ResourceLocation dimensionId,
    int chunkX,
    int chunkZ) {

  }

  private record ChunkCacheKey(
    ResourceLocation dimensionId,
    int chunkX,
    int chunkZ,
    EntityType<?> entityType) {

  }

  private record NearPlayerCacheKey(
    ResourceLocation dimensionId,
    int chunkX,
    int chunkZ,
    EntityType<?> entityType) {

  }

  private record WorldCacheKey(
    ResourceLocation dimensionId,
    EntityType<?> entityType) {

  }

  private record SpecialSpawnBonus(
    int perPlayerBonus,
    int perChunkBonus,
    int perWorldBonus,
    int perServerBonus) {

    private static final SpecialSpawnBonus NONE = new SpecialSpawnBonus(0, 0, 0, 0);
  }
}
