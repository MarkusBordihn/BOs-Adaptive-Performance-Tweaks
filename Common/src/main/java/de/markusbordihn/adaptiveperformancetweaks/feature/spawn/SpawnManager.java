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
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SpawnManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_SPAWN);
  private static final int SERVER_STARTED_DELAY_TICKS = 20 * 20;
  private static final Map<String, Map<String, Integer>> worldCountBase = new HashMap<>();
  private static final Map<String, Map<String, Integer>> worldCountDelta = new HashMap<>();
  private static volatile ServerLoadLevel currentLoadLevel = ServerLoadLevel.NORMAL;
  private static volatile boolean serverStarted = false;
  private static volatile boolean serverStartedDelay = false;
  private static int serverStartedDelayTicks = 0;
  private static int friendlyChunkCounter = 0;

  private SpawnManager() {
  }

  public static void handleServerAboutToStart() {
    currentLoadLevel = ServerLoadLevel.NORMAL;
    serverStarted = false;
    serverStartedDelay = false;
    serverStartedDelayTicks = 0;
    friendlyChunkCounter = 0;
    worldCountBase.clear();
    worldCountDelta.clear();
  }

  public static void handleServerStarted() {
    serverStarted = true;
    log.info("Spawn manager: server started, waiting {}s before applying limits.",
      SERVER_STARTED_DELAY_TICKS / 20);
  }

  public static void handleServerTick() {
    worldCountBase.clear();
    worldCountDelta.clear();
    if (serverStarted && !serverStartedDelay) {
      if (++serverStartedDelayTicks >= SERVER_STARTED_DELAY_TICKS) {
        serverStartedDelay = true;
        log.info("Spawn limits are now active.");
      }
    }
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    currentLoadLevel = event.getServerLoadLevel();
  }

  public static boolean shouldDenyNaturalSpawn(MobCategory category, ServerLevel level,
    BlockPos pos) {
    if (!SpawnConfig.naturalSpawnLimitationEnabled || !serverStartedDelay) {
      return false;
    }

    double effectivePassRate = getNaturalSpawnPassRate(currentLoadLevel);
    if (SpawnConfig.naturalSpawnPrioritizeByTimeOfDay
      && !level.dimensionType().hasFixedTime()
      && level.isNight()) {
      if (category == MobCategory.MONSTER) {
        effectivePassRate = Math.min(1.0,
          effectivePassRate + SpawnConfig.naturalSpawnNightMonsterBonus);
      } else {
        effectivePassRate = Math.max(0.0,
          effectivePassRate - SpawnConfig.naturalSpawnNightPassivePenalty);
      }
    }
    boolean denied = ThreadLocalRandom.current().nextDouble() > effectivePassRate;
    PerformanceStats.naturalSpawnChecks++;
    if (denied) {
      PerformanceStats.naturalSpawnsDenied++;
      if (log.isDebugEnabled()) {
        log.debug("[Natural Spawn] Denied {} at {},{} in {} — load={} passRate={}",
          category, pos.getX(), pos.getZ(), level.dimension().location(), currentLoadLevel,
          String.format("%.2f", effectivePassRate));
      }
    }

    return denied;
  }

  public static boolean shouldDenyMobSpawn(Mob mob, ServerLevel level, MobSpawnType spawnType) {
    return shouldDenyMobSpawnAt(mob.getType(), level, mob.blockPosition(), spawnType);
  }

  public static boolean shouldDenyMobSpawnAt(EntityType<?> entityType, ServerLevel level,
    BlockPos pos, MobSpawnType spawnType) {
    String entityId = BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString();
    String dimensionId = level.dimension().location().toString();
    PerformanceStats.mobSpawnChecks++;
    boolean deny = evaluateDenyMobSpawn(entityType, level, pos, spawnType, entityId, dimensionId);
    if (deny) {
      PerformanceStats.mobSpawnsDenied++;
    } else {
      trackAllowedSpawn(dimensionId, entityId);
    }
    return deny;
  }

  public static boolean shouldDenyMobSpawnBeforeCreation(EntityType<?> entityType,
    ServerLevel level) {
    if (!FeatureToggle.SPAWN.isEnabled() || !SpawnConfig.spawnLimitationEnabled
      || !serverStartedDelay) {
      return false;
    }
    String entityId = BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString();
    String dimensionId = level.dimension().location().toString();
    if (SpawnPresetRegistry.evaluate(entityId, dimensionId) == SpawnDecision.DENY) {
      PerformanceStats.mobSpawnChecks++;
      PerformanceStats.mobSpawnsDenied++;
      return true;
    }
    return false;
  }

  private static boolean evaluateDenyMobSpawn(EntityType<?> entityType, ServerLevel level,
    BlockPos pos, MobSpawnType spawnType, String entityId, String dimensionId) {
    if (!FeatureToggle.SPAWN.isEnabled() || !SpawnConfig.spawnLimitationEnabled
      || !serverStartedDelay) {
      return false;
    }

    if (SpawnConfig.spawnEggBypassLimitations && spawnType == MobSpawnType.SPAWN_EGG) {
      log.debug("[Spawn Egg] Allow {} in {}", entityId, dimensionId);
      return false;
    }

    SpawnDecision decision = SpawnPresetRegistry.evaluate(entityId, dimensionId);
    if (decision == SpawnDecision.DENY) {
      log.debug("[Denied Entity] Deny {} at {} in {}", entityId, pos, dimensionId);
      return true;
    }

    if (decision == SpawnDecision.IGNORE_DIMENSION) {
      log.debug("[Ignored Dimension] Allow {} in {}", entityId, dimensionId);
      return false;
    }

    int perChunkMax = SpawnPresetRegistry.getEffectivePerChunkMax(entityId, dimensionId,
      currentLoadLevel);
    if (perChunkMax >= 0) {
      int inChunk = countInChunk(entityType, pos, level);
      if (inChunk >= perChunkMax) {
        log.debug("[Per-Chunk Limit] Deny {} at {} in {} — {}/{}", entityId, pos,
          dimensionId, inChunk, perChunkMax);
        return true;
      }
    }

    int perPlayerMax = SpawnPresetRegistry.getEffectivePerPlayerMax(entityId, dimensionId,
      currentLoadLevel);
    if (perPlayerMax >= 0) {
      int nearPlayer = countNearPlayer(entityType, Vec3.atCenterOf(pos), level);
      if (nearPlayer >= perPlayerMax) {
        log.debug("[Per-Player Limit] Deny {} at {} in {} — {}/{}", entityId, pos,
          dimensionId, nearPlayer, perPlayerMax);
        return true;
      }
    }

    int perWorldMax = SpawnPresetRegistry.getEffectivePerWorldMax(entityId, dimensionId,
      currentLoadLevel);
    if (perWorldMax >= 0) {
      int inWorld = countInWorld(entityType, level);
      if (inWorld >= perWorldMax) {
        if (isFriendlyChunkSpawn(entityType, pos, level)) {
          log.debug("[Friendly Chunk Spawn] Allow {} in {} — world limit {}/{} but chunk empty",
            entityId, dimensionId, inWorld, perWorldMax);
          return false;
        }
        log.debug("[Per-World Limit] Deny {} at {} in {} — {}/{}", entityId, pos,
          dimensionId, inWorld, perWorldMax);
        return true;
      }
    }

    int perServerMax = SpawnPresetRegistry.getEffectivePerServerMax(entityId, dimensionId,
      currentLoadLevel);
    if (perServerMax >= 0) {
      int onServer = countOnServer(entityType);
      if (onServer >= perServerMax) {
        if (isFriendlyChunkSpawn(entityType, pos, level)) {
          log.debug("[Friendly Chunk Spawn] Allow {} in {} — server limit {}/{} but chunk empty",
            entityId, dimensionId, onServer, perServerMax);
          return false;
        }
        log.debug("[Per-Server Limit] Deny {} at {} in {} — {}/{}", entityId, pos,
          dimensionId, onServer, perServerMax);
        return true;
      }
    }

    return false;
  }

  private static void trackAllowedSpawn(String dimensionId, String entityTypeId) {
    worldCountDelta
      .computeIfAbsent(dimensionId, ignored -> new HashMap<>())
      .merge(entityTypeId, 1, Integer::sum);
  }

  private static boolean isFriendlyChunkSpawn(EntityType<?> entityType, BlockPos pos,
    ServerLevel level) {
    int rate = SpawnConfig.friendlyChunkSpawnRate;
    if (rate <= 0) {
      return false;
    }

    if (currentLoadLevel != ServerLoadLevel.VERY_LOW && currentLoadLevel != ServerLoadLevel.LOW) {
      return false;
    }

    int chunkX = pos.getX() >> 4;
    int chunkZ = pos.getZ() >> 4;
    AABB chunkBounds = new AABB(
      chunkX * 16.0, level.getMinBuildHeight(), chunkZ * 16.0,
      chunkX * 16.0 + 16.0, level.getMaxBuildHeight(), chunkZ * 16.0 + 16.0);
    if (!level.getEntitiesOfClass(Mob.class, chunkBounds).isEmpty()) {
      return false;
    }

    if (++friendlyChunkCounter < rate) {
      return false;
    }

    friendlyChunkCounter = 0;

    return true;
  }

  public static boolean shouldThrottleSpawner(ServerLevel level) {
    return SpawnConfig.spawnLimitationEnabled && currentLoadLevel == ServerLoadLevel.VERY_HIGH;
  }

  public static void handleEntityConversion(Entity entity) {
    if (entity == null || entity.level() == null || entity.level().isClientSide()) {
      return;
    }

    log.debug("[Entity Conversion] {}", entity);
  }

  private static int countInChunk(EntityType<?> entityType, BlockPos pos, ServerLevel level) {
    int chunkX = pos.getX() >> 4;
    int chunkZ = pos.getZ() >> 4;
    AABB chunkBounds = new AABB(
      chunkX * 16.0, level.getMinBuildHeight(), chunkZ * 16.0,
      chunkX * 16.0 + 16.0, level.getMaxBuildHeight(), chunkZ * 16.0 + 16.0);
    return level.getEntitiesOfClass(Mob.class, chunkBounds,
      existingMob -> existingMob.getType() == entityType).size();
  }

  private static int countNearPlayer(EntityType<?> entityType, Vec3 spawnPos, ServerLevel level) {
    double viewDistance = SpawnConfig.viewAreaEnabled ? 64.0 : 32.0;
    Player realPlayer = level.getNearestPlayer(spawnPos.x, spawnPos.y, spawnPos.z, -1, false);
    Vec3 virtualPos = VirtualPlayerManager.nearest(level, spawnPos);
    if (realPlayer == null && virtualPos == null) {
      return 0;
    }

    AABB countArea;
    Vec3 anchorPos;
    if (virtualPos != null && (realPlayer == null
      || spawnPos.distanceToSqr(virtualPos) < spawnPos.distanceToSqr(realPlayer.position()))) {
      anchorPos = virtualPos;
    } else {
      anchorPos = realPlayer.position();
    }
    countArea = new AABB(
      anchorPos.x - viewDistance, level.getMinBuildHeight(), anchorPos.z - viewDistance,
      anchorPos.x + viewDistance, level.getMaxBuildHeight(), anchorPos.z + viewDistance);

    return level.getEntitiesOfClass(Mob.class, countArea,
      existingMob -> existingMob.getType() == entityType).size();
  }

  private static int countInWorld(EntityType<?> entityType, ServerLevel level) {
    String dimensionId = level.dimension().location().toString();
    String entityTypeId = BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString();
    Map<String, Integer> dimensionCache = worldCountBase.get(dimensionId);
    if (dimensionCache == null) {
      dimensionCache = new HashMap<>();
      for (Entity entity : level.getAllEntities()) {
        if (entity instanceof Mob mob && !mob.isRemoved() && !mob.hasCustomName()) {
          dimensionCache.merge(
            BuiltInRegistries.ENTITY_TYPE.getKey(mob.getType()).toString(), 1, Integer::sum);
        }
      }
      worldCountBase.put(dimensionId, dimensionCache);
    }
    int base = dimensionCache.getOrDefault(entityTypeId, 0);
    int delta = worldCountDelta
      .getOrDefault(dimensionId, Map.of())
      .getOrDefault(entityTypeId, 0);
    return base + delta;
  }

  private static int countOnServer(EntityType<?> entityType) {
    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null) {
      return 0;
    }

    int total = 0;
    for (ServerLevel level : server.getAllLevels()) {
      total += countInWorld(entityType, level);
    }

    return total;
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
}
