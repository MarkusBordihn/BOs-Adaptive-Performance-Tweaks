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
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SpawnManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_SPAWN);
  private static final int SERVER_STARTED_DELAY_TICKS = 20 * 20;
  private static final Map<String, Map<String, Integer>> worldCountDelta = new HashMap<>();
  private static volatile ServerLoadLevel currentLoadLevel = ServerLoadLevel.NORMAL;
  private static volatile boolean serverStarted = false;
  private static volatile boolean serverStartedDelay = false;
  private static int serverStartedDelayTicks = 0;
  private static int friendlyChunkCounter = 0;

  private SpawnManager() {}

  public static void handleServerAboutToStart() {
    currentLoadLevel = ServerLoadLevel.NORMAL;
    serverStarted = false;
    serverStartedDelay = false;
    serverStartedDelayTicks = 0;
    friendlyChunkCounter = 0;
    worldCountDelta.clear();
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

  public static boolean shouldDenyNaturalSpawn(
      MobCategory category, ServerLevel level, BlockPos pos) {
    if (!SpawnConfig.naturalSpawnLimitationEnabled || !serverStartedDelay) {
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
    String entityId = BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString();
    String dimensionId = level.dimension().location().toString();
    boolean deny = evaluateDenyMobSpawn(entityType, level, pos, spawnType, entityId, dimensionId);
    if (deny) {
      PerformanceStats.mobSpawnsDenied++;
    } else {
      trackAllowedSpawn(dimensionId, entityId);
    }
    return deny;
  }

  private static boolean evaluateDenyMobSpawn(
      EntityType<?> entityType,
      ServerLevel level,
      BlockPos pos,
      MobSpawnType spawnType,
      String entityId,
      String dimensionId) {
    if (!FeatureToggle.SPAWN.isEnabled()
        || !SpawnConfig.spawnLimitationEnabled
        || !serverStartedDelay) {
      PerformanceStats.mobSpawnsExcluded++;
      return false;
    }

    if (SpawnConfig.spawnEggBypassLimitations && spawnType == MobSpawnType.SPAWN_EGG) {
      log.debug("[Spawn Egg] Allow {} in {}", entityId, dimensionId);
      PerformanceStats.mobSpawnsExcluded++;
      return false;
    }

    SpawnDecision decision = SpawnPresetRegistry.evaluate(entityId, dimensionId);
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
    int perChunkMax = SpawnPresetRegistry.getEffectivePerChunkMax(entityId, dimensionId, loadLevel);
    if (perChunkMax >= 0) {
      int inChunk = countInChunk(entityType, pos, level);
      if (inChunk >= perChunkMax) {
        log.debug(
            "[Per-Chunk Limit] Deny {} at {} in {} - {}/{}",
            entityId,
            pos,
            dimensionId,
            inChunk,
            perChunkMax);
        return true;
      }
    }

    int perPlayerMax =
        SpawnPresetRegistry.getEffectivePerPlayerMax(entityId, dimensionId, loadLevel);
    if (perPlayerMax >= 0) {
      int nearPlayer = countNearPlayer(entityType, Vec3.atCenterOf(pos), level);
      if (nearPlayer >= perPlayerMax) {
        log.debug(
            "[Per-Player Limit] Deny {} at {} in {} - {}/{}",
            entityId,
            pos,
            dimensionId,
            nearPlayer,
            perPlayerMax);
        return true;
      }
    }

    int perWorldMax = SpawnPresetRegistry.getEffectivePerWorldMax(entityId, dimensionId, loadLevel);
    if (perWorldMax >= 0) {
      int inWorld = countInWorld(entityType, level);
      if (inWorld >= perWorldMax) {
        if (isFriendlyChunkSpawn(entityType, pos, level)) {
          log.debug(
              "[Friendly Chunk Spawn] Allow {} in {} - world limit {}/{} but chunk empty",
              entityId,
              dimensionId,
              inWorld,
              perWorldMax);
          return false;
        }
        log.debug(
            "[Per-World Limit] Deny {} at {} in {} - {}/{}",
            entityId,
            pos,
            dimensionId,
            inWorld,
            perWorldMax);
        return true;
      }
    }

    int perServerMax =
        SpawnPresetRegistry.getEffectivePerServerMax(entityId, dimensionId, loadLevel);
    if (perServerMax >= 0) {
      int onServer = countOnServer(entityType);
      if (onServer >= perServerMax) {
        if (isFriendlyChunkSpawn(entityType, pos, level)) {
          log.debug(
              "[Friendly Chunk Spawn] Allow {} in {} - server limit {}/{} but chunk empty",
              entityId,
              dimensionId,
              onServer,
              perServerMax);
          return false;
        }
        log.debug(
            "[Per-Server Limit] Deny {} at {} in {} - {}/{}",
            entityId,
            pos,
            dimensionId,
            onServer,
            perServerMax);
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

    String dimensionId = level.dimension().location().toString();
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

  public static void handleEntityConversion(Entity entity) {
    if (entity == null || entity.level() == null || entity.level().isClientSide()) {
      return;
    }

    log.debug("[Entity Conversion] {}", entity);
  }

  private static int countInChunk(EntityType<?> entityType, BlockPos pos, ServerLevel level) {
    return CoreEntityManager.getNumberOfEntitiesInChunk(
        level.dimension().location().toString(),
        BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString(),
        pos);
  }

  private static int countNearPlayer(EntityType<?> entityType, Vec3 spawnPos, ServerLevel level) {
    double viewDistance = SpawnConfig.viewAreaEnabled ? 64.0 : 32.0;
    Player realPlayer = level.getNearestPlayer(spawnPos.x, spawnPos.y, spawnPos.z, -1, false);
    Vec3 virtualPos = VirtualPlayerManager.nearest(level, spawnPos);
    if (realPlayer == null && virtualPos == null) {
      return 0;
    }

    Vec3 anchorPos;
    if (virtualPos != null
        && (realPlayer == null
            || spawnPos.distanceToSqr(virtualPos)
                < spawnPos.distanceToSqr(realPlayer.position()))) {
      anchorPos = virtualPos;
    } else {
      anchorPos = realPlayer.position();
    }

    return CoreEntityManager.getNumberOfEntitiesNearPosition(
        level.dimension().location().toString(),
        BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString(),
        anchorPos,
        viewDistance);
  }

  private static int countInWorld(EntityType<?> entityType, ServerLevel level) {
    String dimensionId = level.dimension().location().toString();
    String entityTypeId = BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString();
    int base = CoreEntityManager.getNumberOfEntities(dimensionId, entityTypeId);
    int delta = worldCountDelta.getOrDefault(dimensionId, Map.of()).getOrDefault(entityTypeId, 0);
    return base + delta;
  }

  private static int countOnServer(EntityType<?> entityType) {
    MinecraftServer server = ServerManager.getMinecraftServer();
    if (server == null) {
      return 0;
    }

    String entityTypeId = BuiltInRegistries.ENTITY_TYPE.getKey(entityType).toString();
    int total = CoreEntityManager.getNumberOfEntities(entityTypeId);
    for (Map<String, Integer> dimensionCounts : worldCountDelta.values()) {
      total += dimensionCounts.getOrDefault(entityTypeId, 0);
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

  private static ServerLoadLevel getLoadLevel(ServerLevel level) {
    return ServerLevelLoad.hasMeasuredLoad(level)
        ? ServerLevelLoad.getLevelLoad(level)
        : currentLoadLevel;
  }
}
