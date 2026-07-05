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

package de.markusbordihn.adaptiveperformancetweaks.core.entity;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnPreset;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Predicate;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.AreaEffectCloud;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LightningBolt;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Marker;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.TamableAnimal;
import net.minecraft.world.entity.animal.Bee;
import net.minecraft.world.entity.boss.EnderDragonPart;
import net.minecraft.world.entity.boss.enderdragon.EndCrystal;
import net.minecraft.world.entity.boss.enderdragon.EnderDragon;
import net.minecraft.world.entity.boss.wither.WitherBoss;
import net.minecraft.world.entity.decoration.ArmorStand;
import net.minecraft.world.entity.decoration.HangingEntity;
import net.minecraft.world.entity.item.FallingBlockEntity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.ElderGuardian;
import net.minecraft.world.entity.monster.warden.Warden;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.projectile.EvokerFangs;
import net.minecraft.world.entity.projectile.EyeOfEnder;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.entity.raid.Raider;
import net.minecraft.world.entity.vehicle.AbstractMinecart;
import net.minecraft.world.entity.vehicle.Boat;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class CoreEntityManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_ENTITIES);
  private static final Gson REPORT_GSON = new GsonBuilder().setPrettyPrinting().create();
  private static final int VERIFICATION_TICK = 5 * 60 * 20;
  private static final int VERIFICATION_ADD_OPERATIONS_THRESHOLD = 500;
  private static final long OPERATION_VERIFICATION_MIN_INTERVAL_MS = 1_000L;
  private static final int CONVERSION_PROTECTION_TICKS = 200;
  private static final ConcurrentHashMap<UUID, Integer> conversionProtectedEntities =
    new ConcurrentHashMap<>();
  private static final Set<ChunkTrackingKey> entityChunkMap = ConcurrentHashMap.newKeySet();
  private static final Object trackingRuleLock = new Object();
  private static final Object verificationLock = new Object();
  private static final ConcurrentHashMap<String, CachedTrackingDecision> entityDecisionCache =
    new ConcurrentHashMap<>();
  private static volatile Set<String> excludedModNamespaces = Collections.emptySet();
  private static volatile Map<String, TrackingRuleInfo> manualNamespaceRules = Collections.emptyMap();
  private static volatile Map<String, TrackingRuleInfo> manualEntityRules = Collections.emptyMap();
  private static volatile Set<String> autoExcludedNamespaces = Collections.emptySet();
  private static volatile Set<String> autoExcludedEntityIds = Collections.emptySet();
  private static volatile Set<String> demotedNamespaces = Collections.emptySet();
  private static volatile Map<String, TrackingCategory> autoNamespaceCategories =
    Collections.emptyMap();
  private static volatile Map<String, TrackingCategory> autoEntityCategories =
    Collections.emptyMap();
  private static volatile Map<String, NamespaceProfile> namespaceProfiles = Collections.emptyMap();
  private static int ticks = 0;
  private static int addOperationCounter = 0;
  private static int operationVerificationStage = 0;
  private static long lastOperationVerificationTime = 0L;
  private static volatile boolean isVerifying = false;
  private static ConcurrentHashMap<EntityTrackingKey, Set<Entity>> entityMap =
    new ConcurrentHashMap<>();
  private static ConcurrentHashMap<ChunkTrackingKey, Set<Entity>> entityMapPerChunk =
    new ConcurrentHashMap<>();
  private static ConcurrentHashMap<EntityType<?>, Set<Entity>> entityMapGlobal =
    new ConcurrentHashMap<>();
  private static ConcurrentHashMap<Entity, ChunkTrackingKey> entityChunkKeyMap =
    new ConcurrentHashMap<>();

  private CoreEntityManager() {
  }

  public static void reloadTrackingRules(List<SpawnPreset> presets) {
    LinkedHashMap<String, TrackingRuleInfo> manualNamespaces = new LinkedHashMap<>();
    LinkedHashMap<String, TrackingRuleInfo> manualEntities = new LinkedHashMap<>();
    NamespaceAnalysis analysis;

    synchronized (trackingRuleLock) {
      for (SpawnPreset preset : presets) {
        TrackingMode mode = preset.mode();
        if (mode == null) {
          continue;
        }

        TrackingCategory category = preset.category() != null
          ? preset.category()
          : TrackingCategory.UNKNOWN;
        String reason = preset.reason() != null ? preset.reason() : "";
        Set<String> entityIds =
          preset.entityIds() != null ? preset.entityIds() : Collections.emptySet();

        switch (mode) {
          case EXCLUDE_NAMESPACE, PROTECT_NAMESPACE -> {
            if (preset.modId() != null) {
              manualNamespaces.put(preset.modId(), new TrackingRuleInfo(mode, category, reason));
            }
          }
          case EXCLUDE_ENTITY, PROTECT_ENTITY -> {
            for (String entityId : entityIds) {
              manualEntities.put(entityId, new TrackingRuleInfo(mode, category, reason));
            }
          }
        }
      }

      analysis = analyzeRegisteredEntities(manualNamespaces.keySet(), manualEntities.keySet());

      excludedModNamespaces = Collections.unmodifiableSet(
        getLegacyExcludedNamespaces(manualNamespaces));
      manualNamespaceRules = Collections.unmodifiableMap(manualNamespaces);
      manualEntityRules = Collections.unmodifiableMap(manualEntities);
      autoExcludedNamespaces = Collections.unmodifiableSet(analysis.autoExcludedNamespaces());
      autoExcludedEntityIds = Collections.unmodifiableSet(analysis.autoExcludedEntityIds());
      autoNamespaceCategories = Collections.unmodifiableMap(analysis.autoNamespaceCategories());
      autoEntityCategories = Collections.unmodifiableMap(analysis.autoEntityCategories());
      namespaceProfiles = Collections.unmodifiableMap(analysis.namespaceProfiles());
      demotedNamespaces = Collections.emptySet();
      entityDecisionCache.clear();

      log.debug(
        "[Entity Manager] Tracking rules reloaded: {} manual namespaces, {} manual entity ids, {} auto namespaces, {} auto entity ids.",
        manualNamespaceRules.size(), manualEntityRules.size(),
        autoExcludedNamespaces.size(), autoExcludedEntityIds.size());
    }

    CompletableFuture.runAsync(
      () -> writeTrackingReport(manualNamespaces, manualEntities, analysis));
  }

  public static void setExcludedModNamespaces(Set<String> namespaces) {
    synchronized (trackingRuleLock) {
      LinkedHashMap<String, TrackingRuleInfo> namespaceRules = new LinkedHashMap<>();
      for (String namespace : namespaces) {
        namespaceRules.put(namespace, new TrackingRuleInfo(
          TrackingMode.EXCLUDE_NAMESPACE, TrackingCategory.UNKNOWN, "Legacy namespace exclusion"));
      }

      excludedModNamespaces = Set.copyOf(namespaces);
      manualNamespaceRules = Collections.unmodifiableMap(namespaceRules);
      manualEntityRules = Collections.emptyMap();
      autoExcludedNamespaces = Collections.emptySet();
      autoExcludedEntityIds = Collections.emptySet();
      autoNamespaceCategories = Collections.emptyMap();
      autoEntityCategories = Collections.emptyMap();
      namespaceProfiles = Collections.emptyMap();
      demotedNamespaces = Collections.emptySet();
      entityDecisionCache.clear();
    }

    log.debug("Excluded mod namespaces from entity tracking: {}", namespaces);
  }

  public static boolean isExcludedModNamespace(String entityId) {
    if (entityId == null || excludedModNamespaces.isEmpty()) {
      return false;
    }

    int colonIdx = entityId.indexOf(':');
    return colonIdx > 0 && excludedModNamespaces.contains(entityId.substring(0, colonIdx));
  }

  public static void reset() {
    entityChunkMap.clear();
    entityMap = new ConcurrentHashMap<>();
    entityMapPerChunk = new ConcurrentHashMap<>();
    entityMapGlobal = new ConcurrentHashMap<>();
    entityChunkKeyMap = new ConcurrentHashMap<>();
    conversionProtectedEntities.clear();
    entityDecisionCache.clear();
    ticks = 0;
    addOperationCounter = 0;
    operationVerificationStage = 0;
    lastOperationVerificationTime = 0L;
    isVerifying = false;
  }

  public static void registerConversionProtection(UUID entityUUID) {
    conversionProtectedEntities.put(entityUUID, ticks + CONVERSION_PROTECTION_TICKS);
  }

  public static void handleServerTick() {
    if (++ticks >= VERIFICATION_TICK) {
      triggerVerificationIfNotRunning(false);
      ticks = 0;
    }
  }

  public static void handleEntityJoinLevel(Entity entity, boolean isClientSide) {
    if (isClientSide || entity == null) {
      return;
    }

    ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
    if (entityKey == null) {
      if (log.isDebugEnabled()) {
        log.debug("[Entity Manager] Skipping unregistered entity {} in {}.", entity,
          entity.level().dimension().location());
      }
      return;
    }

    String entityName = entityKey.toString();
    PerformanceStats.trackingEvaluations++;
    if (!isRelevantEntity(entity, entityName)) {
      return;
    }

    ResourceLocation levelName = entity.level().dimension().location();
    addEntity(entity, entityName, levelName);
    PerformanceStats.trackingTracked++;
  }

  public static void handleEntityLeaveLevel(Entity entity, boolean isClientSide) {
    if (isClientSide || entity == null) {
      return;
    }

    ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
    if (entityKey == null) {
      return;
    }

    removeEntity(entity, entityKey.toString(), entity.level().dimension().location());
  }

  public static void handleLivingDeath(Entity entity, boolean isClientSide) {
    if (isClientSide || entity == null) {
      return;
    }

    ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
    if (entityKey == null) {
      return;
    }

    removeEntity(entity, entityKey.toString(), entity.level().dimension().location());
  }

  public static void addEntity(Entity entity, String entityName, ResourceLocation levelName) {
    EntityTrackingKey entityMapKey = new EntityTrackingKey(levelName, entityName);
    Set<Entity> entities =
      entityMap.computeIfAbsent(entityMapKey, key -> ConcurrentHashMap.newKeySet());
    entities.add(entity);

    ChunkTrackingKey entityChunkKey = new ChunkTrackingKey(levelName, entity.blockPosition());
    Set<Entity> entitiesPerChunk =
      entityMapPerChunk.computeIfAbsent(entityChunkKey, key -> ConcurrentHashMap.newKeySet());
    entitiesPerChunk.add(entity);
    entityChunkKeyMap.put(entity, entityChunkKey);

    Set<Entity> entitiesGlobal =
      entityMapGlobal.computeIfAbsent(entity.getType(), key -> ConcurrentHashMap.newKeySet());
    entitiesGlobal.add(entity);

    entityChunkMap.add(entityChunkKey);

    if (++addOperationCounter >= VERIFICATION_ADD_OPERATIONS_THRESHOLD) {
      addOperationCounter = 0;
      triggerVerificationIfNotRunning(true);
    }
  }

  public static void removeEntity(Entity entity, String entityName, ResourceLocation levelName) {
    EntityTrackingKey mapKey = new EntityTrackingKey(levelName, entityName);
    Set<Entity> entities = entityMap.get(mapKey);
    boolean wasTracked = entities != null && entities.remove(entity);
    if (!wasTracked) {
      return;
    }
    if (entities.isEmpty()) {
      entityMap.remove(mapKey);
    }

    ChunkTrackingKey originalChunkKey = entityChunkKeyMap.remove(entity);
    if (originalChunkKey != null) {
      Set<Entity> entitiesPerChunk = entityMapPerChunk.get(originalChunkKey);
      if (entitiesPerChunk != null) {
        entitiesPerChunk.remove(entity);
        if (entitiesPerChunk.isEmpty()) {
          entityMapPerChunk.remove(originalChunkKey);
          entityChunkMap.remove(originalChunkKey);
        }
      }
    }

    EntityType<?> entityType = getEntityType(entityName);
    if (entityType == null) {
      return;
    }

    Set<Entity> entitiesGlobal = entityMapGlobal.get(entityType);
    if (entitiesGlobal != null) {
      entitiesGlobal.remove(entity);
      if (entitiesGlobal.isEmpty()) {
        entityMapGlobal.remove(entityType);
      }
    }
  }

  public static Map<String, Set<Entity>> getEntities() {
    Map<String, Set<Entity>> entities = new LinkedHashMap<>(entityMap.size());
    for (Map.Entry<EntityTrackingKey, Set<Entity>> entry : entityMap.entrySet()) {
      entities.put(entry.getKey().asString(), entry.getValue());
    }
    return entities;
  }

  public static Map<String, Set<Entity>> getEntitiesPerChunk() {
    Map<String, Set<Entity>> entities = new LinkedHashMap<>(entityMapPerChunk.size());
    for (Map.Entry<ChunkTrackingKey, Set<Entity>> entry : entityMapPerChunk.entrySet()) {
      entities.put(entry.getKey().asString(), entry.getValue());
    }
    return entities;
  }

  public static Map<String, Set<Entity>> getEntitiesGlobal() {
    Map<String, Set<Entity>> entities = new LinkedHashMap<>(entityMapGlobal.size());
    for (Map.Entry<EntityType<?>, Set<Entity>> entry : entityMapGlobal.entrySet()) {
      String entityName = getEntityName(entry.getKey());
      if (entityName != null) {
        entities.put(entityName, entry.getValue());
      }
    }
    return entities;
  }

  public static int getNumberOfEntities(String levelName, String entityName) {
    ResourceLocation levelKey = resolveLevelKey(levelName);
    if (levelKey == null) {
      return 0;
    }

    return getNumberOfEntities(levelKey, entityName);
  }

  public static int getNumberOfEntities(ResourceLocation levelName, String entityName) {
    EntityType<?> entityType = getEntityType(entityName);
    if (entityType == null) {
      return 0;
    }

    return getNumberOfEntities(levelName, entityType);
  }

  public static int getNumberOfEntities(String levelName, EntityType<?> entityType) {
    ResourceLocation levelKey = resolveLevelKey(levelName);
    if (levelKey == null) {
      return 0;
    }

    return getNumberOfEntities(levelKey, entityType);
  }

  public static int getNumberOfEntities(ResourceLocation levelName, EntityType<?> entityType) {
    if (entityType == null) {
      return 0;
    }

    String entityName = getEntityName(entityType);
    if (entityName == null) {
      return 0;
    }

    Set<Entity> entities = entityMap.get(new EntityTrackingKey(levelName, entityName));
    return entities != null ? entities.size() : 0;
  }

  public static int getNumberOfEntities(String entityName) {
    EntityType<?> entityType = getEntityType(entityName);
    if (entityType == null) {
      return 0;
    }

    return getNumberOfEntities(entityType);
  }

  public static int getNumberOfEntities(EntityType<?> entityType) {
    if (entityType == null) {
      return 0;
    }

    Set<Entity> entities = entityMapGlobal.get(entityType);
    return entities != null ? entities.size() : 0;
  }

  public static int getNumberOfEntitiesInChunk(String levelName, String entityName,
    BlockPos blockPos) {
    ResourceLocation levelKey = resolveLevelKey(levelName);
    if (levelKey == null) {
      return 0;
    }

    return getNumberOfEntitiesInChunk(levelKey, entityName, blockPos);
  }

  public static int getNumberOfEntitiesInChunk(ResourceLocation levelName, String entityName,
    BlockPos blockPos) {
    EntityType<?> entityType = getEntityType(entityName);
    if (entityType == null) {
      return 0;
    }

    return getNumberOfEntitiesInChunk(levelName, entityType, blockPos);
  }

  public static int getNumberOfEntitiesInChunk(String levelName, EntityType<?> entityType,
    BlockPos blockPos) {
    ResourceLocation levelKey = resolveLevelKey(levelName);
    if (levelKey == null) {
      return 0;
    }

    return getNumberOfEntitiesInChunk(levelKey, entityType, blockPos);
  }

  public static int getNumberOfEntitiesInChunk(ResourceLocation levelName, EntityType<?> entityType,
    BlockPos blockPos) {
    if (entityType == null) {
      return 0;
    }

    Set<Entity> entities = entityMapPerChunk.get(new ChunkTrackingKey(levelName, blockPos));
    if (entities == null || entities.isEmpty()) {
      return 0;
    }

    int counter = 0;
    for (Entity entity : entities) {
      if (entity != null && !entity.isRemoved() && entity.getType() == entityType) {
        counter++;
      }
    }

    return counter;
  }


  public static int getNumberOfEntitiesNearPosition(
    String levelName, EntityType<?> entityType, Vec3 center, double horizontalRange) {
    ResourceLocation levelKey = resolveLevelKey(levelName);
    if (levelKey == null) {
      return 0;
    }

    return getNumberOfEntitiesNearPosition(levelKey, entityType, center, horizontalRange);
  }

  public static int getNumberOfEntitiesNearPosition(
    ResourceLocation levelName, EntityType<?> entityType, Vec3 center, double horizontalRange) {
    if (entityType == null) {
      return 0;
    }

    String entityName = getEntityName(entityType);
    if (entityName == null) {
      return 0;
    }

    Set<Entity> rawSet = entityMap.get(new EntityTrackingKey(levelName, entityName));
    if (rawSet == null || rawSet.isEmpty()) {
      return 0;
    }

    int counter = 0;
    for (Entity entity : rawSet) {
      if (entity == null || entity.isRemoved()) {
        continue;
      }

      if (entity.getType() == entityType
        && Math.abs(entity.getX() - center.x) <= horizontalRange
        && Math.abs(entity.getZ() - center.z) <= horizontalRange) {
        counter++;
      }
    }

    return counter;
  }

  public static int getTrackedEntityCountInChunk(String levelName, BlockPos blockPos) {
    ResourceLocation levelKey = resolveLevelKey(levelName);
    if (levelKey == null) {
      return 0;
    }

    return getTrackedEntityCountInChunk(levelKey, blockPos);
  }

  public static int getTrackedEntityCountInChunk(ResourceLocation levelName, BlockPos blockPos) {
    return getActiveEntityCount(entityMapPerChunk.get(new ChunkTrackingKey(levelName, blockPos)));
  }

  public static int getTotalTrackedEntityCount() {
    int total = 0;
    for (Set<Entity> entities : entityMapGlobal.values()) {
      total += getActiveEntityCount(entities);
    }

    return total;
  }

  public static boolean hasEntitySpawnedInChunk(String levelName, BlockPos blockPos) {
    ResourceLocation levelKey = resolveLevelKey(levelName);
    if (levelKey == null) {
      return false;
    }

    return hasEntitySpawnedInChunk(levelKey, blockPos);
  }

  public static boolean hasEntitySpawnedInChunk(ResourceLocation levelName, BlockPos blockPos) {
    return entityChunkMap.contains(new ChunkTrackingKey(levelName, blockPos));
  }

  public static ChunkMobCleanupResult cleanupChunkMobFarms(int perTypeLimit) {
    return cleanupChunkMobFarms(perTypeLimit, entity -> true);
  }

  public static ChunkMobCleanupResult cleanupChunkMobFarms(int perTypeLimit,
    Predicate<Entity> extraEntityFilter) {
    if (perTypeLimit < 0 || entityMapPerChunk.isEmpty()) {
      return ChunkMobCleanupResult.EMPTY;
    }

    Predicate<Entity> cleanupFilter =
      extraEntityFilter != null ? extraEntityFilter : entity -> true;
    int removedEntities = 0;
    Set<ChunkTrackingKey> affectedChunks = new HashSet<>();
    Set<EntityType<?>> affectedTypes = new HashSet<>();

    Map<ChunkTrackingKey, Map<EntityType<?>, List<Entity>>> chunkEntitiesByType = new HashMap<>();
    for (Map.Entry<ChunkTrackingKey, Set<Entity>> entry : entityMapPerChunk.entrySet()) {
      Set<Entity> rawEntities = entry.getValue();
      if (rawEntities == null || rawEntities.isEmpty()) {
        continue;
      }

      for (Entity entity : new ArrayList<>(rawEntities)) {
        if (!isMobCleanupEligible(entity) || !cleanupFilter.test(entity)) {
          continue;
        }

        ChunkTrackingKey liveChunkKey =
          new ChunkTrackingKey(entry.getKey().levelName(), entity.blockPosition());
        chunkEntitiesByType
          .computeIfAbsent(liveChunkKey, ignored -> new HashMap<>())
          .computeIfAbsent(entity.getType(), ignored -> new ArrayList<>())
          .add(entity);
      }
    }

    for (Map.Entry<ChunkTrackingKey, Map<EntityType<?>, List<Entity>>> chunkEntry
      : chunkEntitiesByType.entrySet()) {
      for (Map.Entry<EntityType<?>, List<Entity>> typeEntry : chunkEntry.getValue().entrySet()) {
        List<Entity> candidates = typeEntry.getValue();
        if (candidates.size() <= perTypeLimit) {
          continue;
        }

        candidates.sort(Comparator.<Entity>comparingInt(entity -> entity.tickCount)
          .thenComparingInt(entity -> entity.getId()));

        for (int index = perTypeLimit; index < candidates.size(); index++) {
          Entity entity = candidates.get(index);
          if (!isChunkCleanupCandidate(entity)) {
            continue;
          }
          if (removeChunkCleanupEntity(entity)) {
            removedEntities++;
            affectedChunks.add(chunkEntry.getKey());
            affectedTypes.add(typeEntry.getKey());
          }
        }
      }
    }

    return removedEntities > 0
      ? new ChunkMobCleanupResult(removedEntities, affectedChunks.size(), affectedTypes.size())
      : ChunkMobCleanupResult.EMPTY;
  }

  public static boolean isRelevantEntity(Entity entity) {
    if (entity == null) {
      return false;
    }

    if (!passesFastInstanceFilters(entity)) {
      return false;
    }

    return passesProtectedInstanceFilters(entity);
  }

  public static boolean isRelevantEntity(Entity entity, String entityName) {
    if (entity == null || entityName == null || entityName.isBlank()) {
      return false;
    }

    CachedTrackingDecision cachedDecision = entityDecisionCache.get(entityName);
    if (cachedDecision != null) {
      return applyCachedDecision(cachedDecision, entity);
    }

    CachedTrackingDecision decision = resolveBaseDecision(entity, entityName);
    entityDecisionCache.put(entityName, decision);
    if (!decision.track()) {
      recordTrackingExclusion(decision, false);
      return false;
    }

    if (decision.source() == TrackingSource.LIVING_GUARD) {
      PerformanceStats.trackingProtectedLiving++;
    }

    if (!passesFastInstanceFilters(entity)) {
      return false;
    }

    return passesProtectedInstanceFilters(entity);
  }

  private static boolean applyCachedDecision(CachedTrackingDecision decision, Entity entity) {
    if (!decision.track()) {
      recordTrackingExclusion(decision, true);
      return false;
    }

    if (decision.source() == TrackingSource.LIVING_GUARD) {
      PerformanceStats.trackingProtectedLiving++;
    }

    if (!passesFastInstanceFilters(entity)) {
      return false;
    }

    return passesProtectedInstanceFilters(entity);
  }

  private static CachedTrackingDecision resolveBaseDecision(Entity entity, String entityId) {
    TrackingRuleInfo manualEntityRule = manualEntityRules.get(entityId);
    if (manualEntityRule != null) {
      return CachedTrackingDecision.exclude(mapManualSource(manualEntityRule.mode()),
        manualEntityRule.category(), manualEntityRule.reason());
    }

    String namespace = getNamespace(entityId);
    TrackingRuleInfo manualNamespaceRule =
      namespace != null ? manualNamespaceRules.get(namespace) : null;
    if (manualNamespaceRule != null) {
      return CachedTrackingDecision.exclude(mapManualSource(manualNamespaceRule.mode()),
        manualNamespaceRule.category(), manualNamespaceRule.reason());
    }

    if (namespace != null && autoExcludedNamespaces.contains(namespace)) {
      if (entity instanceof LivingEntity) {
        demoteNamespace(namespace);
        TrackingCategory category =
          autoNamespaceCategories.getOrDefault(namespace, TrackingCategory.UNKNOWN);
        return CachedTrackingDecision.livingGuard(
          category, "LivingEntity prevented auto namespace exclusion");
      }

      return CachedTrackingDecision.exclude(
        TrackingSource.AUTO_NAMESPACE,
        autoNamespaceCategories.getOrDefault(namespace, TrackingCategory.TECHNICAL),
        "Auto namespace classification");
    }

    if (autoExcludedEntityIds.contains(entityId)) {
      if (entity instanceof LivingEntity) {
        TrackingCategory category =
          autoEntityCategories.getOrDefault(entityId, TrackingCategory.UNKNOWN);
        return CachedTrackingDecision.livingGuard(
          category, "LivingEntity prevented auto entity exclusion");
      }

      return CachedTrackingDecision.exclude(
        TrackingSource.AUTO_ENTITY,
        autoEntityCategories.getOrDefault(entityId, TrackingCategory.TECHNICAL),
        "Auto entity classification");
    }

    return CachedTrackingDecision.allow();
  }

  private static boolean passesFastInstanceFilters(Entity entity) {
    return !entity.isRemoved()
      && !entity.isSpectator()
      && !entity.isInvisible()
      && !entity.isInvulnerable()
      && !entity.isVehicle()
      && !entity.isPassenger()
      && !(entity instanceof Player)
      && !(entity instanceof ExperienceOrb)
      && !(entity instanceof Projectile)
      && !(entity instanceof AreaEffectCloud)
      && !(entity instanceof LightningBolt)
      && !(entity instanceof FallingBlockEntity)
      && !(entity instanceof EvokerFangs)
      && !(entity instanceof EyeOfEnder)
      && !(entity instanceof HangingEntity)
      && !(entity instanceof Marker)
      && !(entity instanceof EnderDragonPart)
      && !(entity instanceof EndCrystal)
      && !(entity instanceof AbstractMinecart)
      && !(entity instanceof Boat)
      && !(entity instanceof ArmorStand)
      && !(entity instanceof ItemEntity)
      && !(entity instanceof Npc)
      && !(entity instanceof EnderDragon)
      && !(entity instanceof WitherBoss)
      && !(entity instanceof ElderGuardian)
      && !(entity instanceof Warden);
  }

  private static boolean passesProtectedInstanceFilters(Entity entity) {
    if (isProtectedPersistentEntity(entity)) {
      PerformanceStats.trackingProtectedPersistent++;
      return false;
    }
    return true;
  }

  private static void recordTrackingExclusion(CachedTrackingDecision decision, boolean cached) {
    if (!PerformanceStats.isDetailedTrackingStatsEnabled()) {
      return;
    }

    if (cached) {
      PerformanceStats.trackingExcludedEarlyCache++;
    }

    switch (decision.source()) {
      case MANUAL_NAMESPACE -> PerformanceStats.trackingExcludedManualNamespace++;
      case MANUAL_ENTITY -> PerformanceStats.trackingExcludedManualEntity++;
      case AUTO_NAMESPACE -> PerformanceStats.trackingExcludedAutoNamespace++;
      case AUTO_ENTITY -> PerformanceStats.trackingExcludedAutoEntity++;
      default -> {
      }
    }

    PerformanceStats.recordTrackingCategory(decision.category());
  }

  private static TrackingSource mapManualSource(TrackingMode mode) {
    return switch (mode) {
      case EXCLUDE_NAMESPACE, PROTECT_NAMESPACE -> TrackingSource.MANUAL_NAMESPACE;
      case EXCLUDE_ENTITY, PROTECT_ENTITY -> TrackingSource.MANUAL_ENTITY;
    };
  }

  private static void demoteNamespace(String namespace) {
    synchronized (trackingRuleLock) {
      if (!autoExcludedNamespaces.contains(namespace) || demotedNamespaces.contains(namespace)) {
        return;
      }

      LinkedHashSet<String> updatedNamespaces = new LinkedHashSet<>(autoExcludedNamespaces);
      updatedNamespaces.remove(namespace);
      autoExcludedNamespaces = Collections.unmodifiableSet(updatedNamespaces);

      LinkedHashSet<String> updatedDemotions = new LinkedHashSet<>(demotedNamespaces);
      updatedDemotions.add(namespace);
      demotedNamespaces = Collections.unmodifiableSet(updatedDemotions);

      String namespacePrefix = namespace + ':';
      for (String entityId : new ArrayList<>(entityDecisionCache.keySet())) {
        if (entityId.startsWith(namespacePrefix)) {
          entityDecisionCache.remove(entityId);
        }
      }
    }
  }

  private static NamespaceAnalysis analyzeRegisteredEntities(
    Set<String> manualNamespaces, Set<String> manualEntityIds) {
    LinkedHashMap<String, NamespaceProfileBuilder> builders = new LinkedHashMap<>();

    for (EntityType<?> entityType : BuiltInRegistries.ENTITY_TYPE) {
      ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entityType);
      if (entityKey == null) {
        continue;
      }

      String entityId = entityKey.toString();
      String namespace = entityKey.getNamespace();
      NamespaceProfileBuilder builder =
        builders.computeIfAbsent(namespace, ignored -> new NamespaceProfileBuilder());
      builder.totalEntityTypes++;

      if (entityType.getCategory() == MobCategory.MISC) {
        builder.miscEntityIds.add(entityId);
      } else {
        builder.nonMiscEntityTypes++;
      }
    }

    LinkedHashSet<String> autoNamespaces = new LinkedHashSet<>();
    LinkedHashSet<String> autoEntityIds = new LinkedHashSet<>();
    LinkedHashMap<String, TrackingCategory> autoNamespaceCategoryMap = new LinkedHashMap<>();
    LinkedHashMap<String, TrackingCategory> autoEntityCategoryMap = new LinkedHashMap<>();
    LinkedHashMap<String, NamespaceProfile> profiles = new LinkedHashMap<>();

    for (Map.Entry<String, NamespaceProfileBuilder> entry : builders.entrySet()) {
      String namespace = entry.getKey();
      NamespaceProfileBuilder builder = entry.getValue();
      boolean allTypesMisc =
        builder.totalEntityTypes > 0 && builder.nonMiscEntityTypes == 0
          && !builder.miscEntityIds.isEmpty();
      boolean mixedTypes =
        builder.nonMiscEntityTypes > 0 && !builder.miscEntityIds.isEmpty();
      NamespaceProfile profile = new NamespaceProfile(
        builder.totalEntityTypes,
        builder.nonMiscEntityTypes,
        Collections.unmodifiableSet(new LinkedHashSet<>(builder.miscEntityIds)),
        allTypesMisc,
        mixedTypes);
      profiles.put(namespace, profile);

      if (manualNamespaces.contains(namespace)) {
        continue;
      }

      if (allTypesMisc) {
        autoNamespaces.add(namespace);
        autoNamespaceCategoryMap.put(namespace, TrackingCategory.TECHNICAL);
        continue;
      }

      if (!mixedTypes) {
        continue;
      }

      for (String entityId : builder.miscEntityIds) {
        if (manualEntityIds.contains(entityId)) {
          continue;
        }

        autoEntityIds.add(entityId);
        autoEntityCategoryMap.put(entityId, TrackingCategory.TECHNICAL);
      }
    }

    return new NamespaceAnalysis(autoNamespaces, autoEntityIds,
      autoNamespaceCategoryMap, autoEntityCategoryMap, profiles);
  }

  private static Set<String> getLegacyExcludedNamespaces(
    Map<String, TrackingRuleInfo> namespaceRules) {
    LinkedHashSet<String> namespaces = new LinkedHashSet<>();
    for (Map.Entry<String, TrackingRuleInfo> entry : namespaceRules.entrySet()) {
      if (entry.getValue().mode() == TrackingMode.EXCLUDE_NAMESPACE) {
        namespaces.add(entry.getKey());
      }
    }

    return namespaces;
  }

  private static void writeTrackingReport(
    Map<String, TrackingRuleInfo> manualNamespaces,
    Map<String, TrackingRuleInfo> manualEntities,
    NamespaceAnalysis analysis) {
    if (!CoreConfig.writeEntityTrackingReport) {
      return;
    }
    Path reportPath = Constants.REPORTS_DIR.resolve("entity_tracking_report.json");

    LinkedHashMap<String, Object> report = new LinkedHashMap<>();
    report.put("manual_namespaces", serializeRuleMap(manualNamespaces));
    report.put("manual_entity_ids", serializeRuleMap(manualEntities));
    report.put("auto_excluded_namespaces", new ArrayList<>(analysis.autoExcludedNamespaces()));
    report.put("auto_excluded_entity_ids", new ArrayList<>(analysis.autoExcludedEntityIds()));
    report.put("mixed_namespaces", getMixedNamespaces(analysis.namespaceProfiles()));
    report.put("unknown_namespaces", getUnknownNamespaces(
      analysis.namespaceProfiles(), manualNamespaces.keySet(),
      analysis.autoExcludedNamespaces(), analysis.autoExcludedEntityIds()));
    report.put("namespace_profiles", serializeNamespaceProfiles(analysis.namespaceProfiles()));

    // The report map is fully built at this point; serialize and write off-thread to
    // keep file I/O away from the game thread during load/reload.
    CompletableFuture.runAsync(() -> {
      try {
        Files.createDirectories(reportPath.getParent());
        Files.writeString(reportPath, REPORT_GSON.toJson(report), StandardCharsets.UTF_8);
      } catch (IOException exception) {
        log.warn("Failed to write entity tracking report '{}': {}", reportPath,
          exception.getMessage());
      }
    });
  }

  private static Map<String, Map<String, Object>> serializeRuleMap(
    Map<String, TrackingRuleInfo> rules) {
    LinkedHashMap<String, Map<String, Object>> serialized = new LinkedHashMap<>();
    for (Map.Entry<String, TrackingRuleInfo> entry : rules.entrySet()) {
      TrackingRuleInfo rule = entry.getValue();
      LinkedHashMap<String, Object> values = new LinkedHashMap<>();
      values.put("mode", rule.mode().getSerializedName());
      values.put("category", rule.category().getSerializedName());
      values.put("reason", rule.reason());
      serialized.put(entry.getKey(), values);
    }

    return serialized;
  }

  private static List<String> getMixedNamespaces(Map<String, NamespaceProfile> profiles) {
    List<String> namespaces = new ArrayList<>();
    for (Map.Entry<String, NamespaceProfile> entry : profiles.entrySet()) {
      if (entry.getValue().mixedTypes()) {
        namespaces.add(entry.getKey());
      }
    }

    return namespaces;
  }

  private static List<String> getUnknownNamespaces(
    Map<String, NamespaceProfile> profiles, Set<String> manualNamespaces,
    Set<String> autoNamespaces, Set<String> autoEntityIds) {
    List<String> namespaces = new ArrayList<>();
    for (Map.Entry<String, NamespaceProfile> entry : profiles.entrySet()) {
      String namespace = entry.getKey();
      if (manualNamespaces.contains(namespace) || autoNamespaces.contains(namespace)) {
        continue;
      }

      boolean hasAutoEntityId = false;
      String namespacePrefix = namespace + ':';
      for (String entityId : autoEntityIds) {
        if (entityId.startsWith(namespacePrefix)) {
          hasAutoEntityId = true;
          break;
        }
      }

      if (!hasAutoEntityId) {
        namespaces.add(namespace);
      }
    }

    return namespaces;
  }

  private static Map<String, Map<String, Object>> serializeNamespaceProfiles(
    Map<String, NamespaceProfile> profiles) {
    LinkedHashMap<String, Map<String, Object>> serialized = new LinkedHashMap<>();
    for (Map.Entry<String, NamespaceProfile> entry : profiles.entrySet()) {
      NamespaceProfile profile = entry.getValue();
      LinkedHashMap<String, Object> values = new LinkedHashMap<>();
      values.put("total_entity_types", profile.totalEntityTypes());
      values.put("non_misc_entity_types", profile.nonMiscEntityTypes());
      values.put("misc_entity_ids", new ArrayList<>(profile.miscEntityIds()));
      values.put("all_types_misc", profile.allTypesMisc());
      values.put("mixed_types", profile.mixedTypes());
      serialized.put(entry.getKey(), values);
    }

    return serialized;
  }

  private static String getNamespace(String entityId) {
    int colonIdx = entityId.indexOf(':');
    return colonIdx > 0 ? entityId.substring(0, colonIdx) : null;
  }

  private static void triggerVerificationIfNotRunning(boolean boundedVerification) {
    synchronized (verificationLock) {
      if (isVerifying) {
        return;
      }

      if (boundedVerification && !shouldRunOperationVerification()) {
        return;
      }

      isVerifying = true;
      try {
        if (boundedVerification) {
          verifyEntitiesBounded();
        } else {
          verifyEntities();
        }
      } finally {
        isVerifying = false;
      }
    }
  }

  private static boolean shouldRunOperationVerification() {
    long now = System.currentTimeMillis();
    if (now - lastOperationVerificationTime < OPERATION_VERIFICATION_MIN_INTERVAL_MS) {
      return false;
    }

    lastOperationVerificationTime = now;
    return true;
  }

  private static void verifyEntities() {
    int removedEntries = removeDiscardedEntities(entityMap);
    int removedChunkEntries = removeDiscardedEntities(entityMapPerChunk);
    int removedGlobalEntries = removeDiscardedEntities(entityMapGlobal);
    int removedChunkKeys = removeDiscardedChunkKeys();
    int removedChunkMarkers = removeEmptyChunkMarkers();
    conversionProtectedEntities.entrySet().removeIf(entry -> ticks > entry.getValue());

    if (removedEntries > 0 || removedChunkEntries > 0 || removedGlobalEntries > 0
      || removedChunkKeys > 0 || removedChunkMarkers > 0) {
      log.debug(
        "[Entity Manager] Cleanup removed {} overview entries, {} chunk entries, {} global entries, {} stale chunk keys and {} chunk markers.",
        removedEntries,
        removedChunkEntries,
        removedGlobalEntries,
        removedChunkKeys,
        removedChunkMarkers);
    }
  }

  private static void verifyEntitiesBounded() {
    int removedEntries = 0;
    int removedChunkEntries = 0;
    int removedGlobalEntries = 0;
    int removedChunkKeys = 0;
    int removedChunkMarkers = 0;

    switch (operationVerificationStage) {
      case 0 -> removedEntries = removeDiscardedEntities(entityMap);
      case 1 -> removedChunkEntries = removeDiscardedEntities(entityMapPerChunk);
      case 2 -> removedGlobalEntries = removeDiscardedEntities(entityMapGlobal);
      case 3 -> removedChunkKeys = removeDiscardedChunkKeys();
      case 4 -> removedChunkMarkers = removeEmptyChunkMarkers();
      default -> {
      }
    }

    operationVerificationStage = (operationVerificationStage + 1) % 5;
    if (removedEntries > 0 || removedChunkEntries > 0 || removedGlobalEntries > 0
      || removedChunkKeys > 0 || removedChunkMarkers > 0) {
      log.debug(
        "[Entity Manager] Bounded cleanup removed {} overview entries, {} chunk entries, {} global entries, {} stale chunk keys and {} chunk markers.",
        removedEntries,
        removedChunkEntries,
        removedGlobalEntries,
        removedChunkKeys,
        removedChunkMarkers);
    }
  }

  private static <K> int removeDiscardedEntities(ConcurrentMap<K, Set<Entity>> entityMapToCheck) {
    if (entityMapToCheck == null || entityMapToCheck.isEmpty()) {
      return 0;
    }

    int removedEntries = 0;
    Iterator<Map.Entry<K, Set<Entity>>> mapIterator =
      entityMapToCheck.entrySet().iterator();

    while (mapIterator.hasNext()) {
      Map.Entry<K, Set<Entity>> entry = mapIterator.next();
      Set<Entity> entities = entry.getValue();

      Iterator<Entity> entityIterator = entities.iterator();
      while (entityIterator.hasNext()) {
        Entity entity = entityIterator.next();
        if (entity == null || entity.isRemoved()) {
          entityIterator.remove();
          removedEntries++;
        }
      }

      if (entities.isEmpty()) {
        mapIterator.remove();
      }
    }

    return removedEntries;
  }

  private static int removeDiscardedChunkKeys() {
    if (entityChunkKeyMap.isEmpty()) {
      return 0;
    }

    int removedEntries = 0;
    Iterator<Map.Entry<Entity, ChunkTrackingKey>> iterator = entityChunkKeyMap.entrySet()
      .iterator();
    while (iterator.hasNext()) {
      Map.Entry<Entity, ChunkTrackingKey> entry = iterator.next();
      Entity entity = entry.getKey();
      if (entity == null || entity.isRemoved()) {
        iterator.remove();
        removedEntries++;
      }
    }

    return removedEntries;
  }

  private static int removeEmptyChunkMarkers() {
    if (entityChunkMap.isEmpty()) {
      return 0;
    }

    int removedEntries = 0;
    Iterator<ChunkTrackingKey> iterator = entityChunkMap.iterator();
    while (iterator.hasNext()) {
      ChunkTrackingKey chunkKey = iterator.next();
      Set<Entity> entities = entityMapPerChunk.get(chunkKey);
      if (entities == null || entities.isEmpty()) {
        iterator.remove();
        removedEntries++;
      }
    }

    return removedEntries;
  }

  private static int getActiveEntityCount(Set<Entity> entities) {
    if (entities == null || entities.isEmpty()) {
      return 0;
    }

    int count = 0;
    for (Entity entity : entities) {
      if (entity != null && !entity.isRemoved()) {
        count++;
      }
    }

    return count;
  }

  private static boolean isProtectedPersistentEntity(Entity entity) {
    if (entity.hasCustomName()) {
      return true;
    }

    if (entity instanceof TamableAnimal tamableAnimal && tamableAnimal.isTame()) {
      return true;
    }

    if (entity instanceof Bee bee && bee.hasHive()) {
      return true;
    }

    if (entity instanceof Raider raider && raider.hasActiveRaid()) {
      return true;
    }

    return entity instanceof Mob mob
      && (mob.isLeashed() || mob.isPersistenceRequired() || mob.requiresCustomPersistence());
  }

  private static boolean isMobCleanupEligible(Entity entity) {
    return entity instanceof Mob
      && entity.isAlive()
      && !entity.isRemoved()
      && !isProtectedPersistentEntity(entity);
  }

  private static boolean isChunkCleanupCandidate(Entity entity) {
    if (!isMobCleanupEligible(entity)) {
      return false;
    }
    Integer protectedUntil = conversionProtectedEntities.get(entity.getUUID());
    return protectedUntil == null || ticks > protectedUntil;
  }

  private static boolean removeChunkCleanupEntity(Entity entity) {
    if (entity == null || entity.isRemoved()) {
      return false;
    }

    ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType());
    if (entityKey == null) {
      return false;
    }

    removeEntity(entity, entityKey.toString(), entity.level().dimension().location());
    entity.discard();
    PerformanceStats.entityChunkCleanupRemoved++;
    return true;
  }

  private static String getEntityName(EntityType<?> entityType) {
    ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entityType);
    return entityKey != null ? entityKey.toString() : null;
  }

  private static EntityType<?> getEntityType(String entityName) {
    if (entityName == null || entityName.isEmpty()) {
      return null;
    }

    ResourceLocation entityKey = ResourceLocation.tryParse(entityName);
    return entityKey != null ? BuiltInRegistries.ENTITY_TYPE.get(entityKey) : null;
  }

  private static ResourceLocation resolveLevelKey(String levelName) {
    return levelName != null && !levelName.isEmpty() ? ResourceLocation.tryParse(levelName) : null;
  }

  private enum TrackingSource {
    NONE,
    LIVING_GUARD,
    MANUAL_NAMESPACE,
    MANUAL_ENTITY,
    AUTO_NAMESPACE,
    AUTO_ENTITY
  }

  private record TrackingRuleInfo(
    TrackingMode mode,
    TrackingCategory category,
    String reason) {

  }

  private record NamespaceProfile(
    int totalEntityTypes,
    int nonMiscEntityTypes,
    Set<String> miscEntityIds,
    boolean allTypesMisc,
    boolean mixedTypes) {

  }

  private record NamespaceAnalysis(
    Set<String> autoExcludedNamespaces,
    Set<String> autoExcludedEntityIds,
    Map<String, TrackingCategory> autoNamespaceCategories,
    Map<String, TrackingCategory> autoEntityCategories,
    Map<String, NamespaceProfile> namespaceProfiles) {

  }

  private static final class NamespaceProfileBuilder {

    private final LinkedHashSet<String> miscEntityIds = new LinkedHashSet<>();
    private int totalEntityTypes = 0;
    private int nonMiscEntityTypes = 0;
  }

  private record CachedTrackingDecision(
    boolean track,
    TrackingSource source,
    TrackingCategory category,
    String reason) {

    private static CachedTrackingDecision exclude(
      TrackingSource source, TrackingCategory category, String reason) {
      return new CachedTrackingDecision(false, source, category, reason);
    }

    private static CachedTrackingDecision livingGuard(TrackingCategory category, String reason) {
      return new CachedTrackingDecision(true, TrackingSource.LIVING_GUARD, category, reason);
    }

    private static CachedTrackingDecision allow() {
      return new CachedTrackingDecision(true, TrackingSource.NONE, TrackingCategory.UNKNOWN, "");
    }
  }

  private record EntityTrackingKey(
    ResourceLocation levelName,
    String entityName) {

    private EntityTrackingKey(String levelName, String entityName) {
      this(resolveLevelKey(levelName), entityName);
    }

    private String asString() {
      return "[" + levelName + ']' + entityName;
    }
  }

  private record ChunkTrackingKey(
    ResourceLocation levelName,
    int chunkX,
    int chunkZ) {

    private ChunkTrackingKey(String levelName, int chunkX, int chunkZ) {
      this(resolveLevelKey(levelName), chunkX, chunkZ);
    }

    private ChunkTrackingKey(String levelName, BlockPos blockPos) {
      this(levelName, blockPos.getX() >> 4, blockPos.getZ() >> 4);
    }

    private ChunkTrackingKey(ResourceLocation levelName, BlockPos blockPos) {
      this(levelName, blockPos.getX() >> 4, blockPos.getZ() >> 4);
    }

    private String asString() {
      return "[" + levelName + ':' + chunkX + 'x' + chunkZ + ']';
    }
  }

}
