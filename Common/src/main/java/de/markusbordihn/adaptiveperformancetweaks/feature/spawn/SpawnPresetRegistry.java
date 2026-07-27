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
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import net.minecraft.core.Holder;
import net.minecraft.core.HolderSet;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class SpawnPresetRegistry {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_SPAWN);
  private static final Map<PresetCacheKey, ResolvedPreset> entityPresetCache = new HashMap<>();
  private static final Set<PresetCacheKey> missingPresetCache = new HashSet<>();
  private static final Map<PresetCacheKey, Boolean> ignoredDimensionCache = new HashMap<>();
  private static Map<EntityType<?>, List<ResolvedPreset>> presetsByEntityType =
    Collections.emptyMap();
  private static List<ResolvedPreset> loadedPresets = Collections.emptyList();

  private SpawnPresetRegistry() {
  }

  public static void reload(List<SpawnPreset> presets) {
    long startTime = System.nanoTime();
    entityPresetCache.clear();
    missingPresetCache.clear();
    ignoredDimensionCache.clear();
    SpawnPresetPartitioner.Partition partition = SpawnPresetPartitioner.partition(presets);
    List<SpawnPreset> sorted = new ArrayList<>(partition.spawnPresets());
    for (SpawnPreset preset : partition.trackingPresets()) {
      log.debug("Skipping tracking-only preset '{}' ({}) during spawn reload.",
        preset.modId(), preset.mode());
    }

    sorted.sort((presetA, presetB) -> Integer.compare(presetB.priority(), presetA.priority()));
    RegistrySnapshot registrySnapshot = buildRegistrySnapshot();
    List<ResolvedPreset> resolvedPresets = new ArrayList<>(sorted.size());
    for (SpawnPreset preset : sorted) {
      resolvedPresets.add(resolvePreset(preset, registrySnapshot));
    }
    presetsByEntityType = buildPresetIndex(resolvedPresets);
    loadedPresets = Collections.unmodifiableList(resolvedPresets);
    double durationMs = (System.nanoTime() - startTime) / 1_000_000.0D;
    log.info("SpawnPresetRegistry loaded {} presets, indexed {} entity types in {} ms",
      loadedPresets.size(), presetsByEntityType.size(), String.format("%.2f", durationMs));
    if (log.isDebugEnabled()) {
      for (ResolvedPreset resolvedPreset : loadedPresets) {
        SpawnPreset preset = resolvedPreset.preset();
        log.debug(
          "[Preset] mod={} priority={} perPlayer={} perWorld={} perServer={} perChunk={} allow={} deny={}",
          preset.modId(), preset.priority(),
          preset.entities().perPlayerMax(), preset.entities().perWorldMax(),
          preset.entities().perServerMax(), preset.entities().perChunkMax(),
          preset.entities().allowList(), preset.entities().denyList());
      }
    }
  }

  public static SpawnDecision evaluate(String entityId, String dimensionId) {
    EntityType<?> entityType = resolveEntityType(entityId);
    if (entityType == null) {
      return SpawnDecision.ALLOW;
    }

    ResourceLocation dimensionKey = ResourceLocation.tryParse(dimensionId);
    if (dimensionKey != null) {
      return evaluate(entityType, dimensionKey);
    }
    return evaluate(entityType, dimensionId);
  }

  public static SpawnDecision evaluate(EntityType<?> entityType, String dimensionId) {
    ResourceLocation dimensionKey = ResourceLocation.tryParse(dimensionId);
    if (dimensionKey != null) {
      return evaluate(entityType, dimensionKey);
    }

    if (isDimensionIgnored(entityType, dimensionId)) {
      return SpawnDecision.IGNORE_DIMENSION;
    }

    ResolvedPreset resolvedPreset = getEffectivePreset(entityType, dimensionId);
    if (resolvedPreset == null) {
      return SpawnDecision.ALLOW;
    }

    if (!resolvedPreset.denyTypes().isEmpty() && resolvedPreset.denyTypes().contains(entityType)) {
      return SpawnDecision.DENY;
    }

    if (!resolvedPreset.allowTypes().isEmpty() && !resolvedPreset.allowTypes()
      .contains(entityType)) {
      return SpawnDecision.DENY;
    }

    return SpawnDecision.ALLOW;
  }

  public static SpawnDecision evaluate(EntityType<?> entityType, ResourceLocation dimensionId) {
    if (isDimensionIgnored(entityType, dimensionId)) {
      return SpawnDecision.IGNORE_DIMENSION;
    }

    ResolvedPreset resolvedPreset = getEffectivePreset(entityType, dimensionId);
    if (resolvedPreset == null) {
      return SpawnDecision.ALLOW;
    }

    if (!resolvedPreset.denyTypes().isEmpty() && resolvedPreset.denyTypes().contains(entityType)) {
      return SpawnDecision.DENY;
    }

    if (!resolvedPreset.allowTypes().isEmpty() && !resolvedPreset.allowTypes()
      .contains(entityType)) {
      return SpawnDecision.DENY;
    }

    return SpawnDecision.ALLOW;
  }

  public static int getEffectivePerPlayerMax(String entityId, String dimensionId,
    ServerLoadLevel loadLevel) {
    EntityType<?> entityType = resolveEntityType(entityId);
    if (entityType == null) {
      return SpawnConfig.spawnLimitationMaxMobsPerPlayer;
    }

    return getEffectivePerPlayerMax(entityType, dimensionId, loadLevel);
  }

  public static int getEffectivePerPlayerMax(EntityType<?> entityType, String dimensionId,
    ServerLoadLevel loadLevel) {
    ResourceLocation dimensionKey = ResourceLocation.tryParse(dimensionId);
    if (dimensionKey != null) {
      return getEffectivePerPlayerMax(entityType, dimensionKey, loadLevel);
    }

    return getEffectiveLimit(entityType, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerPlayer, ResolvedPreset::perPlayerLimits);
  }

  public static int getEffectivePerPlayerMax(EntityType<?> entityType, ResourceLocation dimensionId,
    ServerLoadLevel loadLevel) {
    return getEffectiveLimit(entityType, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerPlayer, ResolvedPreset::perPlayerLimits);
  }

  public static int getEffectivePerWorldMax(String entityId, String dimensionId,
    ServerLoadLevel loadLevel) {
    EntityType<?> entityType = resolveEntityType(entityId);
    if (entityType == null) {
      return SpawnConfig.spawnLimitationMaxMobsPerWorld;
    }

    return getEffectivePerWorldMax(entityType, dimensionId, loadLevel);
  }

  public static int getEffectivePerWorldMax(EntityType<?> entityType, String dimensionId,
    ServerLoadLevel loadLevel) {
    ResourceLocation dimensionKey = ResourceLocation.tryParse(dimensionId);
    if (dimensionKey != null) {
      return getEffectivePerWorldMax(entityType, dimensionKey, loadLevel);
    }

    return getEffectiveLimit(entityType, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerWorld, ResolvedPreset::perWorldLimits);
  }

  public static int getEffectivePerWorldMax(EntityType<?> entityType, ResourceLocation dimensionId,
    ServerLoadLevel loadLevel) {
    return getEffectiveLimit(entityType, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerWorld, ResolvedPreset::perWorldLimits);
  }

  public static int getEffectivePerServerMax(String entityId, String dimensionId,
    ServerLoadLevel loadLevel) {
    EntityType<?> entityType = resolveEntityType(entityId);
    if (entityType == null) {
      return SpawnConfig.spawnLimitationMaxMobsPerServer;
    }

    return getEffectivePerServerMax(entityType, dimensionId, loadLevel);
  }

  public static int getEffectivePerServerMax(EntityType<?> entityType, String dimensionId,
    ServerLoadLevel loadLevel) {
    ResourceLocation dimensionKey = ResourceLocation.tryParse(dimensionId);
    if (dimensionKey != null) {
      return getEffectivePerServerMax(entityType, dimensionKey, loadLevel);
    }

    return getEffectiveLimit(entityType, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerServer, ResolvedPreset::perServerLimits);
  }

  public static int getEffectivePerServerMax(EntityType<?> entityType, ResourceLocation dimensionId,
    ServerLoadLevel loadLevel) {
    return getEffectiveLimit(entityType, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerServer, ResolvedPreset::perServerLimits);
  }

  public static int getEffectivePerChunkMax(String entityId, String dimensionId,
    ServerLoadLevel loadLevel) {
    EntityType<?> entityType = resolveEntityType(entityId);
    if (entityType == null) {
      return SpawnConfig.spawnLimitationMaxMobsPerChunk;
    }

    return getEffectivePerChunkMax(entityType, dimensionId, loadLevel);
  }

  public static int getEffectivePerChunkMax(EntityType<?> entityType, String dimensionId,
    ServerLoadLevel loadLevel) {
    ResourceLocation dimensionKey = ResourceLocation.tryParse(dimensionId);
    if (dimensionKey != null) {
      return getEffectivePerChunkMax(entityType, dimensionKey, loadLevel);
    }

    return getEffectiveLimit(entityType, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerChunk, ResolvedPreset::perChunkLimits);
  }

  public static int getEffectivePerChunkMax(EntityType<?> entityType, ResourceLocation dimensionId,
    ServerLoadLevel loadLevel) {
    return getEffectiveLimit(entityType, dimensionId, loadLevel,
      SpawnConfig.spawnLimitationMaxMobsPerChunk, ResolvedPreset::perChunkLimits);
  }

  private static int getEffectiveLimit(
    EntityType<?> entityType, String dimensionId, ServerLoadLevel loadLevel,
    int globalDefault, Function<ResolvedPreset, int[]> limitExtractor) {
    ResolvedPreset preset = getEffectivePreset(entityType, dimensionId);
    if (preset == null) {
      return globalDefault;
    }

    return limitExtractor.apply(preset)[loadLevel.ordinal()];
  }

  private static int getEffectiveLimit(
    EntityType<?> entityType, ResourceLocation dimensionId, ServerLoadLevel loadLevel,
    int globalDefault, Function<ResolvedPreset, int[]> limitExtractor) {
    ResolvedPreset preset = getEffectivePreset(entityType, dimensionId);
    if (preset == null) {
      return globalDefault;
    }

    return limitExtractor.apply(preset)[loadLevel.ordinal()];
  }

  private static ResolvedPreset getEffectivePreset(EntityType<?> entityType, String dimensionId) {
    PresetCacheKey cacheKey = new PresetCacheKey(entityType, dimensionId);
    ResolvedPreset cachedPreset = entityPresetCache.get(cacheKey);
    if (cachedPreset != null) {
      return cachedPreset;
    }

    if (missingPresetCache.contains(cacheKey)) {
      return null;
    }

    ResolvedPreset preset = findPreset(entityType, dimensionId);
    if (preset != null) {
      entityPresetCache.put(cacheKey, preset);
      return preset;
    }

    missingPresetCache.add(cacheKey);
    return null;
  }

  private static ResolvedPreset getEffectivePreset(
    EntityType<?> entityType, ResourceLocation dimensionId) {
    PresetCacheKey cacheKey = new PresetCacheKey(entityType, dimensionId.toString());
    ResolvedPreset cachedPreset = entityPresetCache.get(cacheKey);
    if (cachedPreset != null) {
      return cachedPreset;
    }

    if (missingPresetCache.contains(cacheKey)) {
      return null;
    }

    ResolvedPreset preset = findPreset(entityType, dimensionId);
    if (preset != null) {
      entityPresetCache.put(cacheKey, preset);
      return preset;
    }

    missingPresetCache.add(cacheKey);
    return null;
  }

  private static ResolvedPreset findPreset(EntityType<?> entityType, String dimensionId) {
    List<ResolvedPreset> candidatePresets = presetsByEntityType.get(entityType);
    if (candidatePresets == null || candidatePresets.isEmpty()) {
      return null;
    }

    for (ResolvedPreset preset : candidatePresets) {
      if (appliesToDimension(preset.preset(), dimensionId)) {
        return preset;
      }
    }

    return null;
  }

  private static ResolvedPreset findPreset(EntityType<?> entityType, ResourceLocation dimensionId) {
    List<ResolvedPreset> candidatePresets = presetsByEntityType.get(entityType);
    if (candidatePresets == null || candidatePresets.isEmpty()) {
      return null;
    }

    for (ResolvedPreset preset : candidatePresets) {
      if (appliesToDimension(preset, dimensionId)) {
        return preset;
      }
    }

    return null;
  }

  private static boolean isDimensionIgnored(EntityType<?> entityType,
    ResourceLocation dimensionId) {
    PresetCacheKey cacheKey = new PresetCacheKey(entityType, dimensionId.toString());
    Boolean cachedResult = ignoredDimensionCache.get(cacheKey);
    if (cachedResult != null) {
      return cachedResult;
    }

    boolean ignored = false;
    List<ResolvedPreset> candidatePresets = presetsByEntityType.get(entityType);
    if (candidatePresets != null) {
      for (ResolvedPreset preset : candidatePresets) {
        // Highest-priority preset covering this entity decides: an ignored dimension
        // exempts the entity from all spawn limits instead of falling back to globals.
        if (preset.ignoredDimensions().contains(dimensionId)) {
          ignored = true;
          break;
        }
        if (appliesToDimension(preset, dimensionId)) {
          break;
        }
      }
    }

    ignoredDimensionCache.put(cacheKey, ignored);
    return ignored;
  }

  private static boolean isDimensionIgnored(EntityType<?> entityType, String dimensionId) {
    List<ResolvedPreset> candidatePresets = presetsByEntityType.get(entityType);
    if (candidatePresets == null) {
      return false;
    }

    for (ResolvedPreset preset : candidatePresets) {
      if (preset.preset().dimensions().ignore().contains(dimensionId)) {
        return true;
      }
      if (appliesToDimension(preset.preset(), dimensionId)) {
        return false;
      }
    }

    return false;
  }

  private static boolean appliesToDimension(SpawnPreset preset, String dimensionId) {
    SpawnPreset.DimensionFilter dimensions = preset.dimensions();
    if (dimensions.ignore().contains(dimensionId)) {
      return false;
    }

    if (dimensions.deny().contains(dimensionId)) {
      return false;
    }

    if (!dimensions.allow().isEmpty() && !dimensions.allow().contains(dimensionId)) {
      return false;
    }

    return true;
  }

  private static boolean appliesToDimension(ResolvedPreset preset, ResourceLocation dimensionId) {
    if (preset.ignoredDimensions().contains(dimensionId)) {
      return false;
    }

    if (preset.deniedDimensions().contains(dimensionId)) {
      return false;
    }

    return preset.allowedDimensions().isEmpty() || preset.allowedDimensions().contains(dimensionId);
  }

  private static RegistrySnapshot buildRegistrySnapshot() {
    Map<String, Set<EntityType<?>>> entityTypesByNamespace = new HashMap<>();
    Set<EntityType<?>> allEntityTypes = new LinkedHashSet<>();
    for (EntityType<?> entityType : BuiltInRegistries.ENTITY_TYPE) {
      ResourceLocation entityKey = BuiltInRegistries.ENTITY_TYPE.getKey(entityType);
      if (entityKey == null) {
        continue;
      }

      allEntityTypes.add(entityType);
      entityTypesByNamespace.computeIfAbsent(entityKey.getNamespace(),
        ignored -> new LinkedHashSet<>()).add(entityType);
    }

    return new RegistrySnapshot(
      Collections.unmodifiableSet(allEntityTypes),
      Collections.unmodifiableMap(entityTypesByNamespace));
  }

  private static Map<EntityType<?>, List<ResolvedPreset>> buildPresetIndex(
    List<ResolvedPreset> resolvedPresets) {
    Map<EntityType<?>, List<ResolvedPreset>> presetIndex = new HashMap<>();
    for (ResolvedPreset preset : resolvedPresets) {
      for (EntityType<?> entityType : preset.coveredTypes()) {
        presetIndex.computeIfAbsent(entityType, ignored -> new ArrayList<>()).add(preset);
      }
    }

    return Collections.unmodifiableMap(presetIndex);
  }

  private static ResolvedPreset resolvePreset(SpawnPreset preset,
    RegistrySnapshot registrySnapshot) {
    Set<EntityType<?>> allowTypes = resolveEntityTypes(
      preset.entities().allowList(), registrySnapshot);
    Set<EntityType<?>> denyTypes = resolveEntityTypes(
      preset.entities().denyList(), registrySnapshot);
    LinkedHashSet<EntityType<?>> coveredTypes = new LinkedHashSet<>(allowTypes);
    coveredTypes.addAll(denyTypes);
    if (coveredTypes.isEmpty() && preset.modId() != null && !preset.modId().isBlank()) {
      coveredTypes.addAll(registrySnapshot.entityTypesByNamespace()
        .getOrDefault(preset.modId(), Collections.emptySet()));
    }

    return new ResolvedPreset(
      preset,
      Collections.unmodifiableSet(allowTypes),
      Collections.unmodifiableSet(denyTypes),
      Collections.unmodifiableSet(coveredTypes),
      resolveDimensionIds(preset.dimensions().allow()),
      resolveDimensionIds(preset.dimensions().deny()),
      resolveDimensionIds(preset.dimensions().ignore()),
      precomputeLimits(preset.entities().perPlayerMax(), preset.loadFactors()),
      precomputeLimits(preset.entities().perWorldMax(), preset.loadFactors()),
      precomputeLimits(preset.entities().perServerMax(), preset.loadFactors()),
      precomputeLimits(preset.entities().perChunkMax(), preset.loadFactors()));
  }

  private static int[] precomputeLimits(int rawLimit, SpawnPreset.LoadFactors loadFactors) {
    int[] limits = new int[ServerLoadLevel.values().length];
    if (rawLimit < 0) {
      Arrays.fill(limits, -1);
      return limits;
    }

    for (ServerLoadLevel loadLevel : ServerLoadLevel.values()) {
      limits[loadLevel.ordinal()] =
        Math.max(1, (int) Math.round(rawLimit * loadFactors.forLevel(loadLevel)));
    }

    return limits;
  }

  private static Set<ResourceLocation> resolveDimensionIds(List<String> dimensionIds) {
    if (dimensionIds == null || dimensionIds.isEmpty()) {
      return Collections.emptySet();
    }

    LinkedHashSet<ResourceLocation> resolvedDimensions = new LinkedHashSet<>();
    for (String dimensionId : dimensionIds) {
      ResourceLocation dimensionKey = ResourceLocation.tryParse(dimensionId);
      if (dimensionKey != null) {
        resolvedDimensions.add(dimensionKey);
      }
    }

    return Collections.unmodifiableSet(resolvedDimensions);
  }

  private static Set<EntityType<?>> resolveEntityTypes(
    Set<String> entityPatterns, RegistrySnapshot registrySnapshot) {
    if (entityPatterns.isEmpty()) {
      return Collections.emptySet();
    }

    LinkedHashSet<EntityType<?>> entityTypes = new LinkedHashSet<>();
    for (String entityPattern : entityPatterns) {
      if (entityPattern == null || entityPattern.isBlank()) {
        continue;
      }

      if ("*".equals(entityPattern)) {
        entityTypes.addAll(registrySnapshot.allEntityTypes());
        continue;
      }

      if (entityPattern.startsWith("#")) {
        resolveEntityTag(entityPattern, entityTypes);
        continue;
      }

      if (entityPattern.endsWith(":*")) {
        entityTypes.addAll(registrySnapshot.entityTypesByNamespace()
          .getOrDefault(entityPattern.substring(0, entityPattern.length() - 2),
            Collections.emptySet()));
        continue;
      }

      EntityType<?> entityType = resolveEntityType(entityPattern);
      if (entityType != null) {
        entityTypes.add(entityType);
      }
    }

    return entityTypes;
  }

  private static void resolveEntityTag(String tagPattern, Set<EntityType<?>> entityTypes) {
    ResourceLocation tagLocation = ResourceLocation.tryParse(tagPattern.substring(1));
    if (tagLocation == null) {
      log.warn("Invalid entity tag '{}' in spawn preset, skipping.", tagPattern);
      return;
    }

    TagKey<EntityType<?>> tagKey = TagKey.create(Registries.ENTITY_TYPE, tagLocation);
    Optional<HolderSet.Named<EntityType<?>>> tagHolders =
      BuiltInRegistries.ENTITY_TYPE.getTag(tagKey);
    if (tagHolders.isEmpty()) {
      log.warn("Unknown entity tag '{}' in spawn preset, skipping.", tagPattern);
      return;
    }

    for (Holder<EntityType<?>> holder : tagHolders.get()) {
      entityTypes.add(holder.value());
    }
  }

  private static EntityType<?> resolveEntityType(String entityId) {
    if (entityId == null || entityId.isBlank()) {
      return null;
    }

    ResourceLocation entityKey = ResourceLocation.tryParse(entityId);
    if (entityKey == null || !BuiltInRegistries.ENTITY_TYPE.containsKey(entityKey)) {
      return null;
    }

    return BuiltInRegistries.ENTITY_TYPE.get(entityKey);
  }

  private record PresetCacheKey(
    EntityType<?> entityType,
    String dimensionId) {

  }

  private record RegistrySnapshot(
    Set<EntityType<?>> allEntityTypes,
    Map<String, Set<EntityType<?>>> entityTypesByNamespace) {

  }

  private record ResolvedPreset(
    SpawnPreset preset,
    Set<EntityType<?>> allowTypes,
    Set<EntityType<?>> denyTypes,
    Set<EntityType<?>> coveredTypes,
    Set<ResourceLocation> allowedDimensions,
    Set<ResourceLocation> deniedDimensions,
    Set<ResourceLocation> ignoredDimensions,
    int[] perPlayerLimits,
    int[] perWorldLimits,
    int[] perServerLimits,
    int[] perChunkLimits) {

  }
}
