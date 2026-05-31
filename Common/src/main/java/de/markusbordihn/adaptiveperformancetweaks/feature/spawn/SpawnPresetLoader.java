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

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModCompat;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingMode;
import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import net.minecraft.resources.FileToIdConverter;
import net.minecraft.resources.Identifier;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.server.packs.resources.SimpleJsonResourceReloadListener;
import net.minecraft.util.ExtraCodecs;
import net.minecraft.util.profiling.InactiveProfiler;
import net.minecraft.util.profiling.ProfilerFiller;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SpawnPresetLoader extends SimpleJsonResourceReloadListener<JsonElement> {

  public static final String ALLOW_FIELD = "allow";
  public static final String ALLOW_LIST_FIELD = "allow_list";
  public static final String DENY_FIELD = "deny";
  public static final String DENY_LIST_FIELD = "deny_list";
  public static final String DIMENSIONS_FIELD = "dimensions";
  public static final String ENTITIES_FIELD = "entities";
  public static final String EXCLUDE_FROM_TRACKING_FIELD = "exclude_from_tracking";
  public static final String HIGH_FIELD = "high";
  public static final String IGNORE_FIELD = "ignore";
  public static final String LOAD_FACTORS_FIELD = "load_factors";
  public static final String LOW_FIELD = "low";
  public static final String MEDIUM_FIELD = "medium";
  public static final String MOD_ID_FIELD = "mod_id";
  public static final String MODE_FIELD = "mode";
  public static final String NORMAL_FIELD = "normal";
  public static final String NOTES_FIELD = "notes";
  public static final String PER_CHUNK_MAX_FIELD = "per_chunk_max";
  public static final String PER_PLAYER_MAX_FIELD = "per_player_max";
  public static final String PER_SERVER_MAX_FIELD = "per_server_max";
  public static final String PER_WORLD_MAX_FIELD = "per_world_max";
  public static final String PRIORITY_FIELD = "priority";
  public static final String REPLACE_FIELD = "replace";
  public static final String REASON_FIELD = "reason";
  public static final String REQUIRED_MODS_FIELD = "required_mods";
  public static final String TEMPLATE_FIELD = "template";
  public static final String CATEGORY_FIELD = "category";
  public static final String ENTITY_IDS_FIELD = "entity_ids";
  public static final String TRACKING_FIELD = "tracking";
  public static final String VERY_HIGH_FIELD = "very_high";
  public static final String VERY_LOW_FIELD = "very_low";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_SPAWN);
  private static final Gson GSON = new GsonBuilder().create();

  public SpawnPresetLoader() {
    super(ExtraCodecs.JSON, FileToIdConverter.json("aptweaks/spawn_presets"));
  }

  private static void scanConfigDirectory(List<SpawnPreset> output) {
    Path configDir =
      Paths.get("config").resolve(Constants.MOD_ID).resolve("spawn_presets").toAbsolutePath();
    if (!Files.isDirectory(configDir)) {
      return;
    }

    try (Stream<Path> paths = Files.walk(configDir)) {
      paths
        .filter(path -> path.toString().endsWith(".json"))
        .forEach(
          path -> {
            try (Reader reader = Files.newBufferedReader(path)) {
              JsonElement json = GSON.fromJson(reader, JsonElement.class);
              parseAndAdd("config:" + configDir.relativize(path), json, output);
            } catch (IOException exception) {
              log.warn(
                "Failed to read spawn preset config file '{}': {}",
                path,
                exception.getMessage());
            }
          });
    } catch (IOException exception) {
      log.warn(
        "Failed to scan spawn preset config directory '{}': {}",
        configDir,
        exception.getMessage());
    }
  }

  private static ParseResult parseAndAdd(String sourceId, JsonElement jsonElement,
    List<SpawnPreset> output) {
    try {
      JsonObject jsonObject = jsonElement.getAsJsonObject();

      if (getBoolean(jsonObject, TEMPLATE_FIELD, false)) {
        log.debug("Skipping template preset '{}'", sourceId);
        return ParseResult.SKIPPED_TEMPLATE;
      }

      String modId =
        jsonObject.has(MOD_ID_FIELD) ? jsonObject.get(MOD_ID_FIELD).getAsString().trim() : null;
      if (modId != null && modId.isBlank()) {
        modId = null;
      }

      if (modId != null && !ModCompat.isModLoaded(modId)) {
        log.debug("Skipping spawn preset '{}' - mod '{}' not loaded", sourceId, modId);
        return ParseResult.SKIPPED_MISSING_MOD;
      }

      List<String> requiredMods = parseStringList(jsonObject, REQUIRED_MODS_FIELD);
      for (String required : requiredMods) {
        if (!ModCompat.isModLoaded(required)) {
          log.debug("Skipping spawn preset '{}' - required mod '{}' not loaded",
            sourceId, required);
          return ParseResult.SKIPPED_MISSING_MOD;
        }
      }

      boolean replace =
        jsonObject.has(REPLACE_FIELD) && jsonObject.get(REPLACE_FIELD).getAsBoolean();
      int priority =
        jsonObject.has(PRIORITY_FIELD) ? jsonObject.get(PRIORITY_FIELD).getAsInt() : 100;

      JsonObject entitiesObject =
        jsonObject.has(ENTITIES_FIELD)
          ? jsonObject.getAsJsonObject(ENTITIES_FIELD)
          : new JsonObject();
      Set<String> allowList =
        expandEntityNames(parseStringList(entitiesObject, ALLOW_LIST_FIELD), modId);
      Set<String> denyList =
        expandEntityNames(parseStringList(entitiesObject, DENY_LIST_FIELD), modId);
      int perPlayerMax =
        entitiesObject.has(PER_PLAYER_MAX_FIELD)
          ? entitiesObject.get(PER_PLAYER_MAX_FIELD).getAsInt()
          : SpawnConfig.spawnLimitationMaxMobsPerPlayer;
      int perWorldMax =
        entitiesObject.has(PER_WORLD_MAX_FIELD)
          ? entitiesObject.get(PER_WORLD_MAX_FIELD).getAsInt()
          : SpawnConfig.spawnLimitationMaxMobsPerWorld;
      int perServerMax =
        entitiesObject.has(PER_SERVER_MAX_FIELD)
          ? entitiesObject.get(PER_SERVER_MAX_FIELD).getAsInt()
          : SpawnConfig.spawnLimitationMaxMobsPerServer;
      int perChunkMax =
        entitiesObject.has(PER_CHUNK_MAX_FIELD)
          ? entitiesObject.get(PER_CHUNK_MAX_FIELD).getAsInt()
          : -1;
      SpawnPreset.EntityLimits entities =
        new SpawnPreset.EntityLimits(
          Collections.unmodifiableSet(allowList),
          Collections.unmodifiableSet(denyList),
          perPlayerMax,
          perWorldMax,
          perServerMax,
          perChunkMax);

      JsonObject dimensionsObject =
        jsonObject.has(DIMENSIONS_FIELD)
          ? jsonObject.getAsJsonObject(DIMENSIONS_FIELD)
          : new JsonObject();
      SpawnPreset.DimensionFilter dimensions =
        new SpawnPreset.DimensionFilter(
          parseStringList(dimensionsObject, ALLOW_FIELD),
          parseStringList(dimensionsObject, DENY_FIELD),
          parseStringList(dimensionsObject, IGNORE_FIELD));

      JsonObject loadFactorsObject =
        jsonObject.has(LOAD_FACTORS_FIELD)
          ? jsonObject.getAsJsonObject(LOAD_FACTORS_FIELD)
          : new JsonObject();
      SpawnPreset.LoadFactors loadFactors =
        new SpawnPreset.LoadFactors(
          getDouble(loadFactorsObject, VERY_LOW_FIELD, 1.0),
          getDouble(loadFactorsObject, LOW_FIELD, 1.0),
          getDouble(loadFactorsObject, NORMAL_FIELD, 0.9),
          getDouble(loadFactorsObject, MEDIUM_FIELD, 0.7),
          getDouble(loadFactorsObject, HIGH_FIELD, 0.4),
          getDouble(loadFactorsObject, VERY_HIGH_FIELD, 0.1));

      TrackingData trackingData = parseTracking(jsonObject, modId);

      output.add(new SpawnPreset(
        replace, modId, requiredMods, priority, dimensions, entities, loadFactors,
        trackingData.mode(), trackingData.category(), trackingData.reason(),
        trackingData.entityIds()));
      return ParseResult.ADDED;
    } catch (Exception exception) {
      log.warn("Failed to parse spawn preset '{}': {}", sourceId, exception.getMessage());
      return ParseResult.FAILED;
    }
  }

  private static Set<String> expandEntityNames(List<String> names, String modId) {
    Set<String> expanded = new LinkedHashSet<>();
    for (String name : names) {
      if (name == null || name.isBlank()) {
        continue;
      }

      if (name.startsWith("#")) {
        expanded.add(name);
        continue;
      }

      if ("*".equals(name)) {
        expanded.add(modId != null ? modId + ":*" : "*");
      } else if (!name.contains(":") && modId != null) {
        expanded.add(modId + ":" + name);
      } else {
        expanded.add(name);
      }
    }

    return expanded;
  }

  private static List<String> parseStringList(JsonObject jsonObject, String key) {
    if (!jsonObject.has(key)) {
      return Collections.emptyList();
    }

    List<String> result = new ArrayList<>();
    for (JsonElement element : jsonObject.getAsJsonArray(key)) {
      String value = element.getAsString().trim();
      if (!value.isEmpty()) {
        result.add(value);
      }
    }

    return Collections.unmodifiableList(result);
  }

  private static TrackingData parseTracking(JsonObject jsonObject, String modId) {
    TrackingMode rootMode = TrackingMode.fromSerializedName(getString(jsonObject, MODE_FIELD),
      null);
    TrackingCategory rootCategory = TrackingCategory.fromSerializedName(
      getString(jsonObject, CATEGORY_FIELD));
    String rootReason = getString(jsonObject, REASON_FIELD);
    Set<String> rootEntityIds = Collections.unmodifiableSet(
      expandEntityNames(parseStringList(jsonObject, ENTITY_IDS_FIELD), modId));
    if (rootMode != null) {
      return new TrackingData(rootMode, rootCategory, rootReason, rootEntityIds);
    }

    boolean excludeFromTracking = getBoolean(jsonObject, EXCLUDE_FROM_TRACKING_FIELD, false);
    if (!jsonObject.has(TRACKING_FIELD)) {
      if (!excludeFromTracking) {
        return TrackingData.empty();
      }

      String legacyReason = getString(jsonObject, NOTES_FIELD);
      return new TrackingData(
        TrackingMode.EXCLUDE_NAMESPACE,
        TrackingCategory.UNKNOWN,
        legacyReason,
        Collections.emptySet());
    }

    JsonObject trackingObject = jsonObject.getAsJsonObject(TRACKING_FIELD);
    TrackingMode defaultMode =
      excludeFromTracking ? TrackingMode.EXCLUDE_NAMESPACE : TrackingMode.EXCLUDE_ENTITY;
    TrackingMode mode = TrackingMode.fromSerializedName(
      getString(trackingObject, MODE_FIELD), defaultMode);
    TrackingCategory category = TrackingCategory.fromSerializedName(
      getString(trackingObject, CATEGORY_FIELD));
    String reason = getString(trackingObject, REASON_FIELD);
    if (reason.isBlank()) {
      reason = getString(jsonObject, NOTES_FIELD);
    }

    Set<String> entityIds = Collections.unmodifiableSet(
      expandEntityNames(parseStringList(trackingObject, ENTITY_IDS_FIELD), modId));
    return new TrackingData(mode, category, reason, entityIds);
  }

  private static double getDouble(JsonObject jsonObject, String key, double defaultValue) {
    return jsonObject.has(key) ? jsonObject.get(key).getAsDouble() : defaultValue;
  }

  private static boolean getBoolean(JsonObject jsonObject, String key, boolean defaultValue) {
    return jsonObject.has(key) ? jsonObject.get(key).getAsBoolean() : defaultValue;
  }

  private static String getString(JsonObject jsonObject, String key) {
    return jsonObject.has(key) ? jsonObject.get(key).getAsString().trim() : "";
  }

  @Override
  protected void apply(
    Map<Identifier, JsonElement> jsons,
    ResourceManager resourceManager,
    ProfilerFiller profiler) {
    List<SpawnPreset> presets = new ArrayList<>();
    int missingModCount = 0;

    for (Map.Entry<Identifier, JsonElement> entry : jsons.entrySet()) {
      ParseResult parseResult = parseAndAdd(entry.getKey().toString(), entry.getValue(), presets);
      if (parseResult == ParseResult.SKIPPED_MISSING_MOD) {
        missingModCount++;
      }
    }

    if (missingModCount > 0) {
      log.debug("Skipped {} mod-specific spawn presets because their mods are not installed.",
        missingModCount);
    }

    scanConfigDirectory(presets);
    SpawnPresetPartitioner.Partition partition = SpawnPresetPartitioner.partition(presets);
    CoreEntityManager.reloadTrackingRules(partition.trackingPresets());
    SpawnPresetRegistry.reload(partition.spawnPresets());
  }

  public void loadPresetsFrom(ResourceManager resourceManager) {
    apply(
      prepare(resourceManager, InactiveProfiler.INSTANCE),
      resourceManager,
      InactiveProfiler.INSTANCE);
  }

  private enum ParseResult {
    ADDED,
    SKIPPED_TEMPLATE,
    SKIPPED_MISSING_MOD,
    FAILED
  }

  private record TrackingData(
    TrackingMode mode,
    TrackingCategory category,
    String reason,
    Set<String> entityIds) {

    private static TrackingData empty() {
      return new TrackingData(null, TrackingCategory.UNKNOWN, "", Collections.emptySet());
    }
  }
}
