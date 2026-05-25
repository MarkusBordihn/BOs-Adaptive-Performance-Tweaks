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
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.server.packs.resources.SimpleJsonResourceReloadListener;
import net.minecraft.util.profiling.InactiveProfiler;
import net.minecraft.util.profiling.ProfilerFiller;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SpawnPresetLoader extends SimpleJsonResourceReloadListener {

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
  public static final String NORMAL_FIELD = "normal";
  public static final String NOTES_FIELD = "notes";
  public static final String PER_CHUNK_MAX_FIELD = "per_chunk_max";
  public static final String PER_PLAYER_MAX_FIELD = "per_player_max";
  public static final String PER_SERVER_MAX_FIELD = "per_server_max";
  public static final String PER_WORLD_MAX_FIELD = "per_world_max";
  public static final String PRIORITY_FIELD = "priority";
  public static final String REPLACE_FIELD = "replace";
  public static final String REQUIRED_MODS_FIELD = "required_mods";
  public static final String TEMPLATE_FIELD = "template";
  public static final String VERY_HIGH_FIELD = "very_high";
  public static final String VERY_LOW_FIELD = "very_low";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_SPAWN);
  private static final Gson GSON = new GsonBuilder().create();

  public SpawnPresetLoader() {
    super(GSON, "aptweaks/spawn_presets");
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

  private static void parseAndAdd(
      String sourceId, JsonElement jsonElement, List<SpawnPreset> output) {
    try {
      JsonObject jsonObject = jsonElement.getAsJsonObject();

      if (getBoolean(jsonObject, TEMPLATE_FIELD, false)) {
        log.debug("Skipping template preset '{}'", sourceId);
        return;
      }

      String modId =
          jsonObject.has(MOD_ID_FIELD) ? jsonObject.get(MOD_ID_FIELD).getAsString().trim() : null;
      if (modId != null && modId.isBlank()) {
        modId = null;
      }

      if (modId != null && !ModCompat.isModLoaded(modId)) {
        log.warn("Skipping spawn preset '{}' — mod '{}' not loaded", sourceId, modId);
        return;
      }

      List<String> requiredMods = parseStringList(jsonObject, REQUIRED_MODS_FIELD);
      for (String required : requiredMods) {
        if (!ModCompat.isModLoaded(required)) {
          log.warn("Skipping spawn preset '{}' — required mod '{}' not loaded", sourceId, required);
          return;
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

      boolean excludeFromTracking = getBoolean(jsonObject, EXCLUDE_FROM_TRACKING_FIELD, false);

      String notes = jsonObject.has(NOTES_FIELD) ? jsonObject.get(NOTES_FIELD).getAsString() : "";
      if (!notes.isBlank()) {
        log.debug("Preset '{}' notes: {}", sourceId, notes);
      }

      output.add(
          new SpawnPreset(
              replace,
              modId,
              requiredMods,
              priority,
              dimensions,
              entities,
              loadFactors,
              excludeFromTracking,
              notes));
    } catch (Exception exception) {
      log.warn("Failed to parse spawn preset '{}': {}", sourceId, exception.getMessage());
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

  private static double getDouble(JsonObject jsonObject, String key, double defaultValue) {
    return jsonObject.has(key) ? jsonObject.get(key).getAsDouble() : defaultValue;
  }

  private static boolean getBoolean(JsonObject jsonObject, String key, boolean defaultValue) {
    return jsonObject.has(key) ? jsonObject.get(key).getAsBoolean() : defaultValue;
  }

  @Override
  protected void apply(
      Map<ResourceLocation, JsonElement> jsons,
      ResourceManager resourceManager,
      ProfilerFiller profiler) {
    List<SpawnPreset> presets = new ArrayList<>();

    for (Map.Entry<ResourceLocation, JsonElement> entry : jsons.entrySet()) {
      parseAndAdd(entry.getKey().toString(), entry.getValue(), presets);
    }

    scanConfigDirectory(presets);

    List<SpawnPreset> spawnPresets = new ArrayList<>();
    Set<String> excludedNamespaces = new LinkedHashSet<>();
    for (SpawnPreset preset : presets) {
      if (preset.excludeFromTracking()) {
        if (preset.modId() != null) {
          excludedNamespaces.add(preset.modId());
        }
      } else {
        spawnPresets.add(preset);
      }
    }

    SpawnPresetRegistry.reload(spawnPresets);
    CoreEntityManager.setExcludedModNamespaces(excludedNamespaces);
  }

  public void loadPresetsFrom(ResourceManager resourceManager) {
    apply(
        prepare(resourceManager, InactiveProfiler.INSTANCE),
        resourceManager,
        InactiveProfiler.INSTANCE);
  }
}
