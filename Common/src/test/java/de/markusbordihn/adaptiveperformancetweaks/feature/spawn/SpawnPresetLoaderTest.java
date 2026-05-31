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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModCompat;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingMode;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class SpawnPresetLoaderTest {

  private static final Path CONFIG_TEST_DIRECTORY = Paths.get("config", Constants.MOD_ID,
    "spawn_presets", "loader_test");
  private static final Path CONFIG_TEST_FILE = CONFIG_TEST_DIRECTORY.resolve("local_test.json");
  private static final String EASY_NPC_PRESET_RESOURCE =
    "data/adaptive_performance_tweaks/aptweaks/spawn_presets/mods/excluded/managed_living/easy_npc.json";

  private static JsonArray jsonArrayOf(String... values) {
    JsonArray array = new JsonArray();
    for (String value : values) {
      array.add(value);
    }
    return array;
  }

  private static Object invokePrivateMethod(String methodName, Class<?>[] parameterTypes,
    Object... args) throws Exception {
    Method method = SpawnPresetLoader.class.getDeclaredMethod(methodName, parameterTypes);
    method.setAccessible(true);
    return method.invoke(null, args);
  }

  private static String parseResultName(Object parseResult) {
    return ((Enum<?>) parseResult).name();
  }

  @BeforeEach
  void setUp() {
    ModCompat.setModLoadedChecker(id -> true);
  }

  @AfterEach
  void tearDown() throws Exception {
    ModCompat.setModLoadedChecker(id -> false);
    Files.deleteIfExists(CONFIG_TEST_FILE);
    Files.deleteIfExists(CONFIG_TEST_DIRECTORY);
  }

  @Test
  void parseAndAddSkipsTemplatePresets() throws Exception {
    JsonObject jsonObject = new JsonObject();
    jsonObject.addProperty("template", true);
    List<SpawnPreset> output = new ArrayList<>();

    Object result = invokePrivateMethod("parseAndAdd",
      new Class<?>[]{String.class, com.google.gson.JsonElement.class, List.class},
      "test:template", jsonObject, output);

    assertEquals("SKIPPED_TEMPLATE", parseResultName(result));
    assertTrue(output.isEmpty());
  }

  @Test
  void parseAndAddExpandsWildcardAndUnqualifiedEntityNames() throws Exception {
    JsonObject jsonObject = new JsonObject();
    jsonObject.addProperty("mod_id", "minecraft");
    JsonObject entities = new JsonObject();
    entities.add("allow_list", jsonArrayOf("zombie", "*", "#forge:animals"));
    jsonObject.add("entities", entities);
    List<SpawnPreset> output = new ArrayList<>();

    Object result = invokePrivateMethod("parseAndAdd",
      new Class<?>[]{String.class, com.google.gson.JsonElement.class, List.class},
      "test:allow_list", jsonObject, output);

    assertEquals("ADDED", parseResultName(result));
    assertEquals(1, output.size());
    assertEquals(Set.of("minecraft:zombie", "minecraft:*", "#forge:animals"),
      output.get(0).entities().allowList());
  }

  @Test
  void parseAndAddUsesLegacyExcludeFromTrackingNamespaceRule() throws Exception {
    JsonObject jsonObject = new JsonObject();
    jsonObject.addProperty("mod_id", "create");
    jsonObject.addProperty("exclude_from_tracking", true);
    jsonObject.addProperty("notes", "Legacy exclusion");
    List<SpawnPreset> output = new ArrayList<>();

    Object result = invokePrivateMethod("parseAndAdd",
      new Class<?>[]{String.class, com.google.gson.JsonElement.class, List.class},
      "test:legacy_tracking", jsonObject, output);

    assertEquals("ADDED", parseResultName(result));
    assertEquals(1, output.size());
    SpawnPreset preset = output.get(0);
    assertEquals(TrackingMode.EXCLUDE_NAMESPACE, preset.mode());
    assertEquals(TrackingCategory.UNKNOWN, preset.category());
    assertEquals("Legacy exclusion", preset.reason());
    assertTrue(preset.entityIds().isEmpty());
  }

  @Test
  void parseAndAddReadsRootLevelTrackingEntityIds() throws Exception {
    JsonObject jsonObject = new JsonObject();
    jsonObject.addProperty("mod_id", "easy_npc");
    jsonObject.addProperty("mode", "protect_entity");
    jsonObject.addProperty("category", "managed_living");
    jsonObject.addProperty("reason", "Protect selected NPC variants");
    jsonObject.add("entity_ids", jsonArrayOf("npc", "easy_npc:merchant"));
    List<SpawnPreset> output = new ArrayList<>();

    Object result = invokePrivateMethod("parseAndAdd",
      new Class<?>[]{String.class, com.google.gson.JsonElement.class, List.class},
      "test:root_tracking_entities", jsonObject, output);

    assertEquals("ADDED", parseResultName(result));
    assertEquals(1, output.size());
    SpawnPreset preset = output.get(0);
    assertEquals(TrackingMode.PROTECT_ENTITY, preset.mode());
    assertEquals(TrackingCategory.MANAGED_LIVING, preset.category());
    assertEquals("Protect selected NPC variants", preset.reason());
    assertEquals(Set.of("easy_npc:npc", "easy_npc:merchant"), preset.entityIds());
  }

  @Test
  void parseAndAddReadsNestedTrackingEntityIdsAndFallsBackToNotes() throws Exception {
    JsonObject jsonObject = new JsonObject();
    jsonObject.addProperty("mod_id", "minecolonies");
    jsonObject.addProperty("notes", "Protect colony citizens");
    JsonObject tracking = new JsonObject();
    tracking.addProperty("mode", "protect_entity");
    tracking.addProperty("category", "managed_living");
    tracking.add("entity_ids", jsonArrayOf("citizen", "minecraft:villager"));
    jsonObject.add("tracking", tracking);
    List<SpawnPreset> output = new ArrayList<>();

    Object result = invokePrivateMethod("parseAndAdd",
      new Class<?>[]{String.class, com.google.gson.JsonElement.class, List.class},
      "test:nested_tracking", jsonObject, output);

    assertEquals("ADDED", parseResultName(result));
    assertEquals(1, output.size());
    SpawnPreset preset = output.get(0);
    assertEquals(TrackingMode.PROTECT_ENTITY, preset.mode());
    assertEquals(TrackingCategory.MANAGED_LIVING, preset.category());
    assertEquals("Protect colony citizens", preset.reason());
    assertEquals(Set.of("minecolonies:citizen", "minecraft:villager"), preset.entityIds());
  }

  @Test
  void parseAndAddSkipsPresetsForMissingRequiredMods() throws Exception {
    ModCompat.setModLoadedChecker(id -> "minecraft".equals(id));
    JsonObject jsonObject = new JsonObject();
    jsonObject.add("required_mods", jsonArrayOf("minecraft", "missing_mod"));
    List<SpawnPreset> output = new ArrayList<>();

    Object result = invokePrivateMethod("parseAndAdd",
      new Class<?>[]{String.class, com.google.gson.JsonElement.class, List.class},
      "test:required_mods", jsonObject, output);

    assertEquals("SKIPPED_MISSING_MOD", parseResultName(result));
    assertTrue(output.isEmpty());
  }

  @Test
  void scanConfigDirectoryLoadsLocalConfigPresets() throws Exception {
    Files.createDirectories(CONFIG_TEST_DIRECTORY);
    Files.writeString(CONFIG_TEST_FILE, """
      {
        "mod_id": "minecraft",
        "priority": 175,
        "dimensions": {
          "allow": ["minecraft:overworld"]
        },
        "entities": {
          "allow_list": ["zombie"],
          "per_player_max": 3,
          "per_world_max": 12,
          "per_server_max": 24,
          "per_chunk_max": 1
        }
      }
      """);
    List<SpawnPreset> output = new ArrayList<>();

    invokePrivateMethod("scanConfigDirectory", new Class<?>[]{List.class}, output);

    assertEquals(1, output.size());
    SpawnPreset preset = output.get(0);
    assertEquals("minecraft", preset.modId());
    assertEquals(175, preset.priority());
    assertEquals(List.of("minecraft:overworld"), preset.dimensions().allow());
    assertEquals(Set.of("minecraft:zombie"), preset.entities().allowList());
    assertEquals(3, preset.entities().perPlayerMax());
    assertEquals(12, preset.entities().perWorldMax());
    assertEquals(24, preset.entities().perServerMax());
    assertEquals(1, preset.entities().perChunkMax());
  }

  @Test
  void bundledEasyNpcPresetProtectsManagedLivingEntities() throws Exception {
    try (InputStream inputStream = SpawnPresetLoaderTest.class.getClassLoader()
      .getResourceAsStream(EASY_NPC_PRESET_RESOURCE)) {
      assertTrue(inputStream != null);
      JsonObject jsonObject = JsonParser.parseReader(
        new java.io.InputStreamReader(inputStream)).getAsJsonObject();
      List<SpawnPreset> output = new ArrayList<>();

      Object result = invokePrivateMethod("parseAndAdd",
        new Class<?>[]{String.class, com.google.gson.JsonElement.class, List.class},
        "resource:easy_npc", jsonObject, output);

      assertEquals("ADDED", parseResultName(result));
      assertEquals(1, output.size());
      SpawnPreset preset = output.get(0);
      assertEquals("easy_npc", preset.modId());
      assertEquals(TrackingMode.PROTECT_NAMESPACE, preset.mode());
      assertEquals(TrackingCategory.MANAGED_LIVING, preset.category());
      assertEquals("Player-placed NPCs should be protected from tracking.", preset.reason());
    }
  }
}
