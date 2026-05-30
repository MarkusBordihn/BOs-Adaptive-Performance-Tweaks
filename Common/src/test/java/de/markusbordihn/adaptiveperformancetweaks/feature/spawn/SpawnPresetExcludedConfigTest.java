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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingMode;
import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class SpawnPresetExcludedConfigTest {

  private static final Gson GSON = new Gson();
  private static final Path EXCLUDED_DIR = Path.of(
    "src/main/resources/data/adaptive_performance_tweaks/aptweaks/spawn_presets/mods/excluded");

  static Stream<Path> excludedPresetFiles() throws IOException {
    return Files.walk(EXCLUDED_DIR)
      .filter(path -> path.toString().endsWith(".json"));
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("excludedPresetFiles")
  void eachExcludedPresetIsValid(Path file) throws IOException {
    String name = EXCLUDED_DIR.relativize(file).toString();
    JsonObject jsonObject;
    try (Reader reader = Files.newBufferedReader(file)) {
      jsonObject = GSON.fromJson(reader, JsonObject.class);
    }

    assertTrue(jsonObject.has("mod_id"), name + ": missing 'mod_id'");
    String modId = jsonObject.get("mod_id").getAsString();
    assertFalse(modId.isBlank(), name + ": 'mod_id' is blank");

    assertTrue(jsonObject.has("priority"), name + ": missing 'priority'");
    assertTrue(jsonObject.get("priority").getAsInt() > 0, name + ": 'priority' must be > 0");

    assertTrue(jsonObject.has("mode"), name + ": missing 'mode'");
    String modeStr = jsonObject.get("mode").getAsString();
    TrackingMode trackingMode = TrackingMode.fromSerializedName(modeStr, null);
    assertNotNull(trackingMode, name + ": unknown 'mode' value '" + modeStr
      + "' - valid values: exclude_namespace, exclude_entity, protect_namespace, protect_entity");

    assertTrue(jsonObject.has("category"), name + ": missing 'category'");
    String categoryStr = jsonObject.get("category").getAsString();
    TrackingCategory trackingCategory = TrackingCategory.fromSerializedName(categoryStr);
    assertNotEquals(TrackingCategory.UNKNOWN, trackingCategory,
      name + ": unknown 'category' value '" + categoryStr
        + "' - valid values: technical, vehicle_structure, world_effect, managed_living, storage_network, manual_override");

    assertFalse(jsonObject.has("entities"),
      name + ": excluded tracking presets must not define 'entities'");
    assertFalse(jsonObject.has("dimensions"),
      name + ": excluded tracking presets must not define 'dimensions'");
    assertFalse(jsonObject.has("load_factors"),
      name + ": excluded tracking presets must not define 'load_factors'");
  }
}
