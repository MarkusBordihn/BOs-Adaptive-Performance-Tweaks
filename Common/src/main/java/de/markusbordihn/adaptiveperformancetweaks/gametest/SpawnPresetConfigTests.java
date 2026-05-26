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

package de.markusbordihn.adaptiveperformancetweaks.gametest;

import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnPresetLoader;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnPresetRegistry;
import net.minecraft.gametest.framework.GameTestHelper;

public final class SpawnPresetConfigTests {

  private static final String OVERWORLD = "minecraft:overworld";

  private SpawnPresetConfigTests() {
  }

  public static void testBatPresetsLoaded(GameTestHelper helper) {
    new SpawnPresetLoader().loadPresetsFrom(helper.getLevel().getServer().getResourceManager());
    String entityId = "minecraft:bat";
    GameTestHelpers.assertEquals(helper,
      entityId + " perPlayerMax mismatch - spawn presets may not be loaded",
      4,
      SpawnPresetRegistry.getEffectivePerPlayerMax(entityId, OVERWORLD, ServerLoadLevel.VERY_LOW));
    GameTestHelpers.assertEquals(helper,
      entityId + " perWorldMax mismatch - spawn presets may not be loaded",
      16,
      SpawnPresetRegistry.getEffectivePerWorldMax(entityId, OVERWORLD, ServerLoadLevel.VERY_LOW));
    GameTestHelpers.assertEquals(helper,
      entityId + " perServerMax mismatch - spawn presets may not be loaded",
      64,
      SpawnPresetRegistry.getEffectivePerServerMax(entityId, OVERWORLD, ServerLoadLevel.VERY_LOW));
    GameTestHelpers.assertEquals(helper,
      entityId + " perChunkMax mismatch - spawn presets may not be loaded",
      2,
      SpawnPresetRegistry.getEffectivePerChunkMax(entityId, OVERWORLD, ServerLoadLevel.VERY_LOW));
    helper.succeed();
  }

  public static void testZombiePresetsLoaded(GameTestHelper helper) {
    new SpawnPresetLoader().loadPresetsFrom(helper.getLevel().getServer().getResourceManager());
    String entityId = "minecraft:zombie";
    GameTestHelpers.assertEquals(helper,
      entityId + " perPlayerMax mismatch - spawn presets may not be loaded",
      10,
      SpawnPresetRegistry.getEffectivePerPlayerMax(entityId, OVERWORLD, ServerLoadLevel.VERY_LOW));
    GameTestHelpers.assertEquals(helper,
      entityId + " perWorldMax mismatch - spawn presets may not be loaded",
      40,
      SpawnPresetRegistry.getEffectivePerWorldMax(entityId, OVERWORLD, ServerLoadLevel.VERY_LOW));
    GameTestHelpers.assertEquals(helper,
      entityId + " perServerMax mismatch - spawn presets may not be loaded",
      320,
      SpawnPresetRegistry.getEffectivePerServerMax(entityId, OVERWORLD, ServerLoadLevel.VERY_LOW));
    helper.succeed();
  }

  public static void testCowPresetsLoaded(GameTestHelper helper) {
    new SpawnPresetLoader().loadPresetsFrom(helper.getLevel().getServer().getResourceManager());
    String entityId = "minecraft:cow";
    GameTestHelpers.assertEquals(helper,
      entityId + " perPlayerMax mismatch - spawn presets may not be loaded",
      10,
      SpawnPresetRegistry.getEffectivePerPlayerMax(entityId, OVERWORLD, ServerLoadLevel.VERY_LOW));
    GameTestHelpers.assertEquals(helper,
      entityId + " perWorldMax mismatch - spawn presets may not be loaded",
      40,
      SpawnPresetRegistry.getEffectivePerWorldMax(entityId, OVERWORLD, ServerLoadLevel.VERY_LOW));
    helper.succeed();
  }
}
