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

import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingMode;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import net.minecraft.SharedConstants;
import net.minecraft.resources.Identifier;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.EntityType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class SpawnPresetRegistryTest {

  private static final String OVERWORLD = "minecraft:overworld";

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

  @AfterEach
  void resetRegistry() {
    SpawnPresetRegistry.reload(Collections.emptyList());
  }

  @Test
  void typedAndStringEvaluationMatchForNamespaceWildcardPreset() {
    SpawnPresetRegistry.reload(List.of(new SpawnPreset(
      false,
      null,
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of("minecraft:*"), Set.of(), 7, 21, 42, 3),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_ENTITY,
      TrackingCategory.UNKNOWN,
      "",
      Set.of())));

    assertEquals(SpawnDecision.ALLOW,
      SpawnPresetRegistry.evaluate(EntityType.ZOMBIE, OVERWORLD));
    assertEquals(SpawnDecision.ALLOW,
      SpawnPresetRegistry.evaluate("minecraft:zombie", OVERWORLD));
    assertEquals(21,
      SpawnPresetRegistry.getEffectivePerWorldMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.VERY_LOW));
    assertEquals(21,
      SpawnPresetRegistry.getEffectivePerWorldMax("minecraft:zombie", OVERWORLD,
        ServerLoadLevel.VERY_LOW));
  }

  @Test
  void wildcardDenyPresetAppliesToAnyRegisteredEntity() {
    SpawnPresetRegistry.reload(List.of(new SpawnPreset(
      false,
      null,
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of(), Set.of("*"), 5, 10, 15, 2),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_ENTITY,
      TrackingCategory.UNKNOWN,
      "",
      Set.of())));

    assertEquals(SpawnDecision.DENY,
      SpawnPresetRegistry.evaluate(EntityType.ZOMBIE, OVERWORLD));
    assertEquals(SpawnDecision.DENY,
      SpawnPresetRegistry.evaluate("minecraft:cow", OVERWORLD));
  }

  @Test
  void modIdOnlyPresetIsExpandedAtReloadTime() {
    SpawnPresetRegistry.reload(List.of(new SpawnPreset(
      false,
      "minecraft",
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of(), Set.of(), 9, 18, 27, 3),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_NAMESPACE,
      TrackingCategory.UNKNOWN,
      "",
      Set.of())));

    assertEquals(18,
      SpawnPresetRegistry.getEffectivePerWorldMax(EntityType.SKELETON, OVERWORLD,
        ServerLoadLevel.VERY_LOW));
    assertEquals(18,
      SpawnPresetRegistry.getEffectivePerWorldMax("minecraft:skeleton", OVERWORLD,
        ServerLoadLevel.VERY_LOW));
  }

  @Test
  void dimensionSpecificPresetOverridesGlobalPresetOnlyInAllowedDimension() {
    SpawnPreset globalPreset = new SpawnPreset(
      false,
      null,
      List.of(),
      10,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of("minecraft:zombie"), Set.of(), 4, 8, 12, 2),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_ENTITY,
      TrackingCategory.UNKNOWN,
      "",
      Set.of());
    SpawnPreset overworldPreset = new SpawnPreset(
      false,
      null,
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(OVERWORLD), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of("minecraft:zombie"), Set.of(), 7, 21, 42, 3),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_ENTITY,
      TrackingCategory.UNKNOWN,
      "",
      Set.of());
    SpawnPresetRegistry.reload(List.of(globalPreset, overworldPreset));

    assertEquals(21,
      SpawnPresetRegistry.getEffectivePerWorldMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.VERY_LOW));
    assertEquals(8,
      SpawnPresetRegistry.getEffectivePerWorldMax(EntityType.ZOMBIE, "minecraft:the_nether",
        ServerLoadLevel.VERY_LOW));
  }

  @Test
  void deniedDimensionFallsBackToLowerPriorityGlobalPreset() {
    SpawnPreset globalPreset = new SpawnPreset(
      false,
      null,
      List.of(),
      10,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of("minecraft:zombie"), Set.of(), 4, 8, 12, 2),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_ENTITY,
      TrackingCategory.UNKNOWN,
      "",
      Set.of());
    SpawnPreset deniedOverworldPreset = new SpawnPreset(
      false,
      null,
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(), List.of(OVERWORLD), List.of()),
      new SpawnPreset.EntityLimits(Set.of("minecraft:zombie"), Set.of(), 7, 21, 42, 3),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_ENTITY,
      TrackingCategory.UNKNOWN,
      "",
      Set.of());
    SpawnPresetRegistry.reload(List.of(globalPreset, deniedOverworldPreset));

    assertEquals(8,
      SpawnPresetRegistry.getEffectivePerWorldMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.VERY_LOW));
    assertEquals(21,
      SpawnPresetRegistry.getEffectivePerWorldMax(EntityType.ZOMBIE, "minecraft:the_nether",
        ServerLoadLevel.VERY_LOW));
  }

  @Test
  void invalidEntityIdFallsBackToGlobalDefault() {
    int expected = SpawnConfig.spawnLimitationMaxMobsPerWorld;
    assertEquals(expected,
      SpawnPresetRegistry.getEffectivePerWorldMax("invalid:not_real", OVERWORLD,
        ServerLoadLevel.VERY_LOW));
    assertEquals(SpawnDecision.ALLOW,
      SpawnPresetRegistry.evaluate("invalid:not_real", OVERWORLD));
  }

  @Test
  void precomputedLoadLevelLimitsMatchConfiguredFactors() {
    SpawnPresetRegistry.reload(List.of(new SpawnPreset(
      false,
      null,
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of("minecraft:zombie"), Set.of(), 10, 20, 30, 4),
      new SpawnPreset.LoadFactors(1.0, 0.8, 0.6, 0.5, 0.4, 0.2),
      TrackingMode.EXCLUDE_ENTITY,
      TrackingCategory.UNKNOWN,
      "",
      Set.of())));

    assertEquals(10,
      SpawnPresetRegistry.getEffectivePerPlayerMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.VERY_LOW));
    assertEquals(8,
      SpawnPresetRegistry.getEffectivePerPlayerMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.LOW));
    assertEquals(6,
      SpawnPresetRegistry.getEffectivePerPlayerMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.NORMAL));
    assertEquals(5,
      SpawnPresetRegistry.getEffectivePerPlayerMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.MEDIUM));
    assertEquals(4,
      SpawnPresetRegistry.getEffectivePerPlayerMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.HIGH));
    assertEquals(2,
      SpawnPresetRegistry.getEffectivePerPlayerMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.VERY_HIGH));
    assertEquals(2,
      SpawnPresetRegistry.getEffectivePerPlayerMax("minecraft:zombie", OVERWORLD,
        ServerLoadLevel.VERY_HIGH));
  }

  @Test
  void typedDimensionLookupMatchesStringLookup() {
    SpawnPreset overworldPreset = new SpawnPreset(
      false,
      null,
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(OVERWORLD), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of("minecraft:zombie"), Set.of(), 7, 21, 42, 3),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_ENTITY,
      TrackingCategory.UNKNOWN,
      "",
      Set.of());
    SpawnPresetRegistry.reload(List.of(overworldPreset));

    Identifier overworld = Identifier.tryParse(OVERWORLD);
    assertEquals(
      SpawnPresetRegistry.evaluate(EntityType.ZOMBIE, OVERWORLD),
      SpawnPresetRegistry.evaluate(EntityType.ZOMBIE, overworld));
    assertEquals(
      SpawnPresetRegistry.getEffectivePerWorldMax(EntityType.ZOMBIE, OVERWORLD,
        ServerLoadLevel.VERY_LOW),
      SpawnPresetRegistry.getEffectivePerWorldMax(EntityType.ZOMBIE, overworld,
        ServerLoadLevel.VERY_LOW));
  }
}
