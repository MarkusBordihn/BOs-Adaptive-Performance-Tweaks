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

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import java.util.function.Consumer;
import net.minecraft.core.registries.Registries;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraftforge.eventbus.api.bus.BusGroup;
import net.minecraftforge.fml.loading.FMLLoader;
import net.minecraftforge.registries.DeferredRegister;

/**
 * Forge ships the {@code @GameTest} annotation but never scans for it, so every test method is
 * registered here and paired with a {@code data/<mod id>/test_instance} entry.
 */
public final class ModGameTests {

  private static final DeferredRegister<Consumer<GameTestHelper>> TEST_FUNCTIONS =
    DeferredRegister.create(Registries.TEST_FUNCTION, Constants.MOD_ID);

  static {
    TEST_FUNCTIONS.register("mod_registered", () -> SmokeTest::testModRegistered);

    TEST_FUNCTIONS.register("no_throttle_under_normal_load",
      () -> AiThrottleTests::testNoThrottleUnderNormalLoad);
    TEST_FUNCTIONS.register("throttle_under_very_high_load",
      () -> AiThrottleTests::testThrottleUnderVeryHighLoad);
    TEST_FUNCTIONS.register("throttle_only_in_high_load_level",
      () -> AiThrottleTests::testThrottleOnlyInHighLoadLevel);

    TEST_FUNCTIONS.register("divisor_is_one_under_normal_load",
      () -> ChunkGenThrottleTests::testDivisorIsOneUnderNormalLoad);
    TEST_FUNCTIONS.register("divisor_increases_under_very_high_load",
      () -> ChunkGenThrottleTests::testDivisorIncreasesUnderVeryHighLoad);
    TEST_FUNCTIONS.register("divisor_uses_per_level_load",
      () -> ChunkGenThrottleTests::testDivisorUsesPerLevelLoad);

    TEST_FUNCTIONS.register("view_distance_decreases_under_very_high_load",
      () -> DistanceAdaptationTests::testViewDistanceDecreasesUnderVeryHighLoad);
    TEST_FUNCTIONS.register("simulation_distance_decreases_under_very_high_load",
      () -> DistanceAdaptationTests::testSimulationDistanceDecreasesUnderVeryHighLoad);

    TEST_FUNCTIONS.register("null_entity_not_relevant",
      () -> EntityFilterTests::testNullEntityNotRelevant);
    TEST_FUNCTIONS.register("regular_zombie_is_relevant",
      () -> EntityFilterTests::testRegularZombieIsRelevant);
    TEST_FUNCTIONS.register("named_zombie_not_relevant",
      () -> EntityFilterTests::testNamedZombieNotRelevant);
    TEST_FUNCTIONS.register("persistence_required_zombie_not_relevant",
      () -> EntityFilterTests::testPersistenceRequiredZombieNotRelevant);
    TEST_FUNCTIONS.register("passenger_zombie_not_relevant",
      () -> EntityFilterTests::testPassengerZombieNotRelevant);
    TEST_FUNCTIONS.register("vehicle_zombie_not_relevant",
      () -> EntityFilterTests::testVehicleZombieNotRelevant);
    TEST_FUNCTIONS.register("projectile_not_relevant",
      () -> EntityFilterTests::testProjectileNotRelevant);
    TEST_FUNCTIONS.register("item_entity_not_relevant",
      () -> EntityFilterTests::testItemEntityNotRelevant);
    TEST_FUNCTIONS.register("tamed_wolf_not_relevant",
      () -> EntityFilterTests::testTamedWolfNotRelevant);
    TEST_FUNCTIONS.register("excluded_namespace_zombie_not_relevant",
      () -> EntityFilterTests::testExcludedNamespaceZombieNotRelevant);
    TEST_FUNCTIONS.register("rejected_duplicate_spawn_not_tracked",
      () -> EntityFilterTests::testRejectedDuplicateSpawnNotTracked);

    TEST_FUNCTIONS.register("core_feature_enabled",
      () -> FeatureToggleTests::testCoreFeatureEnabled);

    TEST_FUNCTIONS.register("very_high_load_disables_game_rules",
      () -> GameRuleAdaptationTests::testVeryHighLoadDisablesGameRules);

    TEST_FUNCTIONS.register("xp_orb_clustering", () -> ItemOptimizationTests::testXpOrbClustering);
    TEST_FUNCTIONS.register("item_entity_merging",
      () -> ItemOptimizationTests::testItemEntityMerging);

    TEST_FUNCTIONS.register("experience_orb_accessor_mixin",
      () -> MixinTests::testExperienceOrbAccessorMixin);
    TEST_FUNCTIONS.register("natural_spawner_throttle_mixin",
      () -> MixinTests::testNaturalSpawnerThrottleMixin);

    TEST_FUNCTIONS.register("monitoring_handles_load_events_without_exception",
      () -> MonitoringTests::testMonitoringHandlesLoadEventsWithoutException);

    TEST_FUNCTIONS.register("damage_unchanged_for_non_child_player",
      () -> PlayerDamageTests::testDamageUnchangedForNonChildPlayer);
    TEST_FUNCTIONS.register("damage_reduced_for_child_player",
      () -> PlayerDamageTests::testDamageReducedForChildPlayer);
    TEST_FUNCTIONS.register("damage_fully_blocked_at_hundred_percent",
      () -> PlayerDamageTests::testDamageFullyBlockedAtHundredPercent);
    TEST_FUNCTIONS.register("attack_damage_increased_for_child_player",
      () -> PlayerDamageTests::testAttackDamageIncreasedForChildPlayer);
    TEST_FUNCTIONS.register("child_mode_hurt_reduction_through_hook",
      () -> helper -> PlayerDamageTests.testChildModeHurtReductionThroughHook(helper,
        "Forge event pipeline"));
    TEST_FUNCTIONS.register("child_mode_full_block_through_hook",
      () -> helper -> PlayerDamageTests.testChildModeFullBlockThroughHook(helper,
        "Forge event pipeline"));
    TEST_FUNCTIONS.register("starter_protection_hurt_reduction_through_hook",
      () -> helper -> PlayerDamageTests.testStarterProtectionHurtReductionThroughHook(helper,
        "Forge event pipeline"));

    TEST_FUNCTIONS.register("protection_applied_on_login",
      () -> PlayerLoginProtectionTests::testProtectionAppliedOnLogin);
    TEST_FUNCTIONS.register("protection_skipped_when_disabled",
      () -> PlayerLoginProtectionTests::testProtectionSkippedWhenDisabled);
    TEST_FUNCTIONS.register("validation_detects_movement",
      () -> PlayerLoginProtectionTests::testValidationDetectsMovement);

    TEST_FUNCTIONS.register("bat_presets_loaded",
      () -> SpawnPresetConfigTests::testBatPresetsLoaded);
    TEST_FUNCTIONS.register("zombie_presets_loaded",
      () -> SpawnPresetConfigTests::testZombiePresetsLoaded);
    TEST_FUNCTIONS.register("cow_presets_loaded",
      () -> SpawnPresetConfigTests::testCowPresetsLoaded);

    TEST_FUNCTIONS.register("spawner_not_throttled_under_normal_load",
      () -> SpawnThrottleTests::testSpawnerNotThrottledUnderNormalLoad);
    TEST_FUNCTIONS.register("spawner_throttled_under_very_high_load",
      () -> SpawnThrottleTests::testSpawnerThrottledUnderVeryHighLoad);
    TEST_FUNCTIONS.register("spawner_throttle_only_in_high_load_level",
      () -> SpawnThrottleTests::testSpawnerThrottleOnlyInHighLoadLevel);
    TEST_FUNCTIONS.register("entity_world_limit_blocks_spawn_at_10",
      () -> SpawnThrottleTests::testEntityWorldLimitBlocksSpawnAt10);
    TEST_FUNCTIONS.register("entity_server_limit_blocks_spawn_at_10",
      () -> SpawnThrottleTests::testEntityServerLimitBlocksSpawnAt10);
    TEST_FUNCTIONS.register("entity_chunk_limit_blocks_spawn_at_5",
      () -> SpawnThrottleTests::testEntityChunkLimitBlocksSpawnAt5);
    TEST_FUNCTIONS.register("entity_player_limit_virtual_zone_at_5",
      () -> SpawnThrottleTests::testEntityPlayerLimitVirtualZoneAt5);
    TEST_FUNCTIONS.register("structure_spawn_gets_world_bonus",
      () -> SpawnThrottleTests::testStructureSpawnGetsWorldBonus);
    TEST_FUNCTIONS.register("structure_spawn_bonus_disabled_above_max_load",
      () -> SpawnThrottleTests::testStructureSpawnBonusDisabledAboveMaxLoad);
    TEST_FUNCTIONS.register("natural_spawn_not_evaluated_on_finalize_spawn",
      () -> SpawnThrottleTests::testNaturalSpawnNotEvaluatedOnFinalizeSpawn);
  }

  private ModGameTests() {
  }

  public static void register(BusGroup modBusGroup) {
    // Test instances are shipped as data pack entries, so registering the matching test functions
    // outside a development environment would offer the game tests through the /test command of a
    // live world.
    if (FMLLoader.isProduction()) {
      return;
    }

    TEST_FUNCTIONS.register(modBusGroup);
  }
}
