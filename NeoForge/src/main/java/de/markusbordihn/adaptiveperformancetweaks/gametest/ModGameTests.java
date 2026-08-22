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
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.gametest.framework.FunctionGameTestInstance;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.TestData;
import net.minecraft.gametest.framework.TestEnvironmentDefinition;
import net.minecraft.resources.Identifier;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.event.RegisterGameTestsEvent;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

/**
 * NeoForge has no annotation based game test discovery, so every test method is registered here and
 * paired with the structure it should run in.
 */
@EventBusSubscriber
public final class ModGameTests {

  private static final DeferredRegister<Consumer<GameTestHelper>> TEST_FUNCTIONS =
    DeferredRegister.create(BuiltInRegistries.TEST_FUNCTION, Constants.MOD_ID);
  private static final List<TestEntry> TEST_ENTRIES = new ArrayList<>();
  private static final Identifier STRUCTURE = Identifier.parse("adaptive_performance_tweaks:gametest.1x1x1");
  private static final int MAX_TICKS = 100;

  static {
    register("mod_registered", SmokeTest::testModRegistered);

    register("no_throttle_under_normal_load", AiThrottleTests::testNoThrottleUnderNormalLoad);
    register("throttle_under_very_high_load", AiThrottleTests::testThrottleUnderVeryHighLoad);
    register("throttle_only_in_high_load_level", AiThrottleTests::testThrottleOnlyInHighLoadLevel);

    register("divisor_is_one_under_normal_load", ChunkGenThrottleTests::testDivisorIsOneUnderNormalLoad);
    register("divisor_increases_under_very_high_load", ChunkGenThrottleTests::testDivisorIncreasesUnderVeryHighLoad);
    register("divisor_uses_per_level_load", ChunkGenThrottleTests::testDivisorUsesPerLevelLoad);

    register("view_distance_decreases_under_very_high_load", DistanceAdaptationTests::testViewDistanceDecreasesUnderVeryHighLoad);
    register("simulation_distance_decreases_under_very_high_load", DistanceAdaptationTests::testSimulationDistanceDecreasesUnderVeryHighLoad);

    register("null_entity_not_relevant", EntityFilterTests::testNullEntityNotRelevant);
    register("regular_zombie_is_relevant", EntityFilterTests::testRegularZombieIsRelevant);
    register("named_zombie_not_relevant", EntityFilterTests::testNamedZombieNotRelevant);
    register("persistence_required_zombie_not_relevant", EntityFilterTests::testPersistenceRequiredZombieNotRelevant);
    register("passenger_zombie_not_relevant", EntityFilterTests::testPassengerZombieNotRelevant);
    register("vehicle_zombie_not_relevant", EntityFilterTests::testVehicleZombieNotRelevant);
    register("projectile_not_relevant", EntityFilterTests::testProjectileNotRelevant);
    register("item_entity_not_relevant", EntityFilterTests::testItemEntityNotRelevant);
    register("tamed_wolf_not_relevant", EntityFilterTests::testTamedWolfNotRelevant);
    register("excluded_namespace_zombie_not_relevant", EntityFilterTests::testExcludedNamespaceZombieNotRelevant);
    register("rejected_duplicate_spawn_not_tracked", EntityFilterTests::testRejectedDuplicateSpawnNotTracked);

    register("core_feature_enabled", FeatureToggleTests::testCoreFeatureEnabled);

    register("very_high_load_disables_game_rules", GameRuleAdaptationTests::testVeryHighLoadDisablesGameRules);

    register("xp_orb_clustering", ItemOptimizationTests::testXpOrbClustering);
    register("item_entity_merging", ItemOptimizationTests::testItemEntityMerging);

    register("experience_orb_accessor_mixin", MixinTests::testExperienceOrbAccessorMixin);

    register("monitoring_handles_load_events_without_exception", MonitoringTests::testMonitoringHandlesLoadEventsWithoutException);

    register("damage_unchanged_for_non_child_player", PlayerDamageTests::testDamageUnchangedForNonChildPlayer);
    register("damage_reduced_for_child_player", PlayerDamageTests::testDamageReducedForChildPlayer);
    register("damage_fully_blocked_at_hundred_percent", PlayerDamageTests::testDamageFullyBlockedAtHundredPercent);
    register("attack_damage_increased_for_child_player", PlayerDamageTests::testAttackDamageIncreasedForChildPlayer);
    register("child_mode_hurt_reduction_through_hook", helper -> PlayerDamageTests.testChildModeHurtReductionThroughHook(helper, "NeoForge event pipeline"));
    register("starter_protection_hurt_reduction_through_hook", helper -> PlayerDamageTests.testStarterProtectionHurtReductionThroughHook(helper, "NeoForge event pipeline"));

    register("protection_applied_on_login", PlayerLoginProtectionTests::testProtectionAppliedOnLogin);
    register("protection_skipped_when_disabled", PlayerLoginProtectionTests::testProtectionSkippedWhenDisabled);
    register("validation_detects_movement", PlayerLoginProtectionTests::testValidationDetectsMovement);

    register("bat_presets_loaded", SpawnPresetConfigTests::testBatPresetsLoaded);
    register("zombie_presets_loaded", SpawnPresetConfigTests::testZombiePresetsLoaded);
    register("cow_presets_loaded", SpawnPresetConfigTests::testCowPresetsLoaded);

    register("spawner_not_throttled_under_normal_load", SpawnThrottleTests::testSpawnerNotThrottledUnderNormalLoad);
    register("spawner_throttled_under_very_high_load", SpawnThrottleTests::testSpawnerThrottledUnderVeryHighLoad);
    register("spawner_throttle_only_in_high_load_level", SpawnThrottleTests::testSpawnerThrottleOnlyInHighLoadLevel);
    register("entity_world_limit_blocks_spawn_at_10", SpawnThrottleTests::testEntityWorldLimitBlocksSpawnAt10);
    register("entity_server_limit_blocks_spawn_at_10", SpawnThrottleTests::testEntityServerLimitBlocksSpawnAt10);
    register("entity_chunk_limit_blocks_spawn_at_5", SpawnThrottleTests::testEntityChunkLimitBlocksSpawnAt5);
    register("entity_player_limit_virtual_zone_at_5", SpawnThrottleTests::testEntityPlayerLimitVirtualZoneAt5);
    register("structure_spawn_gets_world_bonus", SpawnThrottleTests::testStructureSpawnGetsWorldBonus);
    register("structure_spawn_bonus_disabled_above_max_load", SpawnThrottleTests::testStructureSpawnBonusDisabledAboveMaxLoad);
    register("natural_spawn_not_evaluated_on_finalize_spawn", SpawnThrottleTests::testNaturalSpawnNotEvaluatedOnFinalizeSpawn);
  }

  private ModGameTests() {
  }

  public static void register(IEventBus modEventBus) {
    // NeoForge only fires RegisterGameTestsEvent outside production, so the test functions would
    // stay unused there anyway.
    if (FMLEnvironment.isProduction()) {
      return;
    }

    TEST_FUNCTIONS.register(modEventBus);
  }

  private static void register(String name, Consumer<GameTestHelper> testFunction) {
    TEST_ENTRIES.add(new TestEntry(TEST_FUNCTIONS.register(name, () -> testFunction)));
  }

  @SubscribeEvent
  public static void registerGameTests(RegisterGameTestsEvent event) {
    Holder<TestEnvironmentDefinition> environment =
      event.registerEnvironment(
        Identifier.fromNamespaceAndPath(Constants.MOD_ID, "default"),
        new TestEnvironmentDefinition.AllOf(List.of()));
    for (TestEntry testEntry : TEST_ENTRIES) {
      event.registerTest(
        testEntry.testFunction().getId(),
        new FunctionGameTestInstance(
          testEntry.testFunction().getKey(),
          new TestData<>(environment, STRUCTURE, MAX_TICKS, 0, true)));
    }
  }

  private record TestEntry(
    DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> testFunction) {
  }
}
