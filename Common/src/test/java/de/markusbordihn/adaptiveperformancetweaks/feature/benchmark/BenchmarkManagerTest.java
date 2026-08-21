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

package de.markusbordihn.adaptiveperformancetweaks.feature.benchmark;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.tree.LiteralCommandNode;
import de.markusbordihn.adaptiveperformancetweaks.core.commands.BenchmarkCommand;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.MsptBucket;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenario;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioContext;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioId;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioResult;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.EntityScenario;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.ExplorationScenario;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.GeneralScenario;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.ItemScenario;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.XpScenario;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.ViewDistanceConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.gamerules.GameRuleManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemsConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.time.Instant;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import net.minecraft.SharedConstants;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.players.PlayerList;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.GameRules;
import net.minecraft.world.phys.Vec3;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockMakers;

class BenchmarkManagerTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

  private static Object invokePrivateMethod(
    String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
    Method method = BenchmarkManager.class.getDeclaredMethod(methodName, parameterTypes);
    method.setAccessible(true);
    return method.invoke(null, args);
  }

  private static void invokePrivateMethod(String methodName) throws Exception {
    invokePrivateMethod(methodName, new Class<?>[0]);
  }

  private static void writeStaticField(Class<?> owner, String fieldName, Object value)
    throws Exception {
    Field field = owner.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static Object readStaticField(Class<?> owner, String fieldName) throws Exception {
    Field field = owner.getDeclaredField(fieldName);
    field.setAccessible(true);
    return field.get(null);
  }

  private static String buildBenchmarkFilename(String modVersion) throws Exception {
    Method method = BenchmarkResultWriter.class.getDeclaredMethod("buildBenchmarkFilename",
      String.class, String.class, String.class, String.class);
    method.setAccessible(true);
    return (String) method.invoke(null,
      "2026-05-27_12-00-00", "1.20.1", "forge", modVersion);
  }

  private static PerformanceStats.Snapshot emptySnapshot() {
    return new PerformanceStats.Snapshot(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      Map.of(TrackingCategory.UNKNOWN, 0L),
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  }

  private static BenchmarkScenarioResult.PhaseResult phaseResult(
    long durationMs, double avgTick, double p95Tick, double avgCpu, double maxCpu,
    PerformanceStats.Snapshot snapshot) {
    EnumMap<ServerLoadLevel, Integer> loadDistribution = new EnumMap<>(ServerLoadLevel.class);
    loadDistribution.put(ServerLoadLevel.VERY_LOW, 1);
    EnumMap<MsptBucket, Integer> msptDistribution = new EnumMap<>(MsptBucket.class);
    msptDistribution.put(MsptBucket.UNDER_5_MS, avgTick <= 5.0 ? 1 : 0);
    msptDistribution.put(MsptBucket.FROM_5_TO_10_MS, avgTick > 5.0 && avgTick <= 10.0 ? 1 : 0);
    msptDistribution.put(MsptBucket.FROM_10_TO_VERY_LOW_MS,
      avgTick > 10.0 && avgTick <= 20.0 ? 1 : 0);
    EnumMap<FineMsptBucket, Integer> fineMsptDistribution = new EnumMap<>(FineMsptBucket.class);
    fineMsptDistribution.put(FineMsptBucket.UNDER_3_MS, avgTick < 3.0 ? 1 : 0);
    fineMsptDistribution.put(FineMsptBucket.FROM_3_TO_5_MS,
      avgTick >= 3.0 && avgTick < 5.0 ? 1 : 0);
    fineMsptDistribution.put(FineMsptBucket.FROM_5_TO_10_MS,
      avgTick >= 5.0 && avgTick < 10.0 ? 1 : 0);
    fineMsptDistribution.put(FineMsptBucket.FROM_10_MS_UP, avgTick >= 10.0 ? 1 : 0);
    return new BenchmarkScenarioResult.PhaseResult(
      durationMs,
      avgTick,
      Math.max(0.0, avgTick - 1.5),
      p95Tick,
      p95Tick + 1.2,
      loadDistribution,
      msptDistribution,
      fineMsptDistribution,
      0L,
      0,
      avgCpu,
      maxCpu,
      distanceControlState(12, 12, 0, false, 0, 12, 12, 0, 0),
      distanceControlState(12, 12, 0, false, 0, 12, 12, 0, 0),
      snapshot,
      BenchmarkScenarioResult.ScenarioValidation.none());
  }

  private static BenchmarkScenarioResult.DistanceControlState distanceControlState(
    int viewDistance, int viewBaselineDistance, int viewWarmupReduction, boolean viewWarmupActive,
    int viewActiveExplorers, int simulationDistance, int simulationBaselineDistance,
    int simulationMovementReduction, int simulationActiveExplorers) {
    return new BenchmarkScenarioResult.DistanceControlState(
      viewDistance,
      viewBaselineDistance,
      viewWarmupReduction,
      viewWarmupActive,
      viewActiveExplorers,
      simulationDistance,
      simulationBaselineDistance,
      simulationMovementReduction,
      simulationActiveExplorers);
  }

  private static BenchmarkScenarioResult.PhaseResult explorationPhaseResult(
    long durationMs, double avgTick, double p95Tick, double avgCpu, double maxCpu,
    PerformanceStats.Snapshot snapshot, int stepCount, int uniqueChunkCount,
    long movementAdjustments, long movementSamples, long maxReduction, long... chunkKeys) {
    return new BenchmarkScenarioResult.PhaseResult(
      phaseResult(durationMs, avgTick, p95Tick, avgCpu, maxCpu, snapshot).measurementDurationMs(),
      avgTick,
      Math.max(0.0, avgTick - 1.5),
      p95Tick,
      p95Tick + 1.2,
      new EnumMap<>(Map.of(ServerLoadLevel.VERY_LOW, 1)),
      new EnumMap<>(Map.of(avgTick <= 5.0 ? MsptBucket.UNDER_5_MS
        : avgTick <= 10.0 ? MsptBucket.FROM_5_TO_10_MS
          : MsptBucket.FROM_10_TO_VERY_LOW_MS, 1)),
      new EnumMap<>(Map.of(avgTick < 3.0 ? FineMsptBucket.UNDER_3_MS
        : avgTick < 5.0 ? FineMsptBucket.FROM_3_TO_5_MS
          : avgTick < 10.0 ? FineMsptBucket.FROM_5_TO_10_MS
            : FineMsptBucket.FROM_10_MS_UP, 1)),
      0L,
      0,
      avgCpu,
      maxCpu,
      distanceControlState(12, 12, 0, false, 0, 12, 12, 0, 0),
      distanceControlState(12, 12, (int) maxReduction > 0 ? 1 : 0, maxReduction > 0, 1, 12, 12,
        (int) maxReduction, movementSamples > 0 ? 1 : 0),
      snapshot,
      new BenchmarkScenarioResult.ScenarioValidation(
        stepCount,
        uniqueChunkCount,
        movementAdjustments,
        movementSamples,
        maxReduction,
        movementAdjustments > 0 || movementSamples > 0 || maxReduction > 0,
        Arrays.stream(chunkKeys).boxed().collect(Collectors.toSet())));
  }

  @Test
  void blockWarmupDurationIs30Seconds() throws Exception {
    Field field =
      BenchmarkManager.class.getDeclaredField("BLOCK_WARMUP_DURATION_MS");
    field.setAccessible(true);
    assertEquals(30_000L, field.get(null));
  }

  @Test
  void minLoadLevelsForceToVeryLowDuringActiveBlockTransition() throws Exception {
    try {
      BenchmarkFeatureState.saveMinLoadLevels();
      invokePrivateMethod("completeBlockTransition", new Class<?>[]{long.class},
        System.currentTimeMillis());
      assertEquals(ServerLoadLevel.VERY_LOW, ItemsConfig.minOptimizationLoadLevel);
      assertEquals(ServerLoadLevel.VERY_LOW, ViewDistanceConfig.minOptimizationLoadLevel);
    } finally {
      BenchmarkFeatureState.restoreMinLoadLevels();
      invokePrivateMethod("clearSessionState");
    }
  }

  @Test
  @DisplayName("The active block reports VERY_HIGH load so every feature applies its full reduction")
  void activeBlockTransitionForcesVeryHighLoadLevel() throws Exception {
    try {
      BenchmarkFeatureState.saveMinLoadLevels();
      invokePrivateMethod("completeBlockTransition", new Class<?>[]{long.class},
        System.currentTimeMillis());
      assertEquals(ServerLoadLevel.VERY_HIGH, ServerLoad.getCurrentServerLoad());
    } finally {
      BenchmarkFeatureState.restoreMinLoadLevels();
      invokePrivateMethod("clearSessionState");
    }
  }

  @Test
  @DisplayName("Restoring the min load levels also drops the forced load level")
  void restoreMinLoadLevelsClearsLoadLevelOverride() throws Exception {
    BenchmarkFeatureState.saveMinLoadLevels();
    try {
      invokePrivateMethod("completeBlockTransition", new Class<?>[]{long.class},
        System.currentTimeMillis());
    } finally {
      BenchmarkFeatureState.restoreMinLoadLevels();
      invokePrivateMethod("clearSessionState");
    }

    assertNotEquals(ServerLoadLevel.VERY_HIGH, ServerLoad.getCurrentServerLoad());
  }

  @Test
  @DisplayName("Restoring without a preceding save keeps the min load levels intact")
  void restoreMinLoadLevelsWithoutSaveKeepsConfiguredLevels() {
    ServerLoadLevel previousItemsMinLoad = ItemsConfig.minOptimizationLoadLevel;
    ServerLoadLevel previousViewDistMinLoad = ViewDistanceConfig.minOptimizationLoadLevel;
    try {
      BenchmarkFeatureState.clearAll();
      BenchmarkFeatureState.restoreMinLoadLevels();

      assertEquals(previousItemsMinLoad, ItemsConfig.minOptimizationLoadLevel);
      assertEquals(previousViewDistMinLoad, ViewDistanceConfig.minOptimizationLoadLevel);
    } finally {
      ItemsConfig.minOptimizationLoadLevel = previousItemsMinLoad;
      ViewDistanceConfig.minOptimizationLoadLevel = previousViewDistMinLoad;
    }
  }

  @Test
  void restoreFeaturesKeepsPreviouslyDisabledFeatureDisabled() throws Exception {
    boolean previousItemsState = FeatureToggle.ITEMS.isEnabled();
    boolean previousSpawnState = FeatureToggle.SPAWN.isEnabled();
    try {
      FeatureToggle.ITEMS.setEnabled(false);
      FeatureToggle.SPAWN.setEnabled(true);

      BenchmarkFeatureState.saveFeatureState();
      BenchmarkFeatureState.disableAllFeatures();
      BenchmarkFeatureState.restoreFeatures();

      assertFalse(FeatureToggle.ITEMS.isEnabled());
      assertTrue(FeatureToggle.SPAWN.isEnabled());
    } finally {
      invokePrivateMethod("clearSessionState");
      FeatureToggle.ITEMS.setEnabled(previousItemsState);
      FeatureToggle.SPAWN.setEnabled(previousSpawnState);
    }
  }

  @Test
  void restoreFeaturesDisablesConflictGatedDistanceFeaturesDuringBenchmark() throws Exception {
    boolean previousViewDistanceState = FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled();
    boolean previousItemsState = FeatureToggle.ITEMS.isEnabled();
    try {
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(true);
      FeatureToggle.ITEMS.setEnabled(true);

      BenchmarkFeatureState.saveFeatureState();
      @SuppressWarnings("unchecked")
      Map<FeatureToggle, ModConflictDetector.FeatureDecision> savedDecisions =
        (Map<FeatureToggle, ModConflictDetector.FeatureDecision>) readStaticField(
          BenchmarkFeatureState.class, "savedFeatureDecision");
      savedDecisions.put(
        FeatureToggle.ADAPTIVE_VIEW_DISTANCE,
        new ModConflictDetector.FeatureDecision(
          true, ModConflictDetector.FeatureActivation.MANUAL_ENABLED, "dynview"));

      BenchmarkFeatureState.disableAllFeatures();
      BenchmarkFeatureState.restoreFeatures();

      assertFalse(FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled());
      assertTrue(FeatureToggle.ITEMS.isEnabled());
    } finally {
      invokePrivateMethod("clearSessionState");
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(previousViewDistanceState);
      FeatureToggle.ITEMS.setEnabled(previousItemsState);
    }
  }

  @Test
  void benchmarkDisableAllFeaturesRestoresServerDefaultsForRuntimeValues() throws Exception {
    boolean previousGameRulesState = FeatureToggle.GAMERULES.isEnabled();
    boolean previousSimDistState = FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled();
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    PlayerList playerList = mock(PlayerList.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    GameRules rules = new GameRules();
    rules.getRule(GameRules.RULE_RANDOMTICKING).set(6, null);
    when(server.getGameRules()).thenReturn(rules);
    when(server.getPlayerList()).thenReturn(playerList);
    when(playerList.getSimulationDistance()).thenReturn(10);

    try {
      writeStaticField(ServerManager.class, "minecraftServer", server);
      FeatureToggle.GAMERULES.setEnabled(false);
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.setEnabled(false);
      FeatureToggle.GAMERULES.setEnabled(true);
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.setEnabled(true);
      writeStaticField(GameRuleManager.class, "configuredRandomTickSpeedMax", 6);
      writeStaticField(SimulationDistanceManager.class, "configuredDistanceMax", 10);
      rules.getRule(GameRules.RULE_RANDOMTICKING).set(1, null);
      writeStaticField(SimulationDistanceManager.class, "currentDistance", 4);

      BenchmarkFeatureState.saveFeatureState();
      BenchmarkFeatureState.disableAllFeatures();

      assertEquals(6, rules.getInt(GameRules.RULE_RANDOMTICKING));
      assertEquals(10, readStaticField(SimulationDistanceManager.class, "currentDistance"));
      verify(playerList).setSimulationDistance(10);
    } finally {
      invokePrivateMethod("clearSessionState");
      FeatureToggle.GAMERULES.setEnabled(previousGameRulesState);
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.setEnabled(previousSimDistState);
      writeStaticField(ServerManager.class, "minecraftServer", null);
    }
  }

  @Test
  void benchmarkFilenameOmitsUnknownVersionSuffix() throws Exception {
    String filename = buildBenchmarkFilename(null);

    assertTrue(filename.endsWith("_forge.md"));
    assertFalse(filename.contains("_vunknown"));
  }

  @Test
  void benchmarkFilenameIncludesKnownVersionSuffix() throws Exception {
    String filename = buildBenchmarkFilename("1.2.3");

    assertTrue(filename.endsWith("_forge_v1.2.3.md"));
  }

  @Test
  void savedMarkdownReportIncludesServerVersionHeader() {
    BenchmarkScenarioResult scenarioResult = new BenchmarkScenarioResult(
      BenchmarkScenarioId.GENERAL,
      phaseResult(120_000L, 8.1, 11.6, 27.8, 43.8, emptySnapshot()),
      phaseResult(120_000L, 3.7, 5.7, 19.2, 31.8, emptySnapshot()));
    EnumMap<BenchmarkScenarioId, Long> durations = new EnumMap<>(BenchmarkScenarioId.class);
    durations.put(BenchmarkScenarioId.GENERAL, 120_000L);

    BenchmarkCompareResult result = new BenchmarkCompareResult(
      "General",
      false,
      120_000L,
      30_000L,
      5_000L,
      3_000L,
      durations,
      List.of(scenarioResult),
      0, 9, 4, 0,
      false, 0, 0, 0,
      Instant.parse("2026-05-27T10:18:54.893486400Z"));

    List<String> lines =
      BenchmarkResultWriter.buildMarkdownReport(result, "1.20.1", "forge", "12.0.0-alpha");

    assertTrue(lines.stream()
      .anyMatch(
        line -> line.equals("- Server version: Minecraft 1.20.1 / forge / APTweaks 12.0.0-alpha")));
  }

  @Test
  void suiteDurationDistributionUsesWeightedSplit() throws Exception {
    @SuppressWarnings("unchecked")
    Map<BenchmarkScenarioId, Long> durations =
      (Map<BenchmarkScenarioId, Long>) invokePrivateMethod("buildSuiteScenarioDurationsMillis",
        new Class<?>[]{long.class}, 240L);

    assertEquals(79_000L, durations.get(BenchmarkScenarioId.GENERAL));
    assertEquals(53_000L, durations.get(BenchmarkScenarioId.EXPLORATION));
    assertEquals(27_000L, durations.get(BenchmarkScenarioId.ITEMS));
    assertEquals(27_000L, durations.get(BenchmarkScenarioId.XP));
    assertEquals(27_000L, durations.get(BenchmarkScenarioId.ENTITIES));
    assertEquals(27_000L, durations.get(BenchmarkScenarioId.RECOVERY));
  }

  @Test
  void suiteDurationValidationRejectsTooShortSuites() throws Exception {
    String validation = (String) invokePrivateMethod("validateSuiteDurationSeconds",
      new Class<?>[]{long.class}, 134L);

    assertNotNull(validation);
    assertTrue(validation.contains("at least"));
  }

  @Test
  void defaultScenarioOrderStartsWithGeneralAndEndsWithRecovery() throws Exception {
    @SuppressWarnings("unchecked")
    List<BenchmarkScenario> scenarios =
      (List<BenchmarkScenario>) invokePrivateMethod("createScenarioSuite", new Class<?>[0]);

    assertEquals(BenchmarkScenarioId.GENERAL, scenarios.get(0).id());
    assertEquals(BenchmarkScenarioId.EXPLORATION, scenarios.get(1).id());
    assertEquals(BenchmarkScenarioId.RECOVERY, scenarios.get(scenarios.size() - 1).id());
  }

  @Test
  void benchmarkCommandRegistersExplorationScenario() {
    @SuppressWarnings("unchecked")
    var benchmarkBuilder =
      (LiteralArgumentBuilder<CommandSourceStack>) BenchmarkCommand.register();
    CommandDispatcher<CommandSourceStack> dispatcher = new CommandDispatcher<>();
    LiteralCommandNode<CommandSourceStack> benchmarkNode = dispatcher.register(benchmarkBuilder);

    assertNotNull(benchmarkNode.getChild("start"));
    assertNotNull(benchmarkNode.getChild("start").getChild("scenario"));
    assertNotNull(benchmarkNode.getChild("start").getChild("scenario").getChild("exploration"));
  }

  @Test
  void visualSpawnScenariosRequestPlayerFacing() {
    BenchmarkScenario itemScenario = new ItemScenario();
    BenchmarkScenario xpScenario = new XpScenario();
    BenchmarkScenario entityScenario = new EntityScenario();
    BenchmarkScenario generalScenario = new GeneralScenario();

    assertTrue(itemScenario.shouldFacePlayerToFocus());
    assertTrue(xpScenario.shouldFacePlayerToFocus());
    assertTrue(entityScenario.shouldFacePlayerToFocus());
    assertFalse(generalScenario.shouldFacePlayerToFocus());
    assertEquals(Vec3.ZERO, itemScenario.playerFocusOffset());
    assertEquals(Vec3.ZERO, xpScenario.playerFocusOffset());
    assertEquals(new Vec3(8.0d, 0.0d, 8.0d), entityScenario.playerFocusOffset());
  }

  @Test
  void generalRoutesUseLocalDeterministicMovementByDefault() throws Exception {
    BenchmarkScenario scenario = new GeneralScenario();
    Vec3 origin = new Vec3(0.0d, 64.0d, 0.0d);
    writeStaticField(BenchmarkManager.class, "autoMoveRequested", false);

    @SuppressWarnings("unchecked")
    List<Vec3> baselineRoute = (List<Vec3>) invokePrivateMethod(
      "computeWaypoints",
      new Class<?>[]{BenchmarkScenario.class, Vec3.class, long.class, boolean.class, Set.class},
      scenario,
      origin,
      79_000L,
      false,
      new HashSet<Long>());
    @SuppressWarnings("unchecked")
    List<Vec3> activeRoute = (List<Vec3>) invokePrivateMethod(
      "computeWaypoints",
      new Class<?>[]{BenchmarkScenario.class, Vec3.class, long.class, boolean.class, Set.class},
      scenario,
      origin,
      79_000L,
      true,
      new HashSet<Long>());

    assertEquals(7, baselineRoute.size());
    assertEquals(7, activeRoute.size());
    assertEquals(-3, ((int) Math.floor(baselineRoute.get(0).x)) >> 4);
    assertEquals(3, ((int) Math.floor(activeRoute.get(0).x)) >> 4);
    assertEquals(-1, (((int) Math.floor(baselineRoute.get(1).z)) >> 4)
      - (((int) Math.floor(baselineRoute.get(0).z)) >> 4));
    assertEquals(1, (((int) Math.floor(activeRoute.get(1).z)) >> 4)
      - (((int) Math.floor(activeRoute.get(0).z)) >> 4));
    assertTrue(Math.abs((((int) Math.floor(baselineRoute.get(2).x)) >> 4)
      - (((int) Math.floor(baselineRoute.get(1).x)) >> 4)) <= 1);
  }

  @Test
  void explorationRoutesAreDeterministicContiguousAndNonOverlapping() throws Exception {
    BenchmarkScenario scenario = new ExplorationScenario();
    Vec3 origin = new Vec3(0.0d, 64.0d, 0.0d);
    Set<Long> reservedChunkKeys = new HashSet<>();

    @SuppressWarnings("unchecked")
    List<Vec3> baselineRoute = (List<Vec3>) invokePrivateMethod(
      "computeWaypoints",
      new Class<?>[]{BenchmarkScenario.class, Vec3.class, long.class, boolean.class, Set.class},
      scenario,
      origin,
      53_000L,
      false,
      reservedChunkKeys);
    @SuppressWarnings("unchecked")
    List<Vec3> activeRoute = (List<Vec3>) invokePrivateMethod(
      "computeWaypoints",
      new Class<?>[]{BenchmarkScenario.class, Vec3.class, long.class, boolean.class, Set.class},
      scenario,
      origin,
      53_000L,
      true,
      reservedChunkKeys);

    assertEquals(53, baselineRoute.size());
    assertEquals(53, activeRoute.size());
    assertEquals(-8, ((int) Math.floor(baselineRoute.get(0).x)) >> 4);
    assertEquals(8, ((int) Math.floor(activeRoute.get(0).x)) >> 4);

    for (int index = 1; index < baselineRoute.size(); index++) {
      int previousChunkX = ((int) Math.floor(baselineRoute.get(index - 1).x)) >> 4;
      int currentChunkX = ((int) Math.floor(baselineRoute.get(index).x)) >> 4;
      assertEquals(1, Math.abs(currentChunkX - previousChunkX));
    }

    for (Vec3 baselineTarget : baselineRoute) {
      long baselineChunkKey = ChunkPos.asLong(
        ((int) Math.floor(baselineTarget.x)) >> 4,
        ((int) Math.floor(baselineTarget.z)) >> 4);
      for (Vec3 activeTarget : activeRoute) {
        long activeChunkKey = ChunkPos.asLong(
          ((int) Math.floor(activeTarget.x)) >> 4,
          ((int) Math.floor(activeTarget.z)) >> 4);
        assertFalse(baselineChunkKey == activeChunkKey);
      }
    }
  }

  @Test
  void formattedReportIncludesScenarioSummaryAndAssessment() {
    PerformanceStats.Snapshot activeSnapshot = new PerformanceStats.Snapshot(
      4, 0, 1, 0, 2, 1, 8, 2, 12, 0, 0, 0, 0, 0, 0, 0, 4,
      Map.of(TrackingCategory.UNKNOWN, 2L),
      7, 1, 3, 0, 0, 1, 1, 2, 1, 0);
    BenchmarkScenarioResult scenarioResult = new BenchmarkScenarioResult(
      BenchmarkScenarioId.GENERAL,
      phaseResult(120_000L, 6.9, 11.7, 21.6, 36.5, emptySnapshot()),
      phaseResult(120_000L, 4.0, 6.0, 20.4, 38.7, activeSnapshot));
    EnumMap<BenchmarkScenarioId, Long> durations = new EnumMap<>(BenchmarkScenarioId.class);
    durations.put(BenchmarkScenarioId.GENERAL, 120_000L);
    durations.put(BenchmarkScenarioId.ITEMS, 30_000L);
    durations.put(BenchmarkScenarioId.XP, 30_000L);
    durations.put(BenchmarkScenarioId.ENTITIES, 30_000L);
    durations.put(BenchmarkScenarioId.RECOVERY, 30_000L);

    BenchmarkCompareResult result = new BenchmarkCompareResult(
      "Full Suite",
      true,
      240_000L,
      30_000L,
      5_000L,
      3_000L,
      durations,
      List.of(scenarioResult),
      0, 9, 4, 0,
      true, 60, 60, 0,
      Instant.parse("2026-05-26T23:18:31.785274300Z"));

    List<String> lines = result.format().stream().map(Component::getString).toList();

    assertTrue(lines.stream().anyMatch(line -> line.contains("Scenario durations")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("General")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Assessment:")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Clear improvement")));
  }

  @Test
  void markdownReportIncludesSummaryBucketsAndConclusion() {
    BenchmarkScenarioResult scenarioResult = new BenchmarkScenarioResult(
      BenchmarkScenarioId.GENERAL,
      phaseResult(120_000L, 8.1, 11.6, 27.8, 43.8, emptySnapshot()),
      phaseResult(120_000L, 3.7, 5.7, 19.2, 31.8, emptySnapshot()));
    EnumMap<BenchmarkScenarioId, Long> durations = new EnumMap<>(BenchmarkScenarioId.class);
    durations.put(BenchmarkScenarioId.GENERAL, 120_000L);

    BenchmarkCompareResult result = new BenchmarkCompareResult(
      "General",
      false,
      120_000L,
      30_000L,
      5_000L,
      3_000L,
      durations,
      List.of(scenarioResult),
      0, 9, 4, 0,
      false, 0, 0, 0,
      Instant.parse("2026-05-27T10:18:54.893486400Z"));

    List<String> lines = result.formatMarkdown();

    assertTrue(lines.stream().anyMatch(line -> line.startsWith("# ")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Bucket shift:")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Conclusion:")));
    assertTrue(lines.stream()
      .anyMatch(line -> line.startsWith("| Scenario | Duration | Baseline | Active |")));
    assertTrue(
      lines.stream().anyMatch(line -> line.startsWith("| Metric | Baseline | Active | Delta |")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("MSPT distribution:")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Fine MSPT distribution:")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("3-5ms")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Load distribution:")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Distance control:")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("view changes=0, sim changes=0")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Distance state start:")));
    assertTrue(lines.stream()
      .anyMatch(line -> line.contains("view=12/12 warmup=no red=0 exp=0 | sim=12/12 red=0 exp=0")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("measurement window")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("short local chunk hops")));
  }

  @Test
  void chatFormatStaysCompactAndPointsToReport() {
    BenchmarkScenarioResult scenarioResult = new BenchmarkScenarioResult(
      BenchmarkScenarioId.GENERAL,
      phaseResult(120_000L, 8.1, 11.6, 27.8, 43.8, emptySnapshot()),
      phaseResult(120_000L, 3.7, 5.7, 19.2, 31.8, emptySnapshot()));
    EnumMap<BenchmarkScenarioId, Long> durations = new EnumMap<>(BenchmarkScenarioId.class);
    durations.put(BenchmarkScenarioId.GENERAL, 120_000L);

    BenchmarkCompareResult result = new BenchmarkCompareResult(
      "General",
      false,
      120_000L,
      30_000L,
      5_000L,
      3_000L,
      durations,
      List.of(scenarioResult),
      0, 9, 4, 0,
      false, 0, 0, 0,
      Instant.parse("2026-05-27T10:18:54.893486400Z"));

    List<String> lines = result.formatChat().stream().map(Component::getString)
      .toList();

    assertTrue(lines.stream().anyMatch(line -> line.contains("Benchmark Summary")));
    assertFalse(lines.stream().anyMatch(line -> line.contains("Baseline actions:")));
    assertFalse(lines.stream().anyMatch(line -> line.contains("Assessment:")));
  }

  @Test
  void explorationReportMarksInconclusiveRuns() {
    BenchmarkScenarioResult scenarioResult = new BenchmarkScenarioResult(
      BenchmarkScenarioId.EXPLORATION,
      explorationPhaseResult(53_000L, 8.1, 11.6, 27.8, 43.8, emptySnapshot(),
        53, 53, 0L, 0L, 0L, -8L, -9L, -10L),
      explorationPhaseResult(53_000L, 7.8, 10.9, 19.2, 31.8, emptySnapshot(),
        53, 53, 0L, 0L, 0L, 8L, 9L, 10L));
    EnumMap<BenchmarkScenarioId, Long> durations = new EnumMap<>(BenchmarkScenarioId.class);
    durations.put(BenchmarkScenarioId.EXPLORATION, 53_000L);

    BenchmarkCompareResult result = new BenchmarkCompareResult(
      "Exploration",
      false,
      53_000L,
      30_000L,
      5_000L,
      3_000L,
      durations,
      List.of(scenarioResult),
      0, 9, 4, 0,
      true, 53, 53, 0,
      Instant.parse("2026-05-27T10:18:54.893486400Z"));

    List<String> lines = result.formatMarkdown();

    assertTrue(lines.stream().anyMatch(line -> line.contains("Exploration*")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Assessment: Inconclusive")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Movement throttle signal: no")));
    assertTrue(lines.stream()
      .anyMatch(line -> line.contains("movement lowers=0, movement samples=0, max reduction=0")));
    assertTrue(lines.stream().anyMatch(line -> line.contains("Distance state end:")));
  }

  @Test
  void summaryScoreDeltaCapsSingleRegressionImpact() {
    BenchmarkScenarioResult general = new BenchmarkScenarioResult(
      BenchmarkScenarioId.GENERAL,
      phaseResult(120_000L, 9.0, 14.7, 22.5, 27.3, emptySnapshot()),
      phaseResult(120_000L, 6.5, 8.4, 19.4, 28.1, emptySnapshot()));
    BenchmarkScenarioResult items = new BenchmarkScenarioResult(
      BenchmarkScenarioId.ITEMS,
      phaseResult(30_000L, 11.6, 12.6, 4.9, 6.0, emptySnapshot()),
      phaseResult(30_000L, 4.6, 5.2, 3.6, 3.9, emptySnapshot()));
    BenchmarkScenarioResult xp = new BenchmarkScenarioResult(
      BenchmarkScenarioId.XP,
      phaseResult(30_000L, 12.7, 13.8, 4.9, 6.7, emptySnapshot()),
      phaseResult(30_000L, 5.0, 5.6, 3.3, 4.5, emptySnapshot()));
    BenchmarkScenarioResult entities = new BenchmarkScenarioResult(
      BenchmarkScenarioId.ENTITIES,
      phaseResult(30_000L, 11.0, 12.7, 3.9, 4.3, emptySnapshot()),
      phaseResult(30_000L, 23.0, 111.6, 5.1, 12.3, emptySnapshot()));
    BenchmarkScenarioResult recovery = new BenchmarkScenarioResult(
      BenchmarkScenarioId.RECOVERY,
      phaseResult(30_000L, 9.2, 10.0, 5.1, 6.7, emptySnapshot()),
      phaseResult(30_000L, 4.1, 4.7, 5.1, 6.8, emptySnapshot()));
    EnumMap<BenchmarkScenarioId, Long> durations = new EnumMap<>(BenchmarkScenarioId.class);
    durations.put(BenchmarkScenarioId.GENERAL, 120_000L);
    durations.put(BenchmarkScenarioId.ITEMS, 30_000L);
    durations.put(BenchmarkScenarioId.XP, 30_000L);
    durations.put(BenchmarkScenarioId.ENTITIES, 30_000L);
    durations.put(BenchmarkScenarioId.RECOVERY, 30_000L);

    BenchmarkCompareResult result = new BenchmarkCompareResult(
      "Full Suite",
      true,
      240_000L,
      30_000L,
      5_000L,
      3_000L,
      durations,
      List.of(general, items, xp, entities, recovery),
      0, 8, 4, 1,
      true, 60, 60, 0,
      Instant.parse("2026-05-27T13:11:57.051718500Z"));

    List<String> lines = result.formatChat().stream().map(Component::getString)
      .toList();

    assertTrue(lines.stream().anyMatch(line -> line.contains("score=+")));
  }

  @Test
  void captureMeasurementStartStatsRunsBeforeSnapshot() throws Exception {
    PerformanceStats.reset();
    RecordingScenario scenario = new RecordingScenario();
    BenchmarkScenarioContext context = new BenchmarkScenarioContext(
      null,
      null,
      Vec3.ZERO,
      BenchmarkScenarioId.ITEMS,
      false,
      false,
      30_000L,
      "benchmark",
      "benchmark_items");

    PerformanceStats.Snapshot snapshot = (PerformanceStats.Snapshot) invokePrivateMethod(
      "captureMeasurementStartStats",
      new Class<?>[]{BenchmarkScenario.class, BenchmarkScenarioContext.class},
      scenario,
      context);

    assertEquals(1L, snapshot.itemsRemoved());
    assertTrue(scenario.beforeMeasurementCalled);
    assertFalse(scenario.onMeasurementTickCalled);
  }

  @Test
  void runScenarioMeasurementTickProducesMeasuredDelta() throws Exception {
    PerformanceStats.reset();
    RecordingScenario scenario = new RecordingScenario();
    BenchmarkScenarioContext context = new BenchmarkScenarioContext(
      null,
      null,
      Vec3.ZERO,
      BenchmarkScenarioId.ENTITIES,
      true,
      false,
      30_000L,
      "benchmark",
      "benchmark_entities");
    PerformanceStats.Snapshot start = PerformanceStats.snapshot();

    invokePrivateMethod(
      "runScenarioMeasurementTick",
      new Class<?>[]{BenchmarkScenario.class, BenchmarkScenarioContext.class},
      scenario,
      context);

    PerformanceStats.Snapshot delta = PerformanceStats.delta(start, PerformanceStats.snapshot());

    assertTrue(scenario.onMeasurementTickCalled);
    assertEquals(1L, delta.entityChunkCleanupRemoved());
  }

  private static final class RecordingScenario implements BenchmarkScenario {

    private boolean beforeMeasurementCalled = false;
    private boolean onMeasurementTickCalled = false;

    @Override
    public BenchmarkScenarioId id() {
      return BenchmarkScenarioId.ITEMS;
    }

    @Override
    public void beforeMeasurement(BenchmarkScenarioContext context) {
      this.beforeMeasurementCalled = true;
      PerformanceStats.itemsRemoved++;
    }

    @Override
    public void onMeasurementTick(BenchmarkScenarioContext context) {
      this.onMeasurementTickCalled = true;
      PerformanceStats.entityChunkCleanupRemoved++;
    }
  }
}
