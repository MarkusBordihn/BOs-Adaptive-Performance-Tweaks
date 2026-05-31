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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import de.markusbordihn.adaptiveperformancetweaks.core.entity.TrackingCategory;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.MsptBucket;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenario;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioContext;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioId;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioResult;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.lang.reflect.Method;
import java.time.Instant;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import net.minecraft.network.chat.Component;
import net.minecraft.world.phys.Vec3;
import org.junit.jupiter.api.Test;

class BenchmarkManagerTest {

  private static Object invokePrivateMethod(
    String methodName, Class<?>[] parameterTypes, Object... args) throws Exception {
    Method method = BenchmarkManager.class.getDeclaredMethod(methodName, parameterTypes);
    method.setAccessible(true);
    return method.invoke(null, args);
  }

  private static void invokePrivateMethod(String methodName) throws Exception {
    invokePrivateMethod(methodName, new Class<?>[0]);
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
    EnumMap<ServerLoadLevel, Integer> loadDist = new EnumMap<>(ServerLoadLevel.class);
    loadDist.put(ServerLoadLevel.VERY_LOW, 1);
    EnumMap<MsptBucket, Integer> msptDist = new EnumMap<>(MsptBucket.class);
    msptDist.put(MsptBucket.UNDER_5_MS, avgTick <= 5.0 ? 1 : 0);
    msptDist.put(MsptBucket.FROM_5_TO_10_MS, avgTick > 5.0 && avgTick <= 10.0 ? 1 : 0);
    msptDist.put(MsptBucket.FROM_10_TO_VERY_LOW_MS, avgTick > 10.0 && avgTick <= 20.0 ? 1 : 0);
    EnumMap<FineMsptBucket, Integer> fineMsptDist = new EnumMap<>(FineMsptBucket.class);
    fineMsptDist.put(FineMsptBucket.UNDER_3_MS, avgTick < 3.0 ? 1 : 0);
    fineMsptDist.put(FineMsptBucket.FROM_3_TO_5_MS, avgTick >= 3.0 && avgTick < 5.0 ? 1 : 0);
    fineMsptDist.put(FineMsptBucket.FROM_5_TO_10_MS, avgTick >= 5.0 && avgTick < 10.0 ? 1 : 0);
    fineMsptDist.put(FineMsptBucket.FROM_10_MS_UP, avgTick >= 10.0 ? 1 : 0);
    return new BenchmarkScenarioResult.PhaseResult(
      durationMs,
      avgTick,
      Math.max(0.0, avgTick - 1.5),
      p95Tick,
      p95Tick + 1.2,
      loadDist,
      msptDist,
      fineMsptDist,
      0L,
      0,
      avgCpu,
      maxCpu,
      snapshot);
  }

  @Test
  void restoreFeaturesKeepsPreviouslyDisabledFeatureDisabled() throws Exception {
    boolean previousItemsState = FeatureToggle.ITEMS.isEnabled();
    boolean previousSpawnState = FeatureToggle.SPAWN.isEnabled();
    try {
      FeatureToggle.ITEMS.setEnabled(false);
      FeatureToggle.SPAWN.setEnabled(true);

      invokePrivateMethod("saveFeatureState");
      invokePrivateMethod("disableAllFeatures");
      invokePrivateMethod("restoreFeatures");

      assertFalse(FeatureToggle.ITEMS.isEnabled());
      assertTrue(FeatureToggle.SPAWN.isEnabled());
    } finally {
      invokePrivateMethod("clearSessionState");
      FeatureToggle.ITEMS.setEnabled(previousItemsState);
      FeatureToggle.SPAWN.setEnabled(previousSpawnState);
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

    assertEquals(120_000L, durations.get(BenchmarkScenarioId.GENERAL));
    assertEquals(30_000L, durations.get(BenchmarkScenarioId.ITEMS));
    assertEquals(30_000L, durations.get(BenchmarkScenarioId.XP));
    assertEquals(30_000L, durations.get(BenchmarkScenarioId.ENTITIES));
    assertEquals(30_000L, durations.get(BenchmarkScenarioId.RECOVERY));
  }

  @Test
  void suiteDurationValidationRejectsTooShortSuites() throws Exception {
    String validation = (String) invokePrivateMethod("validateSuiteDurationSeconds",
      new Class<?>[]{long.class}, 119L);

    assertNotNull(validation);
    assertTrue(validation.contains("at least"));
  }

  @Test
  void defaultScenarioOrderStartsWithGeneralAndEndsWithRecovery() throws Exception {
    @SuppressWarnings("unchecked")
    List<BenchmarkScenario> scenarios =
      (List<BenchmarkScenario>) invokePrivateMethod("createScenarioSuite", new Class<?>[0]);

    assertEquals(BenchmarkScenarioId.GENERAL, scenarios.get(0).id());
    assertEquals(BenchmarkScenarioId.RECOVERY, scenarios.get(scenarios.size() - 1).id());
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
    assertTrue(lines.stream().anyMatch(line -> line.contains("Measures broad world activity")));
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
      beforeMeasurementCalled = true;
      PerformanceStats.itemsRemoved++;
    }

    @Override
    public void onMeasurementTick(BenchmarkScenarioContext context) {
      onMeasurementTickCalled = true;
      PerformanceStats.entityChunkCleanupRemoved++;
    }
  }
}
