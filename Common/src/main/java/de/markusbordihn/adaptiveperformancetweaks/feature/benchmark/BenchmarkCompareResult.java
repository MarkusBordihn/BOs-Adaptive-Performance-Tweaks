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

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.server.MsptBucket;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioId;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioResult;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.BenchmarkScenarioResult.PhaseResult;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;

public record BenchmarkCompareResult(
  String requestedScenarioLabel,
  boolean suiteMode,
  long blockDurationMs,
  long warmupDurationMs,
  long settleDurationMs,
  long cleanupSettleDurationMs,
  Map<BenchmarkScenarioId, Long> scenarioDurationsMs,
  List<BenchmarkScenarioResult> scenarioResults,
  int manualEnabledFeatureCount,
  int autoEnabledFeatureCount,
  int manuallyDisabledFeatureCount,
  int conflictDisabledFeatureCount,
  boolean movementRoutesEnabled,
  int baselineMovementTargetCount,
  int activeMovementTargetCount,
  int sharedMovementTargetCount,
  Instant timestamp) {

  private static final MsptBucket[] TEN_PLUS_BUCKETS = {
    MsptBucket.FROM_10_TO_VERY_LOW_MS,
    MsptBucket.FROM_VERY_LOW_TO_LOW_MS,
    MsptBucket.FROM_LOW_TO_NORMAL_MS,
    MsptBucket.FROM_NORMAL_TO_MEDIUM_MS,
    MsptBucket.FROM_MEDIUM_TO_HIGH_MS,
    MsptBucket.FROM_HIGH_TO_100_MS,
    MsptBucket.FROM_100_MS_UP
  };

  private static Component assessmentLine(BenchmarkScenarioResult result) {
    if (result.isInconclusive()) {
      return Component.literal("Assessment: ").withStyle(ChatFormatting.GRAY)
        .append(Component.literal("Inconclusive").withStyle(ChatFormatting.YELLOW))
        .append(Component.literal(
            "  (Active exploration never triggered movement throttle; use this run as route/load context only.)")
          .withStyle(ChatFormatting.WHITE));
    }

    double improvement = result.tickTimeImprovementPercent();
    String assessment;
    ChatFormatting assessmentColor;
    if (improvement >= 10.0d) {
      assessment = "Clear improvement";
      assessmentColor = ChatFormatting.GREEN;
    } else if (improvement >= 3.0d) {
      assessment = "Noticeable improvement";
      assessmentColor = ChatFormatting.GREEN;
    } else if (improvement > -3.0d) {
      assessment = "No significant MSPT impact";
      assessmentColor = ChatFormatting.YELLOW;
    } else {
      assessment = "Performance regression detected";
      assessmentColor = ChatFormatting.RED;
    }

    return Component.literal("Assessment: ").withStyle(ChatFormatting.GRAY)
      .append(Component.literal(assessment).withStyle(assessmentColor))
      .append(Component.literal(String.format(
          "  (Active avg/P95: %.1f/%.1fms | score %.1f | headroom %.1f%%)",
          result.active().avgTickMs(),
          result.active().p95TickMs(),
          result.activePerformanceScore(),
          result.activeHeadroomPercent()))
        .withStyle(ChatFormatting.WHITE));
  }

  private static String summarizeStats(PerformanceStats.Snapshot snapshot) {
    long manualExcluded = snapshot.trackingExcludedManualNamespace()
      + snapshot.trackingExcludedManualEntity();
    long autoExcluded = snapshot.trackingExcludedAutoNamespace()
      + snapshot.trackingExcludedAutoEntity();

    return String.format(
      "spawn=%d denied=%d natural=%d/%d items=%d/%d xp=%d/%d farm=%d arrows=%d sim=%d moves=%d tracking=%d manual=%d auto=%d",
      snapshot.mobSpawnChecks(),
      snapshot.mobSpawnsDenied(),
      snapshot.naturalSpawnChecks(),
      snapshot.naturalSpawnsDenied(),
      snapshot.itemsMerged(),
      snapshot.itemsRemoved(),
      snapshot.xpOrbsMerged(),
      snapshot.xpOrbsRemoved(),
      snapshot.entityChunkCleanupRemoved(),
      snapshot.arrowsRemoved(),
      snapshot.simulationDistanceChanges(),
      snapshot.simulationDistanceMovementAdjustments(),
      snapshot.trackingEvaluations(),
      manualExcluded,
      autoExcluded);
  }

  private static String formatMspt(double value) {
    return String.format("%.1fms", value);
  }

  private static String formatSignedPercent(double value) {
    return String.format("%+.1f%%", value);
  }

  private static String formatSignedMs(double value) {
    return String.format("%+.1fms", value);
  }

  private static String formatSignedNumber(double value) {
    return String.format("%+.1f", value);
  }

  private static String formatSignedBytes(long bytes) {
    return (bytes >= 0L ? "+" : "-") + BenchmarkMessenger.formatBytes(Math.abs(bytes));
  }

  private static String formatHeapBytes(long bytes) {
    return bytes >= 0L ? BenchmarkMessenger.formatBytes(bytes) : "n/a";
  }

  private static double calculateFastRatio(Map<MsptBucket, Integer> distribution) {
    return calculateBucketRatio(distribution, MsptBucket.UNDER_5_MS, MsptBucket.FROM_5_TO_10_MS);
  }

  private static double calculateTenPlusRatio(Map<MsptBucket, Integer> distribution) {
    return calculateBucketRatio(distribution, TEN_PLUS_BUCKETS);
  }

  private static double calculateBucketRatio(
    Map<MsptBucket, Integer> distribution, MsptBucket... buckets) {
    int total = distribution.values().stream().mapToInt(Integer::intValue).sum();
    if (total == 0) {
      return 0.0d;
    }
    int matched = 0;
    for (MsptBucket bucket : buckets) {
      matched += distribution.getOrDefault(bucket, 0);
    }
    return 100.0d * matched / total;
  }

  private static Component scenarioSummaryLine(BenchmarkScenarioResult scenarioResult) {
    double p95Delta =
      scenarioResult.baseline().p95TickMs() - scenarioResult.active().p95TickMs();
    double scoreDelta =
      scenarioResult.activePerformanceScore() - scenarioResult.baselinePerformanceScore();
    return Component.literal(String.format("%-13s %-8s ", scenarioResult.summaryDisplayName(),
        BenchmarkMessenger.formatDuration(scenarioResult.active().measurementDurationMs())))
      .withStyle(ChatFormatting.WHITE)
      .append(Component.literal(String.format("%-12s",
          formatMspt(scenarioResult.baseline().avgTickMs())))
        .withStyle(ChatFormatting.AQUA))
      .append(Component.literal(String.format("%-12s",
          formatMspt(scenarioResult.active().avgTickMs())))
        .withStyle(ChatFormatting.GREEN))
      .append(Component.literal(String.format("%-8s",
          formatSignedPercent(scenarioResult.tickTimeImprovementPercent())))
        .withStyle(getPositiveDeltaColor(scenarioResult.tickTimeImprovementPercent())))
      .append(Component.literal(String.format("%-8s", formatSignedMs(p95Delta)))
        .withStyle(getPositiveDeltaColor(p95Delta)))
      .append(Component.literal(String.format("%-8s", formatSignedNumber(scoreDelta)))
        .withStyle(getPositiveDeltaColor(scoreDelta)));
  }

  private static String chatScenarioLabel(BenchmarkScenarioResult scenarioResult) {
    return switch (scenarioResult.scenarioId()) {
      case GENERAL -> "General";
      case EXPLORATION -> scenarioResult.isInconclusive() ? "Expl.*" : "Expl.";
      case ITEMS -> "Items";
      case XP -> "XP";
      case ENTITIES -> "Entities";
      case RECOVERY -> "Recovery";
    };
  }

  private static Component chatScenarioSummaryLine(BenchmarkScenarioResult scenarioResult) {
    double p95Delta =
      scenarioResult.baseline().p95TickMs() - scenarioResult.active().p95TickMs();
    double scoreDelta =
      scenarioResult.activePerformanceScore() - scenarioResult.baselinePerformanceScore();
    return Component.literal(String.format("%-9s %-6s ", chatScenarioLabel(scenarioResult),
        BenchmarkMessenger.formatDuration(scenarioResult.active().measurementDurationMs())))
      .withStyle(ChatFormatting.WHITE)
      .append(Component.literal(String.format("%-9s",
          formatMspt(scenarioResult.baseline().avgTickMs())))
        .withStyle(ChatFormatting.AQUA))
      .append(Component.literal(String.format("%-9s",
          formatMspt(scenarioResult.active().avgTickMs())))
        .withStyle(ChatFormatting.GREEN))
      .append(Component.literal(String.format("%-8s",
          formatSignedPercent(scenarioResult.tickTimeImprovementPercent())))
        .withStyle(getPositiveDeltaColor(scenarioResult.tickTimeImprovementPercent())))
      .append(Component.literal(String.format("%-7s", formatSignedMs(p95Delta)))
        .withStyle(getPositiveDeltaColor(p95Delta)))
      .append(Component.literal(String.format("%-7s", formatSignedNumber(scoreDelta)))
        .withStyle(getPositiveDeltaColor(scoreDelta)));
  }

  private static String markdownScenarioSummaryLine(BenchmarkScenarioResult scenarioResult) {
    double p95Delta =
      scenarioResult.baseline().p95TickMs() - scenarioResult.active().p95TickMs();
    double scoreDelta =
      scenarioResult.activePerformanceScore() - scenarioResult.baselinePerformanceScore();
    return markdownTableRow(
      new String[]{
        scenarioResult.summaryDisplayName(),
        BenchmarkMessenger.formatDuration(scenarioResult.active().measurementDurationMs()),
        formatMspt(scenarioResult.baseline().avgTickMs()),
        formatMspt(scenarioResult.active().avgTickMs()),
        formatSignedPercent(scenarioResult.tickTimeImprovementPercent()),
        formatSignedMs(p95Delta),
        formatSignedNumber(scoreDelta)
      },
      new int[]{8, 8, 8, 6, 10, 9, 10},
      false, false, true, true, true, true, true);
  }

  private static String markdownMetricLine(
    String label, String baselineValue, String activeValue, String deltaValue) {
    return markdownTableRow(
      new String[]{label, baselineValue, activeValue, deltaValue},
      new int[]{22, 8, 8, 7},
      false, true, true, true);
  }

  private static Component distributionLine(String label, double baselineRatio,
    double activeRatio) {
    return Component.literal(String.format("  %-20s ", label)).withStyle(ChatFormatting.DARK_GRAY)
      .append(Component.literal(String.format("%-10s", formatPercentInt(baselineRatio)))
        .withStyle(ChatFormatting.AQUA))
      .append(Component.literal(formatPercentInt(activeRatio)).withStyle(ChatFormatting.GREEN));
  }

  private static String markdownDistributionLine(String label, double baselineRatio,
    double activeRatio) {
    return markdownTableRow(
      new String[]{label, formatPercentInt(baselineRatio), formatPercentInt(activeRatio)},
      new int[]{10, 8, 6},
      false, true, true);
  }

  private static String scenarioDescription(BenchmarkScenarioId scenarioId) {
    return switch (scenarioId) {
      case GENERAL ->
        "Measures broad world activity with short local chunk hops around the benchmark origin. Relevant areas: ambient spawn pressure, general tracking overhead and other global APTweaks effects during normal player movement without long-range exploration.";
      case EXPLORATION ->
        "Moves the benchmark player across a deterministic chunk route. Relevant areas: exploration-triggered simulation-distance behavior, chunk travel pressure and global APTweaks effects during sustained movement.";
      case ITEMS ->
        "Spawns a dense field of dropped cobblestone items and keeps adding fresh drops during measurement. Relevant areas: item merge behavior, item cleanup pressure and item-related entity load inside the measured window.";
      case XP ->
        "Spawns many small XP orbs close together and keeps adding more during measurement. Relevant areas: XP orb merging, XP cleanup pressure and short-lived entity handling inside the measured window.";
      case ENTITIES ->
        "Spawns a dense passive-mob farm across neighboring chunks and keeps feeding it during measurement. Relevant areas: steady-state entity/tracking cost and chunk cleanup pressure, while ambient spawn-denial counters still mainly reflect the surrounding world.";
      case RECOVERY ->
        "Adds no new load after cleanup. Relevant areas: how quickly the server returns to a low-load state once the previous benchmark pressure is gone.";
    };
  }

  private static String markdownTableRow(String[] values, int[] widths, boolean... rightAlign) {
    StringBuilder row = new StringBuilder("|");
    for (int index = 0; index < values.length; index++) {
      int width = Math.max(widths[index], values[index].length());
      String value = rightAlign[index]
        ? String.format("%" + width + "s", values[index])
        : String.format("%-" + width + "s", values[index]);
      row.append(' ').append(value).append(" |");
    }

    return row.toString();
  }

  private static double ratio(long value, long total) {
    return total <= 0L ? 0.0d : 100.0d * value / total;
  }

  private static <T> boolean hasDistributionData(
    Map<T, Integer> baselineDistribution, Map<T, Integer> activeDistribution, T key) {
    return baselineDistribution.getOrDefault(key, 0) > 0
      || activeDistribution.getOrDefault(key, 0) > 0;
  }

  private static <T> double percentage(Map<T, Integer> distribution, T key) {
    int total = distribution.values().stream().mapToInt(Integer::intValue).sum();
    if (total == 0) {
      return 0.0d;
    }

    return 100.0d * distribution.getOrDefault(key, 0) / total;
  }

  private static String formatPercentInt(double value) {
    return String.format("%.0f%%", value);
  }

  private static Component metricLine(
    String label, String baselineValue, String activeValue, String deltaValue,
    ChatFormatting deltaColor) {
    return Component.literal(String.format("%-12s ", label + ':')).withStyle(ChatFormatting.GRAY)
      .append(Component.literal(baselineValue).withStyle(ChatFormatting.AQUA))
      .append(Component.literal(" -> ").withStyle(ChatFormatting.DARK_GRAY))
      .append(Component.literal(activeValue).withStyle(ChatFormatting.GREEN))
      .append(Component.literal(" [" + deltaValue + ']').withStyle(deltaColor));
  }

  private static ChatFormatting getPositiveDeltaColor(double value) {
    if (value > 0.0d) {
      return ChatFormatting.GREEN;
    }

    if (value < 0.0d) {
      return ChatFormatting.RED;
    }

    return ChatFormatting.YELLOW;
  }

  private static ChatFormatting getEfficiencyColor(double resourceDelta, double tickDeltaPercent) {
    if (resourceDelta < 0.0d && tickDeltaPercent > 0.0d) {
      return ChatFormatting.GREEN;
    }

    if (resourceDelta > 0.0d && tickDeltaPercent < 0.0d) {
      return ChatFormatting.RED;
    }

    return ChatFormatting.WHITE;
  }

  private static Component explorationValidationLine(BenchmarkScenarioResult result) {
    BenchmarkScenarioResult.ScenarioValidation baseline = result.baseline().validation();
    BenchmarkScenarioResult.ScenarioValidation active = result.active().validation();
    return Component.literal("Exploration: ").withStyle(ChatFormatting.GRAY)
      .append(Component.literal(String.format(
          "steps %d -> %d | unique chunks %d -> %d | overlap %d | movement throttle %s",
          baseline.movementStepCount(),
          active.movementStepCount(),
          baseline.uniqueChunkTargetCount(),
          active.uniqueChunkTargetCount(),
          result.sharedMovementChunkTargetCount(),
          active.movementSignalObserved()
            ? String.format("yes (%d samples, max reduction %d)",
            active.movementThrottleSampleCount(),
            active.movementMaxReduction())
            : "no"))
        .withStyle(active.movementSignalObserved() ? ChatFormatting.GREEN : ChatFormatting.YELLOW));
  }

  private static void appendExplorationValidation(List<String> lines,
    BenchmarkScenarioResult result) {
    BenchmarkScenarioResult.ScenarioValidation baseline = result.baseline().validation();
    BenchmarkScenarioResult.ScenarioValidation active = result.active().validation();
    lines.add("**Exploration validation:**");
    lines.add("");
    lines.add("- Baseline route: steps=" + baseline.movementStepCount()
      + ", unique chunks=" + baseline.uniqueChunkTargetCount());
    lines.add("- Active route: steps=" + active.movementStepCount()
      + ", unique chunks=" + active.uniqueChunkTargetCount());
    lines.add("- Route overlap: " + result.sharedMovementChunkTargetCount());
    lines.add("- Movement throttle signal: " + (active.movementSignalObserved()
      ? String.format("yes (%d samples, additional reduction %d)",
      active.movementThrottleSampleCount(),
      active.movementMaxReduction())
      : "no"));
  }

  private static String formatDistanceControlState(
    BenchmarkScenarioResult.DistanceControlState state) {
    return String.format(
      "view=%s/%s warmup=%s red=%d exp=%d | sim=%s/%s red=%d exp=%d",
      formatDistanceValue(state.viewDistance()),
      formatDistanceValue(state.viewBaselineDistance()),
      state.viewWarmupActive() ? "yes" : "no",
      state.viewWarmupReduction(),
      state.viewActiveExplorers(),
      formatDistanceValue(state.simulationDistance()),
      formatDistanceValue(state.simulationBaselineDistance()),
      state.simulationMovementReduction(),
      state.simulationActiveExplorers());
  }

  private static String formatDistanceValue(int value) {
    return value >= 0 ? Integer.toString(value) : "n/a";
  }

  public List<Component> format() {
    List<Component> lines = new ArrayList<>();
    lines.add(Component.literal(String.format("=== %s Benchmark Report ===", Constants.MOD_NAME))
      .withStyle(ChatFormatting.GOLD));
    lines.add(Component.literal(String.format(
      "Target: %s  |  Timestamp: %s",
      requestedScenarioLabel,
      timestamp)));
    lines.add(Component.literal(String.format(
      "Block warm-up: %s  |  Settle: %s  |  Cleanup settle: %s",
      BenchmarkMessenger.formatDuration(warmupDurationMs),
      BenchmarkMessenger.formatDuration(settleDurationMs),
      BenchmarkMessenger.formatDuration(cleanupSettleDurationMs))));
    lines.add(Component.literal("Scenario durations: " + formatScenarioDurations()));
    lines.add(Component.literal(String.format(
      "Features: manual=%d auto=%d off=%d conflict=%d",
      manualEnabledFeatureCount,
      autoEnabledFeatureCount,
      manuallyDisabledFeatureCount,
      conflictDisabledFeatureCount)));
    if (movementRoutesEnabled) {
      lines.add(Component.literal(String.format(
        "Movement routes: baseline=%d chunks  active=%d chunks  overlap=%d",
        baselineMovementTargetCount,
        activeMovementTargetCount,
        sharedMovementTargetCount)));
    }

    lines.add(Component.literal(""));
    lines.add(overallSummary());
    lines.add(overallBucketSummary());
    lines.add(overallConclusion());
    appendDistributionDetails(lines,
      aggregateMsptDistribution(false), aggregateMsptDistribution(true),
      aggregateFineMsptDistribution(false), aggregateFineMsptDistribution(true),
      aggregateLoadDistribution(false), aggregateLoadDistribution(true));
    lines.add(Component.literal(""));
    lines.add(Component.literal(
        String.format("%-13s %-8s %-12s %-12s %-8s %-8s %-8s",
          "Scenario", "Dur", "Baseline", "Active", "Saved", "P95 sav", "Score +"))
      .withStyle(ChatFormatting.GRAY));
    for (BenchmarkScenarioResult scenarioResult : scenarioResults) {
      lines.add(scenarioSummaryLine(scenarioResult));
    }
    if (scenarioResults.stream().anyMatch(BenchmarkScenarioResult::isInconclusive)) {
      lines.add(
        Component.literal("* Exploration* = active route never triggered movement throttle.")
          .withStyle(ChatFormatting.YELLOW));
    }

    for (BenchmarkScenarioResult scenarioResult : scenarioResults) {
      lines.add(Component.literal(""));
      lines.add(Component.literal("-- " + scenarioResult.detailDisplayName() + " --")
        .withStyle(ChatFormatting.GRAY));
      appendScenarioDetails(lines, scenarioResult);
    }

    return lines;
  }

  public List<Component> formatChat() {
    List<Component> lines = new ArrayList<>();
    lines.add(Component.literal("=== Benchmark Summary ===").withStyle(ChatFormatting.GOLD));
    lines.add(overallSummary());
    lines.add(chatBucketSummary());
    lines.add(chatOverallConclusion());
    lines.add(Component.literal(""));
    lines.add(Component.literal(
        String.format("%-9s %-6s %-9s %-9s %-8s %-7s %-7s",
          "Scene", "Dur", "Base", "Active", "Saved", "P95 sv", "Score+"))
      .withStyle(ChatFormatting.GRAY));
    for (BenchmarkScenarioResult scenarioResult : scenarioResults) {
      lines.add(chatScenarioSummaryLine(scenarioResult));
    }
    if (scenarioResults.stream().anyMatch(BenchmarkScenarioResult::isInconclusive)) {
      lines.add(Component.literal("* Expl.* = active route never triggered movement throttle.")
        .withStyle(ChatFormatting.YELLOW));
    }

    return lines;
  }

  public List<String> formatMarkdown() {
    List<String> lines = new ArrayList<>();
    lines.add("# " + Constants.MOD_NAME + " Benchmark Report");
    lines.add("");
    lines.add("- Target: " + requestedScenarioLabel);
    lines.add("- Timestamp: " + timestamp);
    lines.add("- Block warm-up: " + BenchmarkMessenger.formatDuration(warmupDurationMs));
    lines.add("- Settle: " + BenchmarkMessenger.formatDuration(settleDurationMs));
    lines.add("- Cleanup settle: " + BenchmarkMessenger.formatDuration(cleanupSettleDurationMs));
    lines.add("- Scenario durations: " + formatScenarioDurations());
    lines.add(String.format(
      "- Features: manual=%d auto=%d off=%d conflict=%d",
      manualEnabledFeatureCount,
      autoEnabledFeatureCount,
      manuallyDisabledFeatureCount,
      conflictDisabledFeatureCount));
    if (movementRoutesEnabled) {
      lines.add(String.format(
        "- Movement routes: baseline=%d chunks, active=%d chunks, overlap=%d",
        baselineMovementTargetCount,
        activeMovementTargetCount,
        sharedMovementTargetCount));
    }

    lines.add("");
    lines.add("## Summary");
    lines.add("");
    lines.add(
      "Compares a baseline block with APTweaks features disabled against an active block with the saved feature state restored. Setup, cleanup and settle time are excluded from the measured window.");
    lines.add(
      "The overall score is intentionally outlier-resistant so one noisy scenario or background task does not dominate the suite summary.");
    lines.add("");
    lines.add("- " + overallSummary().getString());
    lines.add("- " + overallBucketSummary().getString());
    lines.add("- " + overallConclusion().getString());
    lines.add("");
    appendMarkdownDistributionRows(lines,
      aggregateMsptDistribution(false), aggregateMsptDistribution(true),
      aggregateFineMsptDistribution(false), aggregateFineMsptDistribution(true),
      aggregateLoadDistribution(false), aggregateLoadDistribution(true));
    lines.add("");
    lines.add("## Scenario Summary");
    lines.add("");
    lines.add(
      "Shows the measured runtime difference per scenario. `Baseline` is the block without optimizations, `Active` is the same scenario with the saved feature state restored.");
    lines.add("");
    lines.add(
      "`MSPT saved`, `P95 saved` and `Score gain` are written so that a positive value always means the active block was better.");
    lines.add("");
    lines.add(
      "| Scenario | Duration | Baseline | Active | MSPT saved | P95 saved | Score gain |");
    lines.add(
      "|----------|----------|---------:|-------:|-----------:|----------:|-----------:|");
    for (BenchmarkScenarioResult scenarioResult : scenarioResults) {
      lines.add(markdownScenarioSummaryLine(scenarioResult));
    }
    if (scenarioResults.stream().anyMatch(BenchmarkScenarioResult::isInconclusive)) {
      lines.add("");
      lines.add("* `Exploration*` means the active route never triggered movement throttle.");
    }

    lines.add("");
    lines.add("## Scenario Details");
    lines.add("");
    lines.add(
      "Each scenario section includes the measured metrics, the MSPT/load distributions, and the recorded APTweaks counters that changed during that measurement window.");
    lines.add("");
    lines.add(
      "For `Avg MSPT`, `P95 MSPT`, `Headroom`, `Perf score` and `Fast <10ms` a positive delta means the active block was better. `CPU avg/max`, `Heap avg` and `Entities` are reported as active minus baseline, so a negative delta is better there.");
    for (BenchmarkScenarioResult scenarioResult : scenarioResults) {
      appendScenarioMarkdown(lines, scenarioResult);
    }

    return lines;
  }

  private void appendScenarioDetails(List<Component> lines, BenchmarkScenarioResult result) {
    BenchmarkScenarioResult.PhaseResult baseline = result.baseline();
    BenchmarkScenarioResult.PhaseResult active = result.active();
    double headroomDelta = result.activeHeadroomPercent() - result.baselineHeadroomPercent();
    double scoreDelta = result.activePerformanceScore() - result.baselinePerformanceScore();
    double fastDelta = result.activeFastRatio() - result.baselineFastRatio();
    boolean heapAvailable =
      baseline.avgHeapUsedBytes() >= 0L && active.avgHeapUsedBytes() >= 0L;
    long heapDelta =
      heapAvailable ? active.avgHeapUsedBytes() - baseline.avgHeapUsedBytes() : 0L;
    double cpuDelta = active.avgCpuPercent() - baseline.avgCpuPercent();
    int entityDelta = active.entityCount() - baseline.entityCount();

    lines.add(metricLine(
      "Avg MSPT",
      formatMspt(baseline.avgTickMs()),
      formatMspt(active.avgTickMs()),
      formatSignedPercent(result.tickTimeImprovementPercent()),
      getPositiveDeltaColor(result.tickTimeImprovementPercent())));
    lines.add(metricLine(
      "P95 MSPT",
      formatMspt(baseline.p95TickMs()),
      formatMspt(active.p95TickMs()),
      formatSignedMs(baseline.p95TickMs() - active.p95TickMs()),
      getPositiveDeltaColor(baseline.p95TickMs() - active.p95TickMs())));
    lines.add(metricLine(
      "Headroom",
      String.format("%.1f%%", result.baselineHeadroomPercent()),
      String.format("%.1f%%", result.activeHeadroomPercent()),
      String.format("%+.1fpp", headroomDelta),
      getPositiveDeltaColor(headroomDelta)));
    lines.add(metricLine(
      "Perf score",
      String.format("%.1f", result.baselinePerformanceScore()),
      String.format("%.1f", result.activePerformanceScore()),
      formatSignedNumber(scoreDelta),
      getPositiveDeltaColor(scoreDelta)));
    lines.add(metricLine(
      "Fast <10ms",
      String.format("%.0f%%", result.baselineFastRatio()),
      String.format("%.0f%%", result.activeFastRatio()),
      String.format("%+.0fpp", fastDelta),
      getPositiveDeltaColor(fastDelta)));
    if (baseline.avgCpuPercent() >= 0.0d && active.avgCpuPercent() >= 0.0d) {
      lines.add(metricLine(
        "CPU avg/max",
        String.format("%.1f/%.1f%%", baseline.avgCpuPercent(), baseline.maxCpuPercent()),
        String.format("%.1f/%.1f%%", active.avgCpuPercent(), active.maxCpuPercent()),
        String.format("%+.1fpp", cpuDelta),
        getEfficiencyColor(cpuDelta, result.tickTimeImprovementPercent())));
    }
    lines.add(metricLine(
      "Heap avg",
      formatHeapBytes(baseline.avgHeapUsedBytes()),
      formatHeapBytes(active.avgHeapUsedBytes()),
      heapAvailable ? formatSignedBytes(heapDelta) : "n/a",
      heapAvailable
        ? getEfficiencyColor(heapDelta, result.tickTimeImprovementPercent())
        : ChatFormatting.GRAY));
    lines.add(metricLine(
      "Entities",
      String.format("%,d", baseline.entityCount()),
      String.format("%,d", active.entityCount()),
      String.format("%+d", entityDelta),
      entityDelta <= 0 ? ChatFormatting.GREEN : ChatFormatting.RED));
    if (result.scenarioId() == BenchmarkScenarioId.EXPLORATION) {
      lines.add(explorationValidationLine(result));
    }
    appendDistributionDetails(lines,
      baseline.msptDistribution(), active.msptDistribution(),
      baseline.fineMsptDistribution(), active.fineMsptDistribution(),
      baseline.loadDistribution(), active.loadDistribution());
    lines.add(assessmentLine(result));
    lines.add(Component.literal("Baseline actions: ").withStyle(ChatFormatting.AQUA)
      .append(
        Component.literal(summarizeStats(baseline.statsDelta())).withStyle(ChatFormatting.WHITE)));
    lines.add(Component.literal("Active actions:   ").withStyle(ChatFormatting.GREEN)
      .append(
        Component.literal(summarizeStats(active.statsDelta())).withStyle(ChatFormatting.WHITE)));
  }

  private void appendScenarioMarkdown(List<String> lines, BenchmarkScenarioResult result) {
    BenchmarkScenarioResult.PhaseResult baseline = result.baseline();
    BenchmarkScenarioResult.PhaseResult active = result.active();
    double headroomDelta = result.activeHeadroomPercent() - result.baselineHeadroomPercent();
    double scoreDelta = result.activePerformanceScore() - result.baselinePerformanceScore();
    double fastDelta = result.activeFastRatio() - result.baselineFastRatio();
    boolean heapAvailable =
      baseline.avgHeapUsedBytes() >= 0L && active.avgHeapUsedBytes() >= 0L;
    long heapDelta =
      heapAvailable ? active.avgHeapUsedBytes() - baseline.avgHeapUsedBytes() : 0L;
    double cpuDelta = active.avgCpuPercent() - baseline.avgCpuPercent();
    int entityDelta = active.entityCount() - baseline.entityCount();

    lines.add("");
    lines.add("### " + result.detailDisplayName());
    lines.add("");
    lines.add(scenarioDescription(result.scenarioId()));
    lines.add("");
    lines.add("| Metric | Baseline | Active | Delta |");
    lines.add("|--------|---------:|-------:|------:|");
    lines.add(markdownMetricLine(
      "Avg MSPT",
      formatMspt(baseline.avgTickMs()),
      formatMspt(active.avgTickMs()),
      formatSignedPercent(result.tickTimeImprovementPercent())));
    lines.add(markdownMetricLine(
      "P95 MSPT",
      formatMspt(baseline.p95TickMs()),
      formatMspt(active.p95TickMs()),
      formatSignedMs(baseline.p95TickMs() - active.p95TickMs())));
    lines.add(markdownMetricLine(
      "Headroom",
      String.format("%.1f%%", result.baselineHeadroomPercent()),
      String.format("%.1f%%", result.activeHeadroomPercent()),
      String.format("%+.1fpp", headroomDelta)));
    lines.add(markdownMetricLine(
      "Perf score",
      String.format("%.1f", result.baselinePerformanceScore()),
      String.format("%.1f", result.activePerformanceScore()),
      formatSignedNumber(scoreDelta)));
    lines.add(markdownMetricLine(
      "Fast <10ms",
      String.format("%.0f%%", result.baselineFastRatio()),
      String.format("%.0f%%", result.activeFastRatio()),
      String.format("%+.0fpp", fastDelta)));
    if (baseline.avgCpuPercent() >= 0.0d && active.avgCpuPercent() >= 0.0d) {
      lines.add(markdownMetricLine(
        "CPU avg/max",
        String.format("%.1f/%.1f%%", baseline.avgCpuPercent(), baseline.maxCpuPercent()),
        String.format("%.1f/%.1f%%", active.avgCpuPercent(), active.maxCpuPercent()),
        String.format("%+.1fpp", cpuDelta)));
    }
    lines.add(markdownMetricLine(
      "Heap avg",
      formatHeapBytes(baseline.avgHeapUsedBytes()),
      formatHeapBytes(active.avgHeapUsedBytes()),
      heapAvailable ? formatSignedBytes(heapDelta) : "n/a"));
    lines.add(markdownMetricLine(
      "Entities",
      String.format("%,d", baseline.entityCount()),
      String.format("%,d", active.entityCount()),
      String.format("%+d", entityDelta)));
    if (result.scenarioId() == BenchmarkScenarioId.EXPLORATION) {
      lines.add("");
      appendExplorationValidation(lines, result);
    }
    lines.add("");
    appendMarkdownDistributionRows(lines,
      baseline.msptDistribution(), active.msptDistribution(),
      baseline.fineMsptDistribution(), active.fineMsptDistribution(),
      baseline.loadDistribution(), active.loadDistribution());
    lines.add("");
    lines.add("Assessment: " + assessmentLine(result).getString().replace("Assessment: ", ""));
    lines.add("");
    lines.add("#### Baseline Activity");
    appendMarkdownActivityDetails(lines, baseline);
    lines.add("");
    lines.add("#### Active Activity");
    appendMarkdownActivityDetails(lines, active);
  }

  private Component overallSummary() {
    double baselineWeightedAvg = weightedAverage(false);
    double activeWeightedAvg = weightedAverage(true);
    double improvement = baselineWeightedAvg == 0.0d ? 0.0d
      : (baselineWeightedAvg - activeWeightedAvg) / baselineWeightedAvg * 100.0d;
    double scoreDelta = summaryScoreDelta();

    return Component.literal("Overall: ").withStyle(ChatFormatting.GRAY)
      .append(Component.literal("baseline=" + formatMspt(baselineWeightedAvg) + "  ")
        .withStyle(ChatFormatting.AQUA))
      .append(Component.literal("active=" + formatMspt(activeWeightedAvg) + "  ")
        .withStyle(ChatFormatting.GREEN))
      .append(Component.literal("MSPT=" + formatSignedPercent(improvement) + "  ")
        .withStyle(getPositiveDeltaColor(improvement)))
      .append(Component.literal("score=" + formatSignedNumber(scoreDelta))
        .withStyle(getPositiveDeltaColor(scoreDelta)));
  }

  private Component overallBucketSummary() {
    Map<MsptBucket, Integer> baselineBuckets = aggregateMsptDistribution(false);
    Map<MsptBucket, Integer> activeBuckets = aggregateMsptDistribution(true);
    double baselineFastRatio = calculateFastRatio(baselineBuckets);
    double activeFastRatio = calculateFastRatio(activeBuckets);
    double baselineUnder5Ratio = calculateBucketRatio(baselineBuckets, MsptBucket.UNDER_5_MS);
    double activeUnder5Ratio = calculateBucketRatio(activeBuckets, MsptBucket.UNDER_5_MS);
    double baselineTenPlusRatio = calculateTenPlusRatio(baselineBuckets);
    double activeTenPlusRatio = calculateTenPlusRatio(activeBuckets);

    return Component.literal("Bucket shift: ").withStyle(ChatFormatting.GRAY)
      .append(Component.literal(String.format("Fast<10ms %.0f%% -> %.0f%%  ",
          baselineFastRatio, activeFastRatio))
        .withStyle(getPositiveDeltaColor(activeFastRatio - baselineFastRatio)))
      .append(Component.literal(String.format("<5ms %.0f%% -> %.0f%%  ",
          baselineUnder5Ratio, activeUnder5Ratio))
        .withStyle(getPositiveDeltaColor(activeUnder5Ratio - baselineUnder5Ratio)))
      .append(Component.literal(String.format("10ms+ %.0f%% -> %.0f%%",
          baselineTenPlusRatio, activeTenPlusRatio))
        .withStyle(getPositiveDeltaColor(baselineTenPlusRatio - activeTenPlusRatio)));
  }

  private Component chatBucketSummary() {
    Map<MsptBucket, Integer> baselineBuckets = aggregateMsptDistribution(false);
    Map<MsptBucket, Integer> activeBuckets = aggregateMsptDistribution(true);
    double baselineUnder5Ratio = calculateBucketRatio(baselineBuckets, MsptBucket.UNDER_5_MS);
    double activeUnder5Ratio = calculateBucketRatio(activeBuckets, MsptBucket.UNDER_5_MS);
    double baselineTenPlusRatio = calculateTenPlusRatio(baselineBuckets);
    double activeTenPlusRatio = calculateTenPlusRatio(activeBuckets);

    return Component.literal("Buckets: ").withStyle(ChatFormatting.GRAY)
      .append(Component.literal(String.format("<5ms %.0f%% -> %.0f%%  ",
          baselineUnder5Ratio, activeUnder5Ratio))
        .withStyle(getPositiveDeltaColor(activeUnder5Ratio - baselineUnder5Ratio)))
      .append(Component.literal(String.format("10ms+ %.0f%% -> %.0f%%",
          baselineTenPlusRatio, activeTenPlusRatio))
        .withStyle(getPositiveDeltaColor(baselineTenPlusRatio - activeTenPlusRatio)));
  }

  private Component overallConclusion() {
    double baselineWeightedAvg = weightedAverage(false);
    double activeWeightedAvg = weightedAverage(true);
    double improvement = baselineWeightedAvg == 0.0d ? 0.0d
      : (baselineWeightedAvg - activeWeightedAvg) / baselineWeightedAvg * 100.0d;
    double scoreDelta = summaryScoreDelta();
    BenchmarkScenarioResult strongestScenario = findStrongestScenario();
    Map<MsptBucket, Integer> baselineBuckets = aggregateMsptDistribution(false);
    Map<MsptBucket, Integer> activeBuckets = aggregateMsptDistribution(true);
    double fastDelta = calculateFastRatio(activeBuckets) - calculateFastRatio(baselineBuckets);

    String verdict;
    ChatFormatting verdictColor;
    if (improvement >= 25.0d && scoreDelta >= 5.0d) {
      verdict = "Large positive change";
      verdictColor = ChatFormatting.GREEN;
    } else if (improvement >= 10.0d) {
      verdict = "Clear positive change";
      verdictColor = ChatFormatting.GREEN;
    } else if (improvement >= 3.0d) {
      verdict = "Moderate positive change";
      verdictColor = ChatFormatting.YELLOW;
    } else if (improvement > -3.0d) {
      verdict = "Little measurable change";
      verdictColor = ChatFormatting.YELLOW;
    } else {
      verdict = "Regression detected";
      verdictColor = ChatFormatting.RED;
    }

    String strongestScenarioText = strongestScenario == null
      ? "n/a"
      : strongestScenario.displayName() + ' '
        + formatSignedPercent(strongestScenario.tickTimeImprovementPercent());

    return Component.literal("Conclusion: ").withStyle(ChatFormatting.GRAY)
      .append(Component.literal(verdict).withStyle(verdictColor))
      .append(Component.literal(String.format(
        "  (Weighted MSPT %s -> %s | score %+.1f | fast<10ms %+.0fpp | strongest %s)",
        formatMspt(baselineWeightedAvg),
        formatMspt(activeWeightedAvg),
        scoreDelta,
        fastDelta,
        strongestScenarioText)).withStyle(ChatFormatting.WHITE));
  }

  private Component chatOverallConclusion() {
    double baselineWeightedAvg = weightedAverage(false);
    double activeWeightedAvg = weightedAverage(true);
    double improvement = baselineWeightedAvg == 0.0d ? 0.0d
      : (baselineWeightedAvg - activeWeightedAvg) / baselineWeightedAvg * 100.0d;
    double scoreDelta = summaryScoreDelta();
    BenchmarkScenarioResult strongestScenario = findStrongestScenario();
    Map<MsptBucket, Integer> baselineBuckets = aggregateMsptDistribution(false);
    Map<MsptBucket, Integer> activeBuckets = aggregateMsptDistribution(true);
    double fastDelta = calculateFastRatio(activeBuckets) - calculateFastRatio(baselineBuckets);

    String verdict;
    ChatFormatting verdictColor;
    if (improvement >= 25.0d && scoreDelta >= 5.0d) {
      verdict = "Large positive change";
      verdictColor = ChatFormatting.GREEN;
    } else if (improvement >= 10.0d) {
      verdict = "Clear positive change";
      verdictColor = ChatFormatting.GREEN;
    } else if (improvement >= 3.0d) {
      verdict = "Moderate positive change";
      verdictColor = ChatFormatting.YELLOW;
    } else if (improvement > -3.0d) {
      verdict = "Little measurable change";
      verdictColor = ChatFormatting.YELLOW;
    } else {
      verdict = "Regression";
      verdictColor = ChatFormatting.RED;
    }

    String strongestScenarioText = strongestScenario == null
      ? "n/a"
      : strongestScenario.displayName() + ' '
        + formatSignedPercent(strongestScenario.tickTimeImprovementPercent());

    return Component.literal("Result: ").withStyle(ChatFormatting.GRAY)
      .append(Component.literal(verdict).withStyle(verdictColor))
      .append(Component.literal(String.format(
        "  (Weighted MSPT %s -> %s | score %+.1f | fast<10ms %+.0fpp | strongest %s)",
        formatMspt(baselineWeightedAvg),
        formatMspt(activeWeightedAvg),
        scoreDelta,
        fastDelta,
        strongestScenarioText)).withStyle(ChatFormatting.WHITE));
  }

  private double weightedAverage(boolean activePhase) {
    double weightedSum = 0.0d;
    long totalDuration = 0L;
    for (BenchmarkScenarioResult scenarioResult : scenarioResults) {
      BenchmarkScenarioResult.PhaseResult phaseResult =
        activePhase ? scenarioResult.active() : scenarioResult.baseline();
      weightedSum += phaseResult.avgTickMs() * phaseResult.measurementDurationMs();
      totalDuration += phaseResult.measurementDurationMs();
    }

    return totalDuration == 0L ? 50.0d : weightedSum / totalDuration;
  }

  private double summaryScoreDelta() {
    double positiveWeightedDelta = 0.0d;
    double negativeWeightedDelta = 0.0d;
    long totalDuration = 0L;
    for (BenchmarkScenarioResult scenarioResult : scenarioResults) {
      long duration = scenarioResult.active().measurementDurationMs();
      double scoreDelta =
        scenarioResult.activePerformanceScore() - scenarioResult.baselinePerformanceScore();
      if (scoreDelta >= 0.0d) {
        positiveWeightedDelta += scoreDelta * duration;
      } else {
        negativeWeightedDelta += -scoreDelta * duration;
      }
      totalDuration += duration;
    }

    if (totalDuration == 0L) {
      return 0.0d;
    }

    double positiveAverage = positiveWeightedDelta / totalDuration;
    double negativeAverage = negativeWeightedDelta / totalDuration;

    return Math.max(0.0d, positiveAverage - Math.min(negativeAverage, positiveAverage * 0.2d));
  }

  private BenchmarkScenarioResult findStrongestScenario() {
    BenchmarkScenarioResult strongest = null;
    for (BenchmarkScenarioResult scenarioResult : scenarioResults) {
      if (scenarioResult.isInconclusive()) {
        continue;
      }

      if (strongest == null
        || scenarioResult.tickTimeImprovementPercent() > strongest.tickTimeImprovementPercent()) {
        strongest = scenarioResult;
      }
    }

    return strongest;
  }

  private Map<MsptBucket, Integer> aggregateMsptDistribution(boolean activePhase) {
    return aggregateDistribution(activePhase,
      PhaseResult::msptDistribution,
      MsptBucket.class);
  }

  private Map<FineMsptBucket, Integer> aggregateFineMsptDistribution(boolean activePhase) {
    return aggregateDistribution(activePhase,
      PhaseResult::fineMsptDistribution,
      FineMsptBucket.class);
  }

  private Map<ServerLoadLevel, Integer> aggregateLoadDistribution(boolean activePhase) {
    return aggregateDistribution(activePhase,
      PhaseResult::loadDistribution,
      ServerLoadLevel.class);
  }

  private <K extends Enum<K>> Map<K, Integer> aggregateDistribution(
    boolean activePhase,
    Function<BenchmarkScenarioResult.PhaseResult, Map<K, Integer>> extractor,
    Class<K> keyType) {
    EnumMap<K, Integer> distribution = new EnumMap<>(keyType);
    for (BenchmarkScenarioResult scenarioResult : scenarioResults) {
      Map<K, Integer> scenarioDistribution = extractor.apply(
        activePhase ? scenarioResult.active() : scenarioResult.baseline());
      for (Map.Entry<K, Integer> entry : scenarioDistribution.entrySet()) {
        distribution.merge(entry.getKey(), entry.getValue(), Integer::sum);
      }
    }

    return distribution;
  }

  private String formatScenarioDurations() {
    List<String> parts = new ArrayList<>();
    for (BenchmarkScenarioId scenarioId : BenchmarkScenarioId.values()) {
      Long duration = scenarioDurationsMs.get(scenarioId);
      if (duration != null) {
        parts.add(scenarioId.getDisplayName() + '=' + BenchmarkMessenger.formatDuration(duration));
      }
    }

    return String.join(" | ", parts);
  }

  private void appendDistributionDetails(List<Component> lines,
    Map<MsptBucket, Integer> baselineMsptDistribution,
    Map<MsptBucket, Integer> activeMsptDistribution,
    Map<FineMsptBucket, Integer> baselineFineMsptDistribution,
    Map<FineMsptBucket, Integer> activeFineMsptDistribution,
    Map<ServerLoadLevel, Integer> baselineLoadDistribution,
    Map<ServerLoadLevel, Integer> activeLoadDistribution) {
    lines.add(Component.literal("MSPT distribution:         Baseline   Active")
      .withStyle(ChatFormatting.GRAY));
    for (MsptBucket bucket : MsptBucket.values()) {
      if (!hasDistributionData(baselineMsptDistribution, activeMsptDistribution, bucket)) {
        continue;
      }
      lines.add(distributionLine(bucket.getLabel(),
        percentage(baselineMsptDistribution, bucket),
        percentage(activeMsptDistribution, bucket)));
    }

    lines.add(Component.literal("Fine MSPT distribution:    Baseline   Active")
      .withStyle(ChatFormatting.GRAY));
    for (FineMsptBucket bucket : FineMsptBucket.values()) {
      if (!hasDistributionData(baselineFineMsptDistribution, activeFineMsptDistribution, bucket)) {
        continue;
      }

      lines.add(distributionLine(bucket.getLabel(),
        percentage(baselineFineMsptDistribution, bucket),
        percentage(activeFineMsptDistribution, bucket)));
    }

    lines.add(Component.literal("Load distribution:         Baseline   Active")
      .withStyle(ChatFormatting.GRAY));
    for (ServerLoadLevel loadLevel : ServerLoadLevel.values()) {
      if (!hasDistributionData(baselineLoadDistribution, activeLoadDistribution, loadLevel)) {
        continue;
      }

      lines.add(distributionLine(loadLevel.name(),
        percentage(baselineLoadDistribution, loadLevel),
        percentage(activeLoadDistribution, loadLevel)));
    }
  }

  private void appendMarkdownDistributionRows(List<String> lines,
    Map<MsptBucket, Integer> baselineMsptDistribution,
    Map<MsptBucket, Integer> activeMsptDistribution,
    Map<FineMsptBucket, Integer> baselineFineMsptDistribution,
    Map<FineMsptBucket, Integer> activeFineMsptDistribution,
    Map<ServerLoadLevel, Integer> baselineLoadDistribution,
    Map<ServerLoadLevel, Integer> activeLoadDistribution) {
    lines.add("**MSPT distribution:**");
    lines.add("");
    lines.add("| Bucket | Baseline | Active |");
    lines.add("|--------|---------:|-------:|");
    for (MsptBucket bucket : MsptBucket.values()) {
      if (!hasDistributionData(baselineMsptDistribution, activeMsptDistribution, bucket)) {
        continue;
      }

      lines.add(markdownDistributionLine(
        bucket.getLabel(),
        percentage(baselineMsptDistribution, bucket),
        percentage(activeMsptDistribution, bucket)));
    }

    lines.add("");
    lines.add("**Fine MSPT distribution:**");
    lines.add("");
    lines.add("| Bucket | Baseline | Active |");
    lines.add("|--------|---------:|-------:|");
    for (FineMsptBucket bucket : FineMsptBucket.values()) {
      if (!hasDistributionData(baselineFineMsptDistribution, activeFineMsptDistribution, bucket)) {
        continue;
      }

      lines.add(markdownDistributionLine(
        bucket.getLabel(),
        percentage(baselineFineMsptDistribution, bucket),
        percentage(activeFineMsptDistribution, bucket)));
    }

    lines.add("");
    lines.add("**Load distribution:**");
    lines.add("");
    lines.add("| Load level | Baseline | Active |");
    lines.add("|------------|---------:|-------:|");
    for (ServerLoadLevel loadLevel : ServerLoadLevel.values()) {
      if (!hasDistributionData(baselineLoadDistribution, activeLoadDistribution, loadLevel)) {
        continue;
      }

      lines.add(markdownDistributionLine(
        loadLevel.name(),
        percentage(baselineLoadDistribution, loadLevel),
        percentage(activeLoadDistribution, loadLevel)));
    }
  }

  private void appendMarkdownActivityDetails(List<String> lines,
    BenchmarkScenarioResult.PhaseResult phaseResult) {
    PerformanceStats.Snapshot snapshot = phaseResult.statsDelta();
    long spawnTotal = snapshot.mobSpawnChecks() + snapshot.mobSpawnsExcluded();
    long manualExcluded = snapshot.trackingExcludedManualNamespace()
      + snapshot.trackingExcludedManualEntity();
    long autoExcluded = snapshot.trackingExcludedAutoNamespace()
      + snapshot.trackingExcludedAutoEntity();
    lines.add("- Spawn limiter: "
      + String.format(
      "total=%d, excluded=%d, checked=%d, denied=%d (%.1f%%), natural=%d/%d blocked (%.1f%%), bonuses=%d",
      spawnTotal,
      snapshot.mobSpawnsExcluded(),
      snapshot.mobSpawnChecks(),
      snapshot.mobSpawnsDenied(),
      ratio(snapshot.mobSpawnsDenied(), Math.max(1L, spawnTotal)),
      snapshot.naturalSpawnsDenied(),
      snapshot.naturalSpawnChecks(),
      ratio(snapshot.naturalSpawnsDenied(), Math.max(1L, snapshot.naturalSpawnChecks())),
      snapshot.specialSpawnBonusesApplied()));
    lines.add("- Item control: "
      + String.format("merged=%d, removed=%d", snapshot.itemsMerged(), snapshot.itemsRemoved()));
    lines.add("- XP control: "
      + String.format("merged=%d, removed=%d", snapshot.xpOrbsMerged(), snapshot.xpOrbsRemoved()));
    lines.add("- Mob farm cleanup: "
      + String.format("removed=%d", snapshot.entityChunkCleanupRemoved()));
    lines.add("- Game rules: "
      + String.format("changes=%d", snapshot.gameRulesChanged()));
    lines.add("- Distance control: "
      + String.format(
      "view changes=%d, sim changes=%d, movement lowers=%d, movement samples=%d, max reduction=%d",
      snapshot.viewDistanceChanges(),
      snapshot.simulationDistanceChanges(),
      snapshot.simulationDistanceMovementAdjustments(),
      snapshot.simulationDistanceMovementThrottleSamples(),
      snapshot.simulationDistanceMovementMaxReduction()));
    lines.add(
      "- Distance state start: " + formatDistanceControlState(phaseResult.distanceControlStart()));
    lines.add("- Distance state end: "
      + formatDistanceControlState(phaseResult.distanceControlEnd()));
    lines.add("- Tracking: "
      + String.format(
      "evaluated=%d, tracked=%d, manual excludes=%d, auto excludes=%d, cache=%d, protected living=%d, protected persistent=%d",
      snapshot.trackingEvaluations(),
      snapshot.trackingTracked(),
      manualExcluded,
      autoExcluded,
      snapshot.trackingExcludedEarlyCache(),
      snapshot.trackingProtectedLiving(),
      snapshot.trackingProtectedPersistent()));
  }
}
