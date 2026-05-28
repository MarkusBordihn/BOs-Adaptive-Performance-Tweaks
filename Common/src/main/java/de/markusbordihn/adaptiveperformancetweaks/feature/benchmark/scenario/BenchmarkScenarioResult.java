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

package de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario;

import de.markusbordihn.adaptiveperformancetweaks.core.server.MsptBucket;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.util.Map;

public record BenchmarkScenarioResult(
  BenchmarkScenarioId scenarioId,
  PhaseResult baseline,
  PhaseResult active) {

  private static double calculateFastRatio(Map<MsptBucket, Integer> distribution) {
    int total = distribution.values().stream().mapToInt(Integer::intValue).sum();
    if (total == 0) {
      return 0.0d;
    }

    int fastCount = distribution.getOrDefault(MsptBucket.UNDER_5_MS, 0)
      + distribution.getOrDefault(MsptBucket.FROM_5_TO_10_MS, 0);
    return 100.0d * fastCount / total;
  }

  private static double calculatePerformanceScore(double avgTickMs, double p95TickMs) {
    return Math.max(0.0d, Math.min(100.0d, 100.0d - avgTickMs - p95TickMs));
  }

  private static double calculateHeadroomPercent(double avgTickMs) {
    return (50.0d - avgTickMs) / 50.0d * 100.0d;
  }

  public String displayName() {
    return this.scenarioId.getDisplayName();
  }

  public double tickTimeImprovementPercent() {
    return baseline.avgTickMs() == 0.0d ? 0.0d
      : (baseline.avgTickMs() - active.avgTickMs()) / baseline.avgTickMs() * 100.0d;
  }

  public double baselineHeadroomPercent() {
    return calculateHeadroomPercent(baseline.avgTickMs());
  }

  public double activeHeadroomPercent() {
    return calculateHeadroomPercent(active.avgTickMs());
  }

  public double baselinePerformanceScore() {
    return calculatePerformanceScore(baseline.avgTickMs(), baseline.p95TickMs());
  }

  public double activePerformanceScore() {
    return calculatePerformanceScore(active.avgTickMs(), active.p95TickMs());
  }

  public double baselineFastRatio() {
    return calculateFastRatio(baseline.msptDistribution());
  }

  public double activeFastRatio() {
    return calculateFastRatio(active.msptDistribution());
  }

  public record PhaseResult(
    long measurementDurationMs,
    double avgTickMs,
    double minTickMs,
    double p95TickMs,
    double maxTickMs,
    Map<ServerLoadLevel, Integer> loadDistribution,
    Map<MsptBucket, Integer> msptDistribution,
    long heapUsedBytes,
    int entityCount,
    double avgCpuPercent,
    double maxCpuPercent,
    PerformanceStats.Snapshot statsDelta) {

  }
}
