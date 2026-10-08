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

package de.markusbordihn.adaptiveperformancetweaks.clienttest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClientBuilder;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.GameClientExtension;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.RuntimeProfile;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.RegisterExtension;

class BenchmarkClientTest {

  private static final String SUITE_NAME = "Adaptive Performance Tweaks";
  private static final String WORLD_NAME = "Adaptive Performance Tweaks Client Test";
  private static final String GAME_DIRECTORY_PROPERTY = "clientruntime.gameDirectory";
  private static final String BENCHMARK_COMMAND_PROPERTY = "aptweaks.benchmarkCommand";
  private static final String SHORT_BENCHMARK_COMMAND =
      "aptweaks benchmark start scenario general 30";
  private static final Path LATEST_LOG = Path.of("logs", "latest.log");
  private static final String REPORT_TITLE = "# Adaptive Performance Tweaks Benchmark Report";
  private static final String BENCHMARK_SAVED_MARKER = "Benchmark result saved to: ";
  private static final String BENCHMARK_SAVE_FAILED_MARKER = "Failed to save benchmark result";
  private static final String DIAGNOSTICS_LAST_LINE_MARKER =
      "[Diagnostics] Orphaned entities removed since start";
  private static final Pattern DIAGNOSTICS_MAP_LINE =
      Pattern.compile("\\[Diagnostics] (\\S+): keys=\\d+ entries=\\d+ stale=\\d+ orphaned=(\\d+)");
  private static final String MOD_STACK_FRAME_MARKER =
      "at de.markusbordihn.adaptiveperformancetweaks.";
  private static final Duration WORLD_TIMEOUT = Duration.ofMinutes(3);
  private static final Duration BENCHMARK_TIMEOUT = Duration.ofMinutes(30);
  private static final Duration DIAGNOSTICS_TIMEOUT = Duration.ofSeconds(30);
  private static final long POLL_INTERVAL_TICKS = 40L;

  @RegisterExtension
  static final GameClientExtension client =
      GameClientExtension.shared(BenchmarkClientTest::configureLaunch);

  private static GameClientBuilder configureLaunch(GameClientBuilder builder) {
    return builder
        .withSuiteName(SUITE_NAME)
        .withRuntimeProfile(RuntimeProfile.BENCHMARK)
        .withAllowedCommands("aptweaks")
        .withWorld(Parameters.of("name", WORLD_NAME));
  }

  private static List<String> readLatestLog() {
    Path latestLog = Path.of(System.getProperty(GAME_DIRECTORY_PROPERTY, "")).resolve(LATEST_LOG);
    if (!Files.isRegularFile(latestLog)) {
      return List.of();
    }

    try {
      return new String(Files.readAllBytes(latestLog), StandardCharsets.UTF_8).lines().toList();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static Optional<String> findLastLogLine(String marker) {
    return readLatestLog().stream()
        .filter(line -> line.contains(marker))
        .reduce((earlierLine, laterLine) -> laterLine);
  }

  private static <T> T awaitPresent(Duration timeout, Supplier<Optional<T>> probe, String reason) {
    Instant deadline = Instant.now().plus(timeout);
    while (Instant.now().isBefore(deadline)) {
      Optional<T> result = probe.get();
      if (result.isPresent()) {
        return result.get();
      }
      client.await(Until.ticksElapsed(POLL_INTERVAL_TICKS));
    }
    return fail(reason + " within " + timeout.toSeconds() + " seconds.");
  }

  @Test
  @DisplayName("A benchmark run writes its report and leaves no orphaned tracked entities")
  void benchmarkWritesReportWithoutOrphanedEntities() throws IOException {
    client.await(WORLD_TIMEOUT, Until.worldLoaded(true), Until.playerAvailable(true));

    client.runCommand(System.getProperty(BENCHMARK_COMMAND_PROPERTY, SHORT_BENCHMARK_COMMAND));
    client.runCommand("aptweaks benchmark confirm");
    String savedLine =
        awaitPresent(
            BENCHMARK_TIMEOUT,
            () -> findLastLogLine(BENCHMARK_SAVED_MARKER),
            "The benchmark saved no report");
    int reportPathStart =
        savedLine.indexOf(BENCHMARK_SAVED_MARKER) + BENCHMARK_SAVED_MARKER.length();
    Path report = Path.of(savedLine.substring(reportPathStart).trim());
    assertTrue(
        Files.readString(report, StandardCharsets.UTF_8).startsWith(REPORT_TITLE),
        "The benchmark report " + report + " has no report title.");
    assertEquals(Optional.empty(), findLastLogLine(BENCHMARK_SAVE_FAILED_MARKER));

    client.runCommand("aptweaks diagnostics");
    awaitPresent(
        DIAGNOSTICS_TIMEOUT,
        () -> findLastLogLine(DIAGNOSTICS_LAST_LINE_MARKER),
        "The diagnostics report did not reach the log");
    List<String> mapsWithOrphanedEntities =
        readLatestLog().stream()
            .map(DIAGNOSTICS_MAP_LINE::matcher)
            .filter(Matcher::find)
            .filter(mapLine -> !mapLine.group(2).equals("0"))
            .map(mapLine -> mapLine.group(1) + " orphaned=" + mapLine.group(2))
            .toList();
    assertEquals(List.of(), mapsWithOrphanedEntities);

    List<String> modStackFrames =
        readLatestLog().stream().filter(line -> line.contains(MOD_STACK_FRAME_MARKER)).toList();
    assertEquals(List.of(), modStackFrames);
  }
}
