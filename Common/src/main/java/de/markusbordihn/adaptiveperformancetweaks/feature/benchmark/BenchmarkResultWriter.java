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
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import net.minecraft.SharedConstants;
import org.apache.logging.log4j.Logger;

final class BenchmarkResultWriter {

  private static final DateTimeFormatter TIMESTAMP_FORMATTER =
    DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss");

  private BenchmarkResultWriter() {
  }

  static Path save(BenchmarkCompareResult result, Logger log) {
    if (result == null) {
      return null;
    }

    try {
      String mcVersion = SharedConstants.getCurrentVersion().name();
      String loader = BenchmarkManager.detectLoader();
      String modVersion = BenchmarkManager.resolveModVersion();
      String timestamp = TIMESTAMP_FORMATTER.format(
        LocalDateTime.ofInstant(result.timestamp(), ZoneId.systemDefault()));
      String filename = buildBenchmarkFilename(timestamp, mcVersion, loader, modVersion);
      Files.createDirectories(Constants.BENCHMARK_DIR);
      Path path = Constants.BENCHMARK_DIR.resolve(filename);
      List<String> report = buildMarkdownReport(result, mcVersion, loader, modVersion);
      CompletableFuture.runAsync(() -> {
        try {
          Files.write(path, report, StandardCharsets.UTF_8,
            StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
          log.info("Benchmark result saved to: {}", path);
        } catch (IOException exception) {
          log.warn("Failed to save benchmark result: {}", exception.getMessage());
        }
      });
      return path;
    } catch (IOException exception) {
      log.warn("Failed to save benchmark result: {}", exception.getMessage());
      return null;
    }
  }

  private static String buildBenchmarkFilename(
    String timestamp, String mcVersion, String loader, String modVersion) {
    if (modVersion == null || modVersion.isBlank()) {
      return String.format("benchmark_%s_mc%s_%s.md", timestamp, mcVersion, loader);
    }

    return String.format("benchmark_%s_mc%s_%s_v%s.md", timestamp, mcVersion, loader, modVersion);
  }

  static List<String> buildMarkdownReport(
    BenchmarkCompareResult result, String mcVersion, String loader, String modVersion) {
    List<String> sourceLines = result.formatMarkdown();
    List<String> lines = new ArrayList<>(sourceLines.size() + 1);
    if (sourceLines.isEmpty()) {
      return lines;
    }

    lines.add(sourceLines.get(0));
    String serverVersionLine = buildServerVersionLine(mcVersion, loader, modVersion);
    if (!serverVersionLine.isBlank()) {
      lines.add(serverVersionLine);
    }

    int startIndex = sourceLines.size() > 1 && sourceLines.get(1).isBlank() ? 2 : 1;
    for (int index = startIndex; index < sourceLines.size(); index++) {
      lines.add(sourceLines.get(index));
    }
    return lines;
  }

  private static String buildServerVersionLine(
    String mcVersion, String loader, String modVersion) {
    StringBuilder line = new StringBuilder("- Server version: Minecraft ")
      .append(mcVersion);
    if (loader != null && !loader.isBlank()) {
      line.append(" / ").append(loader);
    }
    if (modVersion != null && !modVersion.isBlank()) {
      line.append(" / APTweaks ").append(modVersion);
    }
    return line.toString();
  }
}
