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
import de.markusbordihn.adaptiveperformancetweaks.core.debug.DebugManager;
import de.markusbordihn.adaptiveperformancetweaks.core.debug.DebugModule;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.lang.management.ManagementFactory;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.GameType;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class BenchmarkManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final long DEFAULT_PHASE_DURATION_MS = 240_000L;
  private static final long TRANSITION_DURATION_MS = 7_000L;
  private static final long SAMPLE_INTERVAL_MS = 5_000L;
  private static final long STATUS_INTERVAL_MS = 30_000L;
  private static final long MOVE_INTERVAL_MS = 10_000L;
  private static final long CONFIRM_TIMEOUT_MS = 120_000L;
  private static final int MOVE_AREA_HALF_SIZE = 10_000;
  private static final int MOVE_WAYPOINT_COUNT = 60;
  private static final double TELEPORT_Y = 100.0;
  private static final List<Double> baselineSamples = new ArrayList<>();
  private static final List<Double> activeSamples = new ArrayList<>();
  private static final List<Double> baselineCpuSamples = new ArrayList<>();
  private static final List<Double> activeCpuSamples = new ArrayList<>();
  private static final EnumMap<ServerLoadLevel, Integer> baselineLoadDist =
    new EnumMap<>(ServerLoadLevel.class);
  private static final EnumMap<ServerLoadLevel, Integer> activeLoadDist =
    new EnumMap<>(ServerLoadLevel.class);
  private static final EnumMap<FeatureToggle, Boolean> savedFeatureState =
    new EnumMap<>(FeatureToggle.class);
  private static final EnumMap<DebugModule, Boolean> savedDebugStates =
    new EnumMap<>(DebugModule.class);
  private static BenchmarkState state = BenchmarkState.IDLE;
  private static long phaseDurationMs = DEFAULT_PHASE_DURATION_MS;
  private static boolean autoMove = false;
  private static ServerPlayer pendingConfirmPlayer;
  private static ServerPlayer benchmarkPlayer;
  private static Vec3 playerStartPos;
  private static List<Vec3> baselineMoveWaypoints = new ArrayList<>();
  private static List<Vec3> activeMoveWaypoints = new ArrayList<>();
  private static int currentWaypointIndex;
  private static long phaseStartMs;
  private static long transitionStartMs;
  private static long lastSampleMs;
  private static long lastStatusMs;
  private static long lastMoveMs;
  private static long baselineHeapUsed;
  private static long activeHeapUsed;
  private static int baselineEntityCount;
  private static int activeEntityCount;
  private static GameType savedGameMode;
  private static double lastCpuPercent = -1.0;
  private static BenchmarkCompareResult lastResult;

  private BenchmarkManager() {
  }

  public static void requestStart(ServerPlayer player, long phaseSecs, boolean withAutoMove) {
    if (state != BenchmarkState.IDLE) {
      sendMessage(player,
        "Benchmark is already running. Use /aptweaks benchmark cancel to stop it.");
      return;
    }

    phaseDurationMs = Math.max(30_000L, phaseSecs * 1000L);
    autoMove = withAutoMove;
    pendingConfirmPlayer = player;
    phaseStartMs = System.currentTimeMillis();
    state = BenchmarkState.PENDING_CONFIRM;

    long mins = phaseSecs / 60;
    long secs = phaseSecs % 60;
    String durationStr =
      mins > 0 ? String.format("%dm %ds", mins, secs) : String.format("%ds", secs);

    sendMessage(player, "=== APTweaks Benchmark - WARNING ===");
    sendMessage(player, String.format("Runtime: ~%s per phase (2 phases total)", durationStr));
    sendMessage(player, "Phase 1: All mod features DISABLED (baseline)");
    sendMessage(player, "Phase 2: All mod features ENABLED (active)");
    if (autoMove) {
      sendMessage(player,
        "[Move] Character will be teleported to random positions (+/-" + MOVE_AREA_HALF_SIZE
          + " blocks) to trigger chunk loading.");
    }
    sendMessage(player, "WARNING: Only run this in a test world!");
    sendMessage(player, "Confirm: /aptweaks benchmark confirm");
    sendMessage(player, "Cancel:  /aptweaks benchmark cancel");
    sendMessage(player, "(Expires in 120 seconds)");
  }

  public static void confirm(ServerPlayer player) {
    if (state != BenchmarkState.PENDING_CONFIRM) {
      sendMessage(player, "No benchmark is waiting for confirmation.");
      return;
    }

    benchmarkPlayer = player;
    playerStartPos = player.position();
    if (autoMove) {
      Set<Long> reservedChunkKeys = new HashSet<>(MOVE_WAYPOINT_COUNT * 2);
      baselineMoveWaypoints = computeWaypoints(playerStartPos, reservedChunkKeys);
      activeMoveWaypoints = computeWaypoints(playerStartPos, reservedChunkKeys);
      currentWaypointIndex = 0;
    }

    baselineSamples.clear();
    activeSamples.clear();
    baselineCpuSamples.clear();
    activeCpuSamples.clear();
    baselineLoadDist.clear();
    activeLoadDist.clear();
    savedFeatureState.clear();

    saveFeatureState();
    disableAllFeatures();
    saveAllDebugStates();
    boolean anyDebugActive = savedDebugStates.containsValue(true);
    disableAllDebug();
    if (anyDebugActive) {
      sendMessage(player,
        "[Benchmark] Debug logging disabled for all modules for accurate results.");
    }
    savedGameMode = player.gameMode.getGameModeForPlayer();
    if (savedGameMode != GameType.CREATIVE) {
      player.setGameMode(GameType.CREATIVE);
      sendMessage(player, "[Benchmark] Switched to Creative mode for teleportation safety.");
    }
    PerformanceStats.reset();

    long now = System.currentTimeMillis();
    phaseStartMs = now;
    lastSampleMs = now;
    lastStatusMs = now;
    lastMoveMs = now;
    state = BenchmarkState.PHASE_BASELINE;

    log.info("Benchmark Phase 1 (Baseline) started by {}", player.getName().getString());
    sendMessage(player, "Benchmark Phase 1/2 (Baseline) started - mod features disabled.");
    sendMessage(player, "Do not move! Phase duration: " + formatDuration(phaseDurationMs));
    if (autoMove && !baselineMoveWaypoints.isEmpty()) {
      sendMessage(player, String.format(
        "[Benchmark] Auto-move uses %,d unique random chunks in Phase 1 and %,d new chunks in Phase 2.",
        baselineMoveWaypoints.size(), activeMoveWaypoints.size()));
      teleportToNextWaypoint(player, baselineMoveWaypoints);
    }
  }

  public static void cancel(ServerPlayer player) {
    if (state == BenchmarkState.IDLE) {
      sendMessage(player, "No benchmark is running.");
      return;
    }

    BenchmarkState prevState = state;
    state = BenchmarkState.IDLE;
    restoreFeatures();
    restoreDebugState();
    restoreGameMode(benchmarkPlayer);
    if ((prevState == BenchmarkState.PHASE_BASELINE || prevState == BenchmarkState.PHASE_TRANSITION
      || prevState == BenchmarkState.PHASE_ACTIVE)
      && benchmarkPlayer != null && playerStartPos != null) {
      teleportToPos(benchmarkPlayer, playerStartPos);
    }
    clearSessionState();
    sendMessage(player, "Benchmark cancelled. Features restored.");
    log.info("Benchmark cancelled by {}", player.getName().getString());
  }

  public static void onServerTick() {
    long now = System.currentTimeMillis();

    if (state == BenchmarkState.PENDING_CONFIRM) {
      if (now - phaseStartMs > CONFIRM_TIMEOUT_MS) {
        state = BenchmarkState.IDLE;
        if (pendingConfirmPlayer != null) {
          sendMessage(pendingConfirmPlayer, "Benchmark confirmation timed out.");
        }
        pendingConfirmPlayer = null;
      }
      return;
    }

    if (state == BenchmarkState.PHASE_TRANSITION) {
      if (now - lastStatusMs >= 3_000L && benchmarkPlayer != null) {
        long remaining = TRANSITION_DURATION_MS - (now - transitionStartMs);
        if (remaining > 0) {
          sendMessage(benchmarkPlayer, String.format(
            "[APTweaks Benchmark] Starting Phase 2 in %s ...", formatDuration(remaining)));
        }
        lastStatusMs = now;
      }
      if (now - transitionStartMs >= TRANSITION_DURATION_MS) {
        startPhaseActive(now);
      }
      return;
    }

    if (state != BenchmarkState.PHASE_BASELINE && state != BenchmarkState.PHASE_ACTIVE) {
      return;
    }

    List<Double> samples = state == BenchmarkState.PHASE_BASELINE ? baselineSamples : activeSamples;
    List<Double> cpuSamples =
      state == BenchmarkState.PHASE_BASELINE ? baselineCpuSamples : activeCpuSamples;
    EnumMap<ServerLoadLevel, Integer> loadDist =
      state == BenchmarkState.PHASE_BASELINE ? baselineLoadDist : activeLoadDist;

    if (now - lastSampleMs >= SAMPLE_INTERVAL_MS) {
      samples.add((double) ServerManager.getAverageTickTime());
      lastCpuPercent = getProcessCpuPercent();
      if (lastCpuPercent >= 0) {
        cpuSamples.add(lastCpuPercent);
      }
      loadDist.merge(ServerLoad.getCurrentServerLoad(), 1, Integer::sum);
      lastSampleMs = now;
    }

    if (now - lastStatusMs >= STATUS_INTERVAL_MS) {
      sendStatusUpdate(now);
      lastStatusMs = now;
    }

    List<Vec3> moveWaypoints =
      state == BenchmarkState.PHASE_BASELINE ? baselineMoveWaypoints : activeMoveWaypoints;
    if (autoMove && benchmarkPlayer != null && !moveWaypoints.isEmpty()
      && now - lastMoveMs >= MOVE_INTERVAL_MS) {
      teleportToNextWaypoint(benchmarkPlayer, moveWaypoints);
      lastMoveMs = now;
    }

    if (now - phaseStartMs >= phaseDurationMs) {
      if (state == BenchmarkState.PHASE_BASELINE) {
        finalizeBaseline(now);
      } else {
        finalizeActive();
      }
    }
  }

  public static boolean isRunning() {
    return state != BenchmarkState.IDLE;
  }

  public static String getStatusMessage() {
    return switch (state) {
      case IDLE -> "No benchmark running. Use /aptweaks benchmark start";
      case PENDING_CONFIRM ->
        "Waiting for confirmation. Use /aptweaks benchmark confirm or cancel.";
      case PHASE_BASELINE -> "Phase 1/2 (Baseline) in progress.";
      case PHASE_TRANSITION -> "Transitioning to Phase 2 ...";
      case PHASE_ACTIVE -> "Phase 2/2 (Active) in progress.";
      case COMPLETE -> "Last benchmark complete. Use /aptweaks benchmark report.";
    };
  }

  public static BenchmarkCompareResult getLastResult() {
    return lastResult;
  }

  public static void reset() {
    if (state != BenchmarkState.IDLE && state != BenchmarkState.COMPLETE) {
      restoreFeatures();
      restoreDebugState();
      restoreGameMode(benchmarkPlayer);
    }
    clearSessionState();
    state = BenchmarkState.IDLE;
  }

  private static void finalizeBaseline(long now) {
    baselineHeapUsed = ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed();
    baselineEntityCount = countEntities();

    restoreFeatures();
    enableAllFeatures();
    disableAllDebug();
    PerformanceStats.reset();

    transitionStartMs = now;
    lastStatusMs = 0;
    state = BenchmarkState.PHASE_TRANSITION;

    log.info("Benchmark Phase 1 (Baseline) complete. Transitioning to Phase 2 in {}s ...",
      TRANSITION_DURATION_MS / 1000);
    if (benchmarkPlayer != null) {
      sendMessage(benchmarkPlayer, String.format(
        "Phase 1 complete - mod features re-enabled. Starting Phase 2 in %s ...",
        formatDuration(TRANSITION_DURATION_MS)));
    }
  }

  private static void startPhaseActive(long now) {
    phaseStartMs = now;
    lastSampleMs = now;
    lastStatusMs = now;
    lastMoveMs = now;

    if (autoMove && benchmarkPlayer != null && !activeMoveWaypoints.isEmpty()) {
      currentWaypointIndex = 0;
      teleportToNextWaypoint(benchmarkPlayer, activeMoveWaypoints);
    }

    state = BenchmarkState.PHASE_ACTIVE;

    log.info("Benchmark Phase 2 (Active) started");
    if (benchmarkPlayer != null) {
      sendMessage(benchmarkPlayer, "Benchmark Phase 2/2 (Active) started - mod features enabled.");
      sendMessage(benchmarkPlayer, "Phase duration: " + formatDuration(phaseDurationMs));
      if (autoMove && !activeMoveWaypoints.isEmpty()) {
        sendMessage(benchmarkPlayer, String.format(
          "[Benchmark] Auto-move switched to %,d fresh random chunks for Phase 2.",
          activeMoveWaypoints.size()));
      }
    }
  }

  private static void finalizeActive() {
    activeHeapUsed = ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed();
    activeEntityCount = countEntities();

    PerformanceStats.Snapshot activeDelta = PerformanceStats.snapshot();

    double baselineAvg = average(baselineSamples);
    double activeAvg = average(activeSamples);
    double improvement = baselineAvg == 0 ? 0.0 : (baselineAvg - activeAvg) / baselineAvg * 100.0;

    double baselineAvgCpu = average(baselineCpuSamples, -1.0);
    double baselineMaxCpu = max(baselineCpuSamples, -1.0);
    double activeAvgCpu = average(activeCpuSamples, -1.0);
    double activeMaxCpu = max(activeCpuSamples, -1.0);

    lastResult = new BenchmarkCompareResult(
      phaseDurationMs,
      baselineAvg, min(baselineSamples), percentile(baselineSamples, 95), max(baselineSamples),
      Map.copyOf(baselineLoadDist),
      baselineHeapUsed, baselineEntityCount,
      baselineAvgCpu, baselineMaxCpu,
      activeAvg, min(activeSamples), percentile(activeSamples, 95), max(activeSamples),
      Map.copyOf(activeLoadDist),
      activeHeapUsed, activeEntityCount,
      activeAvgCpu, activeMaxCpu,
      autoMove, baselineMoveWaypoints.size(), activeMoveWaypoints.size(),
      countSharedChunkTargets(baselineMoveWaypoints, activeMoveWaypoints),
      activeDelta, improvement, Instant.now());

    restoreFeatures();
    restoreDebugState();

    ServerPlayer player = benchmarkPlayer;
    Vec3 startPos = playerStartPos;
    restoreGameMode(player);

    state = BenchmarkState.COMPLETE;
    clearSessionState();

    if (player != null && startPos != null) {
      teleportToPos(player, startPos);
    }

    log.info("Benchmark complete. Tick time improvement: {}%", String.format("%.1f", improvement));
    for (Component line : lastResult.format()) {
      log.info(line.getString());
    }

    if (player != null) {
      sendMessage(player, "Benchmark complete! Use /aptweaks benchmark report to view results.");
      for (Component line : lastResult.format()) {
        sendMessage(player, line);
      }
    }
  }

  private static void sendStatusUpdate(long now) {
    if (benchmarkPlayer == null) {
      return;
    }

    int phase = state == BenchmarkState.PHASE_BASELINE ? 1 : 2;
    String phaseName = phase == 1 ? "Baseline" : "Active";
    long remainingMs = phaseDurationMs - (now - phaseStartMs);
    double mspt = ServerManager.getAverageTickTime();
    double headroom = (50.0 - mspt) / 50.0 * 100.0;
    String cpuStr = lastCpuPercent >= 0 ? String.format(" | CPU: %.0f%%", lastCpuPercent) : "";
    sendMessage(benchmarkPlayer, String.format(
      "[APTweaks Benchmark] Phase %d/2 (%s): %s remaining | MSPT: %.1fms (headroom: %.0f%%)%s | Do not move!",
      phase, phaseName, formatDuration(remainingMs), mspt, headroom, cpuStr));
  }

  private static void saveFeatureState() {
    for (FeatureToggle t : FeatureToggle.values()) {
      savedFeatureState.put(t, t.isEnabled());
    }
  }

  private static void disableAllFeatures() {
    for (FeatureToggle t : FeatureToggle.values()) {
      if (t != FeatureToggle.CORE) {
        t.setEnabled(false);
      }
    }
  }

  private static void enableAllFeatures() {
    for (FeatureToggle t : FeatureToggle.values()) {
      if (t != FeatureToggle.CORE) {
        t.setEnabled(true);
      }
    }
  }

  private static void restoreFeatures() {
    savedFeatureState.forEach(FeatureToggle::setEnabled);
  }

  private static void saveAllDebugStates() {
    savedDebugStates.clear();
    for (DebugModule module : DebugModule.values()) {
      savedDebugStates.put(module, DebugManager.isDebugLevel(module.getLoggerName()));
    }
  }

  private static void disableAllDebug() {
    for (DebugModule module : DebugModule.values()) {
      DebugManager.enableDebugLevel(module.getLoggerName(), false);
    }
  }

  private static void restoreDebugState() {
    savedDebugStates.forEach((module, wasEnabled) -> {
      if (wasEnabled) {
        DebugManager.enableDebugLevel(module.getLoggerName(), true);
      }
    });
    savedDebugStates.clear();
  }

  private static void clearSessionState() {
    benchmarkPlayer = null;
    pendingConfirmPlayer = null;
    playerStartPos = null;
    savedGameMode = null;
    lastCpuPercent = -1.0;
    baselineMoveWaypoints.clear();
    activeMoveWaypoints.clear();
    currentWaypointIndex = 0;
    baselineSamples.clear();
    activeSamples.clear();
    baselineCpuSamples.clear();
    activeCpuSamples.clear();
    baselineLoadDist.clear();
    activeLoadDist.clear();
    savedFeatureState.clear();
    savedDebugStates.clear();
  }

  private static List<Vec3> computeWaypoints(Vec3 origin, Set<Long> reservedChunkKeys) {
    List<Vec3> points = new ArrayList<>(MOVE_WAYPOINT_COUNT);
    Random rng = new Random();
    int originChunkX = blockToChunk(origin.x);
    int originChunkZ = blockToChunk(origin.z);
    int moveAreaHalfChunks = Math.max(1, MOVE_AREA_HALF_SIZE >> 4);
    int maxAttempts = MOVE_WAYPOINT_COUNT * 50;
    int attempts = 0;
    while (points.size() < MOVE_WAYPOINT_COUNT && attempts++ < maxAttempts) {
      int chunkX =
        originChunkX + rng.nextInt(moveAreaHalfChunks * 2 + 1) - moveAreaHalfChunks;
      int chunkZ =
        originChunkZ + rng.nextInt(moveAreaHalfChunks * 2 + 1) - moveAreaHalfChunks;
      long chunkKey = ChunkPos.asLong(chunkX, chunkZ);
      if (!reservedChunkKeys.add(chunkKey)) {
        continue;
      }
      points.add(new Vec3(chunkX * 16.0 + 8.0, origin.y, chunkZ * 16.0 + 8.0));
    }
    return points;
  }

  private static int blockToChunk(double blockCoord) {
    return ((int) Math.floor(blockCoord)) >> 4;
  }

  private static int countSharedChunkTargets(List<Vec3> baselineTargets, List<Vec3> activeTargets) {
    Set<Long> baselineChunkKeys = new HashSet<>(baselineTargets.size());
    for (Vec3 target : baselineTargets) {
      baselineChunkKeys.add(getChunkKey(target));
    }
    int sharedTargets = 0;
    for (Vec3 target : activeTargets) {
      if (baselineChunkKeys.contains(getChunkKey(target))) {
        sharedTargets++;
      }
    }
    return sharedTargets;
  }

  private static long getChunkKey(Vec3 target) {
    return ChunkPos.asLong(blockToChunk(target.x), blockToChunk(target.z));
  }

  private static void teleportToNextWaypoint(ServerPlayer player, List<Vec3> moveWaypoints) {
    Vec3 target = moveWaypoints.get(currentWaypointIndex % moveWaypoints.size());
    currentWaypointIndex++;
    teleportToSurface(player, target);
  }

  private static void teleportToSurface(ServerPlayer player, Vec3 target) {
    player.teleportTo(target.x, TELEPORT_Y, target.z);
  }

  private static void teleportToPos(ServerPlayer player, Vec3 pos) {
    player.teleportTo(pos.x, pos.y, pos.z);
  }

  private static void restoreGameMode(ServerPlayer player) {
    if (player != null && savedGameMode != null && savedGameMode != GameType.CREATIVE) {
      player.setGameMode(savedGameMode);
    }
    savedGameMode = null;
  }

  private static int countEntities() {
    int count = 0;
    for (ServerLevel level : ServerManager.getAllLevels()) {
      for (var ignored : level.getAllEntities()) {
        count++;
      }
    }
    return count;
  }

  private static double getProcessCpuPercent() {
    try {
      java.lang.management.OperatingSystemMXBean osBean =
        ManagementFactory.getOperatingSystemMXBean();
      if (osBean instanceof com.sun.management.OperatingSystemMXBean sunBean) {
        double load = sunBean.getProcessCpuLoad();
        return load < 0 ? -1.0 : load * 100.0;
      }
    } catch (Exception ignored) {
      // Not available on this JVM
    }
    return -1.0;
  }

  private static double average(List<Double> samples) {
    return average(samples, 50.0);
  }

  private static double average(List<Double> samples, double fallback) {
    if (samples.isEmpty()) {
      return fallback;
    }

    double sum = 0;
    for (double sample : samples) {
      sum += sample;
    }
    return sum / samples.size();
  }

  private static double min(List<Double> samples) {
    return samples.stream().mapToDouble(Double::doubleValue).min().orElse(50.0);
  }

  private static double percentile(List<Double> samples, double p) {
    if (samples.isEmpty()) {
      return 50.0;
    }

    List<Double> sorted = new ArrayList<>(samples);
    Collections.sort(sorted);
    int index = (int) Math.ceil(p / 100.0 * sorted.size()) - 1;
    return sorted.get(Math.max(0, Math.min(index, sorted.size() - 1)));
  }

  private static double max(List<Double> samples) {
    return max(samples, 50.0);
  }

  private static double max(List<Double> samples, double fallback) {
    return samples.stream().mapToDouble(Double::doubleValue).max().orElse(fallback);
  }

  private static String formatDuration(long ms) {
    long secs = Math.max(0, ms / 1000);
    long mins = secs / 60;
    secs = secs % 60;
    if (mins > 0) {
      return String.format("%dm %ds", mins, secs);
    }
    return String.format("%ds", secs);
  }

  private static void sendMessage(ServerPlayer player, String message) {
    player.sendSystemMessage(Component.literal(message));
  }

  private static void sendMessage(ServerPlayer player, Component message) {
    player.sendSystemMessage(message);
  }

  private enum BenchmarkState {
    IDLE, PENDING_CONFIRM, PHASE_BASELINE, PHASE_TRANSITION, PHASE_ACTIVE, COMPLETE
  }

  public record BenchmarkCompareResult(
    long phaseDurationMs,
    double baselineAvgTick, double baselineMinTick, double baselineP95Tick, double baselineMaxTick,
    Map<ServerLoadLevel, Integer> baselineLoadDist,
    long baselineHeapUsed, int baselineEntityCount,
    double baselineAvgCpu, double baselineMaxCpu,
    double activeAvgTick, double activeMinTick, double activeP95Tick, double activeMaxTick,
    Map<ServerLoadLevel, Integer> activeLoadDist,
    long activeHeapUsed, int activeEntityCount,
    double activeAvgCpu, double activeMaxCpu,
    boolean autoMoveEnabled,
    int baselineMoveTargetCount, int activeMoveTargetCount,
    int sharedMoveTargetCount,
    PerformanceStats.Snapshot activeDelta,
    double tickTimeImprovementPercent,
    Instant timestamp) {

    private static String formatBytes(long bytes) {
      if (bytes >= 1_073_741_824L) {
        return String.format("%.2f GB", bytes / 1_073_741_824.0);
      } else if (bytes >= 1_048_576L) {
        return String.format("%.0f MB", bytes / 1_048_576.0);
      }
      return String.format("%.0f KB", bytes / 1024.0);
    }

    public List<Component> format() {
      List<Component> lines = new ArrayList<>();
      long durSecs = phaseDurationMs / 1000;
      double baselineHeadroom = (50.0 - baselineAvgTick) / 50.0 * 100.0;
      double activeHeadroom = (50.0 - activeAvgTick) / 50.0 * 100.0;
      double headroomDelta = activeHeadroom - baselineHeadroom;
      long heapDeltaBytes = activeHeapUsed - baselineHeapUsed;
      double cpuDelta = activeAvgCpu - baselineAvgCpu;

      lines.add(Component.literal(String.format("=== %s Benchmark Report ===", Constants.MOD_NAME))
        .withStyle(ChatFormatting.GOLD));
      lines.add(Component.literal(
        String.format("Phase duration: %dm %ds  |  %s", durSecs / 60, durSecs % 60, timestamp)));
      lines.add(Component.literal(""));
      lines.add(
        Component.literal(String.format("%-14s %-12s %-12s %s", "", "Baseline", "Active", "Delta"))
          .withStyle(ChatFormatting.GRAY));

      ChatFormatting msptColor = tickTimeImprovementPercent > 0 ? ChatFormatting.GREEN
        : tickTimeImprovementPercent < 0 ? ChatFormatting.RED : ChatFormatting.WHITE;
      lines.add(Component.literal(String.format("%-14s %-12s %-12s ", "Avg MSPT",
          String.format("%.1fms", baselineAvgTick), String.format("%.1fms", activeAvgTick)))
        .append(Component.literal(String.format("%+.1f%%", tickTimeImprovementPercent))
          .withStyle(msptColor)));

      ChatFormatting minMsptColor =
        activeMinTick <= baselineMinTick ? ChatFormatting.GREEN : ChatFormatting.RED;
      lines.add(Component.literal(
          String.format("%-14s %-12s ", "Min MSPT", String.format("%.1fms", baselineMinTick)))
        .append(Component.literal(String.format("%.1fms", activeMinTick)).withStyle(minMsptColor)));

      ChatFormatting p95Color =
        activeP95Tick <= baselineP95Tick ? ChatFormatting.GREEN : ChatFormatting.RED;
      lines.add(Component.literal(
          String.format("%-14s %-12s ", "P95 MSPT", String.format("%.1fms", baselineP95Tick)))
        .append(Component.literal(String.format("%.1fms", activeP95Tick)).withStyle(p95Color)));

      ChatFormatting maxMsptColor =
        activeMaxTick <= baselineMaxTick ? ChatFormatting.GREEN : ChatFormatting.RED;
      lines.add(Component.literal(
          String.format("%-14s %-12s ", "Max MSPT", String.format("%.1fms", baselineMaxTick)))
        .append(Component.literal(String.format("%.1fms", activeMaxTick)).withStyle(maxMsptColor)));

      ChatFormatting headroomColor = headroomDelta > 0 ? ChatFormatting.GREEN
        : headroomDelta < 0 ? ChatFormatting.RED : ChatFormatting.WHITE;
      lines.add(Component.literal(String.format("%-14s %-12s %-12s ", "Headroom",
          String.format("%.1f%%", baselineHeadroom), String.format("%.1f%%", activeHeadroom)))
        .append(
          Component.literal(String.format("%+.1fpp", headroomDelta)).withStyle(headroomColor)));

      if (baselineAvgCpu >= 0 && activeAvgCpu >= 0) {
        // CPU: green only if less CPU AND faster; red only if more CPU AND slower; otherwise neutral
        ChatFormatting cpuColor;
        if (cpuDelta < 0 && tickTimeImprovementPercent > 0) {
          cpuColor = ChatFormatting.GREEN;
        } else if (cpuDelta > 0 && tickTimeImprovementPercent < 0) {
          cpuColor = ChatFormatting.RED;
        } else {
          cpuColor = ChatFormatting.WHITE;
        }
        lines.add(Component.literal(String.format("%-14s %-12s %-12s ", "Avg CPU",
            String.format("%.1f%%", baselineAvgCpu), String.format("%.1f%%", activeAvgCpu)))
          .append(Component.literal(String.format("%+.1fpp", cpuDelta)).withStyle(cpuColor)));

        ChatFormatting maxCpuColor;
        if (activeMaxCpu < baselineMaxCpu && tickTimeImprovementPercent > 0) {
          maxCpuColor = ChatFormatting.GREEN;
        } else if (activeMaxCpu > baselineMaxCpu && tickTimeImprovementPercent < 0) {
          maxCpuColor = ChatFormatting.RED;
        } else {
          maxCpuColor = ChatFormatting.WHITE;
        }
        lines.add(Component.literal(
            String.format("%-14s %-12s ", "Max CPU", String.format("%.1f%%", baselineMaxCpu)))
          .append(Component.literal(String.format("%.1f%%", activeMaxCpu)).withStyle(maxCpuColor)));
      }

      // Heap: green only if less heap AND faster; red only if more heap AND slower; otherwise neutral
      ChatFormatting heapColor;
      if (heapDeltaBytes < 0 && tickTimeImprovementPercent > 0) {
        heapColor = ChatFormatting.GREEN;
      } else if (heapDeltaBytes > 0 && tickTimeImprovementPercent < 0) {
        heapColor = ChatFormatting.RED;
      } else {
        heapColor = ChatFormatting.WHITE;
      }
      lines.add(Component.literal(String.format("%-14s %-12s %-12s ", "Heap used",
          formatBytes(baselineHeapUsed), formatBytes(activeHeapUsed)))
        .append(Component.literal(
            (heapDeltaBytes >= 0 ? "+" : "") + formatBytes(Math.abs(heapDeltaBytes)))
          .withStyle(heapColor)));

      if (autoMoveEnabled) {
        lines.add(Component.literal(String.format(
          "%-14s %-12s %-12s overlap=%d",
          "Auto-move",
          String.format("%d chunks", baselineMoveTargetCount),
          String.format("%d chunks", activeMoveTargetCount),
          sharedMoveTargetCount)));
      }

      lines.add(Component.literal(""));
      lines.add(Component.literal("Load distribution:         Baseline   Active")
        .withStyle(ChatFormatting.GRAY));

      int baselineTotal = baselineLoadDist.values().stream().mapToInt(Integer::intValue).sum();
      int activeTotal = activeLoadDist.values().stream().mapToInt(Integer::intValue).sum();
      for (ServerLoadLevel level : ServerLoadLevel.values()) {
        int baselineCount = baselineLoadDist.getOrDefault(level, 0);
        int activeCount = activeLoadDist.getOrDefault(level, 0);
        if (baselineCount > 0 || activeCount > 0) {
          lines.add(Component.literal(String.format("  %-12s %6.0f%%     %6.0f%%",
            level.name(),
            baselineTotal == 0 ? 0.0 : 100.0 * baselineCount / baselineTotal,
            activeTotal == 0 ? 0.0 : 100.0 * activeCount / activeTotal)));
        }
      }

      lines.add(Component.literal(""));
      lines.add(Component.literal("--- Mod actions (Phase 2) ---").withStyle(ChatFormatting.GRAY));
      long spawnTotal = activeDelta.mobSpawnChecks() + activeDelta.mobSpawnsExcluded();
      lines.add(Component.literal(String.format(
        "Spawn checks:   %,d total  excluded: %,d  checked: %,d  denied: %,d (%.1f%%)",
        spawnTotal, activeDelta.mobSpawnsExcluded(), activeDelta.mobSpawnChecks(),
        activeDelta.mobSpawnsDenied(),
        spawnTotal == 0 ? 0.0 : 100.0 * activeDelta.mobSpawnsDenied() / spawnTotal)));
      lines.add(Component.literal(String.format("Natural spawns: %,d  blocked: %,d (%.1f%%)",
        activeDelta.naturalSpawnChecks(), activeDelta.naturalSpawnsDenied(),
        activeDelta.naturalSpawnChecks() == 0 ? 0.0
          : 100.0 * activeDelta.naturalSpawnsDenied() / activeDelta.naturalSpawnChecks())));
      lines.add(Component.literal(String.format("Items:          %,d merged, %,d removed",
        activeDelta.itemsMerged(), activeDelta.itemsRemoved())));
      lines.add(Component.literal(String.format("XP orbs:        %,d merged, %,d removed",
        activeDelta.xpOrbsMerged(), activeDelta.xpOrbsRemoved())));

      int entityDelta = activeEntityCount - baselineEntityCount;
      lines.add(
        Component.literal(String.format("Entities:       %,d (baseline) -> %,d (active)  [%+d]",
          baselineEntityCount, activeEntityCount, entityDelta)));

      lines.add(Component.literal(""));
      String assessmentLabel;
      ChatFormatting assessmentColor;
      if (tickTimeImprovementPercent >= 10.0) {
        assessmentLabel = "Clear improvement";
        assessmentColor = ChatFormatting.GREEN;
      } else if (tickTimeImprovementPercent >= 3.0) {
        assessmentLabel = "Noticeable improvement";
        assessmentColor = ChatFormatting.GREEN;
      } else if (tickTimeImprovementPercent > -3.0) {
        assessmentLabel = "No significant MSPT impact";
        assessmentColor = ChatFormatting.YELLOW;
      } else {
        assessmentLabel = "Performance regression detected";
        assessmentColor = ChatFormatting.RED;
      }
      String cpuNote = (baselineAvgCpu >= 0 && activeAvgCpu >= 0)
        ? String.format(" | CPU: %+.1fpp", cpuDelta) : "";
      lines.add(Component.literal("Assessment: ")
        .append(Component.literal(assessmentLabel).withStyle(assessmentColor))
        .append(Component.literal(String.format(
          "  (MSPT: %+.1f%% | Headroom: %+.1fpp%s)",
          tickTimeImprovementPercent, headroomDelta, cpuNote))));

      return lines;
    }
  }
}

