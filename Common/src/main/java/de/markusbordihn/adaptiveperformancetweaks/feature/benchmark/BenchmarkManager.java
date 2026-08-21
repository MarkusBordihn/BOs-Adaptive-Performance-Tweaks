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

import com.sun.management.OperatingSystemMXBean;
import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModConflictDetector;
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
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.RecoveryScenario;
import de.markusbordihn.adaptiveperformancetweaks.feature.benchmark.scenario.XpScenario;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.ViewDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.lang.management.ManagementFactory;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.arguments.EntityAnchorArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.projectile.AbstractArrow;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.GameType;
import net.minecraft.world.level.levelgen.Heightmap;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class BenchmarkManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final long DEFAULT_BLOCK_DURATION_MS = 240_000L;
  private static final long BLOCK_WARMUP_DURATION_MS = 30_000L;
  private static final long SCENARIO_SETTLE_DURATION_MS = 5_000L;
  private static final long SCENARIO_SETTLE_MAX_DURATION_MS = 45_000L;
  private static final long SCENARIO_POST_SETTLE_DURATION_MS = 3_000L;
  private static final long SAMPLE_INTERVAL_MS = 2_000L;
  private static final long GENERAL_MOVE_INTERVAL_MS = 12_000L;
  private static final long GENERAL_POST_MOVE_SETTLE_DELAY_MS = 4_000L;
  private static final long LEGACY_MOVE_INTERVAL_MS = 10_000L;
  private static final long LEGACY_POST_MOVE_SETTLE_DELAY_MS = 7_500L;
  private static final long EXPLORATION_START_SETTLE_DELAY_MS = 1_000L;
  private static final long CONFIRM_TIMEOUT_MS = 120_000L;
  private static final long TICK_DURATION_MS = 50L;
  private static final int MOVE_AREA_HALF_SIZE = 10_000;
  private static final int GENERAL_ROUTE_START_OFFSET_CHUNKS = 3;
  private static final int EXPLORATION_ROUTE_START_OFFSET_CHUNKS = 8;
  private static final long MIN_SUITE_GENERAL_SECONDS = 60L;
  private static final long MIN_SUITE_SPECIAL_SECONDS = 15L;
  private static final double TELEPORT_Y = 100.0d;
  private static final String BENCHMARK_TAG = "aptweaks_benchmark";
  private static final String OPEN_RESULT_COMMAND = "/aptweaks benchmark openresult";
  private static final List<BenchmarkScenario> DEFAULT_SCENARIOS = createScenarioSuite();
  private static final EnumMap<BenchmarkScenarioId, Long> scenarioDurationMs =
    new EnumMap<>(BenchmarkScenarioId.class);
  private static final EnumMap<BenchmarkScenarioId, BenchmarkScenarioResult.PhaseResult>
    baselineScenarioResults = new EnumMap<>(BenchmarkScenarioId.class);
  private static final EnumMap<BenchmarkScenarioId, BenchmarkScenarioResult.PhaseResult>
    activeScenarioResults = new EnumMap<>(BenchmarkScenarioId.class);
  private static final List<Double> currentSamples = new ArrayList<>();
  private static final List<Double> currentCpuSamples = new ArrayList<>();
  private static final List<Double> currentHeapSamples = new ArrayList<>();
  private static final EnumMap<ServerLoadLevel, Integer> currentLoadDist =
    new EnumMap<>(ServerLoadLevel.class);
  private static final EnumMap<MsptBucket, Integer> currentMsptDist =
    new EnumMap<>(MsptBucket.class);
  private static final EnumMap<BenchmarkScenarioId, List<Vec3>> baselineMoveWaypoints =
    new EnumMap<>(BenchmarkScenarioId.class);
  private static final EnumMap<BenchmarkScenarioId, List<Vec3>> activeMoveWaypoints =
    new EnumMap<>(BenchmarkScenarioId.class);
  private static final Set<Long> currentMovementChunkTargets = new HashSet<>();
  private static BenchmarkState state = BenchmarkState.IDLE;
  private static BenchmarkBlock currentBlock = BenchmarkBlock.BASELINE;
  private static boolean suiteMode = true;
  private static boolean autoMoveRequested = false;
  private static String requestedScenarioLabel = "Full Suite";
  private static ServerPlayer pendingConfirmPlayer;
  private static ServerPlayer benchmarkPlayer;
  private static Vec3 playerStartPos;
  private static Vec3 benchmarkOriginPos;
  private static List<BenchmarkScenario> configuredScenarios = List.of();
  private static int currentScenarioIndex;
  private static int currentWaypointIndex;
  private static int currentMovementStepCount;
  private static long configuredBlockDurationMs = DEFAULT_BLOCK_DURATION_MS;
  private static long currentScenarioDurationMs;
  private static long stageStartMs;
  private static long lastSampleMs;
  private static long lastMoveMs;
  private static long sampleBlockedUntilMs;
  private static long settleStableSinceMs;
  private static int settleViewDistance = Integer.MIN_VALUE;
  private static int settleSimulationDistance = Integer.MIN_VALUE;
  private static GameType savedGameMode;
  private static double lastCpuPercent = -1.0d;
  private static PerformanceStats.Snapshot currentMeasurementStartStats;
  private static BenchmarkScenarioResult.DistanceControlState currentMeasurementStartDistanceState;
  private static BenchmarkCompareResult lastResult;
  private static Path lastResultPath;

  private BenchmarkManager() {
  }

  public static void requestStart(ServerPlayer player, long blockSecs, boolean withAutoMove) {
    requestStart(player, null, blockSecs, withAutoMove);
  }

  public static void requestScenarioStart(
    ServerPlayer player, BenchmarkScenarioId scenarioId, long seconds, boolean withAutoMove) {
    requestStart(player, scenarioId, seconds, withAutoMove);
  }

  public static void confirm(ServerPlayer player) {
    if (state != BenchmarkState.PENDING_CONFIRM) {
      BenchmarkMessenger.sendMessage(player, "No benchmark is waiting for confirmation.");
      return;
    }

    benchmarkPlayer = player;
    playerStartPos = player.position();
    benchmarkOriginPos = resolveBenchmarkOriginPos(player);
    currentBlock = BenchmarkBlock.BASELINE;
    currentScenarioIndex = 0;
    currentWaypointIndex = 0;
    baselineScenarioResults.clear();
    activeScenarioResults.clear();
    currentMeasurementStartStats = null;
    currentMeasurementStartDistanceState = null;
    currentSamples.clear();
    currentCpuSamples.clear();
    currentHeapSamples.clear();
    currentLoadDist.clear();
    currentMsptDist.clear();

    baselineMoveWaypoints.clear();
    activeMoveWaypoints.clear();
    if (configuredScenarios.stream()
      .anyMatch(scenario -> scenario.usesAutoMove(autoMoveRequested))) {
      Set<Long> reservedChunkKeys = new HashSet<>();
      baselineMoveWaypoints.putAll(
        buildMoveWaypoints(benchmarkOriginPos, false, reservedChunkKeys));
      activeMoveWaypoints.putAll(buildMoveWaypoints(benchmarkOriginPos, true, reservedChunkKeys));
    }

    BenchmarkFeatureState.saveFeatureState();
    BenchmarkFeatureState.saveMinLoadLevels();
    BenchmarkFeatureState.disableAllFeatures();
    BenchmarkFeatureState.saveAllDebugStates();
    boolean anyDebugActive = BenchmarkFeatureState.hasAnyDebugActive();
    BenchmarkFeatureState.disableAllDebug();
    if (anyDebugActive) {
      BenchmarkMessenger.sendNoteMessage(player,
        "Debug logging disabled for all modules for accurate results.");
    }

    savedGameMode = player.gameMode.getGameModeForPlayer();
    if (savedGameMode != GameType.CREATIVE) {
      player.setGameMode(GameType.CREATIVE);
      BenchmarkMessenger.sendNoteMessage(player, "Switched to Creative mode for benchmark safety.");
    }

    if (player.getAbilities().flying) {
      player.getAbilities().flying = false;
      player.onUpdateAbilities();
    }

    if (benchmarkOriginPos != null) {
      teleportToPos(player, benchmarkOriginPos);
    }

    PerformanceStats.reset();
    PerformanceStats.setDetailedTrackingStatsEnabled(true);
    startBlockWarmup(System.currentTimeMillis(), BenchmarkBlock.BASELINE);
  }

  public static void cancel(ServerPlayer player) {
    if (state == BenchmarkState.IDLE || state == BenchmarkState.COMPLETE) {
      BenchmarkMessenger.sendMessage(player, "No benchmark is running.");
      return;
    }

    BenchmarkFeatureState.restoreFeatures();
    BenchmarkFeatureState.restoreDebugState();
    BenchmarkFeatureState.restoreMinLoadLevels();
    cleanupAllBenchmarkArtifacts();
    PerformanceStats.setDetailedTrackingStatsEnabled(false);
    restoreGameMode(benchmarkPlayer);
    if (benchmarkPlayer != null && playerStartPos != null) {
      teleportToPos(benchmarkPlayer, playerStartPos);
    }

    state = BenchmarkState.IDLE;
    clearSessionState();
    BenchmarkMessenger.sendMessage(player, "Benchmark cancelled. Features restored.");
    log.info("Benchmark cancelled by {}", player.getName().getString());
  }

  public static void onServerTick() {
    long now = System.currentTimeMillis();

    if (state == BenchmarkState.PENDING_CONFIRM) {
      if (now - stageStartMs > CONFIRM_TIMEOUT_MS) {
        state = BenchmarkState.IDLE;
        if (pendingConfirmPlayer != null) {
          BenchmarkMessenger.sendWarningMessage(pendingConfirmPlayer,
            "Benchmark confirmation timed out.");
        }
        pendingConfirmPlayer = null;
      }

      return;
    }

    switch (state) {
      case IDLE, COMPLETE -> {
        return;
      }
      case BLOCK_WARMUP -> handlePassiveStage(now, BLOCK_WARMUP_DURATION_MS, "warm-up",
        thisStageComplete -> startScenarioSetup(now));
      case SCENARIO_SETUP -> runScenarioSetup(now);
      case SCENARIO_SETTLE -> handleScenarioSettle(now);
      case SCENARIO_MEASURE -> handleScenarioMeasurement(now);
      case SCENARIO_CLEANUP -> runScenarioCleanup(now);
      case SCENARIO_POST_SETTLE -> handlePassiveStage(now, SCENARIO_POST_SETTLE_DURATION_MS,
        "cleanup settle", thisStageComplete -> advanceScenarioOrBlock(now));
      case BLOCK_TRANSITION -> completeBlockTransition(now);
      case PENDING_CONFIRM -> {
      }
    }
  }

  public static boolean isRunning() {
    return state != BenchmarkState.IDLE && state != BenchmarkState.COMPLETE;
  }

  public static String getStatusMessage() {
    return switch (state) {
      case IDLE -> "No benchmark running. Use /aptweaks benchmark start";
      case PENDING_CONFIRM ->
        "Waiting for confirmation. Use /aptweaks benchmark confirm or cancel.";
      case BLOCK_WARMUP -> currentBlock.getDisplayName() + " block warm-up in progress.";
      case SCENARIO_SETUP -> currentScenarioLabel() + " setup in progress.";
      case SCENARIO_SETTLE -> currentScenarioLabel() + " settle phase in progress.";
      case SCENARIO_MEASURE -> currentScenarioLabel() + " measurement in progress.";
      case SCENARIO_CLEANUP -> currentScenarioLabel() + " cleanup in progress.";
      case SCENARIO_POST_SETTLE -> currentScenarioLabel() + " cleanup settle in progress.";
      case BLOCK_TRANSITION -> "Switching from baseline to active block.";
      case COMPLETE -> "Last benchmark complete. Start a new one with /aptweaks benchmark start.";
    };
  }

  public static BenchmarkCompareResult getLastResult() {
    return lastResult;
  }

  public static Path getLastResultPath() {
    return lastResultPath;
  }

  public static void reset() {
    if (state != BenchmarkState.IDLE && state != BenchmarkState.COMPLETE) {
      BenchmarkFeatureState.restoreFeatures();
      BenchmarkFeatureState.restoreDebugState();
      BenchmarkFeatureState.restoreMinLoadLevels();
      cleanupAllBenchmarkArtifacts();
      restoreGameMode(benchmarkPlayer);
    }

    PerformanceStats.setDetailedTrackingStatsEnabled(false);
    clearSessionState();
    state = BenchmarkState.IDLE;
  }

  private static List<BenchmarkScenario> createScenarioSuite() {
    return List.of(
      new GeneralScenario(),
      new ExplorationScenario(),
      new ItemScenario(),
      new XpScenario(),
      new EntityScenario(),
      new RecoveryScenario());
  }

  private static void requestStart(
    ServerPlayer player, BenchmarkScenarioId scenarioId, long seconds, boolean withAutoMove) {
    if (state == BenchmarkState.COMPLETE) {
      state = BenchmarkState.IDLE;
    } else if (state != BenchmarkState.IDLE) {
      BenchmarkMessenger.sendMessage(player,
        "Benchmark is already running. Use /aptweaks benchmark cancel to stop it.");
      return;
    }

    List<BenchmarkScenario> scenarioSelection = scenarioId == null
      ? DEFAULT_SCENARIOS
      : DEFAULT_SCENARIOS.stream().filter(scenario -> scenario.id() == scenarioId).toList();
    if (scenarioSelection.isEmpty()) {
      BenchmarkMessenger.sendMessage(player, "Unable to resolve benchmark scenario.");
      return;
    }

    suiteMode = scenarioId == null;
    configuredScenarios = List.copyOf(scenarioSelection);
    autoMoveRequested = withAutoMove;
    requestedScenarioLabel = suiteMode ? "Full Suite" : scenarioId.getDisplayName();
    configuredBlockDurationMs = Math.max(30_000L, seconds * 1000L);
    pendingConfirmPlayer = player;
    stageStartMs = System.currentTimeMillis();
    state = BenchmarkState.PENDING_CONFIRM;
    currentBlock = BenchmarkBlock.BASELINE;
    scenarioDurationMs.clear();

    if (suiteMode) {
      String validationError = validateSuiteDurationSeconds(seconds);
      if (validationError != null) {
        state = BenchmarkState.IDLE;
        pendingConfirmPlayer = null;
        BenchmarkMessenger.sendMessage(player, validationError);
        return;
      }

      scenarioDurationMs.putAll(buildSuiteScenarioDurationsMillis(seconds));
    } else {
      scenarioDurationMs.put(scenarioId, configuredBlockDurationMs);
    }

    sendBenchmarkWarning(player);
  }

  private static void sendBenchmarkWarning(ServerPlayer player) {
    long totalRuntimeMs = suiteMode
      ? totalSuiteRuntimeMs()
      : totalSingleScenarioRuntimeMs(configuredScenarios.get(0).id());
    BenchmarkMessenger.sendWarningMessage(player, "=== APTweaks Benchmark - WARNING ===");
    BenchmarkMessenger.sendMessage(player, String.format(
      "Target: %s | total runtime: ~%s",
      requestedScenarioLabel, BenchmarkMessenger.formatDuration(totalRuntimeMs)));
    BenchmarkMessenger.sendMessage(player, String.format(
      "Block warm-up: %s | settle: %s or longer | cleanup settle: %s",
      BenchmarkMessenger.formatDuration(BLOCK_WARMUP_DURATION_MS),
      BenchmarkMessenger.formatDuration(SCENARIO_SETTLE_DURATION_MS),
      BenchmarkMessenger.formatDuration(SCENARIO_POST_SETTLE_DURATION_MS)));
    if (suiteMode) {
      BenchmarkMessenger.sendMessage(player, "Scenario order:");
      BenchmarkMessenger.sendMessage(player,
        "Baseline General -> Exploration -> Items -> XP -> Entities -> Recovery");
      BenchmarkMessenger.sendMessage(player,
        "Active   General -> Exploration -> Items -> XP -> Entities -> Recovery");
      BenchmarkMessenger.sendMessage(player,
        "Scenario durations per block: " + formatScenarioDurationsForMessage());
    } else {
      BenchmarkScenarioId scenarioId = configuredScenarios.get(0).id();
      BenchmarkMessenger.sendMessage(player, String.format(
        "Single scenario: %s | measurement per block: %s",
        scenarioId.getDisplayName(),
        BenchmarkMessenger.formatDuration(scenarioDurationMs.get(scenarioId))));
    }
    if (autoMoveRequested) {
      BenchmarkMessenger.sendPrefixedMessage(player, "[Move] ",
        "Wider General travel enabled for comparison. Exploration keeps its own long-range route.",
        ChatFormatting.LIGHT_PURPLE, ChatFormatting.GRAY);
    }
    BenchmarkMessenger.sendWarningMessage(player, "Only run this in a test world!");
    BenchmarkMessenger.sendCommandMessage(player, "Confirm", "/aptweaks benchmark confirm");
    BenchmarkMessenger.sendCommandMessage(player, "Cancel", "/aptweaks benchmark cancel");
    BenchmarkMessenger.sendNoteMessage(player, "Expires in 120 seconds.");
  }

  private static void startBlockWarmup(long now, BenchmarkBlock block) {
    currentBlock = block;
    currentScenarioIndex = 0;
    currentWaypointIndex = 0;
    currentMovementStepCount = 0;
    currentScenarioDurationMs = 0L;
    stageStartMs = now;
    lastSampleMs = now;
    lastMoveMs = now;
    sampleBlockedUntilMs = now;
    currentMovementChunkTargets.clear();
    state = BenchmarkState.BLOCK_WARMUP;

    log.info("{} benchmark block warm-up started.", block.getDisplayName());
    if (benchmarkPlayer != null) {
      BenchmarkMessenger.sendNoteMessage(benchmarkPlayer, block == BenchmarkBlock.BASELINE
        ? "Base block warm-up started - all mod features disabled."
        : "Active block warm-up started - configured feature state restored.");
    }
  }

  private static void handlePassiveStage(
    long now, long durationMs, String stageLabel, PassiveStageCallback callback) {
    if (now - lastSampleMs >= SAMPLE_INTERVAL_MS) {
      lastCpuPercent = getProcessCpuPercent();
      sendStatusUpdate(now, stageLabel, durationMs, false);
      lastSampleMs = now;
    }

    if (now - stageStartMs >= durationMs) {
      callback.run(true);
    }
  }

  private static void handleScenarioSettle(long now) {
    trackDistanceControlStability(now);
    handlePassiveStage(now, SCENARIO_SETTLE_DURATION_MS, "settle", thisStageComplete -> {
      boolean settleTimedOut = now - stageStartMs >= SCENARIO_SETTLE_MAX_DURATION_MS;
      if (now - settleStableSinceMs < SCENARIO_SETTLE_DURATION_MS && !settleTimedOut) {
        return;
      }
      if (settleTimedOut) {
        log.warn("[Benchmark] Distance control did not settle within {}, measuring anyway.",
          BenchmarkMessenger.formatDuration(SCENARIO_SETTLE_MAX_DURATION_MS));
      }
      startScenarioMeasurement(now);
    });
  }

  private static void trackDistanceControlStability(long now) {
    int viewDistance = ViewDistanceManager.getCurrentViewDistance();
    int simulationDistance = SimulationDistanceManager.getCurrentSimulationDistance();
    if (viewDistance == settleViewDistance && simulationDistance == settleSimulationDistance) {
      return;
    }

    settleViewDistance = viewDistance;
    settleSimulationDistance = simulationDistance;
    settleStableSinceMs = now;
  }

  private static void startScenarioSetup(long now) {
    state = BenchmarkState.SCENARIO_SETUP;
    stageStartMs = now;
  }

  private static void runScenarioSetup(long now) {
    BenchmarkScenario scenario = currentScenario();
    if (benchmarkPlayer != null && benchmarkOriginPos != null) {
      teleportToPos(benchmarkPlayer, benchmarkOriginPos);
    }

    BenchmarkScenarioContext context = currentScenarioContext(scenario);
    facePlayerToScenarioFocus(scenario, context);
    scenario.setup(context);

    currentScenarioDurationMs = scenarioDurationMs.getOrDefault(scenario.id(),
      configuredBlockDurationMs);
    stageStartMs = now;
    lastSampleMs = now;
    lastMoveMs = now;
    sampleBlockedUntilMs = now;
    settleViewDistance = Integer.MIN_VALUE;
    settleSimulationDistance = Integer.MIN_VALUE;
    settleStableSinceMs = now;
    state = BenchmarkState.SCENARIO_SETTLE;

    if (benchmarkPlayer != null) {
      BenchmarkMessenger.sendStageMessage(benchmarkPlayer, currentBlock, scenario.displayName(),
        String.format(
          "setup complete. Settle: at least %s, until view and simulation distance hold steady.",
          BenchmarkMessenger.formatDuration(SCENARIO_SETTLE_DURATION_MS)));
    }
  }

  private static void startScenarioMeasurement(long now) {
    BenchmarkScenario scenario = currentScenario();
    currentSamples.clear();
    currentCpuSamples.clear();
    currentHeapSamples.clear();
    currentLoadDist.clear();
    currentMsptDist.clear();
    currentMovementStepCount = 0;
    currentMovementChunkTargets.clear();
    currentMeasurementStartStats =
      captureMeasurementStartStats(scenario, currentScenarioContext(scenario));
    currentMeasurementStartDistanceState = captureDistanceControlState();

    stageStartMs = now;
    lastSampleMs = now;
    lastMoveMs = now;
    sampleBlockedUntilMs = now;
    state = BenchmarkState.SCENARIO_MEASURE;

    if (scenario.usesAutoMove(autoMoveRequested) && !getCurrentMoveWaypoints().isEmpty()
      && benchmarkPlayer != null) {
      teleportToNextWaypoint(benchmarkPlayer, getCurrentMoveWaypoints());
      sampleBlockedUntilMs = now + getInitialMoveSettleDelayMs(scenario);
    }

    log.info("{} {} measurement started for {}.", currentBlock.getDisplayName(),
      scenario.displayName(), BenchmarkMessenger.formatDuration(currentScenarioDurationMs));
    if (benchmarkPlayer != null) {
      BenchmarkMessenger.sendStageMessage(benchmarkPlayer, currentBlock, scenario.displayName(),
        String.format(
          "measurement started (%s).",
          BenchmarkMessenger.formatDuration(currentScenarioDurationMs)));
    }
  }

  private static void handleScenarioMeasurement(long now) {
    BenchmarkScenario scenario = currentScenario();
    runScenarioMeasurementTick(scenario, currentScenarioContext(scenario));
    boolean sampleDue = now - lastSampleMs >= SAMPLE_INTERVAL_MS;
    boolean sampleAllowed = sampleDue && now >= sampleBlockedUntilMs;
    boolean moveDue = scenario.usesAutoMove(autoMoveRequested)
      && benchmarkPlayer != null
      && !getCurrentMoveWaypoints().isEmpty()
      && now - lastMoveMs >= getMoveIntervalMs(scenario);

    if (sampleAllowed) {
      double sampleMspt = ServerManager.getAverageTickTime();
      MsptBucket sampleBucket = MsptBucket.fromTickTime(sampleMspt);
      currentSamples.add(sampleMspt);
      currentHeapSamples.add((double) getCurrentHeapUsage());
      lastCpuPercent = getProcessCpuPercent();
      if (lastCpuPercent >= 0.0d) {
        currentCpuSamples.add(lastCpuPercent);
      }
      currentLoadDist.merge(ServerLoad.getMeasuredServerLoad(), 1, Integer::sum);
      currentMsptDist.merge(sampleBucket, 1, Integer::sum);
      sendStatusUpdate(now, "measure", currentScenarioDurationMs, true);
      lastSampleMs = now;
    }

    if (moveDue) {
      teleportToNextWaypoint(benchmarkPlayer, getCurrentMoveWaypoints());
      lastMoveMs = now;
      sampleBlockedUntilMs = now + getPostMoveSettleDelayMs(scenario);
    }

    if (now - stageStartMs >= currentScenarioDurationMs) {
      finalizeCurrentScenarioMeasurement(now);
    }
  }

  private static void finalizeCurrentScenarioMeasurement(long now) {
    BenchmarkScenario scenario = currentScenario();
    BenchmarkScenarioResult.DistanceControlState endDistanceState = captureDistanceControlState();
    PerformanceStats.Snapshot endStats = PerformanceStats.snapshot();
    PerformanceStats.Snapshot statsDelta = PerformanceStats.delta(currentMeasurementStartStats,
      endStats);

    BenchmarkScenarioResult.PhaseResult phaseResult = new BenchmarkScenarioResult.PhaseResult(
      currentScenarioDurationMs,
      average(currentSamples),
      min(currentSamples),
      percentile(currentSamples, 95),
      max(currentSamples),
      Map.copyOf(currentLoadDist),
      Map.copyOf(currentMsptDist),
      buildFineMsptDistribution(currentSamples),
      getAverageHeapUsedBytes(),
      countEntities(),
      average(currentCpuSamples, -1.0d),
      max(currentCpuSamples, -1.0d),
      currentMeasurementStartDistanceState != null
        ? currentMeasurementStartDistanceState
        : BenchmarkScenarioResult.DistanceControlState.none(),
      endDistanceState,
      statsDelta,
      buildScenarioValidation(scenario, statsDelta));

    if (currentBlock == BenchmarkBlock.BASELINE) {
      baselineScenarioResults.put(scenario.id(), phaseResult);
    } else {
      activeScenarioResults.put(scenario.id(), phaseResult);
    }

    stageStartMs = now;
    state = BenchmarkState.SCENARIO_CLEANUP;
  }

  private static void runScenarioCleanup(long now) {
    BenchmarkScenario scenario = currentScenario();
    BenchmarkScenarioContext context = currentScenarioContext(scenario);
    scenario.cleanup(context);
    cleanupBenchmarkArtifacts(context.level(), context.center(), scenario.cleanupRadius(),
      context.scenarioTag());
    if (benchmarkPlayer != null && benchmarkOriginPos != null) {
      teleportToPos(benchmarkPlayer, benchmarkOriginPos);
    }

    stageStartMs = now;
    lastSampleMs = now;
    lastMoveMs = now;
    sampleBlockedUntilMs = now;
    state = BenchmarkState.SCENARIO_POST_SETTLE;

    if (benchmarkPlayer != null) {
      BenchmarkMessenger.sendStageMessage(benchmarkPlayer, currentBlock, scenario.displayName(),
        String.format(
          "cleanup complete. Post-settle: %s",
          BenchmarkMessenger.formatDuration(SCENARIO_POST_SETTLE_DURATION_MS)));
    }
  }

  private static void advanceScenarioOrBlock(long now) {
    currentScenarioIndex++;
    if (currentScenarioIndex < configuredScenarios.size()) {
      startScenarioSetup(now);
      return;
    }

    if (currentBlock == BenchmarkBlock.BASELINE) {
      state = BenchmarkState.BLOCK_TRANSITION;
      stageStartMs = now;
      return;
    }

    finalizeBenchmark();
  }

  private static void completeBlockTransition(long now) {
    BenchmarkFeatureState.restoreFeatures();
    BenchmarkFeatureState.disableAllDebug();
    BenchmarkFeatureState.forceMinLoadLevels();
    PerformanceStats.reset();
    PerformanceStats.setDetailedTrackingStatsEnabled(true);
    startBlockWarmup(now, BenchmarkBlock.ACTIVE);
  }

  private static void finalizeBenchmark() {
    List<BenchmarkScenarioResult> scenarioResults = new ArrayList<>(configuredScenarios.size());
    for (BenchmarkScenario scenario : configuredScenarios) {
      BenchmarkScenarioResult.PhaseResult baseline = baselineScenarioResults.get(scenario.id());
      BenchmarkScenarioResult.PhaseResult active = activeScenarioResults.get(scenario.id());
      if (baseline != null && active != null) {
        scenarioResults.add(new BenchmarkScenarioResult(scenario.id(), baseline, active));
      }
    }

    lastResult = new BenchmarkCompareResult(
      requestedScenarioLabel,
      suiteMode,
      configuredBlockDurationMs,
      BLOCK_WARMUP_DURATION_MS,
      SCENARIO_SETTLE_DURATION_MS,
      SCENARIO_POST_SETTLE_DURATION_MS,
      Map.copyOf(scenarioDurationMs),
      List.copyOf(scenarioResults),
      BenchmarkFeatureState.countFeaturesByActivation(
        ModConflictDetector.FeatureActivation.MANUAL_ENABLED),
      BenchmarkFeatureState.countFeaturesByActivation(
        ModConflictDetector.FeatureActivation.AUTO_ENABLED),
      BenchmarkFeatureState.countFeaturesByActivation(
        ModConflictDetector.FeatureActivation.MANUAL_DISABLED),
      BenchmarkFeatureState.countFeaturesByActivation(
        ModConflictDetector.FeatureActivation.CONFLICT_DISABLED),
      !baselineMoveWaypoints.isEmpty() || !activeMoveWaypoints.isEmpty(),
      countChunkTargets(baselineMoveWaypoints),
      countChunkTargets(activeMoveWaypoints),
      countSharedChunkTargets(baselineMoveWaypoints, activeMoveWaypoints),
      Instant.now());

    BenchmarkFeatureState.restoreFeatures();
    BenchmarkFeatureState.restoreDebugState();
    BenchmarkFeatureState.restoreMinLoadLevels();
    cleanupAllBenchmarkArtifacts();
    PerformanceStats.setDetailedTrackingStatsEnabled(false);

    ServerPlayer player = benchmarkPlayer;
    Vec3 startPos = playerStartPos;
    restoreGameMode(player);
    state = BenchmarkState.COMPLETE;
    clearSessionState();

    if (player != null && startPos != null) {
      teleportToPos(player, startPos);
    }

    lastResultPath = BenchmarkResultWriter.save(lastResult, log);

    log.info("Benchmark complete for {}.", requestedScenarioLabel);
    for (Component line : lastResult.format()) {
      log.debug(line.getString());
    }

    if (player != null) {
      if (lastResultPath == null) {
        BenchmarkMessenger.sendMessage(player, "Benchmark complete!");
      }

      for (Component line : lastResult.formatChat()) {
        BenchmarkMessenger.sendMessage(player, line);
      }

      if (lastResultPath == null) {
        return;
      }

      BenchmarkMessenger.sendReportLocation(player, lastResultPath);
    }
  }

  private static void sendStatusUpdate(
    long now, String stageLabel, long stageDurationMs, boolean measurement) {
    if (benchmarkPlayer == null) {
      return;
    }

    long elapsedMs = Math.max(0L, now - stageStartMs);
    ChatFormatting stageColor = BenchmarkMessenger.getStageColor(stageLabel, currentBlock);
    MutableComponent message = Component.literal("[APT Benchmark] ").withStyle(ChatFormatting.GOLD)
      .append(Component.literal(BenchmarkMessenger.formatProgressBar(elapsedMs, stageDurationMs))
        .withStyle(stageColor))
      .append(Component.literal(
          String.format(" %s: %s %s ", currentBlock.getStatusLabel(), currentScenarioShortLabel(),
            BenchmarkMessenger.formatStageLabel(stageLabel)))
        .withStyle(stageColor));

    if (measurement) {
      int sampleCount = currentSamples.size();
      int totalSamples = Math.max(1, (int) (stageDurationMs / SAMPLE_INTERVAL_MS));
      message = message.append(Component.literal(sampleCount + "/" + totalSamples + " | "));
    }

    long remainingMs = Math.max(0L, stageDurationMs - elapsedMs);
    double mspt = ServerManager.getAverageTickTime();
    double headroom = (50.0d - mspt) / 50.0d * 100.0d;
    message = message
      .append(Component.literal(BenchmarkMessenger.formatDuration(remainingMs) + " | "))
      .append(Component.literal(String.format("%.1fms", mspt))
        .withStyle(BenchmarkMessenger.getMsptColor(mspt)))
      .append(Component.literal(String.format(" | %.0f%% hr", headroom))
        .withStyle(BenchmarkMessenger.getHeadroomColor(headroom)))
      .append(Component.literal(
          " | RAM " + BenchmarkMessenger.formatBytes(
            ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed()))
        .withStyle(ChatFormatting.AQUA));

    if (lastCpuPercent >= 0.0d) {
      message = message.append(
        Component.literal(String.format(" | CPU %.0f%%", lastCpuPercent))
          .withStyle(ChatFormatting.GRAY));
    }

    BenchmarkMessenger.sendMessage(benchmarkPlayer, message);
  }

  private static PerformanceStats.Snapshot captureMeasurementStartStats(BenchmarkScenario scenario,
    BenchmarkScenarioContext context) {
    scenario.beforeMeasurement(context);
    return PerformanceStats.snapshot();
  }

  private static BenchmarkScenarioResult.DistanceControlState captureDistanceControlState() {
    return new BenchmarkScenarioResult.DistanceControlState(
      ViewDistanceManager.getCurrentViewDistance(),
      ViewDistanceManager.getCurrentLoadBaselineDistance(),
      ViewDistanceManager.getCurrentWarmupReduction(),
      ViewDistanceManager.isWarmupActive(),
      ViewDistanceManager.getActiveExplorerCount(),
      SimulationDistanceManager.getCurrentSimulationDistance(),
      SimulationDistanceManager.getCurrentLoadBaselineDistance(),
      SimulationDistanceManager.getCurrentMovementReduction(),
      SimulationDistanceManager.getActiveExplorerCount());
  }

  private static long getCurrentHeapUsage() {
    return ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed();
  }

  private static long getAverageHeapUsedBytes() {
    return currentHeapSamples.isEmpty() ? -1L : (long) average(currentHeapSamples);
  }

  private static void runScenarioMeasurementTick(BenchmarkScenario scenario,
    BenchmarkScenarioContext context) {
    scenario.onMeasurementTick(context);
  }

  private static void facePlayerToScenarioFocus(BenchmarkScenario scenario,
    BenchmarkScenarioContext context) {
    ServerPlayer player = context.player();
    if (player == null || !scenario.shouldFacePlayerToFocus()) {
      return;
    }

    Vec3 focusTarget = context.center().add(scenario.playerFocusOffset());
    try {
      player.lookAt(EntityAnchorArgument.Anchor.EYES, focusTarget);
    } catch (Exception exception) {
      log.warn("[Benchmark] Scenario focus look-at for {} failed: {}",
        scenario.displayName(), exception.getMessage());
    }
  }

  private static BenchmarkScenarioContext currentScenarioContext(BenchmarkScenario scenario) {
    Vec3 baseCenter = benchmarkOriginPos != null ? benchmarkOriginPos : benchmarkPlayer.position();
    Vec3 centerOffset = scenario.centerOffset();
    Vec3 center = baseCenter.add(centerOffset.x, centerOffset.y, centerOffset.z);

    return new BenchmarkScenarioContext(
      benchmarkPlayer,
      benchmarkPlayer.serverLevel(),
      center,
      scenario.id(),
      currentBlock == BenchmarkBlock.ACTIVE,
      scenario.usesAutoMove(autoMoveRequested),
      currentScenarioDurationMs,
      BENCHMARK_TAG,
      BENCHMARK_TAG + '_' + scenario.id().getId());
  }

  private static List<Vec3> getCurrentMoveWaypoints() {
    EnumMap<BenchmarkScenarioId, List<Vec3>> moveWaypoints = currentBlock == BenchmarkBlock.BASELINE
      ? baselineMoveWaypoints
      : activeMoveWaypoints;
    return moveWaypoints.getOrDefault(currentScenario().id(), List.of());
  }

  private static void cleanupAllBenchmarkArtifacts() {
    for (ServerLevel level : ServerManager.getAllLevels()) {
      List<Entity> taggedEntities = new ArrayList<>();
      for (Entity entity : level.getAllEntities()) {
        if (entity.getTags().contains(BENCHMARK_TAG)) {
          taggedEntities.add(entity);
        }
      }

      for (Entity entity : taggedEntities) {
        removeBenchmarkEntity(entity, entity instanceof LivingEntity);
      }
    }
  }

  private static void cleanupBenchmarkArtifacts(
    ServerLevel level, Vec3 center, double radius, String scenarioTag) {
    double safeRadius = Math.max(1.0d, radius);
    AABB cleanupArea = new AABB(center, center).inflate(safeRadius);

    List<Entity> taggedEntities = level.getEntities((Entity) null, cleanupArea, entity ->
      entity.getTags().contains(BENCHMARK_TAG) || entity.getTags().contains(scenarioTag));
    for (Entity entity : taggedEntities) {
      removeBenchmarkEntity(entity, entity instanceof LivingEntity
        && entity.getTags().contains(scenarioTag));
    }

    List<Entity> sweepEntities = level.getEntities((Entity) null, cleanupArea, entity ->
      entity instanceof ExperienceOrb
        || entity instanceof AbstractArrow
        || entity instanceof ItemEntity);
    for (Entity entity : sweepEntities) {
      removeBenchmarkEntity(entity, false);
    }
  }

  private static void removeBenchmarkEntity(Entity entity, boolean allowKill) {
    if (entity == null || entity.isRemoved()) {
      return;
    }

    try {
      if (allowKill && entity instanceof LivingEntity livingEntity) {
        livingEntity.kill();
      } else if (entity instanceof Projectile projectile) {
        projectile.discard();
      } else {
        entity.discard();
      }
    } catch (Exception exception) {
      log.warn("Benchmark cleanup failed for {}: {}", entity.getType(), exception.getMessage());
    }
  }

  private static String validateSuiteDurationSeconds(long seconds) {
    Map<BenchmarkScenarioId, Long> secondsByScenario = buildSuiteScenarioDurationsSeconds(seconds);
    long generalSeconds = secondsByScenario.getOrDefault(BenchmarkScenarioId.GENERAL, 0L);
    if (generalSeconds < MIN_SUITE_GENERAL_SECONDS) {
      return String.format(
        "Benchmark suite too short. General needs at least %ds but would get %ds.",
        MIN_SUITE_GENERAL_SECONDS,
        generalSeconds);
    }

    for (BenchmarkScenarioId scenarioId : BenchmarkScenarioId.values()) {
      if (scenarioId == BenchmarkScenarioId.GENERAL) {
        continue;
      }

      long scenarioSeconds = secondsByScenario.getOrDefault(scenarioId, 0L);
      if (scenarioSeconds < MIN_SUITE_SPECIAL_SECONDS) {
        return String.format(
          "Benchmark suite too short. %s needs at least %ds but would get %ds.",
          scenarioId.getDisplayName(),
          MIN_SUITE_SPECIAL_SECONDS,
          scenarioSeconds);
      }
    }

    return null;
  }

  private static Map<BenchmarkScenarioId, Long> buildSuiteScenarioDurationsMillis(long seconds) {
    EnumMap<BenchmarkScenarioId, Long> durationMap = new EnumMap<>(BenchmarkScenarioId.class);
    buildSuiteScenarioDurationsSeconds(seconds).forEach(
      (scenarioId, scenarioSeconds) -> durationMap.put(scenarioId, scenarioSeconds * 1000L));

    return durationMap;
  }

  private static Map<BenchmarkScenarioId, Long> buildSuiteScenarioDurationsSeconds(long seconds) {
    EnumMap<BenchmarkScenarioId, Long> durationMap = new EnumMap<>(BenchmarkScenarioId.class);
    int totalWeight = 0;
    for (BenchmarkScenarioId scenarioId : BenchmarkScenarioId.values()) {
      totalWeight += scenarioId.getSuiteWeight();
    }
    long unitSeconds = Math.max(1L, seconds / totalWeight);
    long usedSeconds = 0L;

    for (BenchmarkScenarioId scenarioId : BenchmarkScenarioId.values()) {
      long scenarioSeconds = unitSeconds * scenarioId.getSuiteWeight();
      durationMap.put(scenarioId, scenarioSeconds);
      usedSeconds += scenarioSeconds;
    }

    long remainingSeconds = Math.max(0L, seconds - usedSeconds);
    BenchmarkScenarioId[] scenarioIds = BenchmarkScenarioId.values();
    int scenarioIndex = 0;
    while (remainingSeconds > 0L) {
      BenchmarkScenarioId scenarioId = scenarioIds[scenarioIndex % scenarioIds.length];
      durationMap.put(scenarioId, durationMap.get(scenarioId) + 1L);
      remainingSeconds--;
      scenarioIndex++;
    }

    return durationMap;
  }

  private static long totalSuiteRuntimeMs() {
    long measurementDurationMs = scenarioDurationMs.values().stream()
      .mapToLong(Long::longValue).sum();
    long perBlockOverheadMs = configuredScenarios.size()
      * (SCENARIO_SETTLE_DURATION_MS + SCENARIO_POST_SETTLE_DURATION_MS);

    return BLOCK_WARMUP_DURATION_MS * 2L + (measurementDurationMs + perBlockOverheadMs) * 2L;
  }

  private static long totalSingleScenarioRuntimeMs(BenchmarkScenarioId scenarioId) {
    long measurementDuration = scenarioDurationMs.getOrDefault(scenarioId,
      configuredBlockDurationMs);

    return BLOCK_WARMUP_DURATION_MS * 2L
      + (measurementDuration + SCENARIO_SETTLE_DURATION_MS + SCENARIO_POST_SETTLE_DURATION_MS) * 2L;
  }

  private static Map<FineMsptBucket, Integer> buildFineMsptDistribution(List<Double> samples) {
    EnumMap<FineMsptBucket, Integer> distribution = new EnumMap<>(FineMsptBucket.class);
    for (double sampleMspt : samples) {
      distribution.merge(FineMsptBucket.fromTickTime(sampleMspt), 1, Integer::sum);
    }

    return Map.copyOf(distribution);
  }

  private static String formatScenarioDurationsForMessage() {
    return configuredScenarios.stream()
      .map(scenario -> scenario.displayName() + '='
        + BenchmarkMessenger.formatDuration(
        scenarioDurationMs.getOrDefault(scenario.id(), configuredBlockDurationMs)))
      .collect(Collectors.joining(" | "));
  }

  private static String currentScenarioLabel() {
    BenchmarkScenario scenario = currentScenario();
    if (scenario == null) {
      return currentBlock.getDisplayName();
    }

    return currentBlock.getStatusLabel() + ": " + scenario.displayName();
  }

  private static String currentScenarioShortLabel() {
    BenchmarkScenario scenario = currentScenario();
    if (scenario == null) {
      return "block";
    }

    return scenario.displayName();
  }

  private static BenchmarkScenario currentScenario() {
    if (configuredScenarios.isEmpty() || currentScenarioIndex >= configuredScenarios.size()) {
      return null;
    }

    return configuredScenarios.get(currentScenarioIndex);
  }

  static String resolveModVersion() {
    return normalizeModVersion(BenchmarkManager.class.getPackage().getImplementationVersion());
  }

  private static String normalizeModVersion(String modVersion) {
    if (modVersion == null) {
      return null;
    }

    String normalized = modVersion.trim();
    if (normalized.isEmpty()
      || "unknown".equalsIgnoreCase(normalized)
      || "MOD_DEV".equalsIgnoreCase(normalized)) {
      return null;
    }

    return normalized;
  }

  static String detectLoader() {
    try {
      Class.forName("net.neoforged.neoforge.common.NeoForge");
      return "neoforge";
    } catch (ClassNotFoundException ignored) {
    }

    try {
      Class.forName("net.minecraftforge.common.MinecraftForge");
      return "forge";
    } catch (ClassNotFoundException ignored) {
    }

    try {
      Class.forName("net.fabricmc.loader.api.FabricLoader");
      return "fabric";
    } catch (ClassNotFoundException ignored) {
    }

    return "unknown";
  }

  private static Vec3 resolveBenchmarkOriginPos(ServerPlayer player) {
    BlockPos surfacePos = player.serverLevel()
      .getHeightmapPos(Heightmap.Types.MOTION_BLOCKING_NO_LEAVES,
        BlockPos.containing(player.getX(), 0.0d, player.getZ()));

    return new Vec3(player.getX(), surfacePos.getY() + 1.0d, player.getZ());
  }

  private static void clearSessionState() {
    benchmarkPlayer = null;
    pendingConfirmPlayer = null;
    playerStartPos = null;
    benchmarkOriginPos = null;
    savedGameMode = null;
    lastCpuPercent = -1.0d;
    configuredScenarios = List.of();
    baselineMoveWaypoints.clear();
    activeMoveWaypoints.clear();
    currentScenarioIndex = 0;
    currentWaypointIndex = 0;
    currentMovementStepCount = 0;
    currentScenarioDurationMs = 0L;
    currentMeasurementStartStats = null;
    currentMeasurementStartDistanceState = null;
    currentMovementChunkTargets.clear();
    currentSamples.clear();
    currentCpuSamples.clear();
    currentHeapSamples.clear();
    currentLoadDist.clear();
    currentMsptDist.clear();
    scenarioDurationMs.clear();
    baselineScenarioResults.clear();
    activeScenarioResults.clear();
    BenchmarkFeatureState.clearAll();
    requestedScenarioLabel = "Full Suite";
    suiteMode = true;
    autoMoveRequested = false;
  }

  private static int blockToChunk(double blockCoord) {
    return ((int) Math.floor(blockCoord)) >> 4;
  }

  private static long getChunkKey(Vec3 target) {
    return ChunkPos.asLong(blockToChunk(target.x), blockToChunk(target.z));
  }

  private static EnumMap<BenchmarkScenarioId, List<Vec3>> buildMoveWaypoints(
    Vec3 origin, boolean activeBlock, Set<Long> reservedChunkKeys) {
    EnumMap<BenchmarkScenarioId, List<Vec3>> moveWaypoints =
      new EnumMap<>(BenchmarkScenarioId.class);
    for (BenchmarkScenario scenario : configuredScenarios) {
      if (!scenario.usesAutoMove(autoMoveRequested)) {
        continue;
      }

      long durationMs = scenarioDurationMs.getOrDefault(scenario.id(), configuredBlockDurationMs);
      List<Vec3> scenarioWaypoints =
        computeWaypoints(scenario, origin, durationMs, activeBlock, reservedChunkKeys);
      moveWaypoints.put(scenario.id(), scenarioWaypoints);
      for (Vec3 target : scenarioWaypoints) {
        reservedChunkKeys.add(getChunkKey(target));
      }
    }

    return moveWaypoints;
  }

  private static List<Vec3> computeWaypoints(BenchmarkScenario scenario, Vec3 origin,
    long durationMs, boolean activeBlock, Set<Long> reservedChunkKeys) {
    if (scenario.id() == BenchmarkScenarioId.GENERAL) {
      return autoMoveRequested
        ? computeLegacyWaypoints(scenario, origin, durationMs, activeBlock, reservedChunkKeys)
        : computeGeneralWaypoints(origin, durationMs, activeBlock);
    }

    return scenario.id() == BenchmarkScenarioId.EXPLORATION
      ? computeExplorationWaypoints(origin, durationMs, activeBlock)
      : computeLegacyWaypoints(scenario, origin, durationMs, activeBlock, reservedChunkKeys);
  }

  private static List<Vec3> computeGeneralWaypoints(Vec3 origin, long durationMs,
    boolean activeBlock) {
    int pointCount = getPlannedMoveCount(durationMs,
      getMoveIntervalMs(BenchmarkScenarioId.GENERAL));
    List<Vec3> points = new ArrayList<>(pointCount);
    int originChunkX = blockToChunk(origin.x);
    int originChunkZ = blockToChunk(origin.z);
    int direction = activeBlock ? 1 : -1;
    int verticalDirection = activeBlock ? 1 : -1;
    int chunkX = originChunkX + direction * GENERAL_ROUTE_START_OFFSET_CHUNKS;
    int chunkZ = originChunkZ;
    boolean alternateLane = false;

    for (int index = 0; index < pointCount; index++) {
      points.add(toChunkCenter(chunkX, chunkZ, origin.y));
      if (alternateLane) {
        chunkX += direction;
      }
      alternateLane = !alternateLane;
      chunkZ = alternateLane ? originChunkZ + verticalDirection : originChunkZ;
    }

    return points;
  }

  private static List<Vec3> computeExplorationWaypoints(
    Vec3 origin, long durationMs, boolean activeBlock) {
    int pointCount = getPlannedMoveCount(durationMs,
      getMoveIntervalMs(BenchmarkScenarioId.EXPLORATION));
    List<Vec3> points = new ArrayList<>(pointCount);
    int direction = activeBlock ? 1 : -1;
    int startChunkX = blockToChunk(origin.x) + direction * EXPLORATION_ROUTE_START_OFFSET_CHUNKS;
    for (int index = 0; index < pointCount; index++) {
      int chunkX = startChunkX + direction * index;
      points.add(toChunkCenter(chunkX, blockToChunk(origin.z), origin.y));
    }

    return points;
  }

  private static List<Vec3> computeLegacyWaypoints(BenchmarkScenario scenario, Vec3 origin,
    long durationMs, boolean activeBlock, Set<Long> reservedChunkKeys) {
    int pointCount = getPlannedMoveCount(durationMs, getMoveIntervalMs(scenario));
    List<Vec3> points = new ArrayList<>(pointCount);
    int originChunkX = blockToChunk(origin.x);
    int originChunkZ = blockToChunk(origin.z);
    int moveAreaHalfChunks = Math.max(1, MOVE_AREA_HALF_SIZE >> 4);
    int maxAttempts = pointCount * 50;
    long seed = 31L * scenario.id().ordinal()
      + (activeBlock ? 1_003L : 509L)
      + 67L * originChunkX
      + 97L * originChunkZ;
    Random random = new Random(seed);
    int attempts = 0;
    while (points.size() < pointCount && attempts++ < maxAttempts) {
      int chunkX = originChunkX + random.nextInt(moveAreaHalfChunks * 2 + 1) - moveAreaHalfChunks;
      int chunkZ = originChunkZ + random.nextInt(moveAreaHalfChunks * 2 + 1) - moveAreaHalfChunks;
      long chunkKey = ChunkPos.asLong(chunkX, chunkZ);
      if (!reservedChunkKeys.add(chunkKey)) {
        continue;
      }

      points.add(toChunkCenter(chunkX, chunkZ, origin.y));
    }

    return points;
  }

  private static int getPlannedMoveCount(long durationMs, long moveIntervalMs) {
    long safeIntervalMs = Math.max(TICK_DURATION_MS, moveIntervalMs);

    return (int) Math.clamp((durationMs + safeIntervalMs - 1L) / safeIntervalMs, 1L,
      Integer.MAX_VALUE);
  }

  private static Vec3 toChunkCenter(int chunkX, int chunkZ, double y) {
    return new Vec3(chunkX * 16.0d + 8.0d, y, chunkZ * 16.0d + 8.0d);
  }

  private static long getMoveIntervalMs(BenchmarkScenario scenario) {
    return getMoveIntervalMs(scenario.id());
  }

  private static long getMoveIntervalMs(BenchmarkScenarioId scenarioId) {
    if (scenarioId == BenchmarkScenarioId.GENERAL && !autoMoveRequested) {
      return GENERAL_MOVE_INTERVAL_MS;
    }

    return scenarioId == BenchmarkScenarioId.EXPLORATION
      ? Math.max(TICK_DURATION_MS,
      SimulationDistanceConfig.movementThrottleSampleTicks * TICK_DURATION_MS)
      : LEGACY_MOVE_INTERVAL_MS;
  }

  private static long getInitialMoveSettleDelayMs(BenchmarkScenario scenario) {
    if (scenario.id() == BenchmarkScenarioId.GENERAL && !autoMoveRequested) {
      return GENERAL_POST_MOVE_SETTLE_DELAY_MS;
    }

    return scenario.id() == BenchmarkScenarioId.EXPLORATION
      ? EXPLORATION_START_SETTLE_DELAY_MS
      : LEGACY_POST_MOVE_SETTLE_DELAY_MS;
  }

  private static long getPostMoveSettleDelayMs(BenchmarkScenario scenario) {
    if (scenario.id() == BenchmarkScenarioId.GENERAL && !autoMoveRequested) {
      return GENERAL_POST_MOVE_SETTLE_DELAY_MS;
    }

    return scenario.id() == BenchmarkScenarioId.EXPLORATION ? 0L : LEGACY_POST_MOVE_SETTLE_DELAY_MS;
  }

  private static BenchmarkScenarioResult.ScenarioValidation buildScenarioValidation(
    BenchmarkScenario scenario, PerformanceStats.Snapshot statsDelta) {
    if (scenario.id() != BenchmarkScenarioId.EXPLORATION) {
      return BenchmarkScenarioResult.ScenarioValidation.none();
    }

    Set<Long> chunkTargets = Set.copyOf(currentMovementChunkTargets);
    long movementAdjustmentCount = statsDelta.simulationDistanceMovementAdjustments();
    long movementThrottleSampleCount = statsDelta.simulationDistanceMovementThrottleSamples();
    long movementMaxReduction = statsDelta.simulationDistanceMovementMaxReduction();
    boolean movementSignalObserved = movementAdjustmentCount > 0L
      || movementThrottleSampleCount > 0L
      || movementMaxReduction > 0L;

    return new BenchmarkScenarioResult.ScenarioValidation(
      currentMovementStepCount,
      chunkTargets.size(),
      movementAdjustmentCount,
      movementThrottleSampleCount,
      movementMaxReduction,
      movementSignalObserved,
      chunkTargets);
  }

  private static int countChunkTargets(Map<BenchmarkScenarioId, List<Vec3>> scenarioTargets) {
    Set<Long> chunkKeys = new HashSet<>();
    for (List<Vec3> targets : scenarioTargets.values()) {
      for (Vec3 target : targets) {
        chunkKeys.add(getChunkKey(target));
      }
    }

    return chunkKeys.size();
  }

  private static int countSharedChunkTargets(Map<BenchmarkScenarioId, List<Vec3>> baselineTargets,
    Map<BenchmarkScenarioId, List<Vec3>> activeTargets) {
    Set<Long> baselineChunkKeys = new HashSet<>();
    for (List<Vec3> targets : baselineTargets.values()) {
      for (Vec3 target : targets) {
        baselineChunkKeys.add(getChunkKey(target));
      }
    }

    Set<Long> sharedChunkKeys = new HashSet<>();
    for (List<Vec3> targets : activeTargets.values()) {
      for (Vec3 target : targets) {
        long chunkKey = getChunkKey(target);
        if (baselineChunkKeys.contains(chunkKey)) {
          sharedChunkKeys.add(chunkKey);
        }
      }
    }

    return sharedChunkKeys.size();
  }

  private static void teleportToNextWaypoint(ServerPlayer player, List<Vec3> moveWaypoints) {
    Vec3 target = moveWaypoints.get(currentWaypointIndex % moveWaypoints.size());
    currentWaypointIndex++;
    currentMovementStepCount++;
    currentMovementChunkTargets.add(getChunkKey(target));
    teleportToSurface(player, target);
  }

  private static void teleportToSurface(ServerPlayer player, Vec3 target) {
    try {
      player.teleportTo(target.x, TELEPORT_Y, target.z);
    } catch (Exception exception) {
      log.warn("[Benchmark] Auto-move teleport to {},{},{} failed: {}",
        (int) target.x, (int) TELEPORT_Y, (int) target.z, exception.getMessage());
    }
  }

  private static void teleportToPos(ServerPlayer player, Vec3 pos) {
    try {
      player.teleportTo(pos.x, pos.y, pos.z);
    } catch (Exception exception) {
      log.warn("[Benchmark] Return teleport to {},{},{} failed: {}",
        (int) pos.x, (int) pos.y, (int) pos.z, exception.getMessage());
    }
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
      for (Entity ignored : level.getAllEntities()) {
        count++;
      }
    }

    return count;
  }

  private static double getProcessCpuPercent() {
    try {
      var operatingSystemMXBean = ManagementFactory.getOperatingSystemMXBean();
      if (operatingSystemMXBean instanceof OperatingSystemMXBean sunBean) {
        double load = sunBean.getProcessCpuLoad();
        return load < 0.0d ? -1.0d : load * 100.0d;
      }
    } catch (Exception ignored) {
    }

    return -1.0d;
  }

  private static double average(List<Double> samples) {
    return average(samples, 50.0d);
  }

  private static double average(List<Double> samples, double fallback) {
    if (samples.isEmpty()) {
      return fallback;
    }

    double sum = 0.0d;
    for (double sample : samples) {
      sum += sample;
    }

    return sum / samples.size();
  }

  private static double min(List<Double> samples) {
    return samples.stream().mapToDouble(Double::doubleValue).min().orElse(50.0d);
  }

  private static double percentile(List<Double> samples, double percentile) {
    if (samples.isEmpty()) {
      return 50.0d;
    }

    List<Double> sorted = new ArrayList<>(samples);
    Collections.sort(sorted);
    int index = (int) Math.ceil(percentile / 100.0d * sorted.size()) - 1;

    return sorted.get(Math.max(0, Math.min(index, sorted.size() - 1)));
  }

  private static double max(List<Double> samples) {
    return max(samples, 50.0d);
  }

  private static double max(List<Double> samples, double fallback) {
    return samples.stream().mapToDouble(Double::doubleValue).max().orElse(fallback);
  }

  @FunctionalInterface
  private interface PassiveStageCallback {

    void run(boolean stageComplete);
  }
}
