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

package de.markusbordihn.adaptiveperformancetweaks.feature.gamerules;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.compat.ModCompat;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPosition;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.gamerules.GameRule;
import net.minecraft.world.level.gamerules.GameRules;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class GameRuleManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_GAMERULES);
  private static final String LOG_PREFIX = "[Gamerule]";
  private static final long MILLIS_PER_TICK = 50L;

  private static GameRules gameRules;
  private static int configuredRandomTickSpeedMax = 3;
  private static int configuredMaxEntityCramming = GameRulesConfig.maxEntityCramming;
  private static boolean configuredBlockExplosionDropDecay;
  private static boolean configuredElytraMovementCheck;
  private static boolean configuredSpawnPhantoms;
  private static int configuredFireSpreadRadiusAroundPlayer = 128;
  private static boolean configuredMobExplosionDropDecay;
  private static boolean configuredRaids;
  private static boolean configuredDoPatrolSpawning;
  private static boolean configuredDoTraderSpawning;
  private static boolean configuredTntExplosionDropDecay;
  private static boolean configuredDoVinesSpread;
  private static boolean configuredDoWardenSpawning;
  private static ServerLoadLevel currentLoadLevel = ServerLoadLevel.NORMAL;
  private static long lastUpdateTime = System.currentTimeMillis();
  private static long lastRandomTickRecoveryTime = System.currentTimeMillis();
  private static long randomTickWarmupUntilTime = 0L;
  private static boolean randomTickPlayerActivityRecoveryPending = false;

  private GameRuleManager() {
  }

  public static void handleServerStarting(MinecraftServer minecraftServer) {
    gameRules = minecraftServer.getWorldData().getGameRules();
    configuredRandomTickSpeedMax = (Integer) gameRules.get(GameRules.RANDOM_TICK_SPEED);
    configuredMaxEntityCramming = (Integer) gameRules.get(GameRules.MAX_ENTITY_CRAMMING);
    configuredBlockExplosionDropDecay =
      (Boolean) gameRules.get(GameRules.BLOCK_EXPLOSION_DROP_DECAY);
    configuredElytraMovementCheck = (Boolean) gameRules.get(GameRules.ELYTRA_MOVEMENT_CHECK);
    configuredSpawnPhantoms = (Boolean) gameRules.get(GameRules.SPAWN_PHANTOMS);
    configuredFireSpreadRadiusAroundPlayer =
      (Integer) gameRules.get(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER);
    configuredMobExplosionDropDecay = (Boolean) gameRules.get(GameRules.MOB_EXPLOSION_DROP_DECAY);
    configuredRaids = (Boolean) gameRules.get(GameRules.RAIDS);
    configuredDoPatrolSpawning = (Boolean) gameRules.get(GameRules.SPAWN_PATROLS);
    configuredDoTraderSpawning = (Boolean) gameRules.get(GameRules.SPAWN_WANDERING_TRADERS);
    configuredTntExplosionDropDecay =
      (Boolean) gameRules.get(GameRules.TNT_EXPLOSION_DROP_DECAY);
    configuredDoVinesSpread = (Boolean) gameRules.get(GameRules.SPREAD_VINES);
    configuredDoWardenSpawning = (Boolean) gameRules.get(GameRules.SPAWN_WARDENS);
    lastUpdateTime = System.currentTimeMillis();
    lastRandomTickRecoveryTime = lastUpdateTime;
    randomTickWarmupUntilTime = 0L;
    randomTickPlayerActivityRecoveryPending = false;

    if (!FeatureToggle.GAMERULES.isEnabled()) {
      return;
    }

    if (GameRulesConfig.randomTickSpeedEnabled) {
      log.debug(
        "{} Random Tick Speed will be optimized between 1 and {}",
        LOG_PREFIX, getConfiguredRandomTickSpeedMax());
      if ((Integer) gameRules.get(GameRules.RANDOM_TICK_SPEED)
        != getConfiguredRandomTickSpeedMax()) {
        setRandomTickSpeed(getConfiguredRandomTickSpeedMax());
      }
    }
    if (GameRulesConfig.entityCrammingEnabled) {
      log.debug(
        "{} Max Entity Cramming will be optimized between {} and {}",
        LOG_PREFIX,
        GameRulesConfig.minEntityCramming,
        GameRulesConfig.maxEntityCramming);
      if ((Integer) gameRules.get(GameRules.MAX_ENTITY_CRAMMING)
        != GameRulesConfig.maxEntityCramming) {
        setMaxEntityCramming(GameRulesConfig.maxEntityCramming);
      }
    }
    logOptimizationInfo();
  }

  public static void handleFeatureEnabled(MinecraftServer minecraftServer) {
    handleServerStarting(minecraftServer);
  }

  public static void handleServerStopping() {
    gameRules = null;
    configuredRandomTickSpeedMax = 3;
    configuredMaxEntityCramming = GameRulesConfig.maxEntityCramming;
    configuredFireSpreadRadiusAroundPlayer = 128;
    lastUpdateTime = System.currentTimeMillis();
    lastRandomTickRecoveryTime = lastUpdateTime;
    randomTickWarmupUntilTime = 0L;
    randomTickPlayerActivityRecoveryPending = false;
  }

  public static void handlePlayerLoggedIn(ServerPlayer player) {
    applyPlayerWarmup("login");
  }

  public static void handlePlayerTeleported(ServerPlayer player) {
    applyPlayerWarmup("teleport");
  }

  public static void handleServerTick() {
    if (!FeatureToggle.GAMERULES.isEnabled() || !GameRulesConfig.randomTickSpeedEnabled) {
      return;
    }

    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null) {
      return;
    }

    gameRules = minecraftServer.getWorldData().getGameRules();
    if (GameRulesConfig.movementWarmupEnabled && hasActiveMovementWarmup()) {
      applyPlayerWarmup("movement");
    }

    if (isRandomTickWarmupActive()) {
      setRandomTickSpeed(1);
      randomTickPlayerActivityRecoveryPending = true;
      return;
    }

    recoverRandomTickSpeedFromPlayerWarmup();
  }

  public static void handleFeatureDisabled() {
    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null) {
      gameRules = null;
      lastRandomTickRecoveryTime = System.currentTimeMillis();
      randomTickWarmupUntilTime = 0L;
      randomTickPlayerActivityRecoveryPending = false;
      return;
    }

    gameRules = minecraftServer.getWorldData().getGameRules();
    lastRandomTickRecoveryTime = System.currentTimeMillis();
    randomTickWarmupUntilTime = 0L;
    randomTickPlayerActivityRecoveryPending = false;
    restoreConfiguredDefaults();
  }

  public static void handleServerLoadEvent(ServerLoadEvent event) {
    if (!FeatureToggle.GAMERULES.isEnabled()) {
      return;
    }

    currentLoadLevel = event.getServerLoadLevel();
    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null) {
      return;
    }
    gameRules = minecraftServer.getWorldData().getGameRules();
    boolean randomTickWarmupActive = isRandomTickWarmupActive();

    if (event.hasVeryHighServerLoad()) {
      applyVeryHighLoadOptimizations(randomTickWarmupActive);
      return;
    }

    if (event.hasHighServerLoad()) {
      applyHighLoadOptimizations(randomTickWarmupActive);
      return;
    }

    if (randomTickWarmupActive && GameRulesConfig.randomTickSpeedEnabled) {
      randomTickPlayerActivityRecoveryPending = true;
      setRandomTickSpeed(1);
    }

    if (System.currentTimeMillis() - lastUpdateTime < 10_000L) {
      return;
    }

    restoreNormalLoad();

    if (!event.getServerLoadLevel().isAtLeast(ServerLoadLevel.NORMAL)) {
      if (GameRulesConfig.randomTickSpeedEnabled && !randomTickWarmupActive
        && !randomTickPlayerActivityRecoveryPending) {
        increaseRandomTickSpeed();
      }
      if (GameRulesConfig.entityCrammingEnabled) {
        increaseMaxEntityCramming();
      }
    }

    lastUpdateTime = System.currentTimeMillis();
  }

  private static void applyVeryHighLoadOptimizations(boolean randomTickWarmupActive) {
    if (GameRulesConfig.entityCrammingEnabled) {
      decreaseMaxEntityCramming();
    }
    if (GameRulesConfig.randomTickSpeedEnabled) {
      if (randomTickWarmupActive) {
        setRandomTickSpeed(1);
      } else {
        decreaseRandomTickSpeed();
      }
    }
    if (GameRulesConfig.blockExplodesEnabled) {
      enableBlockExplosionDropDecay();
    }
    if (GameRulesConfig.elytraMovementCheckEnabled) {
      disableElytraMovementCheck();
    }
    if (GameRulesConfig.fireTickEnabled) {
      disableFireTick();
    }
    if (GameRulesConfig.insomniaEnabled) {
      disableInsomnia();
    }
    if (GameRulesConfig.mobExplodesEnabled) {
      enableMobExplosionDropDecay();
    }
    if (GameRulesConfig.raidsEnabled) {
      disableRaids();
    }
    if (GameRulesConfig.patrolSpawningEnabled) {
      disablePatrolSpawning();
    }
    if (GameRulesConfig.traderSpawningEnabled) {
      disableTraderSpawning();
    }
    if (GameRulesConfig.tntExplodesEnabled) {
      enableTntExplosionDropDecay();
    }
    if (GameRulesConfig.vinesSpreadEnabled) {
      disableVinesSpread();
    }
    if (GameRulesConfig.wardenSpawningEnabled) {
      disableWardenSpawning();
    }
  }

  private static void applyHighLoadOptimizations(boolean randomTickWarmupActive) {
    if (GameRulesConfig.randomTickSpeedEnabled) {
      if (randomTickWarmupActive) {
        setRandomTickSpeed(1);
      } else {
        decreaseRandomTickSpeed();
      }
    }
    if (GameRulesConfig.fireTickEnabled) {
      disableFireTick();
    }
    if (GameRulesConfig.raidsEnabled) {
      disableRaids();
    }
  }

  private static void restoreNormalLoad() {
    if (GameRulesConfig.elytraMovementCheckEnabled) {
      enableElytraMovementCheck();
    }
    if (GameRulesConfig.fireTickEnabled) {
      enableFireTick();
    }
    if (GameRulesConfig.raidsEnabled) {
      enableRaids();
    }
    if (GameRulesConfig.patrolSpawningEnabled) {
      enablePatrolSpawning();
    }
    if (GameRulesConfig.insomniaEnabled) {
      enableInsomnia();
    }
    if (GameRulesConfig.traderSpawningEnabled) {
      enableTraderSpawning();
    }
    if (GameRulesConfig.tntExplodesEnabled) {
      disableTntExplosionDropDecay();
    }
    if (GameRulesConfig.vinesSpreadEnabled) {
      enableVinesSpread();
    }
    if (GameRulesConfig.wardenSpawningEnabled) {
      enableWardenSpawning();
    }
  }

  private static void logOptimizationInfo() {
    StringBuilder active = new StringBuilder();
    if (GameRulesConfig.blockExplodesEnabled) {
      log.debug("{} Block explosions will use drop decay during very high server load.",
        LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("blockExplosions");
    }
    if (GameRulesConfig.elytraMovementCheckEnabled) {
      log.debug("{} Elytra movement check will be disabled during very high server load.",
        LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("elytraMovementCheck");
    }
    if (GameRulesConfig.fireTickEnabled) {
      log.debug("{} Fire spread around players will be disabled during high server load.",
        LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("fireTick");
    }
    if (GameRulesConfig.insomniaEnabled) {
      log.debug("{} Insomnia will be disabled during very high server load.", LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("insomnia");
    }
    if (GameRulesConfig.mobExplodesEnabled) {
      log.debug("{} Mob explosions will use drop decay during very high server load.",
        LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("mobExplosions");
    }
    if (GameRulesConfig.patrolSpawningEnabled) {
      log.debug("{} Patrol spawning will be disabled during very high server load.", LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("patrolSpawning");
    }
    if (GameRulesConfig.raidsEnabled) {
      log.debug("{} Raids will be disabled during very high server load.", LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("raids");
    }
    if (GameRulesConfig.traderSpawningEnabled) {
      log.debug("{} Trader spawning will be disabled during very high server load.", LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("traderSpawning");
    }
    if (GameRulesConfig.tntExplodesEnabled) {
      log.debug("{} TNT explosions will use drop decay during very high server load.",
        LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("tntExplosions");
    }
    if (GameRulesConfig.vinesSpreadEnabled) {
      log.debug("{} Vines spread will be disabled during very high server load.", LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("vinesSpread");
    }
    if (GameRulesConfig.wardenSpawningEnabled) {
      log.debug("{} Warden spawning will be disabled during very high server load.", LOG_PREFIX);
      active.append(!active.isEmpty() ? ", " : "").append("wardenSpawning");
    }
    if (!active.isEmpty()) {
      log.info("{} Very-high-load rules active: {}", LOG_PREFIX, active);
    }
  }

  private static void enableBlockExplosionDropDecay() {
    if (!(Boolean) gameRules.get(GameRules.BLOCK_EXPLOSION_DROP_DECAY)) {
      log.debug("{} blockExplosionDropDecay -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.BLOCK_EXPLOSION_DROP_DECAY, true);
    }
  }

  private static void restoreConfiguredDefaults() {
    restoreRandomTickSpeed();
    restoreMaxEntityCramming();
    setGameRule(GameRules.BLOCK_EXPLOSION_DROP_DECAY, configuredBlockExplosionDropDecay);
    setGameRule(GameRules.ELYTRA_MOVEMENT_CHECK, configuredElytraMovementCheck);
    setGameRule(GameRules.SPAWN_PHANTOMS, configuredSpawnPhantoms);
    setGameRule(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER,
      configuredFireSpreadRadiusAroundPlayer);
    setGameRule(GameRules.MOB_EXPLOSION_DROP_DECAY, configuredMobExplosionDropDecay);
    setGameRule(GameRules.RAIDS, configuredRaids);
    setGameRule(GameRules.SPAWN_PATROLS, configuredDoPatrolSpawning);
    setGameRule(GameRules.SPAWN_WANDERING_TRADERS, configuredDoTraderSpawning);
    setGameRule(GameRules.TNT_EXPLOSION_DROP_DECAY, configuredTntExplosionDropDecay);
    setGameRule(GameRules.SPREAD_VINES, configuredDoVinesSpread);
    setGameRule(GameRules.SPAWN_WARDENS, configuredDoWardenSpawning);
  }

  private static void enableElytraMovementCheck() {
    if (!(Boolean) gameRules.get(GameRules.ELYTRA_MOVEMENT_CHECK)) {
      log.debug("{} elytraMovementCheck -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.ELYTRA_MOVEMENT_CHECK, true);
    }
  }

  private static void disableElytraMovementCheck() {
    if ((Boolean) gameRules.get(GameRules.ELYTRA_MOVEMENT_CHECK)) {
      log.debug("{} elytraMovementCheck -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.ELYTRA_MOVEMENT_CHECK, false);
    }
  }

  public static void enableFireTick() {
    int current = (Integer) gameRules.get(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER);
    if (current != configuredFireSpreadRadiusAroundPlayer) {
      log.debug("{} fireSpreadRadiusAroundPlayer: {} -> {}", LOG_PREFIX, current,
        configuredFireSpreadRadiusAroundPlayer);
      executeGameRuleChange(
        GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER, configuredFireSpreadRadiusAroundPlayer);
    }
  }

  private static void disableFireTick() {
    int current = (Integer) gameRules.get(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER);
    if (current != 0) {
      log.debug("{} fireSpreadRadiusAroundPlayer: {} -> 0", LOG_PREFIX, current);
      executeGameRuleChange(GameRules.FIRE_SPREAD_RADIUS_AROUND_PLAYER, 0);
    }
  }

  public static void enableInsomnia() {
    if (!(Boolean) gameRules.get(GameRules.SPAWN_PHANTOMS)) {
      log.debug("{} spawnPhantoms -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPAWN_PHANTOMS, true);
    }
  }

  private static void disableInsomnia() {
    if ((Boolean) gameRules.get(GameRules.SPAWN_PHANTOMS)) {
      log.debug("{} spawnPhantoms -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPAWN_PHANTOMS, false);
    }
  }

  private static void enableMobExplosionDropDecay() {
    if (!(Boolean) gameRules.get(GameRules.MOB_EXPLOSION_DROP_DECAY)) {
      log.debug("{} mobExplosionDropDecay -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.MOB_EXPLOSION_DROP_DECAY, true);
    }
  }

  public static void enablePatrolSpawning() {
    if (!(Boolean) gameRules.get(GameRules.SPAWN_PATROLS)) {
      log.debug("{} doPatrolSpawning -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPAWN_PATROLS, true);
    }
  }

  private static void disablePatrolSpawning() {
    if ((Boolean) gameRules.get(GameRules.SPAWN_PATROLS)) {
      log.debug("{} doPatrolSpawning -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPAWN_PATROLS, false);
    }
  }

  public static void enableRaids() {
    if (!(Boolean) gameRules.get(GameRules.RAIDS)) {
      log.debug("{} raids -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RAIDS, true);
    }
  }

  private static void disableRaids() {
    if ((Boolean) gameRules.get(GameRules.RAIDS)) {
      log.debug("{} raids -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RAIDS, false);
    }
  }

  private static void enableTraderSpawning() {
    if (!(Boolean) gameRules.get(GameRules.SPAWN_WANDERING_TRADERS)) {
      log.debug("{} doTraderSpawning -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPAWN_WANDERING_TRADERS, true);
    }
  }

  private static void disableTraderSpawning() {
    if ((Boolean) gameRules.get(GameRules.SPAWN_WANDERING_TRADERS)) {
      log.debug("{} doTraderSpawning -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPAWN_WANDERING_TRADERS, false);
    }
  }

  private static void enableTntExplosionDropDecay() {
    if (!(Boolean) gameRules.get(GameRules.TNT_EXPLOSION_DROP_DECAY)) {
      log.debug("{} tntExplosionDropDecay -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.TNT_EXPLOSION_DROP_DECAY, true);
    }
  }

  private static void disableTntExplosionDropDecay() {
    if ((Boolean) gameRules.get(GameRules.TNT_EXPLOSION_DROP_DECAY)) {
      log.debug("{} tntExplosionDropDecay -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.TNT_EXPLOSION_DROP_DECAY, false);
    }
  }

  private static void enableVinesSpread() {
    if (!(Boolean) gameRules.get(GameRules.SPREAD_VINES)) {
      log.debug("{} doVinesSpread -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPREAD_VINES, true);
    }
  }

  private static void disableVinesSpread() {
    if ((Boolean) gameRules.get(GameRules.SPREAD_VINES)) {
      log.debug("{} doVinesSpread -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPREAD_VINES, false);
    }
  }

  private static void enableWardenSpawning() {
    if (!(Boolean) gameRules.get(GameRules.SPAWN_WARDENS)) {
      log.debug("{} doWardenSpawning -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPAWN_WARDENS, true);
    }
  }

  private static void disableWardenSpawning() {
    if ((Boolean) gameRules.get(GameRules.SPAWN_WARDENS)) {
      log.debug("{} doWardenSpawning -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.SPAWN_WARDENS, false);
    }
  }

  private static void decreaseRandomTickSpeed() {
    setRandomTickSpeed((Integer) gameRules.get(GameRules.RANDOM_TICK_SPEED) - 1);
  }

  private static void increaseRandomTickSpeed() {
    setRandomTickSpeed((Integer) gameRules.get(GameRules.RANDOM_TICK_SPEED) + 1);
  }

  private static void setRandomTickSpeed(int tickSpeed) {
    int clamped = Math.max(1, Math.min(tickSpeed, getConfiguredRandomTickSpeedMax()));
    int current = (Integer) gameRules.get(GameRules.RANDOM_TICK_SPEED);
    if (current != clamped) {
      log.debug("{} randomTickSpeed: {} -> {}", LOG_PREFIX, current, clamped);
      executeGameRuleChange(GameRules.RANDOM_TICK_SPEED, clamped);
    }
  }

  private static void decreaseMaxEntityCramming() {
    setMaxEntityCramming((Integer) gameRules.get(GameRules.MAX_ENTITY_CRAMMING) - 1);
  }

  private static void increaseMaxEntityCramming() {
    setMaxEntityCramming((Integer) gameRules.get(GameRules.MAX_ENTITY_CRAMMING) + 1);
  }

  private static void setMaxEntityCramming(int maxEntity) {
    int clamped = Math.max(GameRulesConfig.minEntityCramming,
      Math.min(maxEntity, GameRulesConfig.maxEntityCramming));
    int preAdjustedClamped = clamped;
    if (ModCompat.isModLoaded("minecolonies")
      && clamped < GameRulesConfig.minEntityCrammingMineColonies) {
      clamped = GameRulesConfig.minEntityCrammingMineColonies;
    }
    int current = (Integer) gameRules.get(GameRules.MAX_ENTITY_CRAMMING);
    if (current != clamped) {
      if (clamped != preAdjustedClamped) {
        log.warn(
          "{} MineColonies detected: raising minEntityCramming from {} to {} to prevent stuck entities",
          LOG_PREFIX,
          preAdjustedClamped,
          GameRulesConfig.minEntityCrammingMineColonies);
      }
      log.debug("{} maxEntityCramming: {} -> {}", LOG_PREFIX, current, clamped);
      executeGameRuleChange(GameRules.MAX_ENTITY_CRAMMING, clamped);
    }
  }

  private static void restoreRandomTickSpeed() {
    int current = (Integer) gameRules.get(GameRules.RANDOM_TICK_SPEED);
    if (current != configuredRandomTickSpeedMax) {
      log.debug("{} randomTickSpeed: {} -> {}", LOG_PREFIX, current, configuredRandomTickSpeedMax);
      executeGameRuleChange(GameRules.RANDOM_TICK_SPEED, configuredRandomTickSpeedMax);
    }
  }

  private static void restoreMaxEntityCramming() {
    int current = (Integer) gameRules.get(GameRules.MAX_ENTITY_CRAMMING);
    if (current != configuredMaxEntityCramming) {
      log.debug("{} maxEntityCramming: {} -> {}", LOG_PREFIX, current,
        configuredMaxEntityCramming);
      executeGameRuleChange(GameRules.MAX_ENTITY_CRAMMING, configuredMaxEntityCramming);
    }
  }

  private static void executeGameRuleChange(GameRule<Boolean> rule, boolean value) {
    PerformanceStats.gameRulesChanged++;
    if (gameRules != null) {
      gameRules.set(rule, value, ServerManager.getMinecraftServer());
    }
  }

  private static void executeGameRuleChange(GameRule<Integer> rule, int value) {
    PerformanceStats.gameRulesChanged++;
    if (gameRules != null) {
      gameRules.set(rule, value, ServerManager.getMinecraftServer());
    }
  }

  private static void setGameRule(GameRule<Boolean> rule, boolean value) {
    if ((Boolean) gameRules.get(rule) != value) {
      executeGameRuleChange(rule, value);
    }
  }

  private static void setGameRule(GameRule<Integer> rule, int value) {
    if ((Integer) gameRules.get(rule) != value) {
      executeGameRuleChange(rule, value);
    }
  }

  private static void applyPlayerWarmup(String triggerSource) {
    if (!FeatureToggle.GAMERULES.isEnabled() || !GameRulesConfig.randomTickSpeedEnabled) {
      return;
    }
    if ("movement".equals(triggerSource)) {
      if (!GameRulesConfig.movementWarmupEnabled) {
        return;
      }
    } else if (!GameRulesConfig.loginWarmupEnabled) {
      return;
    }

    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null) {
      return;
    }

    gameRules = minecraftServer.getWorldData().getGameRules();
    long now = System.currentTimeMillis();
    long previousWarmupUntilTime = randomTickWarmupUntilTime;
    long warmupUntilTime = now + SimulationDistanceConfig.movementThrottleLoginTicks
      * MILLIS_PER_TICK;
    randomTickWarmupUntilTime = Math.max(randomTickWarmupUntilTime, warmupUntilTime);
    lastRandomTickRecoveryTime = now;
    randomTickPlayerActivityRecoveryPending = true;
    if (now >= previousWarmupUntilTime) {
      log.debug("{} randomTick {} warmup started for {} ticks", LOG_PREFIX, triggerSource,
        SimulationDistanceConfig.movementThrottleLoginTicks);
    } else if (!"movement".equals(triggerSource)
      || randomTickWarmupUntilTime - previousWarmupUntilTime >= 1_000L) {
      log.debug("{} randomTick {} warmup extended to {}ms", LOG_PREFIX, triggerSource,
        randomTickWarmupUntilTime - now);
    }
    setRandomTickSpeed(1);
  }

  private static boolean hasActiveMovementWarmup() {
    for (PlayerPosition playerPosition : PlayerPositionManager.getPlayerPositionMap().values()) {
      if (playerPosition.hasRecentMovementDistance(
        SimulationDistanceConfig.movementThrottleDistanceThresholdBlocks)) {
        return true;
      }
    }

    return false;
  }

  private static void recoverRandomTickSpeedFromPlayerWarmup() {
    if (!randomTickPlayerActivityRecoveryPending
      || currentLoadLevel.isAtLeast(ServerLoadLevel.NORMAL)
      || gameRules == null) {
      return;
    }

    long now = System.currentTimeMillis();
    if (now - lastRandomTickRecoveryTime < 10_000L) {
      return;
    }

    int targetRandomTickSpeed = getConfiguredRandomTickSpeedMax();
    int currentRandomTickSpeed = (Integer) gameRules.get(GameRules.RANDOM_TICK_SPEED);
    if (currentRandomTickSpeed >= targetRandomTickSpeed) {
      randomTickPlayerActivityRecoveryPending = false;
      lastRandomTickRecoveryTime = now;
      return;
    }

    int nextRandomTickSpeed = Math.min(targetRandomTickSpeed, currentRandomTickSpeed + 1);
    log.debug("{} randomTick player warmup recovery: {} -> {} (load={})", LOG_PREFIX,
      currentRandomTickSpeed, nextRandomTickSpeed, currentLoadLevel);
    setRandomTickSpeed(nextRandomTickSpeed);
    lastRandomTickRecoveryTime = now;
    randomTickPlayerActivityRecoveryPending = nextRandomTickSpeed < targetRandomTickSpeed;
  }

  private static int getConfiguredRandomTickSpeedMax() {
    return Math.max(1, Math.min(configuredRandomTickSpeedMax, GameRulesConfig.randomTickSpeed));
  }

  private static boolean isRandomTickWarmupActive() {
    return System.currentTimeMillis() < randomTickWarmupUntilTime;
  }
}
