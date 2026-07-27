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
import net.minecraft.world.level.GameRules;
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
  private static boolean configuredDisableElytraMovementCheck;
  private static boolean configuredDoFireTick;
  private static boolean configuredDoInsomnia;
  private static boolean configuredMobExplosionDropDecay;
  private static boolean configuredDisableRaids;
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
    gameRules = minecraftServer.getGameRules();
    configuredRandomTickSpeedMax = gameRules.getInt(GameRules.RULE_RANDOMTICKING);
    configuredMaxEntityCramming = gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING);
    configuredBlockExplosionDropDecay =
      gameRules.getBoolean(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY);
    configuredDisableElytraMovementCheck =
      gameRules.getBoolean(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK);
    configuredDoFireTick = gameRules.getBoolean(GameRules.RULE_DOFIRETICK);
    configuredDoInsomnia = gameRules.getBoolean(GameRules.RULE_DOINSOMNIA);
    configuredMobExplosionDropDecay = gameRules.getBoolean(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY);
    configuredDisableRaids = gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS);
    configuredDoPatrolSpawning = gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING);
    configuredDoTraderSpawning = gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING);
    configuredTntExplosionDropDecay =
      gameRules.getBoolean(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY);
    configuredDoVinesSpread = gameRules.getBoolean(GameRules.RULE_DO_VINES_SPREAD);
    configuredDoWardenSpawning = gameRules.getBoolean(GameRules.RULE_DO_WARDEN_SPAWNING);
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
      if (gameRules.getInt(GameRules.RULE_RANDOMTICKING) != getConfiguredRandomTickSpeedMax()) {
        setRandomTickSpeed(getConfiguredRandomTickSpeedMax());
      }
    }
    if (GameRulesConfig.entityCrammingEnabled) {
      log.debug(
        "{} Max Entity Cramming will be optimized between {} and {}",
        LOG_PREFIX,
        GameRulesConfig.minEntityCramming,
        GameRulesConfig.maxEntityCramming);
      if (gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING)
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
    if (gameRules != null && FeatureToggle.GAMERULES.isEnabled()) {
      restoreConfiguredDefaults();
    }
    gameRules = null;
    configuredRandomTickSpeedMax = 3;
    configuredMaxEntityCramming = GameRulesConfig.maxEntityCramming;
    configuredDoFireTick = true;
    lastUpdateTime = System.currentTimeMillis();
    lastRandomTickRecoveryTime = lastUpdateTime;
    randomTickWarmupUntilTime = 0L;
    randomTickPlayerActivityRecoveryPending = false;
  }

  public static void handlePlayerLoggedIn(ServerPlayer player) {
    trackPlayerForMovementWarmup(player);
    applyPlayerWarmup("login");
  }

  public static void handlePlayerTeleported(ServerPlayer player) {
    trackPlayerForMovementWarmup(player);
    applyPlayerWarmup("teleport");
  }

  private static void trackPlayerForMovementWarmup(ServerPlayer player) {
    if (FeatureToggle.GAMERULES.isEnabled() && GameRulesConfig.randomTickSpeedEnabled
      && GameRulesConfig.movementWarmupEnabled) {
      PlayerPositionManager.handlePlayerLoggedIn(player);
    }
  }

  public static void handleServerTick() {
    if (!FeatureToggle.GAMERULES.isEnabled() || !GameRulesConfig.randomTickSpeedEnabled) {
      return;
    }

    MinecraftServer minecraftServer = ServerManager.getMinecraftServer();
    if (minecraftServer == null) {
      return;
    }

    gameRules = minecraftServer.getGameRules();
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

    gameRules = minecraftServer.getGameRules();
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
    gameRules = minecraftServer.getGameRules();
    boolean randomTickWarmupActive = isRandomTickWarmupActive();
    boolean optimizationAllowed =
      event.getServerLoadLevel().isAtLeast(GameRulesConfig.minOptimizationLoadLevel);

    if (event.hasVeryHighServerLoad() && optimizationAllowed) {
      applyVeryHighLoadOptimizations(randomTickWarmupActive);
      return;
    }

    if (event.hasHighServerLoad() && optimizationAllowed) {
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
      log.debug("{} Fire spread will be disabled during high server load.", LOG_PREFIX);
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
    if (!gameRules.getBoolean(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY)) {
      log.debug("{} blockExplosionDropDecay -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY, true);
    }
  }

  private static void restoreConfiguredDefaults() {
    if (GameRulesConfig.randomTickSpeedEnabled) {
      restoreRandomTickSpeed();
    }
    if (GameRulesConfig.entityCrammingEnabled) {
      restoreMaxEntityCramming();
    }
    if (GameRulesConfig.blockExplodesEnabled) {
      setGameRule(GameRules.RULE_BLOCK_EXPLOSION_DROP_DECAY, configuredBlockExplosionDropDecay);
    }
    if (GameRulesConfig.elytraMovementCheckEnabled) {
      setGameRule(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK,
        configuredDisableElytraMovementCheck);
    }
    if (GameRulesConfig.fireTickEnabled) {
      setGameRule(GameRules.RULE_DOFIRETICK, configuredDoFireTick);
    }
    if (GameRulesConfig.insomniaEnabled) {
      setGameRule(GameRules.RULE_DOINSOMNIA, configuredDoInsomnia);
    }
    if (GameRulesConfig.mobExplodesEnabled) {
      setGameRule(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY, configuredMobExplosionDropDecay);
    }
    if (GameRulesConfig.raidsEnabled) {
      setGameRule(GameRules.RULE_DISABLE_RAIDS, configuredDisableRaids);
    }
    if (GameRulesConfig.patrolSpawningEnabled) {
      setGameRule(GameRules.RULE_DO_PATROL_SPAWNING, configuredDoPatrolSpawning);
    }
    if (GameRulesConfig.traderSpawningEnabled) {
      setGameRule(GameRules.RULE_DO_TRADER_SPAWNING, configuredDoTraderSpawning);
    }
    if (GameRulesConfig.tntExplodesEnabled) {
      setGameRule(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY, configuredTntExplosionDropDecay);
    }
    if (GameRulesConfig.vinesSpreadEnabled) {
      setGameRule(GameRules.RULE_DO_VINES_SPREAD, configuredDoVinesSpread);
    }
    if (GameRulesConfig.wardenSpawningEnabled) {
      setGameRule(GameRules.RULE_DO_WARDEN_SPAWNING, configuredDoWardenSpawning);
    }
  }

  private static void enableElytraMovementCheck() {
    if (gameRules.getBoolean(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK)) {
      log.debug("{} disableElytraMovementCheck -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK, false);
    }
  }

  private static void disableElytraMovementCheck() {
    if (!gameRules.getBoolean(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK)) {
      log.debug("{} disableElytraMovementCheck -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DISABLE_ELYTRA_MOVEMENT_CHECK, true);
    }
  }

  public static void enableFireTick() {
    if (!gameRules.getBoolean(GameRules.RULE_DOFIRETICK)) {
      log.debug("{} doFireTick -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DOFIRETICK, true);
    }
  }

  private static void disableFireTick() {
    if (gameRules.getBoolean(GameRules.RULE_DOFIRETICK)) {
      log.debug("{} doFireTick -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DOFIRETICK, false);
    }
  }

  public static void enableInsomnia() {
    if (!gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)) {
      log.debug("{} doInsomnia -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DOINSOMNIA, true);
    }
  }

  private static void disableInsomnia() {
    if (gameRules.getBoolean(GameRules.RULE_DOINSOMNIA)) {
      log.debug("{} doInsomnia -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DOINSOMNIA, false);
    }
  }

  private static void enableMobExplosionDropDecay() {
    if (!gameRules.getBoolean(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY)) {
      log.debug("{} mobExplosionDropDecay -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_MOB_EXPLOSION_DROP_DECAY, true);
    }
  }

  public static void enablePatrolSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)) {
      log.debug("{} doPatrolSpawning -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DO_PATROL_SPAWNING, true);
    }
  }

  private static void disablePatrolSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_PATROL_SPAWNING)) {
      log.debug("{} doPatrolSpawning -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DO_PATROL_SPAWNING, false);
    }
  }

  public static void enableRaids() {
    if (gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)) {
      log.debug("{} disableRaids -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DISABLE_RAIDS, false);
    }
  }

  private static void disableRaids() {
    if (!gameRules.getBoolean(GameRules.RULE_DISABLE_RAIDS)) {
      log.debug("{} disableRaids -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DISABLE_RAIDS, true);
    }
  }

  private static void enableTraderSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)) {
      log.debug("{} doTraderSpawning -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DO_TRADER_SPAWNING, true);
    }
  }

  private static void disableTraderSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_TRADER_SPAWNING)) {
      log.debug("{} doTraderSpawning -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DO_TRADER_SPAWNING, false);
    }
  }

  private static void enableTntExplosionDropDecay() {
    if (!gameRules.getBoolean(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY)) {
      log.debug("{} tntExplosionDropDecay -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY, true);
    }
  }

  private static void disableTntExplosionDropDecay() {
    if (gameRules.getBoolean(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY)) {
      log.debug("{} tntExplosionDropDecay -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_TNT_EXPLOSION_DROP_DECAY, false);
    }
  }

  private static void enableVinesSpread() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_VINES_SPREAD)) {
      log.debug("{} doVinesSpread -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DO_VINES_SPREAD, true);
    }
  }

  private static void disableVinesSpread() {
    if (gameRules.getBoolean(GameRules.RULE_DO_VINES_SPREAD)) {
      log.debug("{} doVinesSpread -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DO_VINES_SPREAD, false);
    }
  }

  private static void enableWardenSpawning() {
    if (!gameRules.getBoolean(GameRules.RULE_DO_WARDEN_SPAWNING)) {
      log.debug("{} doWardenSpawning -> true", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DO_WARDEN_SPAWNING, true);
    }
  }

  private static void disableWardenSpawning() {
    if (gameRules.getBoolean(GameRules.RULE_DO_WARDEN_SPAWNING)) {
      log.debug("{} doWardenSpawning -> false", LOG_PREFIX);
      executeGameRuleChange(GameRules.RULE_DO_WARDEN_SPAWNING, false);
    }
  }

  private static void decreaseRandomTickSpeed() {
    setRandomTickSpeed(gameRules.getInt(GameRules.RULE_RANDOMTICKING) - 1);
  }

  private static void increaseRandomTickSpeed() {
    setRandomTickSpeed(gameRules.getInt(GameRules.RULE_RANDOMTICKING) + 1);
  }

  private static void setRandomTickSpeed(int tickSpeed) {
    int clamped = Math.max(1, Math.min(tickSpeed, getConfiguredRandomTickSpeedMax()));
    int current = gameRules.getInt(GameRules.RULE_RANDOMTICKING);
    if (current != clamped) {
      log.debug("{} randomTickSpeed: {} -> {}", LOG_PREFIX, current, clamped);
      executeGameRuleChange(GameRules.RULE_RANDOMTICKING, clamped);
    }
  }

  private static void decreaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) - 1);
  }

  private static void increaseMaxEntityCramming() {
    setMaxEntityCramming(gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING) + 1);
  }

  private static void setMaxEntityCramming(int maxEntity) {
    int clamped = Math.max(GameRulesConfig.minEntityCramming,
      Math.min(maxEntity, GameRulesConfig.maxEntityCramming));
    int preAdjustedClamped = clamped;
    if (ModCompat.isModLoaded("minecolonies")
      && clamped < GameRulesConfig.minEntityCrammingMineColonies) {
      clamped = GameRulesConfig.minEntityCrammingMineColonies;
    }
    int current = gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING);
    if (current != clamped) {
      if (clamped != preAdjustedClamped) {
        log.warn(
          "{} MineColonies detected: raising minEntityCramming from {} to {} to prevent stuck entities",
          LOG_PREFIX,
          preAdjustedClamped,
          GameRulesConfig.minEntityCrammingMineColonies);
      }
      log.debug("{} maxEntityCramming: {} -> {}", LOG_PREFIX, current, clamped);
      executeGameRuleChange(GameRules.RULE_MAX_ENTITY_CRAMMING, clamped);
    }
  }

  private static void restoreRandomTickSpeed() {
    int current = gameRules.getInt(GameRules.RULE_RANDOMTICKING);
    if (current != configuredRandomTickSpeedMax) {
      log.debug("{} randomTickSpeed: {} -> {}", LOG_PREFIX, current, configuredRandomTickSpeedMax);
      executeGameRuleChange(GameRules.RULE_RANDOMTICKING, configuredRandomTickSpeedMax);
    }
  }

  private static void restoreMaxEntityCramming() {
    int current = gameRules.getInt(GameRules.RULE_MAX_ENTITY_CRAMMING);
    if (current != configuredMaxEntityCramming) {
      log.debug("{} maxEntityCramming: {} -> {}", LOG_PREFIX, current,
        configuredMaxEntityCramming);
      executeGameRuleChange(GameRules.RULE_MAX_ENTITY_CRAMMING, configuredMaxEntityCramming);
    }
  }

  private static void executeGameRuleChange(
    GameRules.Key<GameRules.BooleanValue> rule, boolean value) {
    PerformanceStats.gameRulesChanged++;
    if (gameRules != null) {
      gameRules.getRule(rule).set(value, ServerManager.getMinecraftServer());
    }
  }

  private static void executeGameRuleChange(
    GameRules.Key<GameRules.IntegerValue> rule, int value) {
    PerformanceStats.gameRulesChanged++;
    if (gameRules != null) {
      gameRules.getRule(rule).set(value, ServerManager.getMinecraftServer());
    }
  }

  private static void setGameRule(GameRules.Key<GameRules.BooleanValue> rule, boolean value) {
    if (gameRules.getBoolean(rule) != value) {
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

    gameRules = minecraftServer.getGameRules();
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
      if (playerPosition.hasRecentMovementSpeed(
        SimulationDistanceConfig.movementThrottleSpeedBlocksPerSecond)) {
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
    int currentRandomTickSpeed = gameRules.getInt(GameRules.RULE_RANDOMTICKING);
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
