/*
 * Copyright 2024 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweaks.core.feature;

import de.markusbordihn.adaptiveperformancetweaks.core.config.CoreConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.ViewDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.gamerules.GameRuleManager;
import java.util.List;
import java.util.Locale;

public enum FeatureToggle {
  CORE(FeatureState.ENABLED, List.of(), List.of(), Scope.BOTH),
  GAMERULES(FeatureState.AUTO, List.of(), List.of(), Scope.SERVER),
  ITEMS(
    FeatureState.AUTO,
    List.of("getittogetherdrops", "eco_stack_manager"),
    List.of("servercore"),
    Scope.SERVER),
  EXPERIENCE_ORBS(
    FeatureState.AUTO,
    List.of("clumps", "eco_stack_manager"),
    List.of("servercore"),
    Scope.SERVER),
  ARROWS(
    FeatureState.AUTO,
    List.of("arrow_clean_up", "persistent_arrows"),
    List.of(),
    Scope.SERVER),
  PLAYER_LOGIN_PROTECTION(
    FeatureState.AUTO,
    List.of("logprot"),
    List.of("loadingprotection", "loadingprotectionrenewed", "joinprotection"),
    Scope.SERVER),
  PLAYER_EASY_CHILD_MODE(FeatureState.AUTO, List.of(), List.of(), Scope.SERVER),
  PLAYER_STARTER_PROTECTION(FeatureState.AUTO, List.of(), List.of(), Scope.SERVER),
  SPAWN(
    FeatureState.AUTO,
    List.of("incontrol", "badmobs"),
    List.of("servercore"),
    Scope.SERVER),
  ADAPTIVE_VIEW_DISTANCE(
    FeatureState.DISABLED,
    List.of("dynview"),
    List.of("servercore"),
    Scope.SERVER),
  ADAPTIVE_SIMULATION_DISTANCE(
    FeatureState.AUTO,
    List.of("dynview"),
    List.of("servercore"),
    Scope.SERVER),
  AI_THROTTLING(
    FeatureState.DISABLED,
    List.of(),
    List.of("aiimprovements", "servercore"),
    Scope.SERVER),
  CHUNK_GEN_THROTTLE(
    FeatureState.DISABLED,
    List.of(),
    List.of("smoothchunksave"),
    Scope.SERVER),
  CLIENT_AFK_OPTIMIZATION(FeatureState.DISABLED, List.of(), List.of(), Scope.CLIENT),
  MONITORING(FeatureState.DISABLED, List.of(), List.of("servercore"), Scope.BOTH);

  private final FeatureState defaultState;
  private final List<String> conflictingMods;
  private final List<String> warningOnlyMods;
  private final Scope scope;

  FeatureToggle(FeatureState defaultState, List<String> conflictingMods,
    List<String> warningOnlyMods, Scope scope) {
    this.defaultState = defaultState;
    this.conflictingMods = conflictingMods;
    this.warningOnlyMods = warningOnlyMods;
    this.scope = scope;
  }

  public static FeatureToggle fromId(String id) {
    String normalized = id.trim().toLowerCase(Locale.ROOT);
    for (FeatureToggle toggle : values()) {
      if (toggle.getId().equals(normalized)) {
        return toggle;
      }
    }
    return null;
  }

  public String getId() {
    return this.name().toLowerCase(Locale.ROOT);
  }

  public FeatureState getDefaultState() {
    return this.defaultState;
  }

  public List<String> getConflictingMods() {
    return this.conflictingMods;
  }

  public List<String> getWarningOnlyMods() {
    return this.warningOnlyMods;
  }

  public Scope scope() {
    return this.scope;
  }

  public boolean isEnabled() {
    if (this == CORE) {
      return true;
    }

    return CoreConfig.isFeatureEnabled(this);
  }

  public void setEnabled(boolean enabled) {
    if (this == CORE) {
      return;
    }

    boolean wasEnabled = isEnabled();
    if (wasEnabled == enabled) {
      return;
    }

    CoreConfig.setFeatureEnabled(this, enabled);
    if (!enabled) {
      restoreFeatureState();
    }
  }

  private void restoreFeatureState() {
    switch (this) {
      case GAMERULES -> GameRuleManager.handleFeatureDisabled();
      case ADAPTIVE_VIEW_DISTANCE -> ViewDistanceManager.handleFeatureDisabled();
      case ADAPTIVE_SIMULATION_DISTANCE -> SimulationDistanceManager.handleFeatureDisabled();
      default -> {
      }
    }
  }

  public enum Scope {
    SERVER,
    CLIENT,
    BOTH
  }
}
