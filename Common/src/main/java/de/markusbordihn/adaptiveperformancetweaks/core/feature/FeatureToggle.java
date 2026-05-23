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
import java.util.List;
import java.util.Locale;

public enum FeatureToggle {
  CORE(FeatureState.ENABLED, List.of(), Scope.BOTH),
  GAMERULES(FeatureState.AUTO, List.of(), Scope.SERVER),
  ITEMS(FeatureState.AUTO, List.of("clumps", "getittogetherdrops"), Scope.SERVER),
  EXPERIENCE_ORBS(FeatureState.AUTO, List.of("clumps"), Scope.SERVER),
  PLAYER_LOGIN_PROTECTION(FeatureState.AUTO, List.of("logprot"), Scope.SERVER),
  PLAYER_EASY_CHILD_MODE(FeatureState.AUTO, List.of(), Scope.SERVER),
  PLAYER_STARTER_PROTECTION(FeatureState.AUTO, List.of(), Scope.SERVER),
  SPAWN(FeatureState.AUTO, List.of("incontrol"), Scope.SERVER),
  ADAPTIVE_VIEW_DISTANCE(FeatureState.DISABLED, List.of("dynview"), Scope.SERVER),
  ADAPTIVE_SIM_DISTANCE(FeatureState.AUTO, List.of(), Scope.SERVER),
  AI_THROTTLING(FeatureState.DISABLED, List.of(), Scope.SERVER),
  CHUNK_GEN_THROTTLE(FeatureState.DISABLED, List.of(), Scope.SERVER),
  CLIENT_AFK_OPTIMIZATION(FeatureState.DISABLED, List.of(), Scope.CLIENT),
  MONITORING(FeatureState.DISABLED, List.of(), Scope.BOTH);

  private final String id;
  private final FeatureState defaultState;
  private final List<String> conflictingMods;
  private final Scope scope;

  FeatureToggle(FeatureState defaultState, List<String> conflictingMods, Scope scope) {
    this.id = name().toLowerCase(Locale.ROOT);
    this.defaultState = defaultState;
    this.conflictingMods = conflictingMods;
    this.scope = scope;
  }

  public static FeatureToggle fromId(String id) {
    String normalized = id.trim().toLowerCase(Locale.ROOT);
    for (FeatureToggle toggle : values()) {
      if (toggle.id.equals(normalized)) {
        return toggle;
      }
    }
    return null;
  }

  public String getId() {
    return this.id;
  }

  public FeatureState getDefaultState() {
    return this.defaultState;
  }

  public List<String> getConflictingMods() {
    return this.conflictingMods;
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

    CoreConfig.setFeatureEnabled(this, enabled);
  }

  public enum Scope {
    SERVER,
    CLIENT,
    BOTH
  }
}
