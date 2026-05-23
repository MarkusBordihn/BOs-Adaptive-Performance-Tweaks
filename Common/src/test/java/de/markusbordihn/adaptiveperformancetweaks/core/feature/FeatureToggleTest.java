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

package de.markusbordihn.adaptiveperformancetweaks.core.feature;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class FeatureToggleTest {

  @Test
  void fromIdFindsToggleByExactId() {
    assertEquals(FeatureToggle.GAMERULES, FeatureToggle.fromId("gamerules"));
    assertEquals(FeatureToggle.ITEMS, FeatureToggle.fromId("items"));
    assertEquals(FeatureToggle.SPAWN, FeatureToggle.fromId("spawn"));
    assertEquals(FeatureToggle.MONITORING, FeatureToggle.fromId("monitoring"));
    assertEquals(FeatureToggle.CLIENT_AFK_OPTIMIZATION,
      FeatureToggle.fromId("client_afk_optimization"));
  }

  @Test
  void fromIdIsCaseInsensitive() {
    assertEquals(FeatureToggle.GAMERULES, FeatureToggle.fromId("GAMERULES"));
    assertEquals(FeatureToggle.GAMERULES, FeatureToggle.fromId("GameRules"));
    assertEquals(FeatureToggle.MONITORING, FeatureToggle.fromId("MONITORING"));
  }

  @Test
  void fromIdTrimsWhitespace() {
    assertEquals(FeatureToggle.ITEMS, FeatureToggle.fromId("  items  "));
  }

  @Test
  void fromIdReturnsNullForUnknownId() {
    assertNull(FeatureToggle.fromId("unknown_feature"));
    assertNull(FeatureToggle.fromId(""));
  }

  @Test
  void getIdMatchesConstructorValue() {
    assertEquals("gamerules", FeatureToggle.GAMERULES.getId());
    assertEquals("adaptive_view_distance", FeatureToggle.ADAPTIVE_VIEW_DISTANCE.getId());
    assertEquals("client_afk_optimization", FeatureToggle.CLIENT_AFK_OPTIMIZATION.getId());
  }

  @Test
  void coreIsAlwaysEnabled() {
    assertTrue(FeatureToggle.CORE.isEnabled());
  }

  @Test
  void coreSetEnabledIsNoOp() {
    FeatureToggle.CORE.setEnabled(false);
    assertTrue(FeatureToggle.CORE.isEnabled());
  }

  @Test
  void defaultStateIsAutoForActiveFeatures() {
    assertEquals(FeatureState.ENABLED, FeatureToggle.CORE.getDefaultState());
    assertEquals(FeatureState.AUTO, FeatureToggle.GAMERULES.getDefaultState());
    assertEquals(FeatureState.AUTO, FeatureToggle.ITEMS.getDefaultState());
    assertEquals(FeatureState.AUTO, FeatureToggle.EXPERIENCE_ORBS.getDefaultState());
    assertEquals(FeatureState.AUTO, FeatureToggle.PLAYER_LOGIN_PROTECTION.getDefaultState());
    assertEquals(FeatureState.AUTO, FeatureToggle.PLAYER_EASY_CHILD_MODE.getDefaultState());
    assertEquals(FeatureState.AUTO, FeatureToggle.PLAYER_STARTER_PROTECTION.getDefaultState());
    assertEquals(FeatureState.AUTO, FeatureToggle.SPAWN.getDefaultState());
    assertEquals(FeatureState.AUTO, FeatureToggle.ADAPTIVE_SIM_DISTANCE.getDefaultState());
  }

  @Test
  void defaultStateIsDisabledForOptInFeatures() {
    assertEquals(FeatureState.DISABLED, FeatureToggle.ADAPTIVE_VIEW_DISTANCE.getDefaultState());
    assertEquals(FeatureState.DISABLED, FeatureToggle.AI_THROTTLING.getDefaultState());
    assertEquals(FeatureState.DISABLED, FeatureToggle.CHUNK_GEN_THROTTLE.getDefaultState());
    assertEquals(FeatureState.DISABLED, FeatureToggle.CLIENT_AFK_OPTIMIZATION.getDefaultState());
    assertEquals(FeatureState.DISABLED, FeatureToggle.MONITORING.getDefaultState());
  }

  @Test
  void conflictingModsDefinedForRelevantFeatures() {
    assertFalse(FeatureToggle.ITEMS.getConflictingMods().isEmpty());
    assertFalse(FeatureToggle.EXPERIENCE_ORBS.getConflictingMods().isEmpty());
    assertFalse(FeatureToggle.PLAYER_LOGIN_PROTECTION.getConflictingMods().isEmpty());
    assertFalse(FeatureToggle.SPAWN.getConflictingMods().isEmpty());
    assertNotNull(FeatureToggle.GAMERULES.getConflictingMods());
  }

  @Test
  void scopes() {
    assertEquals(FeatureToggle.Scope.BOTH, FeatureToggle.CORE.scope());
    assertEquals(FeatureToggle.Scope.BOTH, FeatureToggle.MONITORING.scope());
    assertEquals(FeatureToggle.Scope.CLIENT, FeatureToggle.CLIENT_AFK_OPTIMIZATION.scope());
    assertEquals(FeatureToggle.Scope.SERVER, FeatureToggle.GAMERULES.scope());
    assertEquals(FeatureToggle.Scope.SERVER, FeatureToggle.ITEMS.scope());
    assertEquals(FeatureToggle.Scope.SERVER, FeatureToggle.SPAWN.scope());
    assertEquals(FeatureToggle.Scope.SERVER, FeatureToggle.PLAYER_STARTER_PROTECTION.scope());
    assertEquals(FeatureToggle.Scope.SERVER, FeatureToggle.ADAPTIVE_VIEW_DISTANCE.scope());
    assertEquals(FeatureToggle.Scope.SERVER, FeatureToggle.ADAPTIVE_SIM_DISTANCE.scope());
  }
}
