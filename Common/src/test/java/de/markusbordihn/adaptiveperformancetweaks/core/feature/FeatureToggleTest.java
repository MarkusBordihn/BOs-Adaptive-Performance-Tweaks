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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;
import de.markusbordihn.adaptiveperformancetweaks.core.player.PlayerPositionManager;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.SimulationDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.distance.ViewDistanceManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.gamerules.GameRuleManager;
import java.lang.reflect.Field;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.players.PlayerList;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockMakers;

class FeatureToggleTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }


  private static void writeStaticField(Class<?> owner, String fieldName, Object value)
    throws Exception {
    Field field = owner.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static Object readStaticField(Class<?> owner, String fieldName) throws Exception {
    Field field = owner.getDeclaredField(fieldName);
    field.setAccessible(true);
    return field.get(null);
  }

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
    assertEquals(FeatureState.AUTO, FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.getDefaultState());
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
    assertTrue(FeatureToggle.ITEMS.getConflictingMods().contains("eco_stack_manager"));
    assertTrue(FeatureToggle.EXPERIENCE_ORBS.getConflictingMods().contains("eco_stack_manager"));
    assertTrue(
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.getConflictingMods().contains("dynview"));
  }

  @Test
  void warningOnlyModsDefinedForOverlappingFeatures() {
    assertTrue(FeatureToggle.ITEMS.getWarningOnlyMods().contains("servercore"));
    assertTrue(FeatureToggle.EXPERIENCE_ORBS.getWarningOnlyMods().contains("servercore"));
    assertTrue(FeatureToggle.SPAWN.getWarningOnlyMods().contains("servercore"));
    assertTrue(FeatureToggle.AI_THROTTLING.getWarningOnlyMods().contains("aiimprovements"));
    assertTrue(
      FeatureToggle.PLAYER_LOGIN_PROTECTION.getWarningOnlyMods().contains("joinprotection"));
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
    assertEquals(FeatureToggle.Scope.SERVER, FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.scope());
  }

  @Test
  void disablingAdaptiveSimulationDistanceRestoresRuntimeState() throws Exception {
    boolean previousState = FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled();
    try {
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.setEnabled(true);
      writeStaticField(SimulationDistanceManager.class, "currentDistance", 6);
      writeStaticField(SimulationDistanceManager.class, "currentLoadBaselineDistance", 8);
      writeStaticField(SimulationDistanceManager.class, "currentMovementReduction", 3);
      writeStaticField(SimulationDistanceManager.class, "activeExplorerCount", 2);
      writeStaticField(SimulationDistanceManager.class, "currentLoadLevel",
        de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel.HIGH);

      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.setEnabled(false);

      assertEquals(-1, readStaticField(SimulationDistanceManager.class, "currentDistance"));
      assertEquals(-1,
        readStaticField(SimulationDistanceManager.class, "currentLoadBaselineDistance"));
      assertEquals(0,
        readStaticField(SimulationDistanceManager.class, "currentMovementReduction"));
      assertEquals(0, readStaticField(SimulationDistanceManager.class, "activeExplorerCount"));
      assertEquals(
        de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel.NORMAL,
        readStaticField(SimulationDistanceManager.class, "currentLoadLevel"));
    } finally {
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.setEnabled(previousState);
    }
  }

  @Test
  void disablingAdaptiveViewDistanceRestoresRuntimeState() throws Exception {
    boolean previousState = FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled();
    try {
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(true);
      writeStaticField(ViewDistanceManager.class, "currentDistance", 5);
      writeStaticField(ViewDistanceManager.class, "warmupUntilTime",
        System.currentTimeMillis() + 1_000L);
      writeStaticField(ViewDistanceManager.class, "currentLoadLevel",
        de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel.HIGH);

      FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(false);

      assertEquals(-1, readStaticField(ViewDistanceManager.class, "currentDistance"));
      assertEquals(0L, readStaticField(ViewDistanceManager.class, "warmupUntilTime"));
      assertEquals(
        de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel.NORMAL,
        readStaticField(ViewDistanceManager.class, "currentLoadLevel"));
    } finally {
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(previousState);
    }
  }

  @Test
  void disablingGamerulesClearsWarmupStateWithoutServer() throws Exception {
    boolean previousState = FeatureToggle.GAMERULES.isEnabled();
    try {
      FeatureToggle.GAMERULES.setEnabled(true);
      writeStaticField(GameRuleManager.class, "gameRules",
        mock(net.minecraft.world.level.GameRules.class));
      writeStaticField(GameRuleManager.class, "randomTickWarmupUntilTime",
        System.currentTimeMillis() + 1_000L);

      FeatureToggle.GAMERULES.setEnabled(false);

      assertNull(readStaticField(GameRuleManager.class, "gameRules"));
      assertEquals(0L, readStaticField(GameRuleManager.class, "randomTickWarmupUntilTime"));
    } finally {
      FeatureToggle.GAMERULES.setEnabled(previousState);
    }
  }

  @Test
  void enablingAdaptiveViewDistanceInitializesRuntimeStateFromServer() throws Exception {
    boolean previousState = FeatureToggle.ADAPTIVE_VIEW_DISTANCE.isEnabled();
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    PlayerList playerList = mock(PlayerList.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    when(server.getPlayerList()).thenReturn(playerList);
    when(playerList.getViewDistance()).thenReturn(6);

    try {
      writeStaticField(ServerManager.class, "minecraftServer", server);
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(false);
      writeStaticField(ViewDistanceManager.class, "currentDistance", -1);
      writeStaticField(ViewDistanceManager.class, "configuredDistanceMax", -1);
      writeStaticField(ViewDistanceManager.class, "warmupUntilTime", 0L);
      writeStaticField(ViewDistanceManager.class, "lastRecoveryTime", 0L);
      writeStaticField(ViewDistanceManager.class, "currentLoadLevel", ServerLoadLevel.NORMAL);

      FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(true);

      assertEquals(6, readStaticField(ViewDistanceManager.class, "currentDistance"));
      assertEquals(6, readStaticField(ViewDistanceManager.class, "configuredDistanceMax"));
    } finally {
      FeatureToggle.ADAPTIVE_VIEW_DISTANCE.setEnabled(previousState);
      writeStaticField(ServerManager.class, "minecraftServer", null);
    }
  }

  @Test
  void enablingAdaptiveSimulationDistanceInitializesRuntimeStateFromServer() throws Exception {
    boolean previousState = FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.isEnabled();
    MinecraftServer server = mock(MinecraftServer.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    PlayerList playerList = mock(PlayerList.class,
      withSettings().mockMaker(MockMakers.SUBCLASS));
    when(server.getPlayerList()).thenReturn(playerList);
    when(playerList.getSimulationDistance()).thenReturn(7);

    try {
      writeStaticField(ServerManager.class, "minecraftServer", server);
      PlayerPositionManager.reset();
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.setEnabled(false);
      writeStaticField(SimulationDistanceManager.class, "currentDistance", -1);
      writeStaticField(SimulationDistanceManager.class, "currentLoadBaselineDistance", -1);
      writeStaticField(SimulationDistanceManager.class, "currentMovementReduction", 0);
      writeStaticField(SimulationDistanceManager.class, "activeExplorerCount", 0);
      writeStaticField(SimulationDistanceManager.class, "configuredDistanceMax", -1);
      writeStaticField(SimulationDistanceManager.class, "recoveryStartTick", -1);
      writeStaticField(SimulationDistanceManager.class, "nextRecoveryTick", -1);
      writeStaticField(SimulationDistanceManager.class, "currentLoadLevel",
        ServerLoadLevel.NORMAL);

      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.setEnabled(true);

      assertEquals(7, readStaticField(SimulationDistanceManager.class, "currentDistance"));
      assertEquals(7, readStaticField(SimulationDistanceManager.class, "configuredDistanceMax"));
    } finally {
      FeatureToggle.ADAPTIVE_SIMULATION_DISTANCE.setEnabled(previousState);
      writeStaticField(ServerManager.class, "minecraftServer", null);
    }
  }

}
