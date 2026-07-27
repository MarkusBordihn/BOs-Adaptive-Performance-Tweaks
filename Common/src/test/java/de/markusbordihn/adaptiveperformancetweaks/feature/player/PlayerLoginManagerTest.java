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

package de.markusbordihn.adaptiveperformancetweaks.feature.player;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerManager;
import java.lang.reflect.Field;
import java.util.Set;
import net.minecraft.SharedConstants;
import net.minecraft.network.chat.Component;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.players.PlayerList;
import net.minecraft.world.phys.Vec3;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockMakers;

class PlayerLoginManagerTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

  private static <T> T minecraftMock(Class<T> type) {
    return mock(type, withSettings().mockMaker(MockMakers.SUBCLASS));
  }

  @SuppressWarnings("unchecked")
  private static Set<?> readPlayerValidationList() throws Exception {
    Field field = PlayerLoginManager.class.getDeclaredField("playerValidationList");
    field.setAccessible(true);
    return (Set<?>) field.get(null);
  }

  private static void setMinecraftServer(MinecraftServer server) throws Exception {
    Field field = ServerManager.class.getDeclaredField("minecraftServer");
    field.setAccessible(true);
    field.set(null, server);
  }

  private static ServerPlayer mockPlayer(String username) {
    ServerPlayer player = minecraftMock(ServerPlayer.class);
    when(player.getName()).thenReturn(Component.literal(username));
    when(player.position()).thenReturn(new Vec3(0.0, 0.0, 0.0));
    return player;
  }

  private static MinecraftServer mockServerWith(String username, ServerPlayer player) {
    MinecraftServer server = minecraftMock(MinecraftServer.class);
    PlayerList playerList = minecraftMock(PlayerList.class);
    when(server.getPlayerList()).thenReturn(playerList);
    when(playerList.getPlayerByName(username)).thenReturn(player);
    return server;
  }

  @Test
  void disabledFeatureSkipsLoginProtectionHandling() throws Exception {
    boolean previousState = FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled();
    boolean previousProtection = PlayerLoginProtectionConfig.protectPlayerDuringLogin;
    try {
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(false);
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = true;
      PlayerLoginManager.handleServerAboutToStart();

      PlayerLoginManager.handlePlayerLoggedIn(minecraftMock(ServerPlayer.class));

      assertEquals(0, readPlayerValidationList().size());
    } finally {
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = previousProtection;
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(previousState);
      PlayerLoginManager.handleServerStopping();
    }
  }

  @Test
  void logoutRestoresModAppliedProtectionFlags() throws Exception {
    boolean previousState = FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled();
    boolean previousProtection = PlayerLoginProtectionConfig.protectPlayerDuringLogin;
    try {
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(true);
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = true;
      PlayerLoginManager.handleServerAboutToStart();

      ServerPlayer player = mockPlayer("TestLogoutPlayer");
      // Not protected at login time, then the mod-applied flags are observed on logout.
      when(player.isInvisible()).thenReturn(false, true);
      when(player.isInvulnerable()).thenReturn(false, true);

      PlayerLoginManager.handlePlayerLoggedIn(player);
      assertEquals(1, readPlayerValidationList().size());

      PlayerLoginManager.handlePlayerLoggedOut(player);

      verify(player).setInvisible(false);
      verify(player).setInvulnerable(false);
      assertEquals(0, readPlayerValidationList().size());
    } finally {
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = previousProtection;
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(previousState);
      PlayerLoginManager.handleServerStopping();
    }
  }

  @Test
  void logoutKeepsPreexistingProtectionFlags() throws Exception {
    boolean previousState = FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled();
    boolean previousProtection = PlayerLoginProtectionConfig.protectPlayerDuringLogin;
    try {
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(true);
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = true;
      PlayerLoginManager.handleServerAboutToStart();

      ServerPlayer player = mockPlayer("TestPreProtectedPlayer");
      // Player was already invisible/invulnerable before login (e.g. game mode or another mod).
      when(player.isInvisible()).thenReturn(true);
      when(player.isInvulnerable()).thenReturn(true);

      PlayerLoginManager.handlePlayerLoggedIn(player);
      PlayerLoginManager.handlePlayerLoggedOut(player);

      verify(player, never()).setInvisible(false);
      verify(player, never()).setInvulnerable(false);
      assertEquals(0, readPlayerValidationList().size());
    } finally {
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = previousProtection;
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(previousState);
      PlayerLoginManager.handleServerStopping();
    }
  }

  @Test
  void logoutRestoresOnlyTheDisconnectingPlayer() throws Exception {
    boolean previousState = FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled();
    boolean previousProtection = PlayerLoginProtectionConfig.protectPlayerDuringLogin;
    try {
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(true);
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = true;
      PlayerLoginManager.handleServerAboutToStart();

      ServerPlayer disconnecting = mockPlayer("DisconnectingPlayer");
      when(disconnecting.isInvisible()).thenReturn(false, true);
      when(disconnecting.isInvulnerable()).thenReturn(false, true);

      ServerPlayer staying = mockPlayer("StayingPlayer");
      when(staying.isInvisible()).thenReturn(false);
      when(staying.isInvulnerable()).thenReturn(false);

      PlayerLoginManager.handlePlayerLoggedIn(disconnecting);
      PlayerLoginManager.handlePlayerLoggedIn(staying);
      assertEquals(2, readPlayerValidationList().size());

      PlayerLoginManager.handlePlayerLoggedOut(disconnecting);

      verify(disconnecting).setInvisible(false);
      verify(disconnecting).setInvulnerable(false);
      verify(staying, never()).setInvisible(false);
      verify(staying, never()).setInvulnerable(false);
      assertEquals(1, readPlayerValidationList().size());
    } finally {
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = previousProtection;
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(previousState);
      PlayerLoginManager.handleServerStopping();
    }
  }

  @Test
  void tickRestoresProtectionAfterMovement() throws Exception {
    boolean previousState = FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled();
    boolean previousProtection = PlayerLoginProtectionConfig.protectPlayerDuringLogin;
    try {
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(true);
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = true;
      PlayerLoginManager.handleServerAboutToStart();

      String username = "TestTickPlayer";
      ServerPlayer player = minecraftMock(ServerPlayer.class);
      when(player.getName()).thenReturn(Component.literal(username));
      // Initial position captured at login, then a moved position on the processing tick.
      when(player.position()).thenReturn(new Vec3(0.0, 0.0, 0.0), new Vec3(1.0, 0.0, 0.0));
      when(player.isInvisible()).thenReturn(false, true);
      when(player.isInvulnerable()).thenReturn(false, true);
      // The tick path re-resolves the player by name via the server.
      setMinecraftServer(mockServerWith(username, player));

      PlayerLoginManager.handlePlayerLoggedIn(player);

      // handleServerTick only processes once the tick interval is reached.
      for (int i = 0; i <= 40; i++) {
        PlayerLoginManager.handleServerTick();
      }

      verify(player).setInvisible(false);
      verify(player).setInvulnerable(false);
      assertEquals(0, readPlayerValidationList().size());
    } finally {
      setMinecraftServer(null);
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = previousProtection;
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(previousState);
      PlayerLoginManager.handleServerStopping();
    }
  }
}
