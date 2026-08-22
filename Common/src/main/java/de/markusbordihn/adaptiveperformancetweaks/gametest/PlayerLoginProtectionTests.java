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

package de.markusbordihn.adaptiveperformancetweaks.gametest;

import com.mojang.authlib.GameProfile;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerLoginManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerLoginProtectionConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerValidation;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ClientInformation;
import net.minecraft.server.level.ServerPlayer;

public final class PlayerLoginProtectionTests {

  private PlayerLoginProtectionTests() {
  }

  public static void testProtectionAppliedOnLogin(GameTestHelper helper) {
    PlayerLoginManager.handleServerAboutToStart();
    PlayerLoginProtectionConfig.protectPlayerDuringLogin = true;

    ServerPlayer player =
      new ServerPlayer(
        helper.getLevel().getServer(),
        helper.getLevel(),
        new GameProfile(UUID.randomUUID(), "TestLoginPlayer"),
        ClientInformation.createDefault());

    GameTestHelpers.assertTrue(
      helper,
      "Player should not be invulnerable before login protection",
      !player.isInvulnerable());

    PlayerLoginManager.handlePlayerLoggedIn(player);

    GameTestHelpers.assertTrue(
      helper, "Player should be invulnerable after login", player.isInvulnerable());
    GameTestHelpers.assertTrue(
      helper, "Player should be invisible after login", player.isInvisible());

    PlayerLoginManager.handleServerAboutToStart();
    helper.succeed();
  }

  public static void testProtectionSkippedWhenDisabled(GameTestHelper helper) {
    PlayerLoginManager.handleServerAboutToStart();
    PlayerLoginProtectionConfig.protectPlayerDuringLogin = false;

    ServerPlayer player =
      new ServerPlayer(
        helper.getLevel().getServer(),
        helper.getLevel(),
        new GameProfile(UUID.randomUUID(), "TestLoginPlayer2"),
        ClientInformation.createDefault());

    boolean invulnerableBefore = player.isInvulnerable();
    PlayerLoginManager.handlePlayerLoggedIn(player);

    GameTestHelpers.assertEquals(
      helper,
      "Invulnerability should not change when login protection is disabled",
      invulnerableBefore,
      player.isInvulnerable());

    PlayerLoginProtectionConfig.protectPlayerDuringLogin = true;
    PlayerLoginManager.handleServerAboutToStart();
    helper.succeed();
  }

  public static void testValidationDetectsMovement(GameTestHelper helper) {
    ServerPlayer player =
      new ServerPlayer(
        helper.getLevel().getServer(),
        helper.getLevel(),
        new GameProfile(UUID.randomUUID(), "TestMovePlayer"),
        ClientInformation.createDefault());
    player.setPos(0.5, 1.0, 0.5);

    PlayerValidation validation = new PlayerValidation(player);

    GameTestHelpers.assertTrue(
      helper, "Player should not have moved initially", !validation.hasPlayerMoved());

    player.setPos(5.0, 1.0, 5.0);

    GameTestHelpers.assertTrue(
      helper,
      "Player should be detected as moved after position change",
      validation.hasPlayerMoved());

    helper.succeed();
  }
}
