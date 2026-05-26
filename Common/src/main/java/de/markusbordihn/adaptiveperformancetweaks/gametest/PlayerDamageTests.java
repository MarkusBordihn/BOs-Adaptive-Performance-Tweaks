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
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerDamageManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerEasyChildModeConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.player.PlayerStarterProtectionConfig;
import java.util.Set;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ClientInformation;
import net.minecraft.server.level.ServerPlayer;

public final class PlayerDamageTests {

  private static final String CHILD_PLAYER_NAME = "TestChildPlayer";

  private PlayerDamageTests() {
  }

  public static void testDamageUnchangedForNonChildPlayer(GameTestHelper helper) {
    boolean wasStarterEnabled = FeatureToggle.PLAYER_STARTER_PROTECTION.isEnabled();
    FeatureToggle.PLAYER_STARTER_PROTECTION.setEnabled(false);
    PlayerEasyChildModeConfig.childPlayerNames = Set.of(CHILD_PLAYER_NAME);
    PlayerEasyChildModeConfig.childPlayerHurtDamageReduction = 50;

    ServerPlayer player =
      new ServerPlayer(
        helper.getLevel().getServer(),
        helper.getLevel(),
        new GameProfile(UUID.randomUUID(), "RegularPlayer"),
        ClientInformation.createDefault());

    float result = PlayerDamageManager.handleLivingHurt(player, 10f);
    GameTestHelpers.assertEquals(
      helper, "Damage should not be reduced for non-child players", 10f, result);

    FeatureToggle.PLAYER_STARTER_PROTECTION.setEnabled(wasStarterEnabled);
    PlayerEasyChildModeConfig.childPlayerNames = Set.of();
    helper.succeed();
  }

  public static void testDamageReducedForChildPlayer(GameTestHelper helper) {
    boolean wasStarterEnabled = FeatureToggle.PLAYER_STARTER_PROTECTION.isEnabled();
    FeatureToggle.PLAYER_STARTER_PROTECTION.setEnabled(false);
    PlayerEasyChildModeConfig.childPlayerNames = Set.of(CHILD_PLAYER_NAME);
    PlayerEasyChildModeConfig.childPlayerHurtDamageReduction = 50;

    ServerPlayer child =
      new ServerPlayer(
        helper.getLevel().getServer(),
        helper.getLevel(),
        new GameProfile(UUID.randomUUID(), CHILD_PLAYER_NAME),
        ClientInformation.createDefault());

    float result = PlayerDamageManager.handleLivingHurt(child, 10f);
    GameTestHelpers.assertEquals(
      helper, "Damage should be reduced by 50% for child player (10 \u2192 5)", 5f, result);

    FeatureToggle.PLAYER_STARTER_PROTECTION.setEnabled(wasStarterEnabled);
    PlayerEasyChildModeConfig.childPlayerNames = Set.of();
    helper.succeed();
  }

  public static void testDamageFullyBlockedAtHundredPercent(GameTestHelper helper) {
    PlayerEasyChildModeConfig.childPlayerNames = Set.of(CHILD_PLAYER_NAME);
    PlayerEasyChildModeConfig.childPlayerHurtDamageReduction = 100;

    ServerPlayer child =
      new ServerPlayer(
        helper.getLevel().getServer(),
        helper.getLevel(),
        new GameProfile(UUID.randomUUID(), CHILD_PLAYER_NAME),
        ClientInformation.createDefault());

    float result = PlayerDamageManager.handleLivingHurt(child, 10f);
    GameTestHelpers.assertEquals(
      helper, "Damage should be fully blocked at 100% reduction", 0f, result);

    PlayerEasyChildModeConfig.childPlayerHurtDamageReduction = 50;
    PlayerEasyChildModeConfig.childPlayerNames = Set.of();
    helper.succeed();
  }

  public static void testAttackDamageIncreasedForChildPlayer(GameTestHelper helper) {
    boolean wasStarterEnabled = FeatureToggle.PLAYER_STARTER_PROTECTION.isEnabled();
    FeatureToggle.PLAYER_STARTER_PROTECTION.setEnabled(false);
    PlayerEasyChildModeConfig.childPlayerNames = Set.of(CHILD_PLAYER_NAME);
    PlayerEasyChildModeConfig.childPlayerAttackDamageIncrease = 50;

    ServerPlayer child =
      new ServerPlayer(
        helper.getLevel().getServer(),
        helper.getLevel(),
        new GameProfile(UUID.randomUUID(), CHILD_PLAYER_NAME),
        ClientInformation.createDefault());

    float result =
      PlayerDamageManager.handleLivingDamage(
        helper.getLevel().damageSources().playerAttack(child), 10f);
    GameTestHelpers.assertEquals(
      helper,
      "Attack damage should be increased by 50% for child player (10 \u2192 15)",
      15f,
      result);

    FeatureToggle.PLAYER_STARTER_PROTECTION.setEnabled(wasStarterEnabled);
    PlayerEasyChildModeConfig.childPlayerNames = Set.of();
    helper.succeed();
  }

  public static void testChildModeHurtReductionThroughHook(
    GameTestHelper helper, String hookDescription) {
    boolean wasEnabled = FeatureToggle.PLAYER_EASY_CHILD_MODE.isEnabled();
    FeatureToggle.PLAYER_EASY_CHILD_MODE.setEnabled(true);
    PlayerEasyChildModeConfig.childPlayerNames = Set.of(CHILD_PLAYER_NAME);
    PlayerEasyChildModeConfig.childPlayerHurtDamageReduction = 50;

    ServerPlayer child =
      new ServerPlayer(
        helper.getLevel().getServer(),
        helper.getLevel(),
        new GameProfile(UUID.randomUUID(), CHILD_PLAYER_NAME),
        ClientInformation.createDefault());
    child.setHealth(20f);

    child.hurt(helper.getLevel().damageSources().generic(), 10f);

    float health = child.getHealth();
    GameTestHelpers.assertTrue(
      helper,
      "Child mode hurt reduction not applied through "
        + hookDescription
        + " (health="
        + health
        + ", expected > 12 for 50% reduction)",
      health > 12f);

    FeatureToggle.PLAYER_EASY_CHILD_MODE.setEnabled(wasEnabled);
    PlayerEasyChildModeConfig.childPlayerNames = Set.of();
    helper.succeed();
  }

  public static void testStarterProtectionHurtReductionThroughHook(
    GameTestHelper helper, String hookDescription) {
    boolean wasEnabled = FeatureToggle.PLAYER_STARTER_PROTECTION.isEnabled();
    FeatureToggle.PLAYER_STARTER_PROTECTION.setEnabled(true);
    int originalReduction = PlayerStarterProtectionConfig.starterHurtDamageReduction;
    PlayerStarterProtectionConfig.starterHurtDamageReduction = 50;

    ServerPlayer starter =
      new ServerPlayer(
        helper.getLevel().getServer(),
        helper.getLevel(),
        new GameProfile(UUID.randomUUID(), "TestStarterPlayer"),
        ClientInformation.createDefault());
    starter.setHealth(20f);

    starter.hurt(helper.getLevel().damageSources().generic(), 10f);

    float health = starter.getHealth();
    GameTestHelpers.assertTrue(
      helper,
      "Starter protection hurt reduction not applied through "
        + hookDescription
        + " (health="
        + health
        + ", expected > 12 for 50% reduction)",
      health > 12f);

    FeatureToggle.PLAYER_STARTER_PROTECTION.setEnabled(wasEnabled);
    PlayerStarterProtectionConfig.starterHurtDamageReduction = originalReduction;
    helper.succeed();
  }
}
