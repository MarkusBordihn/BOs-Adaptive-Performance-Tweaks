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

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import net.minecraft.server.level.ServerPlayer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class PlayerDamageManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME_PLAYERS);
  private static final String CHILD_MODE_PREFIX = "[Child Mode]";
  private static final String STARTER_PREFIX = "[Starter Protection]";

  private PlayerDamageManager() {
  }

  public static void handleServerStarting() {
    if (!PlayerEasyChildModeConfig.childPlayerNames.isEmpty()) {
      if (PlayerEasyChildModeConfig.childPlayerHurtDamageReduction > 0) {
        log.info(
          "{} Hurt reduction: {}% for {}",
          CHILD_MODE_PREFIX,
          PlayerEasyChildModeConfig.childPlayerHurtDamageReduction,
          PlayerEasyChildModeConfig.childPlayerNames);
      }
      if (PlayerEasyChildModeConfig.childPlayerAttackDamageIncrease > 0) {
        log.info(
          "{} Attack increase: {}% for {}",
          CHILD_MODE_PREFIX,
          PlayerEasyChildModeConfig.childPlayerAttackDamageIncrease,
          PlayerEasyChildModeConfig.childPlayerNames);
      }
    }
  }

  public static float handleLivingHurt(net.minecraft.world.entity.LivingEntity targetEntity,
    float amount) {
    if (!(targetEntity instanceof ServerPlayer serverPlayer)) {
      return amount;
    }

    float modified = amount;

    if (FeatureToggle.PLAYER_EASY_CHILD_MODE.isEnabled()
      && PlayerEasyChildModeConfig.childPlayerHurtDamageReduction > 0
      && !PlayerEasyChildModeConfig.childPlayerNames.isEmpty()
      && PlayerEasyChildModeConfig.childPlayerNames.contains(
      serverPlayer.getName().getString())) {
      if (PlayerEasyChildModeConfig.childPlayerHurtDamageReduction == 100) {
        log.debug("{} {}: hurt {} -> 0 (100% reduction)",
          CHILD_MODE_PREFIX, serverPlayer.getName().getString(), amount);
        return 0f;
      }

      modified =
        modified
          - (modified
          / (100f / PlayerEasyChildModeConfig.childPlayerHurtDamageReduction));
      log.debug("{} {}: hurt {} -> {}",
        CHILD_MODE_PREFIX, serverPlayer.getName().getString(), amount, modified);
    }

    if (FeatureToggle.PLAYER_STARTER_PROTECTION.isEnabled()
      && PlayerStarterProtectionConfig.starterHurtDamageReduction > 0
      && serverPlayer.experienceLevel
      < PlayerStarterProtectionConfig.starterMaxExperienceLevel) {
      if (PlayerStarterProtectionConfig.starterHurtDamageReduction == 100) {
        log.debug("{} {}: hurt {} -> 0 (100% reduction)",
          STARTER_PREFIX, serverPlayer.getName().getString(), amount);
        return 0f;
      }

      modified =
        modified
          - (modified
          / (100f / PlayerStarterProtectionConfig.starterHurtDamageReduction));
      log.debug("{} {}: hurt {} -> {}",
        STARTER_PREFIX, serverPlayer.getName().getString(), amount, modified);
    }

    return modified;
  }

  public static float handleLivingDamage(
    net.minecraft.world.damagesource.DamageSource damageSource, float amount) {
    if (damageSource == null || !(damageSource.getEntity() instanceof ServerPlayer attacker)) {
      return amount;
    }

    float modified = amount;

    if (FeatureToggle.PLAYER_EASY_CHILD_MODE.isEnabled()
      && PlayerEasyChildModeConfig.childPlayerAttackDamageIncrease > 0
      && !PlayerEasyChildModeConfig.childPlayerNames.isEmpty()
      && PlayerEasyChildModeConfig.childPlayerNames.contains(
      attacker.getName().getString())) {
      modified =
        modified
          + (modified
          / (100f / PlayerEasyChildModeConfig.childPlayerAttackDamageIncrease));
      log.debug("{} {}: attack {} -> {}",
        CHILD_MODE_PREFIX, attacker.getName().getString(), amount, modified);
    }

    if (FeatureToggle.PLAYER_STARTER_PROTECTION.isEnabled()
      && PlayerStarterProtectionConfig.starterAttackDamageIncrease > 0
      && attacker.experienceLevel
      < PlayerStarterProtectionConfig.starterMaxExperienceLevel) {
      modified =
        modified
          + (modified
          / (100f / PlayerStarterProtectionConfig.starterAttackDamageIncrease));
      log.debug("{} {}: attack {} -> {}",
        STARTER_PREFIX, attacker.getName().getString(), amount, modified);
    }

    return modified;
  }
}
