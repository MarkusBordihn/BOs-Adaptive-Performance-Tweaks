/**
 * Copyright 2022 Markus Bordihn
 *
 * <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * <p>The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * <p>THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.adaptiveperformancetweaksplayer.player;

import de.markusbordihn.adaptiveperformancetweaksplayer.Constants;
import de.markusbordihn.adaptiveperformancetweaksplayer.config.CommonConfig;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.event.entity.living.LivingDamageEvent;
import net.minecraftforge.event.entity.living.LivingHurtEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber
public class PlayerDamageManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;

  protected PlayerDamageManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    if (COMMON.childPlayerHurtDamageReduction.get() > 0) {
      log.info(
          "Child player hurt damage will be reduced by {}% for {}",
          COMMON.childPlayerHurtDamageReduction.get(), COMMON.childPlayerProtectionList.get());
    } else {
      log.warn("Child player hurt damage will not be reduced!");
    }

    if (COMMON.childPlayerAttackDamageIncrease.get() > 0) {
      log.info(
          "Child player attack damage will be increased by {}% for {}",
          COMMON.childPlayerAttackDamageIncrease.get(), COMMON.childPlayerProtectionList.get());
    } else {
      log.warn("Child player attack damage will not be increased!");
    }
  }

  @SubscribeEvent
  public static void handleLivingHurtEvent(LivingHurtEvent event) {
    // Reduce hurt damage for child players.
    if (COMMON.childPlayerHurtDamageReduction.get() > 0
        && !COMMON.childPlayerProtectionList.get().isEmpty()
        && event.getEntity() instanceof ServerPlayer serverPlayer
        && COMMON.childPlayerProtectionList.get().contains(serverPlayer.getName().getString())) {
      float hurtDamage = event.getAmount();

      // Cancel event, if hurt damage reduction is 100%.
      if (COMMON.childPlayerHurtDamageReduction.get() == 100) {
        log.debug("Ignore {} hurt damage for {}", hurtDamage, serverPlayer);
        event.setCanceled(true);
      } else {
        // Calculate new hurt damage based on damage reduction.
        float reducedHurtDamage =
            hurtDamage - (hurtDamage / (100f / COMMON.childPlayerHurtDamageReduction.get()));
        log.debug(
            "Reduce hurt damage for {} from {} to {}", serverPlayer, hurtDamage, reducedHurtDamage);
        event.setAmount(reducedHurtDamage);
      }
    }
  }

  @SubscribeEvent
  public static void handleLivingDamageEvent(LivingDamageEvent event) {
    // Increase attack damage for child players.
    if (COMMON.childPlayerAttackDamageIncrease.get() > 0
        && !COMMON.childPlayerProtectionList.get().isEmpty()
        && event.getSource() != null
        && event.getSource().getEntity() instanceof ServerPlayer serverPlayer
        && COMMON.childPlayerProtectionList.get().contains(serverPlayer.getName().getString())) {
      float attackDamage = event.getAmount();
      float increasedAttackDamage =
          attackDamage + (attackDamage / (100f / COMMON.childPlayerAttackDamageIncrease.get()));
      log.debug(
          "Increase attack damage for {} from {} to {}",
          serverPlayer,
          attackDamage,
          increasedAttackDamage);
      event.setAmount(increasedAttackDamage);
    }
  }
}
