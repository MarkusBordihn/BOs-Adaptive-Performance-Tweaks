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

package de.markusbordihn.adaptiveperformancetweaks.entity;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import net.minecraftforge.event.entity.EntityJoinLevelEvent;
import net.minecraftforge.event.entity.EntityLeaveLevelEvent;
import net.minecraftforge.event.entity.living.LivingDeathEvent;
import net.minecraftforge.eventbus.api.listener.Priority;
import net.minecraftforge.eventbus.api.listener.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

@SuppressWarnings("unused")
@EventBusSubscriber(modid = Constants.MOD_ID)
public final class EntityEventHandler {

  private EntityEventHandler() {
  }

  @SubscribeEvent(priority = Priority.HIGH)
  public static boolean handleEntityJoinLevel(EntityJoinLevelEvent event) {
    return CommonEntityEventHandler.handleEntityJoinLevel(event.getEntity(), event.getLevel());
  }

  @SubscribeEvent(priority = Priority.HIGH)
  public static void handleEntityLeaveLevel(EntityLeaveLevelEvent event) {
    CommonEntityEventHandler.handleEntityLeaveLevel(event.getEntity(), event.getLevel());
  }

  @SubscribeEvent(priority = Priority.HIGH)
  public static void handleLivingDeath(LivingDeathEvent event) {
    CommonEntityEventHandler.handleLivingDeath(event.getEntity());
  }
}
