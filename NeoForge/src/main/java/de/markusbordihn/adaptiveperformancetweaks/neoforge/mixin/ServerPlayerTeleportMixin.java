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

package de.markusbordihn.adaptiveperformancetweaks.neoforge.mixin;

import de.markusbordihn.adaptiveperformancetweaks.server.CommonServerEventHandler;
import java.util.Set;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.RelativeMovement;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(ServerPlayer.class)
public class ServerPlayerTeleportMixin {

  @Inject(method = "teleportTo(DDD)V", at = @At("TAIL"))
  private void aptweaks_handleSameDimensionTeleport(double x, double y, double z, CallbackInfo ci) {
    if (!((Object) this instanceof ServerPlayer serverPlayer)) {
      return;
    }

    CommonServerEventHandler.handlePlayerTeleported(serverPlayer);
  }

  @Inject(
    method = "teleportTo(Lnet/minecraft/server/level/ServerLevel;DDDLjava/util/Set;FF)Z",
    at = @At("RETURN"))
  private void aptweaks_handleTeleport(ServerLevel level, double x, double y, double z,
    Set<RelativeMovement> relatives, float yRot, float xRot,
    CallbackInfoReturnable<Boolean> cir) {
    if (Boolean.TRUE.equals(cir.getReturnValue())
      && (Object) this instanceof ServerPlayer serverPlayer) {
      CommonServerEventHandler.handlePlayerTeleported(serverPlayer);
    }
  }
}
