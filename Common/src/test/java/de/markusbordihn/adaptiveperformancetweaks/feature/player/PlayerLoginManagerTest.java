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

import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.lang.reflect.Field;
import java.util.Set;
import net.minecraft.server.level.ServerPlayer;
import org.junit.jupiter.api.Test;

class PlayerLoginManagerTest {

  @SuppressWarnings("unchecked")
  private static Set<?> readPlayerValidationList() throws Exception {
    Field field = PlayerLoginManager.class.getDeclaredField("playerValidationList");
    field.setAccessible(true);
    return (Set<?>) field.get(null);
  }

  @Test
  void disabledFeatureSkipsLoginProtectionHandling() throws Exception {
    boolean previousState = FeatureToggle.PLAYER_LOGIN_PROTECTION.isEnabled();
    boolean previousProtection = PlayerLoginProtectionConfig.protectPlayerDuringLogin;
    try {
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(false);
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = true;
      PlayerLoginManager.handleServerAboutToStart();

      PlayerLoginManager.handlePlayerLoggedIn(mock(ServerPlayer.class));

      assertEquals(0, readPlayerValidationList().size());
    } finally {
      PlayerLoginProtectionConfig.protectPlayerDuringLogin = previousProtection;
      FeatureToggle.PLAYER_LOGIN_PROTECTION.setEnabled(previousState);
      PlayerLoginManager.handleServerStopping();
    }
  }
}
