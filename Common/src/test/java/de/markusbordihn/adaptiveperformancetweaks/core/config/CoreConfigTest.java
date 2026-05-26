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

package de.markusbordihn.adaptiveperformancetweaks.core.config;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureState;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.lang.reflect.Field;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class CoreConfigTest {

  @BeforeEach
  @SuppressWarnings("unchecked")
  void clearFeatureFlags() throws Exception {
    Field field = CoreConfig.class.getDeclaredField("featureFlags");
    field.setAccessible(true);
    ((Map<FeatureToggle, Boolean>) field.get(null)).clear();
  }

  @Test
  void isFeatureEnabledFallsBackToDefaultWhenNotLoaded() {
    for (FeatureToggle toggle : FeatureToggle.values()) {
      boolean expected = toggle.getDefaultState() != FeatureState.DISABLED;
      assertEquals(
        expected,
        CoreConfig.isFeatureEnabled(toggle),
        "Default mismatch for " + toggle.getId());
    }
  }

  @Test
  void setFeatureEnabledOverridesDefault() {
    CoreConfig.setFeatureEnabled(FeatureToggle.MONITORING, true);
    assertTrue(CoreConfig.isFeatureEnabled(FeatureToggle.MONITORING));

    CoreConfig.setFeatureEnabled(FeatureToggle.GAMERULES, false);
    assertFalse(CoreConfig.isFeatureEnabled(FeatureToggle.GAMERULES));
  }

  @Test
  void serverLoadThresholdDefaults() {
    assertEquals(20, CoreConfig.serverLoadVeryLowThreshold);
    assertEquals(40, CoreConfig.serverLoadLowThreshold);
    assertEquals(46, CoreConfig.serverLoadNormalThreshold);
    assertEquals(49, CoreConfig.serverLoadMediumThreshold);
    assertEquals(55, CoreConfig.serverLoadHighThreshold);
  }

  @Test
  void otherConfigDefaults() {
    assertEquals(5, CoreConfig.timeBetweenUpdates);
    assertTrue(CoreConfig.logServerLoad);
    assertFalse(CoreConfig.logServerLevelLoadChanges);
    assertEquals(60, CoreConfig.serverLoadLogIntervalSeconds);
    assertEquals(2, CoreConfig.serverLoadLogSignificantChangeSteps);
    assertEquals(5, CoreConfig.serverLoadLogTopWorldCount);
  }

  @Test
  void thresholdsAreStrictlyAscending() {
    assertTrue(CoreConfig.serverLoadVeryLowThreshold < CoreConfig.serverLoadLowThreshold);
    assertTrue(CoreConfig.serverLoadLowThreshold < CoreConfig.serverLoadNormalThreshold);
    assertTrue(CoreConfig.serverLoadNormalThreshold < CoreConfig.serverLoadMediumThreshold);
    assertTrue(CoreConfig.serverLoadMediumThreshold < CoreConfig.serverLoadHighThreshold);
  }
}
