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

package de.markusbordihn.adaptiveperformancetweaks.core.compat;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureState;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class ModConflictDetectorTest {

  @AfterEach
  void resetModChecker() {
    ModCompat.setModLoadedChecker(id -> false);
  }

  @Test
  void autoFeatureBecomesConflictDisabledWhenConflictingModIsLoaded() {
    ModCompat.setModLoadedChecker("eco_stack_manager"::equals);

    ModConflictDetector.FeatureDecision decision =
      ModConflictDetector.resolveFeatureDecision(FeatureToggle.ITEMS, FeatureState.AUTO);

    assertFalse(decision.enabled());
    assertEquals(ModConflictDetector.FeatureActivation.CONFLICT_DISABLED, decision.activation());
    assertEquals("eco_stack_manager", decision.relatedModId());
  }

  @Test
  void explicitDisableStaysManualDisabled() {
    ModConflictDetector.FeatureDecision decision =
      ModConflictDetector.resolveFeatureDecision(
        FeatureToggle.ADAPTIVE_VIEW_DISTANCE, FeatureState.DISABLED);

    assertFalse(decision.enabled());
    assertEquals(ModConflictDetector.FeatureActivation.MANUAL_DISABLED, decision.activation());
  }

  @Test
  void explicitEnableStaysManualEnabledEvenWithConflict() {
    ModCompat.setModLoadedChecker("dynview"::equals);

    ModConflictDetector.FeatureDecision decision =
      ModConflictDetector.resolveFeatureDecision(
        FeatureToggle.ADAPTIVE_VIEW_DISTANCE, FeatureState.ENABLED);

    assertTrue(decision.enabled());
    assertEquals(ModConflictDetector.FeatureActivation.MANUAL_ENABLED, decision.activation());
    assertEquals("dynview", decision.relatedModId());
  }
}
