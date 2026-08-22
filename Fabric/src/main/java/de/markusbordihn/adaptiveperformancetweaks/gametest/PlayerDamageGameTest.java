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

import net.fabricmc.fabric.api.gametest.v1.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;

@SuppressWarnings("unused")
public class PlayerDamageGameTest {

  private static final String STRUCTURE = "adaptive_performance_tweaks:gametest.1x1x1";

  @GameTest(structure = STRUCTURE)
  public void testDamageUnchangedForNonChildPlayer(GameTestHelper helper) {
    PlayerDamageTests.testDamageUnchangedForNonChildPlayer(helper);
  }

  @GameTest(structure = STRUCTURE)
  public void testDamageReducedForChildPlayer(GameTestHelper helper) {
    PlayerDamageTests.testDamageReducedForChildPlayer(helper);
  }

  @GameTest(structure = STRUCTURE)
  public void testDamageFullyBlockedAtHundredPercent(GameTestHelper helper) {
    PlayerDamageTests.testDamageFullyBlockedAtHundredPercent(helper);
  }

  @GameTest(structure = STRUCTURE)
  public void testAttackDamageIncreasedForChildPlayer(GameTestHelper helper) {
    PlayerDamageTests.testAttackDamageIncreasedForChildPlayer(helper);
  }

  @GameTest(structure = STRUCTURE)
  public void testChildModeHurtReductionThroughHook(GameTestHelper helper) {
    PlayerDamageTests.testChildModeHurtReductionThroughHook(helper, "Fabric mixin");
  }

  @GameTest(structure = STRUCTURE)
  public void testStarterProtectionHurtReductionThroughHook(GameTestHelper helper) {
    PlayerDamageTests.testStarterProtectionHurtReductionThroughHook(helper, "Fabric mixin");
  }
}
