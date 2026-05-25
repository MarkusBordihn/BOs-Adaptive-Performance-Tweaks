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

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.neoforged.neoforge.gametest.GameTestHolder;
import net.neoforged.neoforge.gametest.PrefixGameTestTemplate;

@SuppressWarnings("unused")
@PrefixGameTestTemplate(value = false)
@GameTestHolder(Constants.MOD_ID)
public class PlayerDamageGameTest {

  @GameTest(template = "gametest.1x1x1")
  public void testDamageUnchangedForNonChildPlayer(GameTestHelper helper) {
    PlayerDamageTests.testDamageUnchangedForNonChildPlayer(helper);
  }

  @GameTest(template = "gametest.1x1x1")
  public void testDamageReducedForChildPlayer(GameTestHelper helper) {
    PlayerDamageTests.testDamageReducedForChildPlayer(helper);
  }

  @GameTest(template = "gametest.1x1x1")
  public void testDamageFullyBlockedAtHundredPercent(GameTestHelper helper) {
    PlayerDamageTests.testDamageFullyBlockedAtHundredPercent(helper);
  }

  @GameTest(template = "gametest.1x1x1")
  public void testAttackDamageIncreasedForChildPlayer(GameTestHelper helper) {
    PlayerDamageTests.testAttackDamageIncreasedForChildPlayer(helper);
  }

  @GameTest(template = "gametest.1x1x1")
  public void testChildModeHurtReductionThroughEventPipeline(GameTestHelper helper) {
    PlayerDamageTests.testChildModeHurtReductionThroughHook(helper, "Forge event pipeline");
  }

  @GameTest(template = "gametest.1x1x1")
  public void testStarterProtectionHurtReductionThroughEventPipeline(GameTestHelper helper) {
    PlayerDamageTests.testStarterProtectionHurtReductionThroughHook(helper, "Forge event pipeline");
  }
}
