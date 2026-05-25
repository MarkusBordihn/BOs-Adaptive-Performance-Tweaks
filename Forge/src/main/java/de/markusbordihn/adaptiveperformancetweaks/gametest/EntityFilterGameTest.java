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
import net.minecraftforge.gametest.GameTestHolder;

@SuppressWarnings("unused")
@GameTestHolder(Constants.MOD_ID)
public class EntityFilterGameTest {

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testNullEntityNotRelevant(GameTestHelper helper) {
    EntityFilterTests.testNullEntityNotRelevant(helper);
  }

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testRegularZombieIsRelevant(GameTestHelper helper) {
    EntityFilterTests.testRegularZombieIsRelevant(helper);
  }

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testNamedZombieNotRelevant(GameTestHelper helper) {
    EntityFilterTests.testNamedZombieNotRelevant(helper);
  }

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testPersistenceRequiredZombieNotRelevant(GameTestHelper helper) {
    EntityFilterTests.testPersistenceRequiredZombieNotRelevant(helper);
  }

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testPassengerZombieNotRelevant(GameTestHelper helper) {
    EntityFilterTests.testPassengerZombieNotRelevant(helper);
  }

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testVehicleZombieNotRelevant(GameTestHelper helper) {
    EntityFilterTests.testVehicleZombieNotRelevant(helper);
  }

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testProjectileNotRelevant(GameTestHelper helper) {
    EntityFilterTests.testProjectileNotRelevant(helper);
  }

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testItemEntityNotRelevant(GameTestHelper helper) {
    EntityFilterTests.testItemEntityNotRelevant(helper);
  }

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testTamedWolfNotRelevant(GameTestHelper helper) {
    EntityFilterTests.testTamedWolfNotRelevant(helper);
  }

  @GameTest(template = "adaptive_performance_tweaks:gametest.1x1x1")
  public void testExcludedNamespaceZombieNotRelevant(GameTestHelper helper) {
    EntityFilterTests.testExcludedNamespaceZombieNotRelevant(helper);
  }
}
