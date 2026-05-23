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

import de.markusbordihn.adaptiveperformancetweaks.accessor.ExperienceOrbAccessor;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ExperienceOrb;

public final class MixinTests {

  private MixinTests() {
  }

  public static void testExperienceOrbAccessorMixin(GameTestHelper helper) {
    ExperienceOrb orb = new ExperienceOrb(EntityType.EXPERIENCE_ORB, helper.getLevel());
    GameTestHelpers.assertTrue(
      helper,
      "ExperienceOrbAccessor mixin was not applied — check mixin config and refmap!",
      orb instanceof ExperienceOrbAccessor);
    ExperienceOrbAccessor accessor = (ExperienceOrbAccessor) orb;
    accessor.setValue(42);
    GameTestHelpers.assertEquals(
      helper,
      "ExperienceOrbAccessor.getValue() returned wrong value — field remapping broken?",
      42,
      accessor.getValue());
    helper.succeed();
  }
}
