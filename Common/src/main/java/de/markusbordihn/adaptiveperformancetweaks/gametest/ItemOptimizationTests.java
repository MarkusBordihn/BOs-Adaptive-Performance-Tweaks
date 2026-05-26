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
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ExperienceOrbManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.items.ItemEntityManager;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;

public final class ItemOptimizationTests {

  private ItemOptimizationTests() {
  }

  public static void testXpOrbClustering(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    ExperienceOrbManager.handleServerAboutToStart();

    ExperienceOrb orb1 = new ExperienceOrb(EntityType.EXPERIENCE_ORB, level);
    ((ExperienceOrbAccessor) orb1).setValue(5);
    orb1.moveTo(0.5, 1.0, 0.5);

    boolean firstJoined = ExperienceOrbManager.handleExperienceOrbJoinLevel(orb1, level);
    GameTestHelpers.assertTrue(
      helper, "First XP orb should not be merged (no existing orbs)", !firstJoined);

    ExperienceOrb orb2 = new ExperienceOrb(EntityType.EXPERIENCE_ORB, level);
    ((ExperienceOrbAccessor) orb2).setValue(3);
    orb2.moveTo(0.5, 1.0, 0.5);

    boolean secondMerged = ExperienceOrbManager.handleExperienceOrbJoinLevel(orb2, level);
    GameTestHelpers.assertTrue(
      helper, "Second XP orb within cluster range should be merged", secondMerged);
    GameTestHelpers.assertEquals(
      helper,
      "Merged XP orb value should be sum of both (5 + 3 = 8)",
      8,
      ((ExperienceOrbAccessor) orb1).getValue());
    GameTestHelpers.assertTrue(helper, "Merged orb should be marked as removed", orb2.isRemoved());

    ExperienceOrbManager.handleServerAboutToStart();
    helper.succeed();
  }

  public static void testItemEntityMerging(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    ItemEntityManager.handleServerAboutToStart();

    ItemEntity item1 = new ItemEntity(level, 0.5, 1.0, 0.5, new ItemStack(Items.DIRT, 1));
    boolean firstJoined = ItemEntityManager.handleItemEntityJoinLevel(item1, level);
    GameTestHelpers.assertTrue(
      helper, "First item entity should not be merged (no existing items)", !firstJoined);

    ItemEntity item2 = new ItemEntity(level, 0.5, 1.0, 0.5, new ItemStack(Items.DIRT, 1));
    boolean secondMerged = ItemEntityManager.handleItemEntityJoinLevel(item2, level);
    GameTestHelpers.assertTrue(
      helper,
      "Second item entity of same type within cluster range should be merged",
      secondMerged);
    GameTestHelpers.assertEquals(
      helper,
      "Item stack of existing entity should grow to 2 after merge",
      2,
      item1.getItem().getCount());

    ItemEntityManager.handleServerAboutToStart();
    helper.succeed();
  }
}
