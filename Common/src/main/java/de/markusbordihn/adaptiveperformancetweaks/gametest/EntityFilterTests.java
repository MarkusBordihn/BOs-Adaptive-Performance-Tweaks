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

import de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager;
import java.util.Collections;
import java.util.Set;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.animal.Wolf;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.Zombie;
import net.minecraft.world.entity.projectile.Arrow;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;

public final class EntityFilterTests {

  private static final String ENTITY_ZOMBIE = "minecraft:zombie";
  private static final String ENTITY_WOLF = "minecraft:wolf";

  private EntityFilterTests() {}

  public static void testNullEntityNotRelevant(GameTestHelper helper) {
    GameTestHelpers.assertFalse(
        helper, "Null entity should not be relevant", CoreEntityManager.isRelevantEntity(null));
    helper.succeed();
  }

  public static void testRegularZombieIsRelevant(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    Zombie zombie = new Zombie(EntityType.ZOMBIE, level);

    GameTestHelpers.assertTrue(
        helper, "Regular zombie should be relevant", CoreEntityManager.isRelevantEntity(zombie));
    GameTestHelpers.assertTrue(
        helper,
        "Regular zombie with entity name should be relevant",
        CoreEntityManager.isRelevantEntity(zombie, ENTITY_ZOMBIE));
    helper.succeed();
  }

  public static void testNamedZombieNotRelevant(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    Zombie zombie = new Zombie(EntityType.ZOMBIE, level);
    zombie.setCustomName(Component.literal("Bob"));

    GameTestHelpers.assertFalse(
        helper,
        "Named zombie should not be relevant (has custom name)",
        CoreEntityManager.isRelevantEntity(zombie, ENTITY_ZOMBIE));
    helper.succeed();
  }

  public static void testPersistenceRequiredZombieNotRelevant(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    Zombie zombie = new Zombie(EntityType.ZOMBIE, level);
    zombie.setPersistenceRequired();

    GameTestHelpers.assertFalse(
        helper,
        "Persistence-required zombie should not be relevant",
        CoreEntityManager.isRelevantEntity(zombie, ENTITY_ZOMBIE));
    helper.succeed();
  }

  public static void testPassengerZombieNotRelevant(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    Zombie carrier = new Zombie(EntityType.ZOMBIE, level);
    Zombie passenger = new Zombie(EntityType.ZOMBIE, level);
    passenger.startRiding(carrier);

    GameTestHelpers.assertFalse(
        helper,
        "Passenger zombie should not be relevant",
        CoreEntityManager.isRelevantEntity(passenger, ENTITY_ZOMBIE));
    helper.succeed();
  }

  public static void testVehicleZombieNotRelevant(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    Zombie carrier = new Zombie(EntityType.ZOMBIE, level);
    Zombie passenger = new Zombie(EntityType.ZOMBIE, level);
    passenger.startRiding(carrier);

    GameTestHelpers.assertFalse(
        helper,
        "Vehicle zombie (carrying a passenger) should not be relevant",
        CoreEntityManager.isRelevantEntity(carrier, ENTITY_ZOMBIE));
    helper.succeed();
  }

  public static void testProjectileNotRelevant(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    Arrow arrow = new Arrow(EntityType.ARROW, level);

    GameTestHelpers.assertFalse(
        helper,
        "Arrow (projectile) should not be relevant",
        CoreEntityManager.isRelevantEntity(arrow));
    helper.succeed();
  }

  public static void testItemEntityNotRelevant(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    ItemEntity item = new ItemEntity(level, 0, 1, 0, new ItemStack(Items.DIRT));

    GameTestHelpers.assertFalse(
        helper, "Item entity should not be relevant", CoreEntityManager.isRelevantEntity(item));
    helper.succeed();
  }

  public static void testTamedWolfNotRelevant(GameTestHelper helper) {
    ServerLevel level = helper.getLevel();
    Wolf wolf = new Wolf(EntityType.WOLF, level);
    wolf.setTame(true, false);

    GameTestHelpers.assertFalse(
        helper,
        "Tamed wolf should not be relevant",
        CoreEntityManager.isRelevantEntity(wolf, ENTITY_WOLF));
    helper.succeed();
  }

  public static void testExcludedNamespaceZombieNotRelevant(GameTestHelper helper) {
    CoreEntityManager.setExcludedModNamespaces(Set.of("testmod"));
    ServerLevel level = helper.getLevel();
    Zombie zombie = new Zombie(EntityType.ZOMBIE, level);

    GameTestHelpers.assertFalse(
        helper,
        "Entity from excluded mod namespace should not be relevant",
        CoreEntityManager.isRelevantEntity(zombie, "testmod:zombie"));

    GameTestHelpers.assertTrue(
        helper,
        "Entity from non-excluded namespace should still be relevant",
        CoreEntityManager.isRelevantEntity(zombie, "minecraft:zombie"));

    CoreEntityManager.setExcludedModNamespaces(Collections.emptySet());
    helper.succeed();
  }
}
