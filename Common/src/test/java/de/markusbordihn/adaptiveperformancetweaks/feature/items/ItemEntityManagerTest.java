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

package de.markusbordihn.adaptiveperformancetweaks.feature.items;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.feature.monitoring.PerformanceStats;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.core.component.DataComponents;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity.RemovalReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.component.CustomData;
import net.minecraft.world.level.Level;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockMakers;

class ItemEntityManagerTest {

  private boolean previousFeatureState;
  private boolean previousOptimizeItems;
  private int previousMaxPerType;
  private int previousMaxPerWorld;
  private int previousClusterRange;
  private int previousMaxStackSize;
  private boolean previousMoveToLastDrop;
  private Set<String> previousAllowList;
  private Set<String> previousDenyList;

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

  private static ServerLevel mockOverworldLevel() {
    ServerLevel level = mock(ServerLevel.class, withSettings().mockMaker(MockMakers.SUBCLASS));
    doReturn(Level.OVERWORLD).when(level).dimension();
    doReturn(false).when(level).canSeeSky(any(BlockPos.class));
    return level;
  }

  private static ItemEntity createItem(ServerLevel level, int identifier, double x, double y,
    double z, ItemStack stack) {
    return new TestItemEntity(level, identifier, x, y, z, stack);
  }

  private static void markProtected(ItemStack stack, int id) {
    CompoundTag tag = new CompoundTag();
    tag.putInt("protected", id);
    stack.set(DataComponents.CUSTOM_DATA, CustomData.of(tag));
  }

  private static void setTicks(short value) throws Exception {
    Field field = ItemEntityManager.class.getDeclaredField("ticks");
    field.setAccessible(true);
    field.setShort(null, value);
  }

  @BeforeEach
  void setUp() {
    this.previousFeatureState = FeatureToggle.ITEMS.isEnabled();
    this.previousOptimizeItems = ItemsConfig.optimizeItems;
    this.previousMaxPerType = ItemsConfig.maxNumberOfItemsPerType;
    this.previousMaxPerWorld = ItemsConfig.maxNumberOfItems;
    this.previousClusterRange = ItemsConfig.itemsClusterRange;
    this.previousMaxStackSize = ItemsConfig.maxStackSize;
    this.previousMoveToLastDrop = ItemsConfig.movePositionToLastDrop;
    this.previousAllowList = new HashSet<>(ItemsConfig.itemsAllowList);
    this.previousDenyList = new HashSet<>(ItemsConfig.itemsDenyList);

    FeatureToggle.ITEMS.setEnabled(true);
    ItemsConfig.optimizeItems = true;
    ItemsConfig.maxNumberOfItemsPerType = 64;
    ItemsConfig.maxNumberOfItems = 128;
    ItemsConfig.itemsClusterRange = 2;
    ItemsConfig.maxStackSize = 64;
    ItemsConfig.movePositionToLastDrop = false;
    ItemsConfig.itemsAllowList = new HashSet<>();
    ItemsConfig.itemsDenyList = new HashSet<>();
    PerformanceStats.reset();
    ItemEntityManager.handleServerAboutToStart();
  }

  @AfterEach
  void tearDown() {
    FeatureToggle.ITEMS.setEnabled(this.previousFeatureState);
    ItemsConfig.optimizeItems = this.previousOptimizeItems;
    ItemsConfig.maxNumberOfItemsPerType = this.previousMaxPerType;
    ItemsConfig.maxNumberOfItems = this.previousMaxPerWorld;
    ItemsConfig.itemsClusterRange = this.previousClusterRange;
    ItemsConfig.maxStackSize = this.previousMaxStackSize;
    ItemsConfig.movePositionToLastDrop = this.previousMoveToLastDrop;
    ItemsConfig.itemsAllowList = this.previousAllowList;
    ItemsConfig.itemsDenyList = this.previousDenyList;
    PerformanceStats.reset();
    ItemEntityManager.handleServerStopping();
  }

  @Test
  void allowListSkipsUnlistedItems() {
    ItemsConfig.itemsAllowList = Set.of("minecraft:diamond");
    ItemEntityManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();

    boolean merged = ItemEntityManager.handleItemEntityJoinLevel(
      createItem(level, 1, 0.0d, 64.0d, 0.0d, new ItemStack(Items.COBBLESTONE, 1)), level);

    assertFalse(merged);
    assertEquals(0, ItemEntityManager.getTrackedItemEntityCount());
  }

  @Test
  void nearbyStackableItemsMergeIntoExistingStack() {
    ServerLevel level = mockOverworldLevel();
    ItemEntity first =
      createItem(level, 1, 0.0d, 64.0d, 0.0d, new ItemStack(Items.COBBLESTONE, 8));
    ItemEntity second =
      createItem(level, 2, 1.0d, 64.0d, 1.0d, new ItemStack(Items.COBBLESTONE, 4));

    boolean firstMerged = ItemEntityManager.handleItemEntityJoinLevel(first, level);
    boolean secondMerged = ItemEntityManager.handleItemEntityJoinLevel(second, level);

    assertFalse(firstMerged);
    assertTrue(secondMerged);
    assertEquals(12, first.getItem().getCount());
    assertEquals(0, second.getItem().getCount());
    assertEquals(1, ItemEntityManager.getTrackedItemEntityCount());
    assertEquals(1L, PerformanceStats.itemsMerged);
  }

  @Test
  void partialMergeKeepsRemainderEntity() {
    ServerLevel level = mockOverworldLevel();
    ItemEntity existing =
      createItem(level, 1, 0.0d, 64.0d, 0.0d, new ItemStack(Items.COBBLESTONE, 60));
    ItemEntity incoming =
      createItem(level, 2, 1.0d, 64.0d, 1.0d, new ItemStack(Items.COBBLESTONE, 10));

    ItemEntityManager.handleItemEntityJoinLevel(existing, level);
    boolean incomingMerged = ItemEntityManager.handleItemEntityJoinLevel(incoming, level);

    assertFalse(incomingMerged);
    assertEquals(64, existing.getItem().getCount());
    assertEquals(6, incoming.getItem().getCount());
    assertEquals(2, ItemEntityManager.getTrackedItemEntityCount());
  }

  @Test
  void worldLimitProtectsItemsWithNbtUntilHardCap() {
    ItemsConfig.maxNumberOfItems = 1;
    ItemEntityManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();
    ItemStack protectedStack = new ItemStack(Items.COBBLESTONE, 1);
    markProtected(protectedStack, 1);
    ItemEntity protectedItem = createItem(level, 1, 0.0d, 64.0d, 0.0d, protectedStack);
    ItemEntity plainItem =
      createItem(level, 2, 20.0d, 64.0d, 20.0d, new ItemStack(Items.DIRT, 1));

    ItemEntityManager.handleItemEntityJoinLevel(protectedItem, level);
    ItemEntityManager.handleItemEntityJoinLevel(plainItem, level);

    assertFalse(protectedItem.isRemoved());
    assertTrue(plainItem.isRemoved());

    ItemStack secondProtectedStack = new ItemStack(Items.COBBLESTONE, 1);
    markProtected(secondProtectedStack, 2);
    ItemEntity secondProtectedItem =
      createItem(level, 3, 40.0d, 64.0d, 40.0d, secondProtectedStack);
    ItemStack thirdProtectedStack = new ItemStack(Items.COBBLESTONE, 1);
    markProtected(thirdProtectedStack, 3);
    ItemEntity thirdProtectedItem =
      createItem(level, 4, 60.0d, 64.0d, 60.0d, thirdProtectedStack);

    ItemEntityManager.handleItemEntityJoinLevel(secondProtectedItem, level);
    assertFalse(secondProtectedItem.isRemoved());

    ItemEntityManager.handleItemEntityJoinLevel(thirdProtectedItem, level);

    assertTrue(protectedItem.isRemoved());
    assertFalse(secondProtectedItem.isRemoved());
    assertFalse(thirdProtectedItem.isRemoved());
  }

  @Test
  void worldLimitRemovesOldestTrackedItem() {
    ItemsConfig.maxNumberOfItems = 1;
    ItemsConfig.maxNumberOfItemsPerType = 64;
    ItemEntityManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();
    ItemEntity oldest =
      createItem(level, 1, 0.0d, 64.0d, 0.0d, new ItemStack(Items.COBBLESTONE, 1));
    ItemEntity newest =
      createItem(level, 2, 10.0d, 64.0d, 10.0d, new ItemStack(Items.DIRT, 1));

    ItemEntityManager.handleItemEntityJoinLevel(oldest, level);
    ItemEntityManager.handleItemEntityJoinLevel(newest, level);

    assertTrue(oldest.isRemoved());
    assertFalse(newest.isRemoved());
    assertEquals(1, ItemEntityManager.getTrackedItemEntityCount());
    assertEquals(1L, PerformanceStats.itemsRemoved);
  }

  @Test
  void typeLimitRemovesOldestTrackedItemOfSameType() {
    ItemsConfig.maxNumberOfItems = 64;
    ItemsConfig.maxNumberOfItemsPerType = 1;
    ItemsConfig.itemsClusterRange = 0;
    ItemEntityManager.handleServerAboutToStart();
    ServerLevel level = mockOverworldLevel();
    ItemEntity oldest =
      createItem(level, 1, 0.0d, 64.0d, 0.0d, new ItemStack(Items.COBBLESTONE, 1));
    ItemEntity newest =
      createItem(level, 2, 20.0d, 64.0d, 20.0d, new ItemStack(Items.COBBLESTONE, 1));

    ItemEntityManager.handleItemEntityJoinLevel(oldest, level);
    ItemEntityManager.handleItemEntityJoinLevel(newest, level);

    assertTrue(oldest.isRemoved());
    assertFalse(newest.isRemoved());
    assertEquals(1, ItemEntityManager.getTrackedItemEntityCount());
    assertEquals(1L, PerformanceStats.itemsRemoved);
  }

  @Test
  void verificationRemovesDiscardedTrackedItems() throws Exception {
    ServerLevel level = mockOverworldLevel();
    ItemEntity active =
      createItem(level, 1, 0.0d, 64.0d, 0.0d, new ItemStack(Items.COBBLESTONE, 1));
    ItemEntity removed =
      createItem(level, 2, 10.0d, 64.0d, 10.0d, new ItemStack(Items.DIRT, 1));

    ItemEntityManager.handleItemEntityJoinLevel(active, level);
    ItemEntityManager.handleItemEntityJoinLevel(removed, level);
    removed.remove(RemovalReason.DISCARDED);
    setTicks((short) 599);

    ItemEntityManager.handleServerTick();

    assertEquals(1, ItemEntityManager.getTrackedItemEntityCount());
    assertEquals(Collections.singletonMap("minecraft:overworld",
        Collections.singletonMap("minecraft:cobblestone", 1)),
      ItemEntityManager.getItemEntityCountsByDimension());
  }

  private static final class TestItemEntity extends ItemEntity {

    private ItemStack itemStack;

    private TestItemEntity(ServerLevel level, int identifier, double x, double y, double z,
      ItemStack itemStack) {
      super(EntityType.ITEM, level);
      this.itemStack = itemStack;
      this.setId(identifier);
      this.setPos(x, y, z);
    }

    @Override
    public ItemStack getItem() {
      return this.itemStack;
    }

    @Override
    public void setItem(ItemStack itemStack) {
      this.itemStack = itemStack;
    }
  }
}
