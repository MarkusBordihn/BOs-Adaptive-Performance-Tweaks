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

import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLevelLoad;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadEvent;
import de.markusbordihn.adaptiveperformancetweaks.core.server.ServerLoadLevel;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnConfig;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnManager;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnPresetRegistry;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.VirtualPlayerManager;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.monster.Zombie;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

public final class SpawnThrottleTests {

  private SpawnThrottleTests() {
  }

  public static void testSpawnerNotThrottledUnderNormalLoad(GameTestHelper helper) {
    SpawnManager.handleServerAboutToStart();
    ServerLevelLoad.reset();
    GameTestHelpers.assertTrue(
      helper,
      "Spawner should NOT be throttled under NORMAL load",
      !SpawnManager.shouldThrottleSpawner(helper.getLevel()));
    helper.succeed();
  }

  public static void testSpawnerThrottledUnderVeryHighLoad(GameTestHelper helper) {
    ServerLevelLoad.reset();
    SpawnManager.handleServerLoadEvent(
      new ServerLoadEvent(ServerLoadLevel.VERY_HIGH, ServerLoadLevel.NORMAL, 200.0, 50.0));
    GameTestHelpers.assertTrue(
      helper,
      "Spawner should be throttled under VERY_HIGH load",
      SpawnManager.shouldThrottleSpawner(helper.getLevel()));
    SpawnManager.handleServerAboutToStart();
    helper.succeed();
  }

  public static void testSpawnerThrottleOnlyInHighLoadLevel(GameTestHelper helper) {
    SpawnManager.handleServerAboutToStart();
    ServerLevelLoad.reset();
    SpawnManager.handleServerLoadEvent(
      new ServerLoadEvent(ServerLoadLevel.NORMAL, ServerLoadLevel.NORMAL, 50.0, 50.0));

    ServerLevel overworld = helper.getLevel();
    ServerLevel nether = GameTestHelpers.getRequiredLevel(helper, Level.NETHER);
    GameTestHelpers.setMeasuredLevelLoad(overworld, ServerLoadLevel.VERY_HIGH, 200.0);
    GameTestHelpers.setMeasuredLevelLoad(nether, ServerLoadLevel.NORMAL, 50.0);

    GameTestHelpers.assertTrue(
      helper,
      "Spawner should be throttled in the measured high-load level",
      SpawnManager.shouldThrottleSpawner(overworld));
    GameTestHelpers.assertFalse(
      helper,
      "Spawner should not be throttled in the measured normal-load level",
      SpawnManager.shouldThrottleSpawner(nether));

    SpawnManager.handleServerAboutToStart();
    ServerLevelLoad.reset();
    helper.succeed();
  }

  public static void testEntityWorldLimitBlocksSpawnAt10(GameTestHelper helper) {
    final int LIMIT = 10;
    final int ATTEMPTS = 20;

    boolean wasSpawnEnabled = FeatureToggle.SPAWN.isEnabled();
    int originalWorldMax = SpawnConfig.spawnLimitationMaxMobsPerWorld;
    int originalFriendlyRate = SpawnConfig.friendlyChunkSpawnRate;

    FeatureToggle.SPAWN.setEnabled(true);
    SpawnConfig.spawnLimitationMaxMobsPerWorld = LIMIT;
    SpawnConfig.friendlyChunkSpawnRate = 0;
    SpawnPresetRegistry.reload(Collections.emptyList());

    SpawnManager.handleServerAboutToStart();
    SpawnManager.handleServerStarted();
    for (int i = 0; i <= 20 * 20; i++) {
      SpawnManager.handleServerTick();
    }

    ServerLevel level = helper.getLevel();
    BlockPos spawnPos = helper.absolutePos(new BlockPos(0, 1, 0));
    List<Zombie> spawnedZombies = new ArrayList<>();
    for (int i = 0; i < ATTEMPTS; i++) {
      Zombie zombie = new Zombie(EntityType.ZOMBIE, level);
      zombie.setPos(spawnPos.getX() + 0.5, spawnPos.getY(), spawnPos.getZ() + 0.5);
      if (!SpawnManager.shouldDenyMobSpawn(zombie, level, MobSpawnType.NATURAL)) {
        level.addFreshEntity(zombie);
        spawnedZombies.add(zombie);
      }
    }

    int allowed = spawnedZombies.size();
    spawnedZombies.forEach(z -> z.remove(Entity.RemovalReason.DISCARDED));
    FeatureToggle.SPAWN.setEnabled(wasSpawnEnabled);
    SpawnConfig.spawnLimitationMaxMobsPerWorld = originalWorldMax;
    SpawnConfig.friendlyChunkSpawnRate = originalFriendlyRate;
    SpawnManager.handleServerAboutToStart();

    GameTestHelpers.assertTrue(
      helper,
      "Spawn limit not enforced: "
        + allowed
        + " zombies allowed (expected > 0 and <= "
        + LIMIT
        + ")",
      allowed > 0 && allowed <= LIMIT);
    helper.succeed();
  }

  public static void testEntityServerLimitBlocksSpawnAt10(GameTestHelper helper) {
    final int LIMIT = 10;
    final int ATTEMPTS = 20;

    boolean wasSpawnEnabled = FeatureToggle.SPAWN.isEnabled();
    int origServerMax = SpawnConfig.spawnLimitationMaxMobsPerServer;
    int origWorldMax = SpawnConfig.spawnLimitationMaxMobsPerWorld;
    int origChunkMax = SpawnConfig.spawnLimitationMaxMobsPerChunk;
    int origPlayerMax = SpawnConfig.spawnLimitationMaxMobsPerPlayer;
    int origFriendlyRate = SpawnConfig.friendlyChunkSpawnRate;

    FeatureToggle.SPAWN.setEnabled(true);
    SpawnConfig.spawnLimitationMaxMobsPerServer = LIMIT;
    SpawnConfig.spawnLimitationMaxMobsPerWorld = -1;
    SpawnConfig.spawnLimitationMaxMobsPerChunk = -1;
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = -1;
    SpawnConfig.friendlyChunkSpawnRate = 0;
    SpawnPresetRegistry.reload(Collections.emptyList());

    SpawnManager.handleServerAboutToStart();
    SpawnManager.handleServerStarted();
    for (int i = 0; i <= 20 * 20; i++) {
      SpawnManager.handleServerTick();
    }

    ServerLevel level = helper.getLevel();
    BlockPos spawnPos = helper.absolutePos(new BlockPos(0, 1, 0));
    List<Zombie> spawnedZombies = new ArrayList<>();
    for (int i = 0; i < ATTEMPTS; i++) {
      Zombie zombie = new Zombie(EntityType.ZOMBIE, level);
      zombie.setPos(spawnPos.getX() + 0.5, spawnPos.getY(), spawnPos.getZ() + 0.5);
      if (!SpawnManager.shouldDenyMobSpawn(zombie, level, MobSpawnType.NATURAL)) {
        level.addFreshEntity(zombie);
        spawnedZombies.add(zombie);
      }
    }

    int allowed = spawnedZombies.size();
    spawnedZombies.forEach(z -> z.remove(Entity.RemovalReason.DISCARDED));
    FeatureToggle.SPAWN.setEnabled(wasSpawnEnabled);
    SpawnConfig.spawnLimitationMaxMobsPerServer = origServerMax;
    SpawnConfig.spawnLimitationMaxMobsPerWorld = origWorldMax;
    SpawnConfig.spawnLimitationMaxMobsPerChunk = origChunkMax;
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = origPlayerMax;
    SpawnConfig.friendlyChunkSpawnRate = origFriendlyRate;
    SpawnManager.handleServerAboutToStart();

    GameTestHelpers.assertTrue(
      helper,
      "Server limit not enforced: "
        + allowed
        + " zombies allowed (expected > 0 and <= "
        + LIMIT
        + ")",
      allowed > 0 && allowed <= LIMIT);
    helper.succeed();
  }

  public static void testEntityChunkLimitBlocksSpawnAt5(GameTestHelper helper) {
    final int LIMIT = 5;
    final int ATTEMPTS = 10;

    boolean wasSpawnEnabled = FeatureToggle.SPAWN.isEnabled();
    int origChunkMax = SpawnConfig.spawnLimitationMaxMobsPerChunk;
    int origWorldMax = SpawnConfig.spawnLimitationMaxMobsPerWorld;
    int origServerMax = SpawnConfig.spawnLimitationMaxMobsPerServer;
    int origPlayerMax = SpawnConfig.spawnLimitationMaxMobsPerPlayer;
    int origFriendlyRate = SpawnConfig.friendlyChunkSpawnRate;

    FeatureToggle.SPAWN.setEnabled(true);
    SpawnConfig.spawnLimitationMaxMobsPerChunk = LIMIT;
    SpawnConfig.spawnLimitationMaxMobsPerWorld = -1;
    SpawnConfig.spawnLimitationMaxMobsPerServer = -1;
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = -1;
    SpawnConfig.friendlyChunkSpawnRate = 0;
    SpawnPresetRegistry.reload(Collections.emptyList());

    SpawnManager.handleServerAboutToStart();
    SpawnManager.handleServerStarted();
    for (int i = 0; i <= 20 * 20; i++) {
      SpawnManager.handleServerTick();
    }

    ServerLevel level = helper.getLevel();
    BlockPos spawnPos = helper.absolutePos(new BlockPos(0, 1, 0));
    List<Zombie> spawnedZombies = new ArrayList<>();
    for (int i = 0; i < ATTEMPTS; i++) {
      Zombie zombie = new Zombie(EntityType.ZOMBIE, level);
      zombie.setPos(spawnPos.getX() + 0.5, spawnPos.getY(), spawnPos.getZ() + 0.5);
      if (!SpawnManager.shouldDenyMobSpawn(zombie, level, MobSpawnType.NATURAL)) {
        level.addFreshEntity(zombie);
        spawnedZombies.add(zombie);
      }
    }

    int allowed = spawnedZombies.size();
    spawnedZombies.forEach(z -> z.remove(Entity.RemovalReason.DISCARDED));
    FeatureToggle.SPAWN.setEnabled(wasSpawnEnabled);
    SpawnConfig.spawnLimitationMaxMobsPerChunk = origChunkMax;
    SpawnConfig.spawnLimitationMaxMobsPerWorld = origWorldMax;
    SpawnConfig.spawnLimitationMaxMobsPerServer = origServerMax;
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = origPlayerMax;
    SpawnConfig.friendlyChunkSpawnRate = origFriendlyRate;
    SpawnManager.handleServerAboutToStart();

    GameTestHelpers.assertTrue(
      helper,
      "Chunk limit not enforced: "
        + allowed
        + " zombies allowed (expected > 0 and <= "
        + LIMIT
        + ")",
      allowed > 0 && allowed <= LIMIT);
    helper.succeed();
  }

  public static void testEntityPlayerLimitVirtualZoneAt5(GameTestHelper helper) {
    final int LIMIT = 5;
    final int ATTEMPTS = 10;

    boolean wasSpawnEnabled = FeatureToggle.SPAWN.isEnabled();
    int origPlayerMax = SpawnConfig.spawnLimitationMaxMobsPerPlayer;
    int origWorldMax = SpawnConfig.spawnLimitationMaxMobsPerWorld;
    int origServerMax = SpawnConfig.spawnLimitationMaxMobsPerServer;
    int origChunkMax = SpawnConfig.spawnLimitationMaxMobsPerChunk;
    int origFriendlyRate = SpawnConfig.friendlyChunkSpawnRate;

    FeatureToggle.SPAWN.setEnabled(true);
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = LIMIT;
    SpawnConfig.spawnLimitationMaxMobsPerWorld = -1;
    SpawnConfig.spawnLimitationMaxMobsPerServer = -1;
    SpawnConfig.spawnLimitationMaxMobsPerChunk = -1;
    SpawnConfig.friendlyChunkSpawnRate = 0;
    SpawnPresetRegistry.reload(Collections.emptyList());

    ServerLevel level = helper.getLevel();
    BlockPos spawnPos = helper.absolutePos(new BlockPos(0, 1, 0));
    Vec3 virtualPos = new Vec3(spawnPos.getX() + 0.5, spawnPos.getY(), spawnPos.getZ() + 0.5);
    VirtualPlayerManager.add(level, virtualPos);

    SpawnManager.handleServerAboutToStart();
    SpawnManager.handleServerStarted();
    for (int i = 0; i <= 20 * 20; i++) {
      SpawnManager.handleServerTick();
    }

    List<Zombie> spawnedZombies = new ArrayList<>();
    for (int i = 0; i < ATTEMPTS; i++) {
      Zombie zombie = new Zombie(EntityType.ZOMBIE, level);
      zombie.setPos(virtualPos.x, virtualPos.y, virtualPos.z);
      if (!SpawnManager.shouldDenyMobSpawn(zombie, level, MobSpawnType.NATURAL)) {
        level.addFreshEntity(zombie);
        spawnedZombies.add(zombie);
      }
    }

    int allowed = spawnedZombies.size();
    spawnedZombies.forEach(z -> z.remove(Entity.RemovalReason.DISCARDED));
    VirtualPlayerManager.clearAll();
    FeatureToggle.SPAWN.setEnabled(wasSpawnEnabled);
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = origPlayerMax;
    SpawnConfig.spawnLimitationMaxMobsPerWorld = origWorldMax;
    SpawnConfig.spawnLimitationMaxMobsPerServer = origServerMax;
    SpawnConfig.spawnLimitationMaxMobsPerChunk = origChunkMax;
    SpawnConfig.friendlyChunkSpawnRate = origFriendlyRate;
    SpawnManager.handleServerAboutToStart();

    GameTestHelpers.assertTrue(
      helper,
      "Virtual zone limit not enforced: "
        + allowed
        + " zombies allowed (expected > 0 and <= "
        + LIMIT
        + ")",
      allowed > 0 && allowed <= LIMIT);
    helper.succeed();
  }

  public static void testStructureSpawnGetsWorldBonus(GameTestHelper helper) {
    final int WORLD_LIMIT = 3;
    final int WORLD_BONUS = 4;
    final int ATTEMPTS = 10;

    boolean wasSpawnEnabled = FeatureToggle.SPAWN.isEnabled();
    int originalWorldMax = SpawnConfig.spawnLimitationMaxMobsPerWorld;
    int originalServerMax = SpawnConfig.spawnLimitationMaxMobsPerServer;
    int originalChunkMax = SpawnConfig.spawnLimitationMaxMobsPerChunk;
    int originalPlayerMax = SpawnConfig.spawnLimitationMaxMobsPerPlayer;
    int originalFriendlyRate = SpawnConfig.friendlyChunkSpawnRate;
    boolean originalSpecialBonusEnabled = SpawnConfig.specialSpawnTypeBonusEnabled;
    Set<String> originalBonusTypes = Set.copyOf(SpawnConfig.specialSpawnBonusTypes);
    int originalSpecialWorldBonus = SpawnConfig.specialSpawnBonusPerWorld;
    int originalSpecialServerBonus = SpawnConfig.specialSpawnBonusPerServer;
    int originalSpecialChunkBonus = SpawnConfig.specialSpawnBonusPerChunk;
    int originalSpecialPlayerBonus = SpawnConfig.specialSpawnBonusPerPlayer;
    ServerLoadLevel originalSpecialMaxLoadLevel = SpawnConfig.specialSpawnBonusMaxLoadLevel;

    FeatureToggle.SPAWN.setEnabled(true);
    SpawnConfig.spawnLimitationMaxMobsPerWorld = WORLD_LIMIT;
    SpawnConfig.spawnLimitationMaxMobsPerServer = -1;
    SpawnConfig.spawnLimitationMaxMobsPerChunk = -1;
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = -1;
    SpawnConfig.friendlyChunkSpawnRate = 0;
    SpawnConfig.specialSpawnTypeBonusEnabled = true;
    SpawnConfig.specialSpawnBonusTypes = Set.of("structure");
    SpawnConfig.specialSpawnBonusPerWorld = WORLD_BONUS;
    SpawnConfig.specialSpawnBonusPerServer = 0;
    SpawnConfig.specialSpawnBonusPerChunk = 0;
    SpawnConfig.specialSpawnBonusPerPlayer = 0;
    SpawnConfig.specialSpawnBonusMaxLoadLevel = ServerLoadLevel.MEDIUM;
    SpawnPresetRegistry.reload(Collections.emptyList());

    SpawnManager.handleServerAboutToStart();
    SpawnManager.handleServerStarted();
    for (int i = 0; i <= 20 * 20; i++) {
      SpawnManager.handleServerTick();
    }

    ServerLevel level = helper.getLevel();
    BlockPos spawnPos = helper.absolutePos(new BlockPos(0, 1, 0));
    List<Zombie> spawnedZombies = new ArrayList<>();
    for (int i = 0; i < ATTEMPTS; i++) {
      Zombie zombie = new Zombie(EntityType.ZOMBIE, level);
      zombie.setPos(spawnPos.getX() + 0.5, spawnPos.getY(), spawnPos.getZ() + 0.5);
      if (!SpawnManager.shouldDenyMobSpawn(zombie, level, MobSpawnType.STRUCTURE)) {
        level.addFreshEntity(zombie);
        spawnedZombies.add(zombie);
      }
    }

    int allowed = spawnedZombies.size();
    spawnedZombies.forEach(z -> z.remove(Entity.RemovalReason.DISCARDED));
    FeatureToggle.SPAWN.setEnabled(wasSpawnEnabled);
    SpawnConfig.spawnLimitationMaxMobsPerWorld = originalWorldMax;
    SpawnConfig.spawnLimitationMaxMobsPerServer = originalServerMax;
    SpawnConfig.spawnLimitationMaxMobsPerChunk = originalChunkMax;
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = originalPlayerMax;
    SpawnConfig.friendlyChunkSpawnRate = originalFriendlyRate;
    SpawnConfig.specialSpawnTypeBonusEnabled = originalSpecialBonusEnabled;
    SpawnConfig.specialSpawnBonusTypes = originalBonusTypes;
    SpawnConfig.specialSpawnBonusPerWorld = originalSpecialWorldBonus;
    SpawnConfig.specialSpawnBonusPerServer = originalSpecialServerBonus;
    SpawnConfig.specialSpawnBonusPerChunk = originalSpecialChunkBonus;
    SpawnConfig.specialSpawnBonusPerPlayer = originalSpecialPlayerBonus;
    SpawnConfig.specialSpawnBonusMaxLoadLevel = originalSpecialMaxLoadLevel;
    ServerLevelLoad.reset();
    SpawnManager.handleServerAboutToStart();

    GameTestHelpers.assertTrue(
      helper,
      "Structure spawn bonus not enforced: " + allowed + " zombies allowed (expected > "
        + WORLD_LIMIT + " and <= " + (WORLD_LIMIT + WORLD_BONUS) + ")",
      allowed > WORLD_LIMIT && allowed <= WORLD_LIMIT + WORLD_BONUS);
    helper.succeed();
  }

  public static void testStructureSpawnBonusDisabledAboveMaxLoad(GameTestHelper helper) {
    final int WORLD_LIMIT = 3;
    final int WORLD_BONUS = 4;
    final int ATTEMPTS = 10;

    boolean wasSpawnEnabled = FeatureToggle.SPAWN.isEnabled();
    int originalWorldMax = SpawnConfig.spawnLimitationMaxMobsPerWorld;
    int originalServerMax = SpawnConfig.spawnLimitationMaxMobsPerServer;
    int originalChunkMax = SpawnConfig.spawnLimitationMaxMobsPerChunk;
    int originalPlayerMax = SpawnConfig.spawnLimitationMaxMobsPerPlayer;
    int originalFriendlyRate = SpawnConfig.friendlyChunkSpawnRate;
    boolean originalSpecialBonusEnabled = SpawnConfig.specialSpawnTypeBonusEnabled;
    Set<String> originalBonusTypes = Set.copyOf(SpawnConfig.specialSpawnBonusTypes);
    int originalSpecialWorldBonus = SpawnConfig.specialSpawnBonusPerWorld;
    int originalSpecialServerBonus = SpawnConfig.specialSpawnBonusPerServer;
    int originalSpecialChunkBonus = SpawnConfig.specialSpawnBonusPerChunk;
    int originalSpecialPlayerBonus = SpawnConfig.specialSpawnBonusPerPlayer;
    ServerLoadLevel originalSpecialMaxLoadLevel = SpawnConfig.specialSpawnBonusMaxLoadLevel;

    FeatureToggle.SPAWN.setEnabled(true);
    SpawnConfig.spawnLimitationMaxMobsPerWorld = WORLD_LIMIT;
    SpawnConfig.spawnLimitationMaxMobsPerServer = -1;
    SpawnConfig.spawnLimitationMaxMobsPerChunk = -1;
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = -1;
    SpawnConfig.friendlyChunkSpawnRate = 0;
    SpawnConfig.specialSpawnTypeBonusEnabled = true;
    SpawnConfig.specialSpawnBonusTypes = Set.of("structure");
    SpawnConfig.specialSpawnBonusPerWorld = WORLD_BONUS;
    SpawnConfig.specialSpawnBonusPerServer = 0;
    SpawnConfig.specialSpawnBonusPerChunk = 0;
    SpawnConfig.specialSpawnBonusPerPlayer = 0;
    SpawnConfig.specialSpawnBonusMaxLoadLevel = ServerLoadLevel.MEDIUM;
    SpawnPresetRegistry.reload(Collections.emptyList());

    SpawnManager.handleServerAboutToStart();
    SpawnManager.handleServerStarted();
    for (int i = 0; i <= 20 * 20; i++) {
      SpawnManager.handleServerTick();
    }

    ServerLevel level = helper.getLevel();
    GameTestHelpers.setMeasuredLevelLoad(level, ServerLoadLevel.HIGH, 100.0);
    BlockPos spawnPos = helper.absolutePos(new BlockPos(0, 1, 0));
    List<Zombie> spawnedZombies = new ArrayList<>();
    for (int i = 0; i < ATTEMPTS; i++) {
      Zombie zombie = new Zombie(EntityType.ZOMBIE, level);
      zombie.setPos(spawnPos.getX() + 0.5, spawnPos.getY(), spawnPos.getZ() + 0.5);
      if (!SpawnManager.shouldDenyMobSpawn(zombie, level, MobSpawnType.STRUCTURE)) {
        level.addFreshEntity(zombie);
        spawnedZombies.add(zombie);
      }
    }

    int allowed = spawnedZombies.size();
    spawnedZombies.forEach(z -> z.remove(Entity.RemovalReason.DISCARDED));
    FeatureToggle.SPAWN.setEnabled(wasSpawnEnabled);
    SpawnConfig.spawnLimitationMaxMobsPerWorld = originalWorldMax;
    SpawnConfig.spawnLimitationMaxMobsPerServer = originalServerMax;
    SpawnConfig.spawnLimitationMaxMobsPerChunk = originalChunkMax;
    SpawnConfig.spawnLimitationMaxMobsPerPlayer = originalPlayerMax;
    SpawnConfig.friendlyChunkSpawnRate = originalFriendlyRate;
    SpawnConfig.specialSpawnTypeBonusEnabled = originalSpecialBonusEnabled;
    SpawnConfig.specialSpawnBonusTypes = originalBonusTypes;
    SpawnConfig.specialSpawnBonusPerWorld = originalSpecialWorldBonus;
    SpawnConfig.specialSpawnBonusPerServer = originalSpecialServerBonus;
    SpawnConfig.specialSpawnBonusPerChunk = originalSpecialChunkBonus;
    SpawnConfig.specialSpawnBonusPerPlayer = originalSpecialPlayerBonus;
    SpawnConfig.specialSpawnBonusMaxLoadLevel = originalSpecialMaxLoadLevel;
    SpawnManager.handleServerAboutToStart();
    ServerLevelLoad.reset();

    GameTestHelpers.assertTrue(
      helper,
      "Structure spawn bonus should be disabled above max load: " + allowed
        + " zombies allowed (expected > 0 and <= " + WORLD_LIMIT + ")",
      allowed > 0 && allowed <= WORLD_LIMIT);
    helper.succeed();
  }
}
