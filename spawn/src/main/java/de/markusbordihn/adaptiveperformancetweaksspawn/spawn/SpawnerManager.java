/**
 * Copyright 2022 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.adaptiveperformancetweaksspawn.spawn;

import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.block.entity.BlockEntity;

import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.living.LivingSpawnEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweaksspawn.Constants;

@EventBusSubscriber
public class SpawnerManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static short ticks = 0;
  private static final short VERIFICATION_TICK = 60 * 20;

  static Set<BaseSpawner> spawnerList = ConcurrentHashMap.newKeySet();

  protected SpawnerManager() {}

  @SubscribeEvent(priority = EventPriority.NORMAL)
  public static void handleLivingCheckSpawnEvent(LivingSpawnEvent.CheckSpawn event) {
    handleSpawnEvent(event);
  }

  @SubscribeEvent
  public static void handleClientServerTickEvent(TickEvent.ServerTickEvent event) {
    if (event.phase == TickEvent.Phase.END && ticks++ >= VERIFICATION_TICK) {
      verifyEntities();
      ticks = 0;
    }
  }

  private static void handleSpawnEvent(LivingSpawnEvent event) {

    // Ignore client side.
    LevelAccessor level = event.getWorld();
    if (level.isClientSide()) {
      return;
    }

    // Ignore null entities and specific entities.
    Entity entity = event.getEntity();
    if (entity == null || entity instanceof Projectile) {
      return;
    }

    if (event instanceof LivingSpawnEvent.CheckSpawn checkSpawn
        && checkSpawn.getSpawner() != null) {

      // Ignore events which are already canceled or denied.
      if (event.isCanceled() || event.getResult() == Event.Result.DENY) {
        return;
      }

      BaseSpawner spawner = checkSpawn.getSpawner();
      addSpawner(spawner);
    }

  }

  public static void addSpawner(BaseSpawner spawner) {
    // Ignore already reported spawner
    if (spawnerList.contains(spawner)) {
      return;
    }
    spawnerList.add(spawner);

    // Only do expensive lookup for debugging.
    if (log.isDebugEnabled()) {
      BlockEntity blockEntity = spawner.getSpawnerBlockEntity();
      if (blockEntity != null) {
        BlockPos blockPos = blockEntity.getBlockPos();
        Level level = blockEntity.getLevel();
        String levelName = level != null ? level.dimension().location().toString() : "";
        CompoundTag spawnerData = blockEntity.serializeNBT();
        String spawnerId = spawnerData.getString("id");
        String spawnEntityId =
            spawnerData.getCompound("SpawnData").getCompound("entity").getString("id");
        log.debug("[Spawner] Found {}({}) at {} in {}", spawnerId, spawnEntityId, blockPos,
            levelName);
      }
    }
  }

  public static Set<BaseSpawner> getSpawnerList() {
    return spawnerList;
  }

  public static void verifyEntities() {
    int removedEntries = 0;

    // Verify spawner entries
    Iterator<BaseSpawner> spawnerIterator = spawnerList.iterator();
    while (spawnerIterator.hasNext()) {
      BaseSpawner spawner = spawnerIterator.next();
      BlockEntity spawnerBlockEntity = spawner != null ? spawner.getSpawnerBlockEntity() : null;
      if (spawner != null && spawnerBlockEntity != null && spawnerBlockEntity.isRemoved()) {
        spawnerIterator.remove();
        removedEntries++;
      }
    }

    if (removedEntries > 0) {
      log.debug("Removed {} entries during the verification", removedEntries);
    }
  }

}
