/**
 * Copyright 2021 Markus Bordihn
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

package de.markusbordihn.adaptiveperformancetweaks.spawn;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.logging.log4j.Logger;

import net.minecraft.block.Block;
import net.minecraft.block.Blocks;
import net.minecraft.entity.Entity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.MobSpawnerTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;
import net.minecraft.world.spawner.AbstractSpawner;
import net.minecraftforge.event.entity.living.LivingSpawnEvent;
import net.minecraftforge.event.world.BlockEvent;
import net.minecraftforge.eventbus.api.Event;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Manager;
import de.markusbordihn.adaptiveperformancetweaks.config.CommonConfig;
import de.markusbordihn.adaptiveperformancetweaks.entity.EntityManager;
import de.markusbordihn.adaptiveperformancetweaks.server.ServerWorldLoadEvent;

@EventBusSubscriber
public class SpawnerManager extends Manager {

  private static Map<String, Double> serverWorldLoadFactorMap = new HashMap<>();
  private static Set<MobSpawnerTileEntity> spawnerList = new HashSet<>();
  private static boolean spawnerEnabled = CommonConfig.COMMON.spawnerEnabled.get();
  private static int spawnerMaxEntityPerChunk = CommonConfig.COMMON.spawnerMaxEntityPerChunk.get();
  private static int spawnerMaxEntityPerWorld = CommonConfig.COMMON.spawnerMaxEntityPerWorld.get();
  private static final Logger log = getLogger(SpawnerManager.class.getSimpleName());

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    serverWorldLoadFactorMap = new HashMap<>();
    spawnerList = new HashSet<>();
    spawnerEnabled = CommonConfig.COMMON.spawnerEnabled.get();
    spawnerMaxEntityPerWorld = CommonConfig.COMMON.spawnerMaxEntityPerWorld.get();
    spawnerMaxEntityPerChunk = CommonConfig.COMMON.spawnerMaxEntityPerChunk.get();
    if (spawnerEnabled) {
      log.info("Optimize Spawner with a limit of {} per world and {} per chunk.",
          spawnerMaxEntityPerWorld, spawnerMaxEntityPerChunk);
    }
  }

  @SubscribeEvent
  public static void handleServerWorldLoadEvent(ServerWorldLoadEvent event) {
    if (event.hasChanged()) {
      String worldName = event.getServerWorldName();
      serverWorldLoadFactorMap.put(worldName, event.getServerWorldLoadLevelFactor());
    }
  }

  @SubscribeEvent
  public static void handleBlockPlaceEvent(final BlockEvent.EntityPlaceEvent event) {
    Block block = event.getState().getBlock();
    if (block != Blocks.SPAWNER) {
      return;
    }
    TileEntity tileEntity = event.getWorld().getBlockEntity(event.getPos());
    if (tileEntity != null) {
      addSpawner((MobSpawnerTileEntity) tileEntity);
    }
  }

  @SubscribeEvent
  public static void handleBlockBreakEvent(final BlockEvent.BreakEvent event) {
    Block block = event.getState().getBlock();
    if (block != Blocks.SPAWNER) {
      return;
    }
    TileEntity tileEntity = event.getWorld().getBlockEntity(event.getPos());
    if (tileEntity != null) {
      removeSpawner((MobSpawnerTileEntity) tileEntity);
    }
  }

  @SubscribeEvent
  public static void handleLivingSpecialSpawnEvent(LivingSpawnEvent.CheckSpawn event) {
    AbstractSpawner spawner = event.getSpawner();
    if (spawner == null) {
      return;
    }

    // Get spawner location and added to the spawner list
    TileEntity tileEntity = event.getWorld().getBlockEntity(spawner.getPos());
    if (tileEntity != null) {
      addSpawner((MobSpawnerTileEntity) tileEntity);
    }

    // Skip the following if spawner optimization is not enabled
    if (!spawnerEnabled) {
      return;
    }

    // Define basic data
    Entity entity = event.getEntity();
    World world = spawner.getLevel();
    String worldName = world.dimension().location().toString();

    // Get current server world load factor to limited spawn if world is overloaded.
    double serverWorldLoadFactor = serverWorldLoadFactorMap.getOrDefault(worldName, 1.0);

    // Check if we are reaching world limit of entities
    int entitiesPerWorld = EntityManager.getNumberOfEntitiesPerWorld(worldName);
    if (entitiesPerWorld * serverWorldLoadFactor >= spawnerMaxEntityPerWorld) {
      log.debug("[Spawner World Limit] Denied spawn event for {} from {} in {}!", entity, spawner,
          worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    // Check if we are reaching chunk limit of entities
    int entitiesInChunk =
        EntityManager.getNumberOfEntitiesInChunkPosition(worldName, entity.position());
    if (entitiesInChunk * serverWorldLoadFactor >= spawnerMaxEntityPerChunk) {
      log.debug("[Spawner Chunk Limit] Denied spawn event for {} from {} in {}!", entity, spawner,
          worldName);
      event.setResult(Event.Result.DENY);
      return;
    }

    log.debug("[Allow Spawner Spawn] For {} from {} in {}.", entity, spawner, worldName);
    event.setResult(Event.Result.DEFAULT);
  }

  public static void addSpawner(AbstractSpawner spawner) {
    TileEntity tileEntity = spawner.getLevel().getBlockEntity(spawner.getPos());
    addSpawner((MobSpawnerTileEntity) tileEntity);
  }

  public static void addSpawner(MobSpawnerTileEntity mobSpawnerTileEntity) {
    if (spawnerList.contains(mobSpawnerTileEntity)) {
      return;
    }
    String worldName = mobSpawnerTileEntity.getLevel().dimension().location().toString();
    CompoundNBT spawnerData = mobSpawnerTileEntity.serializeNBT();
    String spawnEntityId = spawnerData.getCompound("SpawnData").getString("id");
    log.debug("[Spawner] {} at {} in {} with {}", spawnEntityId, mobSpawnerTileEntity.getBlockPos(),
        worldName);
    spawnerList.add(mobSpawnerTileEntity);
  }

  public static void removeSpawner(AbstractSpawner spawner) {
    TileEntity tileEntity = spawner.getLevel().getBlockEntity(spawner.getPos());
    removeSpawner((MobSpawnerTileEntity) tileEntity);
  }

  public static void removeSpawner(MobSpawnerTileEntity mobSpawnerTileEntity) {
    spawnerList.remove(mobSpawnerTileEntity);
  }

  public static Set<MobSpawnerTileEntity> getSpawnerList() {
    return spawnerList;
  }

}
