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

import java.util.HashSet;
import java.util.Set;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.MobSpawnerTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.spawner.AbstractSpawner;
import net.minecraftforge.event.entity.living.LivingSpawnEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;

import de.markusbordihn.adaptiveperformancetweaks.Manager;

@EventBusSubscriber
public class SpawnerManager extends Manager {

  private static Set<MobSpawnerTileEntity> spawnerList = new HashSet<>();

  @SubscribeEvent
  public static void handleLivingSpecialSpawnEvent(LivingSpawnEvent.SpecialSpawn event) {
    AbstractSpawner spawner = event.getSpawner();
    if (spawner == null) {
      return;
    }
    TileEntity tileEntity = event.getWorld().getTileEntity(spawner.getSpawnerPosition());
    if (tileEntity != null) {
      addSpawner((MobSpawnerTileEntity) tileEntity);
    }
  }

  public static void addSpawner(MobSpawnerTileEntity mobSpawnerTileEntity) {
    if (spawnerList.contains(mobSpawnerTileEntity)) {
      return;
    }
    String worldName = mobSpawnerTileEntity.getWorld().getDimensionKey().getLocation().toString();
    CompoundNBT spawnerData = mobSpawnerTileEntity.serializeNBT();
    String spawnEntityId = spawnerData.getCompound("SpawnData").getString("id");
    log.debug("[Spawner] {} at {} in {} with {}", spawnEntityId, mobSpawnerTileEntity.getPos(),
        worldName);
    spawnerList.add(mobSpawnerTileEntity);
  }

  public static void removeSpawner(MobSpawnerTileEntity mobSpawnerTileEntity) {
    spawnerList.remove(mobSpawnerTileEntity);
  }

  public static Set<MobSpawnerTileEntity> getSpawnerList() {
    return spawnerList;
  }

}
