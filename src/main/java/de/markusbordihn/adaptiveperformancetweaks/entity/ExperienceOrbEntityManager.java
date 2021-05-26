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

package de.markusbordihn.adaptiveperformancetweaks.entity;

import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.minecraft.entity.Entity;
import net.minecraft.entity.item.ExperienceOrbEntity;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.event.server.FMLServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweaks.Constants;
import de.markusbordihn.adaptiveperformancetweaks.Manager;

@EventBusSubscriber
public class ExperienceOrbEntityManager extends Manager {

  private static Map<String, Set<ExperienceOrbEntity>> experienceOrbEntityMap =
      new ConcurrentHashMap<>();
  private static boolean optimizeExperienceOrbs = COMMON.optimizeExperienceOrbs.get();
  private static int experienceOrbsClusterRange = COMMON.experienceOrbsClusterRange.get();

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(FMLServerAboutToStartEvent event) {
    optimizeExperienceOrbs = COMMON.optimizeExperienceOrbs.get();
    experienceOrbsClusterRange = COMMON.experienceOrbsClusterRange.get();
    if (optimizeExperienceOrbs) {
      log.info("Enable clustering of Experience Orbs with a radius of {} blocks.", experienceOrbsClusterRange);
      if (ModList.get().isLoaded(Constants.CLUMPS_MOD)) {
        log.error(
            "Clumps groups XP orbs together into a new single entity, which will conflict with the XP Orb feature of this mod. Don't use both optimizations together!");
      }
    } else {
      log.info("Disable Experience Orbs clustering ...");
    }
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleExperienceOrbEntityJoinWorldEvent(EntityJoinWorldEvent event) {
    // Ignore client side world.
    World world = event.getWorld();
    if (world.isClientSide) {
      return;
    }

    // Ignore everything else besides experience orbs
    Entity entity = event.getEntity();
    if (!(entity instanceof ExperienceOrbEntity)) {
      return;
    }
    ExperienceOrbEntity experienceOrbEntity = (ExperienceOrbEntity) entity;

    // Get world name and ignore orb if it has 0 xp.
    String worldName = world.dimension().location().toString();
    if (optimizeExperienceOrbs && experienceOrbEntity.value <= 0) {
      log.debug("Remove Experience Orb {} with {} xp from {}.", experienceOrbEntity,
          experienceOrbEntity.value, worldName);
      experienceOrbEntity.remove();
      return;
    } else {
      log.debug("Experience Orb {} with {} xp joined {}.", experienceOrbEntity,
          experienceOrbEntity.value, worldName);
    }

    // Check if orb should be merged with existing orbs and ignore orb if it has 0 xp.
    experienceOrbEntityMap.computeIfAbsent(worldName, k -> new LinkedHashSet<>());
    Set<ExperienceOrbEntity> experienceOrbWorldEntities = experienceOrbEntityMap.get(worldName);
    if (optimizeExperienceOrbs && !experienceOrbWorldEntities.isEmpty()) {
      Set<ExperienceOrbEntity> experienceOrbsEntities = new HashSet<>(experienceOrbWorldEntities);
      Iterator<ExperienceOrbEntity> experienceOrbsEntitiesIterator =
          experienceOrbsEntities.iterator();
      int x = (int) experienceOrbEntity.getX();
      int y = (int) experienceOrbEntity.getY();
      int z = (int) experienceOrbEntity.getZ();
      int xStart = x - experienceOrbsClusterRange;
      int yStart = y - experienceOrbsClusterRange;
      int zStart = z - experienceOrbsClusterRange;
      int xEnd = x + experienceOrbsClusterRange;
      int yEnd = y + experienceOrbsClusterRange;
      int zEnd = z + experienceOrbsClusterRange;
      while (experienceOrbsEntitiesIterator.hasNext()) {
        ExperienceOrbEntity existingExperienceOrbEntity = experienceOrbsEntitiesIterator.next();
        int xSub = (int) existingExperienceOrbEntity.getX();
        int ySub = (int) existingExperienceOrbEntity.getY();
        int zSub = (int) existingExperienceOrbEntity.getZ();
        if (experienceOrbEntity.getId() != existingExperienceOrbEntity.getId()
            && existingExperienceOrbEntity.isAlive() && (xStart < xSub && xSub < xEnd)
            && (yStart < ySub && ySub < yEnd) && (zStart < zSub && zSub < zEnd)) {
          int newExperienceValue = existingExperienceOrbEntity.value + experienceOrbEntity.value;
          log.debug("Merge experience orb {} with {} and {} xp.", experienceOrbEntity,
              existingExperienceOrbEntity, newExperienceValue);
          existingExperienceOrbEntity.value = newExperienceValue;
          experienceOrbEntity.value = 0;
          experienceOrbEntity.moveTo(existingExperienceOrbEntity.getX(),
              existingExperienceOrbEntity.getY(), existingExperienceOrbEntity.getZ());
          experienceOrbEntity.remove();
          return;
        }
      }
    }

    // Storing experience orbs per world regardless of value
    experienceOrbWorldEntities.add(experienceOrbEntity);
  }

  @SubscribeEvent
  public static void handleExperienceOrbEntityLeaveWorldEvent(EntityLeaveWorldEvent event) {

    // Ignore client side world.
    World world = event.getWorld();
    if (world.isClientSide) {
      return;
    }

    // Ignore everything else besides experience orbs
    Entity entity = event.getEntity();
    if (!(entity instanceof ExperienceOrbEntity)) {
      return;
    }
    ExperienceOrbEntity experienceOrbEntity = (ExperienceOrbEntity) entity;

    // Get world name and start processing of data
    String worldName = world.dimension().location().toString();
    log.debug("Experience Orb {} with {} xp left {}.", experienceOrbEntity,
        experienceOrbEntity.value, worldName);

    // Remove item from world type map.
    Set<ExperienceOrbEntity> experienceOrbWorldEntities = experienceOrbEntityMap.get(worldName);
    if (experienceOrbWorldEntities != null) {
      experienceOrbWorldEntities.remove(experienceOrbEntity);
    }
  }

}
