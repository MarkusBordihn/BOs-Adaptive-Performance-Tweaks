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

package de.markusbordihn.adaptiveperformancetweaksitems.entity;

import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.Entity.RemovalReason;
import net.minecraft.world.level.Level;

import net.minecraftforge.event.entity.EntityJoinWorldEvent;
import net.minecraftforge.event.entity.EntityLeaveWorldEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.event.server.ServerAboutToStartEvent;

import de.markusbordihn.adaptiveperformancetweakscore.CoreConstants;
import de.markusbordihn.adaptiveperformancetweaksitems.Constants;
import de.markusbordihn.adaptiveperformancetweaksitems.config.CommonConfig;

@EventBusSubscriber
public class ExperienceOrbManager {

  private static final CommonConfig.Config COMMON = CommonConfig.COMMON;
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static Map<String, Set<ExperienceOrb>> experienceOrbEntityMap = new ConcurrentHashMap<>();
  private static boolean optimizeExperienceOrbs = COMMON.optimizeExperienceOrbs.get();
  private static int experienceOrbsClusterRange = COMMON.experienceOrbsClusterRange.get();

  protected ExperienceOrbManager() {}

  @SubscribeEvent
  public static void handleServerAboutToStartEvent(ServerAboutToStartEvent event) {
    experienceOrbEntityMap = new ConcurrentHashMap<>();
    experienceOrbsClusterRange = COMMON.experienceOrbsClusterRange.get();
    optimizeExperienceOrbs = COMMON.optimizeExperienceOrbs.get();

    if (optimizeExperienceOrbs) {
      log.info("Enable clustering of Experience Orbs with a radius of {} blocks.",
          experienceOrbsClusterRange);

      // Additional checks for conflicting mods.
      if (CoreConstants.CLUMPS_LOADED) {
        log.error(
            "WARNING: Clumps groups XP orbs together into a new single entity, which will conflict with the XP Orb feature of this mod!");
        log.warn(
            "Don't use both optimizations together! Clustering of Experience Orbs will be automatically disabled!");
        optimizeExperienceOrbs = false;
      }
    } else {
      log.info("Disable Experience Orbs clustering ...");
    }
  }

  @SubscribeEvent(priority = EventPriority.HIGH)
  public static void handleExperienceOrbJoinWorldEvent(EntityJoinWorldEvent event) {
    // Ignore client side world.
    Level level = event.getWorld();
    if (level.isClientSide || event.isCanceled()) {
      return;
    }

    // Ignore everything else besides experience orbs.
    Entity entity = event.getEntity();
    if (!(entity instanceof ExperienceOrb)) {
      return;
    }

    // Ignore events which are already canceled
    if (event.isCanceled()) {
      log.debug("Ignore canceled Experience Orb event {}!", event);
      return;
    }

    ExperienceOrb experienceOrbEntity = (ExperienceOrb) entity;

    // Get world name and ignore orb if it has 0 xp.
    String levelName = level.dimension().location().toString();
    if (optimizeExperienceOrbs && experienceOrbEntity.value <= 0) {
      log.debug("Remove Experience Orb {} with {} xp from {}.", experienceOrbEntity,
          experienceOrbEntity.value, levelName);
      experienceOrbEntity.remove(RemovalReason.DISCARDED);
      return;
    } else {
      log.debug("Experience Orb {} with {} xp joined {}.", experienceOrbEntity,
          experienceOrbEntity.value, levelName);
    }

    // Check if orb should be merged with existing orbs and ignore orb if it has 0 xp.
    experienceOrbEntityMap.computeIfAbsent(levelName, k -> new LinkedHashSet<>());
    Set<ExperienceOrb> experienceOrbWorldEntities = experienceOrbEntityMap.get(levelName);
    if (optimizeExperienceOrbs && !experienceOrbWorldEntities.isEmpty()) {
      Set<ExperienceOrb> experienceOrbsEntities = new HashSet<>(experienceOrbWorldEntities);
      Iterator<ExperienceOrb> experienceOrbsEntitiesIterator = experienceOrbsEntities.iterator();
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
        ExperienceOrb existingExperienceOrb = experienceOrbsEntitiesIterator.next();
        int xSub = (int) existingExperienceOrb.getX();
        int ySub = (int) existingExperienceOrb.getY();
        int zSub = (int) existingExperienceOrb.getZ();
        if (experienceOrbEntity.getId() != existingExperienceOrb.getId()
            && existingExperienceOrb.isAlive() && (xStart < xSub && xSub < xEnd)
            && (yStart < ySub && ySub < yEnd) && (zStart < zSub && zSub < zEnd)) {
          int newExperienceValue = existingExperienceOrb.value + experienceOrbEntity.value;
          log.debug("Merge experience orb {} with {} and {} xp.", experienceOrbEntity,
              existingExperienceOrb, newExperienceValue);
          existingExperienceOrb.value = newExperienceValue;
          experienceOrbEntity.value = 0;
          experienceOrbEntity.moveTo(existingExperienceOrb.getX(), existingExperienceOrb.getY(),
              existingExperienceOrb.getZ());
          experienceOrbEntity.remove(RemovalReason.DISCARDED);

          // Chancel event to remove entity
          event.setCanceled(true);
          return;
        }
      }
    }

    // Storing experience orbs per world regardless of value
    experienceOrbWorldEntities.add(experienceOrbEntity);
  }

  @SubscribeEvent
  public static void handleExperienceOrbLeaveWorldEvent(EntityLeaveWorldEvent event) {

    // Ignore client side world.
    Level level = event.getWorld();
    if (level.isClientSide) {
      return;
    }

    // Ignore everything else besides experience orbs
    Entity entity = event.getEntity();
    if (!(entity instanceof ExperienceOrb)) {
      return;
    }
    ExperienceOrb experienceOrbEntity = (ExperienceOrb) entity;

    // Get level name and start processing of data
    String levelName = level.dimension().location().toString();
    log.debug("Experience Orb {} with {} xp left {}.", experienceOrbEntity,
        experienceOrbEntity.value, levelName);

    // Remove item from level type map.
    Set<ExperienceOrb> experienceOrbWorldEntities = experienceOrbEntityMap.get(levelName);
    if (experienceOrbWorldEntities != null) {
      experienceOrbWorldEntities.remove(experienceOrbEntity);
    }
  }

}
