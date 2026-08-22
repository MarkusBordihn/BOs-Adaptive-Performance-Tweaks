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

package de.markusbordihn.adaptiveperformancetweaks.feature.spawn;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import de.markusbordihn.adaptiveperformancetweaks.core.feature.FeatureToggle;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Map;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.Identifier;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EntityTypes;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.phys.Vec3;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SpawnManagerTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

  @SuppressWarnings("unchecked")
  private static <T> T readStaticField(String fieldName) throws Exception {
    Field field = SpawnManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    return (T) field.get(null);
  }

  private static void writeStaticField(String fieldName, Object value) throws Exception {
    Field field = SpawnManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static Object newNestedRecord(
    String simpleName, Class<?>[] parameterTypes, Object... args) throws Exception {
    Class<?> keyClass = Class.forName(
      "de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnManager$" + simpleName);
    Constructor<?> constructor = keyClass.getDeclaredConstructor(parameterTypes);
    constructor.setAccessible(true);
    return constructor.newInstance(args);
  }

  private static Object newChunkCacheKey(Identifier dimensionId, int chunkX, int chunkZ,
    EntityType<?> entityType) throws Exception {
    return newNestedRecord("ChunkCacheKey",
      new Class<?>[]{Identifier.class, int.class, int.class, EntityType.class},
      dimensionId, chunkX, chunkZ, entityType);
  }

  private static Object newNearPlayerCacheKey(Identifier dimensionId, int chunkX, int chunkZ,
    EntityType<?> entityType) throws Exception {
    return newNestedRecord("NearPlayerCacheKey",
      new Class<?>[]{Identifier.class, int.class, int.class, EntityType.class},
      dimensionId, chunkX, chunkZ, entityType);
  }

  private static Object newWorldCacheKey(Identifier dimensionId, EntityType<?> entityType)
    throws Exception {
    return newNestedRecord("WorldCacheKey",
      new Class<?>[]{Identifier.class, EntityType.class},
      dimensionId, entityType);
  }

  private static Object newAnchorCacheKey(Identifier dimensionId, int chunkX, int chunkZ)
    throws Exception {
    return newNestedRecord("AnchorCacheKey",
      new Class<?>[]{Identifier.class, int.class, int.class},
      dimensionId, chunkX, chunkZ);
  }

  private static int invokeCountMethod(String methodName, Class<?>[] parameterTypes, Object... args)
    throws Exception {
    Method method = SpawnManager.class.getDeclaredMethod(methodName, parameterTypes);
    method.setAccessible(true);
    return (int) method.invoke(null, args);
  }

  @BeforeEach
  void resetCaches() {
    SpawnManager.handleServerAboutToStart();
  }

  @Test
  void countInChunkUsesTypedCacheKey() throws Exception {
    Identifier dimensionId = Identifier.tryParse("minecraft:overworld");
    BlockPos pos = BlockPos.ZERO;
    Object cacheKey = newChunkCacheKey(dimensionId, 0, 0, EntityTypes.ZOMBIE);
    Map<Object, Integer> tickCache = readStaticField("tickChunkEntityCountCache");
    Map<Object, Integer> deltaCache = readStaticField("chunkCountDelta");
    tickCache.put(cacheKey, 4);
    deltaCache.put(cacheKey, 2);

    int result = invokeCountMethod("countInChunk",
      new Class<?>[]{EntityType.class, BlockPos.class, ServerLevel.class, Identifier.class},
      EntityTypes.ZOMBIE, pos, mock(ServerLevel.class), dimensionId);

    assertEquals(6, result);
  }

  @Test
  void countNearPlayerUsesTypedCacheKey() throws Exception {
    Identifier dimensionId = Identifier.tryParse("minecraft:overworld");
    Vec3 spawnPos = new Vec3(8.0, 64.0, 8.0);
    Object anchorKey = newAnchorCacheKey(dimensionId, 0, 0);
    Map<Object, Vec3> anchorCache = readStaticField("playerAnchorCache");
    anchorCache.put(anchorKey, spawnPos);

    Object cacheKey = newNearPlayerCacheKey(dimensionId, 0, 0, EntityTypes.ZOMBIE);
    Map<Object, Integer> tickCache = readStaticField("tickNearPlayerEntityCountCache");
    Map<Object, Integer> deltaCache = readStaticField("nearPlayerCountDelta");
    tickCache.put(cacheKey, 3);
    deltaCache.put(cacheKey, 1);

    int result = invokeCountMethod("countNearPlayer",
      new Class<?>[]{EntityType.class, Vec3.class, ServerLevel.class, Identifier.class},
      EntityTypes.ZOMBIE, spawnPos, mock(ServerLevel.class), dimensionId);

    assertEquals(4, result);
  }

  @Test
  void countInWorldUsesTypedCacheKey() throws Exception {
    Identifier dimensionId = Identifier.tryParse("minecraft:overworld");
    Object cacheKey = newWorldCacheKey(dimensionId, EntityTypes.ZOMBIE);
    Map<Object, Integer> tickCache = readStaticField("tickWorldEntityCountCache");
    Map<Identifier, Map<EntityType<?>, Integer>> deltaCache = readStaticField(
      "worldCountDelta");
    tickCache.put(cacheKey, 5);
    deltaCache.put(dimensionId, Map.of(EntityTypes.ZOMBIE, 2));

    int result = invokeCountMethod("countInWorld",
      new Class<?>[]{EntityType.class, ServerLevel.class, Identifier.class},
      EntityTypes.ZOMBIE, mock(ServerLevel.class), dimensionId);

    assertEquals(7, result);
  }

  @Test
  @DisplayName("Natural spawn denial is skipped while the spawn feature is disabled")
  void naturalSpawnDenialRespectsFeatureToggle() throws Exception {
    boolean wasEnabled = FeatureToggle.SPAWN.isEnabled();
    boolean wasPrioritizeByTimeOfDay = SpawnConfig.naturalSpawnPrioritizeByTimeOfDay;
    double previousPassRate = SpawnConfig.naturalSpawnPassRateNormal;
    SpawnConfig.naturalSpawnLimitationEnabled = true;
    SpawnConfig.naturalSpawnPrioritizeByTimeOfDay = false;
    SpawnConfig.naturalSpawnPassRateNormal = 0.0;
    writeStaticField("serverStartedDelay", true);
    ServerLevel level = mock(ServerLevel.class);

    try {
      FeatureToggle.SPAWN.setEnabled(true);
      assertTrue(
        SpawnManager.shouldDenyNaturalSpawn(MobCategory.MONSTER, level, BlockPos.ZERO));

      FeatureToggle.SPAWN.setEnabled(false);
      assertFalse(
        SpawnManager.shouldDenyNaturalSpawn(MobCategory.MONSTER, level, BlockPos.ZERO));
    } finally {
      FeatureToggle.SPAWN.setEnabled(wasEnabled);
      SpawnConfig.naturalSpawnPrioritizeByTimeOfDay = wasPrioritizeByTimeOfDay;
      SpawnConfig.naturalSpawnPassRateNormal = previousPassRate;
    }
  }
}
