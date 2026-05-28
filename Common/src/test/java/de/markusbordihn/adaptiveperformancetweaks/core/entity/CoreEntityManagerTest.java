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

package de.markusbordihn.adaptiveperformancetweaks.core.entity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;
import de.markusbordihn.adaptiveperformancetweaks.feature.spawn.SpawnPreset;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.Entity.RemovalReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.monster.Skeleton;
import net.minecraft.world.entity.monster.Zombie;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.MockMakers;

class CoreEntityManagerTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
    Bootstrap.validate();
  }

  private static void setExcludedNamespacesViaReflection(Set<String> value) throws Exception {
    Field field = CoreEntityManager.class.getDeclaredField("excludedModNamespaces");
    field.setAccessible(true);
    field.set(null, value);
  }

  @SuppressWarnings("unchecked")
  private static <T> T readStaticField(String fieldName) throws Exception {
    Field field = CoreEntityManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    return (T) field.get(null);
  }

  private static void writeStaticField(String fieldName, Object value) throws Exception {
    Field field = CoreEntityManager.class.getDeclaredField(fieldName);
    field.setAccessible(true);
    field.set(null, value);
  }

  private static Object newEntityTrackingKey(String levelName, String entityName) throws Exception {
    Class<?> keyClass = Class.forName(
      "de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager$EntityTrackingKey");
    Constructor<?> constructor = keyClass.getDeclaredConstructor(String.class, String.class);
    constructor.setAccessible(true);
    return constructor.newInstance(levelName, entityName);
  }

  private static Object newChunkTrackingKey(String levelName, int chunkX, int chunkZ)
    throws Exception {
    Class<?> keyClass = Class.forName(
      "de.markusbordihn.adaptiveperformancetweaks.core.entity.CoreEntityManager$ChunkTrackingKey");
    Constructor<?> constructor =
      keyClass.getDeclaredConstructor(String.class, int.class, int.class);
    constructor.setAccessible(true);
    return constructor.newInstance(levelName, chunkX, chunkZ);
  }

  private static Entity mockEntity(boolean removed) {
    return mockEntity(EntityType.ZOMBIE, removed);
  }

  private static Entity mockEntity(EntityType<?> entityType, boolean removed) {
    ServerLevel level = mock(ServerLevel.class, withSettings().mockMaker(MockMakers.SUBCLASS));
    Entity entity;
    if (entityType == EntityType.ZOMBIE) {
      entity = new Zombie(EntityType.ZOMBIE, level);
    } else if (entityType == EntityType.SKELETON) {
      entity = new Skeleton(EntityType.SKELETON, level);
    } else {
      throw new IllegalArgumentException("Unsupported test entity type: " + entityType);
    }

    if (removed) {
      entity.remove(RemovalReason.DISCARDED);
    }

    return entity;
  }

  private static Set<Entity> newEntitySet(Entity... entities) {
    Set<Entity> entitySet = ConcurrentHashMap.newKeySet();
    Collections.addAll(entitySet, entities);
    return entitySet;
  }

  @BeforeEach
  void resetExcludedNamespaces() throws Exception {
    setExcludedNamespacesViaReflection(Collections.emptySet());
    CoreEntityManager.reset();
  }

  @AfterEach
  void cleanupExcludedNamespaces() throws Exception {
    setExcludedNamespacesViaReflection(Collections.emptySet());
    CoreEntityManager.reset();
  }

  @Test
  void nullEntityIdNotExcluded() {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create"));
    assertFalse(CoreEntityManager.isExcludedModNamespace(null));
  }

  @Test
  void emptyExclusionSetNeverExcludes() {
    assertFalse(CoreEntityManager.isExcludedModNamespace("create:contraption"));
  }

  @Test
  void entityIdWithoutColonNotExcluded() {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create"));
    assertFalse(CoreEntityManager.isExcludedModNamespace("contraption"));
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "create:contraption",
    "create:seat",
    "create:super_glue",
    "botania:spark",
    "botania:mana_burst",
    "mana-and-artifice:residual_magic",
    "appliedenergistics2:meteor",
    "mekanism:robit",
    "minecolonies:citizen",
    "immersive_aircraft:biplane",
    "immersiveengineering:tesla_coil",
    "industrialforegoing:pink_slime",
    "fluxnetworks:flux_point",
    "guardvillagers:archer",
    "xnet:connector"
  })
  void excludedNamespaceEntityIsExcluded(String entityId) {
    String namespace = entityId.substring(0, entityId.indexOf(':'));
    CoreEntityManager.setExcludedModNamespaces(Set.of(namespace));
    assertTrue(CoreEntityManager.isExcludedModNamespace(entityId));
  }

  @ParameterizedTest
  @ValueSource(
    strings = {
      "minecraft:zombie",
      "minecraft:skeleton",
      "minecraft:creeper",
      "alexsmobs:hammerhead_shark",
      "exoticbirds:parrot",
      "friendsandfoes:frog"
    })
  void nonExcludedNamespaceNotExcluded(String entityId) {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create", "botania", "mekanism"));
    assertFalse(CoreEntityManager.isExcludedModNamespace(entityId));
  }

  @Test
  void setExcludedModNamespacesIsImmutable() {
    HashSet<String> mutable = new HashSet<>(Set.of("create"));
    CoreEntityManager.setExcludedModNamespaces(mutable);
    mutable.add("botania");
    assertFalse(
      CoreEntityManager.isExcludedModNamespace("botania:spark"),
      "External modification of source set should not affect internal state");
  }

  @Test
  void multipleNamespacesAllExcluded() {
    CoreEntityManager.setExcludedModNamespaces(
      Set.of("create", "botania", "mana-and-artifice", "minecolonies",
        "appliedenergistics2", "mekanism", "industrialforegoing",
        "immersive_aircraft", "immersiveengineering", "fluxnetworks", "guardvillagers",
        "human_companions", "lootr", "biggerreactors", "modularrouters",
        "pipez", "pokecube_aio", "refinedstorage", "storagedrawers",
        "ultimate_car", "viescraft_machines", "weather2", "xnet",
        "corpse", "easy_npc"));

    assertTrue(CoreEntityManager.isExcludedModNamespace("create:contraption"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("botania:mana_burst"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("mana-and-artifice:residual_magic"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("minecolonies:citizen"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("appliedenergistics2:tiny_tnt"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("mekanism:robit"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("immersive_aircraft:biplane"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("industrialforegoing:pink_slime"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("fluxnetworks:flux_point"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("guardvillagers:archer"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("xnet:connector"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("corpse:human_corpse"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("easy_npc:npc"));
    assertFalse(CoreEntityManager.isExcludedModNamespace("minecraft:zombie"));
    assertFalse(CoreEntityManager.isExcludedModNamespace("mekanismadditions:baby_creeper"));
  }

  @Test
  void nullEntityReturnsFalse() {
    assertFalse(CoreEntityManager.isRelevantEntity(null));
  }

  @Test
  void protectNamespaceRuleKeepsManagedLivingEntitiesOutOfTracking() {
    SpawnPreset preset = new SpawnPreset(
      false,
      "testnpc",
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of(), Set.of(), 1, 1, 1, 1),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.PROTECT_NAMESPACE,
      TrackingCategory.MANAGED_LIVING,
      "Managed living test namespace",
      Set.of());
    CoreEntityManager.reloadTrackingRules(List.of(preset));

    Zombie zombie = new Zombie(EntityType.ZOMBIE, mock(ServerLevel.class));
    assertFalse(CoreEntityManager.isRelevantEntity(zombie, "testnpc:guard"));
    assertFalse(CoreEntityManager.isRelevantEntity(zombie, "testnpc:guard"),
      "Second call should use the cached namespace decision");
  }

  @Test
  void excludeNamespaceRuleTreatsAeronauticsEntitiesAsTechnical() {
    SpawnPreset preset = new SpawnPreset(
      false,
      "aeronautics",
      List.of(),
      100,
      new SpawnPreset.DimensionFilter(List.of(), List.of(), List.of()),
      new SpawnPreset.EntityLimits(Set.of(), Set.of(), 1, 1, 1, 1),
      SpawnPreset.LoadFactors.defaults(),
      TrackingMode.EXCLUDE_NAMESPACE,
      TrackingCategory.VEHICLE_STRUCTURE,
      "Aeronautics vehicle entities should not be tracked.",
      Set.of());
    CoreEntityManager.reloadTrackingRules(List.of(preset));

    Zombie zombie = new Zombie(EntityType.ZOMBIE, mock(ServerLevel.class));
    assertFalse(CoreEntityManager.isRelevantEntity(zombie, "aeronautics:airship_assembler"));
  }

  @Test
  void replacingExclusionSetTakesPrecedence() {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create"));
    assertTrue(CoreEntityManager.isExcludedModNamespace("create:contraption"));

    CoreEntityManager.setExcludedModNamespaces(Set.of("botania"));
    assertFalse(
      CoreEntityManager.isExcludedModNamespace("create:contraption"),
      "Old exclusion set should be fully replaced");
    assertTrue(CoreEntityManager.isExcludedModNamespace("botania:spark"));
  }

  @Test
  void emptyReplacementClearsAllExclusions() {
    CoreEntityManager.setExcludedModNamespaces(Set.of("create", "botania"));
    CoreEntityManager.setExcludedModNamespaces(Collections.emptySet());
    assertFalse(CoreEntityManager.isExcludedModNamespace("create:contraption"));
    assertFalse(CoreEntityManager.isExcludedModNamespace("botania:spark"));
  }

  @Test
  void verifyEntitiesRemovesDiscardedEntityReferencesFromAllTrackingMaps() throws Exception {
    Entity activeEntity = mockEntity(false);
    Entity removedEntity = mockEntity(true);
    String levelName = "minecraft:overworld";
    String entityName = "minecraft:zombie";
    Object entityMapKey = newEntityTrackingKey(levelName, entityName);
    Object activeChunkKey = newChunkTrackingKey(levelName, 0, 0);
    Object staleChunkKey = newChunkTrackingKey(levelName, 1, 1);

    ConcurrentHashMap<Object, Set<Entity>> entityMap = new ConcurrentHashMap<>();
    entityMap.put(entityMapKey, newEntitySet(activeEntity, removedEntity));

    ConcurrentHashMap<Object, Set<Entity>> entityMapPerChunk = new ConcurrentHashMap<>();
    entityMapPerChunk.put(activeChunkKey, newEntitySet(activeEntity, removedEntity));

    ConcurrentHashMap<EntityType<?>, Set<Entity>> entityMapGlobal = new ConcurrentHashMap<>();
    entityMapGlobal.put(EntityType.ZOMBIE, newEntitySet(activeEntity, removedEntity));

    ConcurrentHashMap<Entity, Object> entityChunkKeyMap = new ConcurrentHashMap<>();
    entityChunkKeyMap.put(activeEntity, activeChunkKey);
    entityChunkKeyMap.put(removedEntity, activeChunkKey);

    writeStaticField("entityMap", entityMap);
    writeStaticField("entityMapPerChunk", entityMapPerChunk);
    writeStaticField("entityMapGlobal", entityMapGlobal);
    writeStaticField("entityChunkKeyMap", entityChunkKeyMap);
    Set<Object> entityChunkMap = readStaticField("entityChunkMap");
    entityChunkMap.clear();
    entityChunkMap.add(activeChunkKey);
    entityChunkMap.add(staleChunkKey);

    Method method = CoreEntityManager.class.getDeclaredMethod("verifyEntities");
    method.setAccessible(true);
    method.invoke(null);

    Map<Object, Set<Entity>> cleanedEntityMap = readStaticField("entityMap");
    Map<Object, Set<Entity>> cleanedEntityMapPerChunk = readStaticField("entityMapPerChunk");
    Map<EntityType<?>, Set<Entity>> cleanedEntityMapGlobal = readStaticField("entityMapGlobal");
    Map<Entity, Object> cleanedChunkKeyMap = readStaticField("entityChunkKeyMap");
    Set<Object> cleanedChunkMap = readStaticField("entityChunkMap");

    assertEquals(Set.of(activeEntity), cleanedEntityMap.get(entityMapKey));
    assertEquals(Set.of(activeEntity), cleanedEntityMapPerChunk.get(activeChunkKey));
    assertEquals(Set.of(activeEntity), cleanedEntityMapGlobal.get(EntityType.ZOMBIE));
    assertEquals(Map.of(activeEntity, activeChunkKey), cleanedChunkKeyMap);
    assertTrue(cleanedChunkMap.contains(activeChunkKey));
    assertFalse(cleanedChunkMap.contains(staleChunkKey));
  }

  @Test
  void chunkEntityCountingMatchesTypedAndStringLookup() throws Exception {
    String levelName = "minecraft:overworld";
    ResourceLocation levelKey = ResourceLocation.tryParse(levelName);
    BlockPos blockPos = BlockPos.ZERO;
    Object chunkKey = newChunkTrackingKey(levelName, 0, 0);

    Entity activeZombie = mockEntity(EntityType.ZOMBIE, false);
    Entity removedZombie = mockEntity(EntityType.ZOMBIE, true);
    Entity activeSkeleton = mockEntity(EntityType.SKELETON, false);

    ConcurrentHashMap<Object, Set<Entity>> entityMapPerChunk = new ConcurrentHashMap<>();
    entityMapPerChunk.put(chunkKey, newEntitySet(activeZombie, removedZombie, activeSkeleton));
    writeStaticField("entityMapPerChunk", entityMapPerChunk);

    assertEquals(1,
      CoreEntityManager.getNumberOfEntitiesInChunk(levelName, EntityType.ZOMBIE, blockPos));
    assertEquals(1,
      CoreEntityManager.getNumberOfEntitiesInChunk(levelKey, EntityType.ZOMBIE, blockPos));
    assertEquals(1,
      CoreEntityManager.getNumberOfEntitiesInChunk(levelName, "minecraft:zombie", blockPos));
    assertEquals(1,
      CoreEntityManager.getNumberOfEntitiesInChunk(levelName, EntityType.SKELETON, blockPos));
    assertEquals(2,
      CoreEntityManager.getTrackedEntityCountInChunk(levelKey, blockPos));
  }

  @Test
  void globalAndLevelEntityCountingMatchesTypedAndStringLookup() throws Exception {
    String levelName = "minecraft:overworld";
    ResourceLocation levelKey = ResourceLocation.tryParse(levelName);
    String entityName = "minecraft:zombie";
    Object entityMapKey = newEntityTrackingKey(levelName, entityName);

    Entity activeZombie = mockEntity(EntityType.ZOMBIE, false);
    Entity removedZombie = mockEntity(EntityType.ZOMBIE, true);
    Entity activeSkeleton = mockEntity(EntityType.SKELETON, false);

    ConcurrentHashMap<Object, Set<Entity>> entityMap = new ConcurrentHashMap<>();
    entityMap.put(entityMapKey, newEntitySet(activeZombie, removedZombie));
    writeStaticField("entityMap", entityMap);

    ConcurrentHashMap<EntityType<?>, Set<Entity>> entityMapGlobal = new ConcurrentHashMap<>();
    entityMapGlobal.put(EntityType.ZOMBIE, newEntitySet(activeZombie, removedZombie));
    entityMapGlobal.put(EntityType.SKELETON, newEntitySet(activeSkeleton));
    writeStaticField("entityMapGlobal", entityMapGlobal);

    assertEquals(2, CoreEntityManager.getNumberOfEntities(levelName, EntityType.ZOMBIE));
    assertEquals(2, CoreEntityManager.getNumberOfEntities(levelKey, EntityType.ZOMBIE));
    assertEquals(2, CoreEntityManager.getNumberOfEntities(levelName, entityName));
    assertEquals(2, CoreEntityManager.getNumberOfEntities(levelKey, entityName));
    assertEquals(2, CoreEntityManager.getNumberOfEntities(EntityType.ZOMBIE));
    assertEquals(2, CoreEntityManager.getNumberOfEntities(entityName));
    assertEquals(1, CoreEntityManager.getNumberOfEntities(EntityType.SKELETON));
    assertTrue(CoreEntityManager.getEntitiesGlobal().containsKey(entityName));
  }
}
