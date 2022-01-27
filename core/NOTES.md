# Development Notes

## Profiles

List of different test profiles for optimization for my standard mob back.
These profiles are used to optimize the different kind of modules.

### 19. Jan 2022

- <https://spark.lucko.me/5870e3r5F7>
- <https://spark.lucko.me/ugzdSskHHZ>
- <https://spark.lucko.me/qb8JQuFRUl>

#### Minecraft High Load

| Name                                               | %     |
| -------------------------------------------------- | ----- |
| net.minecraft.world.entity.npc.Villager.tick()     | 6.28% |
| net.minecraft.world.entity.animal.Sheep.aiStep()   | 4.22% |
| net.minecraft.world.entity.animal.Chicken.aiStep() | 3.53% |
| net.minecraft.world.entity.monster.Skeleton.tick() | 2.35% |
| net.minecraft.world.entity.monster.Creeper.tick()  | 1.96% |
| net.minecraft.world.entity.monster.Zombie.tick()   | 1.93% |
| net.minecraft.world.entity.ambient.Bat.tick()      | 1.49% |
| net.minecraft.world.entity.monster.Spider.tick()   | 1.13% |

#### Mods High Load

| Name                                                                               | %      |
| ---------------------------------------------------------------------------------- | ------ |
| vazkii.quark.content.automation.module.FeedingTroughModule.buildTroughSet()        | 13.07% |
| vazkii.quark.content.mobs.entity.Shiba.tick()                                      | 2.29%  |
| top.theillusivec4.polymorph.common.capability.AbstractBlockEntityRecipeData.tick() | 0.86%  |
