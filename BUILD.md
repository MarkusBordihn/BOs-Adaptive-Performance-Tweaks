# Development Environment

The mod was build in a [Visual Studio Code][visual_studio_code] environment, but you could technical use any other JAVA IDE as well.

## Forge Docs

Please take a look at the [Forge Docs][forge_docs] to see an full overview of the Forge API.

## Grandle

The build system is based on [gradle][gradle].

## Testing

For testing you basically only need the following gradlew commands.

### Test with Client

`.\gradlew.bat runClient`

### Test with Server

`.\gradlew.bat runServer`

Note: Make sure to read and accept the eula.txt inside the run directory, otherwise the server will not start.

## CurseGrandle

A gradle plugin for publishing artifacts to CurseForge.

### Add Token

`.\gradlew.bat addCredentials --key curseforge_api_token --value ...`

### Remove Token

`.\gradlew.bat removeCredentials --key curseforge_api_token`

### Push to CurseForge

`.\gradlew.bat curseforge`

[forge_docs]: https://mcforge.readthedocs.io/en/latest/
[gradle]: https://docs.gradle.org/
[visual_studio_code]: https://code.visualstudio.com/
