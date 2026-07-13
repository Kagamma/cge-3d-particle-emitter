- [Spawning Methods](#spawning-methods)
- [Life Cycle](#life-cycle)
- [Rotation](#rotation)
- [Anchors](#anchors)
- [Attractors](#attractors)
- [Custom Shaders](#custom-shaders)

## Spawning Methods

There are several spawning methods, controlled by the `SourceType` parameter.

#### Common parameters:
- `SourcePosition`: The location of the source.
- `SourcePositionLocationVariance`: Determines the position variance of each spawned particle using the following formula:  
  `particle's position + SourcePositionLocationVariance * normalize(vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0))`

#### Special parameters:
`SourcePositionVariance` changes meaning depending on the value of `SourceType`:

- `SourceType = pstBox`: `SourcePositionVariance` contains the half-extents (X/2, Y/2, Z/2) of the box. Particles spawn inside the box.
- `SourceType = pstBoxSurface`: `SourcePositionVariance` contains the half-extents (X/2, Y/2, Z/2) of the box. Particles spawn on the surface of the box.
- `SourceType = pstSpheroid`: `SourcePositionVariance` contains the radius of the spheroid. Particles spawn inside the spheroid.
- `SourceType = pstSpheroidSurface`: `SourcePositionVariance` contains the radius of the spheroid. Particles spawn on the surface of the spheroid.
- `SourceType = pstCylinderSurface`: `SourcePositionVariance` X and Z represent the radius, and Y represents the height of the cylinder. Particles spawn on the surface of the cylinder.

## Life Cycle

- Each particle’s lifetime is controlled by the `Lifespan` and `LifespanVariance` parameters. For example, if `Lifespan` is 2 and `LifespanVariance` is 0.5, the particle will die after `2 + 0.5 * (random(2) - 1)` seconds.
- A particle emitter’s lifetime is controlled by the `Duration` parameter. If `Duration` is less than 0, the emitter never stops. If `Duration` is greater than 0, the emitter stops after the specified number of seconds. The `ReleaseWhenDone` parameter determines whether the emitter is automatically freed after it stops emitting.

## Rotation

There are two types of rotation, controlled by `RotationType`.

#### prtDefault
The default rotation method, using the following parameters:
- `Rotation`: Euler angles (ZYX order)
- `RotationVariance`:
- `RotationSpeed`: `Rotation = Rotation + RotationSpeed`
- `RotationSpeedVariance`:

#### prtPreviousPosition
Makes particles face toward their previous position. This mode completely ignores the four parameters used by `prtDefault`.

## Anchors

A particle’s base size and color are set in the Effect.

A particle’s size and color can change over its lifetime via **Anchors**.

There's limitation of 4 anchors per Effect.

`TimeNormalized` (range [0..1]) determines at what point during the particle’s lifetime each anchor is applied.

`KillDistance` if the value is greater than or equal to 0, then the attractor will automatically kill any particles that come near it: `kill = (particle's distance to attractor) <= KillDistance`

Size, Color, SizeVariance, and ColorVariance are updated using linear interpolation between anchors.

## Attractors

Attractors influence the velocity of particles.

An attractor must be a child of the emitter it affects.

There's limitation of 4 attractors per emitter.

There are two types of attractors: `patDistance` and `patGravityPoint`.

- `patDistance`: Updates velocity using the formula:  
  `velocity = velocity + (particle's position - attractor's position) * attractor’s Attraction`
- `patGravityPoint`: Updates velocity using the formula:  
  `velocity = velocity + normalize(particle's position - attractor's position) * (6.674 * attractor’s Attraction / distance between particle and attractor)`

## Custom Shaders

There are two types of shaders that can be customized:

#### Render Shader
Used to render particles on screen. Controlled by the `CustomRenderVertexShader` and `CustomRenderFragmentShader` parameters.

This shader does not replace the default shader. Instead, it is inserted into the default shader as **plug**, similar to CGE's PLUG system.

**Currently supported plugs:**
- `void PLUG_vertex_object_space(inout vec3 vertex_object)`: Only for vertex shader, allowed to modify vertex in object space.
- `void PLUG_texture_coord(inout vec2 texture_coord)`: Only for vertex shader, allowed to modify texture coordinate.
- `void PLUG_color(inout vec4 color)`: Only for fragment shader, allowed to modify particle's color value.
- `void PLUG_texture_color(inout vec4 texture_color)`: Only for fragment shader, allowed to modify particle's texture color value.

#### Transform Feedback Shader
Used to update particle position, rotation, velocity, size, and color. Controlled by the `CustomTransformFeedbackVertexShader` parameter.

This shader does not replace the default shader. Instead, it is inserted into the default shader as **plug**, similar to CGE's PLUG system.

**Currently supported plugs:**
- `void PLUG_update_after()`: Called after the particle’s calculations are complete. Primarily used to modify particles after all other updates. See the `Snow` effect in the gallery demo for an example.
