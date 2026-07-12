- [Spawning Methods](#spawning-methods)
- [Life cycle](#life-cycle)
- [Anchors](#anchors)
- [Attractors](#attractors)
- [Custom Shaders](#custom-shaders)

## Spawning Methods
There're several spawning methods, controlled by `SourceType` parameter.

### Common parameters:
- `SourcePosition`: The location of the source.
- `SourcePositionLocationVariance`: Determine the position variance of the spawned particle. Calculated by using the following formula: `particle's position + SourcePositionLocationVariance * normalize(vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0));`
### Special parameters:
`SourcePositionVariance` changed their meaning base on `SourceType`'s values:
- `SourceType = pstBox`: `SourcePositionVariance` contains the X / 2, Y / 2, Z / 2 dimension of the box. The particles will spawn inside the box.
- `SourceType = pstBoxSurface`: `SourcePositionVariance` contains the X / 2, Y / 2, Z / 2 dimension of the box. The particles will spawn on the surface of the box.
- `SourceType = pstSpheroid`: `SourcePositionVariance` contains the radius of the sphere. The particles will spawn inside the sphere.
- `SourceType = pstSpheroidSurface`: `SourcePositionVariance` contains the radius of the sphere. The particles will spawn on the surface of the sphere.
- `SourceType = pstCylinderSurface`: `SourcePositionVariance` X and Z are the radius, Y is the height of the cylinder. The particles will spawn on the surface of the cylinder.

## Life cycle
- Each particle's life is controlled by the `Lifespan` and `LifespanVariance` parameters. For example, if `Lifespan` is 2 and `LifespanVariance = 0.5`, then a particle will die after `2 + 0.5 * (random(2) - 1)` seconds.
- A particle emitter's life is controlled by `Duration` parameter. If `Duration` is lower than 0, then this particle emitter will never stop emitting. If `Duration` is higher than 0, then particle emiiter will stop emitting after `Duration` seconds. `ReleaseWhenDone` will determine if this particle emitter will be automatically freed after stop emitting.

## Anchors
A particle's base size and color can be set in Effect.

A particle's size and color can change during it's lifetime, via `Anchors`.

There's a limitation of 4 anchors per Effect.

`TimeNormalized`, which has the range of [0..1], is used to determine when the anchor will take place during the particle's lifetime.

Size, Color, SizeVariance and ColorVariance are changed via linear interpolation between anchors.

## Attractors
Attractors are used to influence the velocity of a particle.

An attractor has to be a child of the Emitter it tries to affect.

There's a limitation of 4 attractors per emitter.

There're 2 type of attractor: `patDistance` and `patGravityPoint`

- `patDistance`: Influence the particle's velocity via the following formula: `velocity = velocity + (particle's position - attractor's position) * attractors's Attraction`
- `patGravityPoint`: Influence the particle's velocity via the following formula: `velocity = velocity + normalize(particle's position - attractor's position) * (6.674 * attractors's Attraction / distance between particle and attractor)`

## Custom Shaders
There're 2 type of shaders we can customize:
### Render shader
Used to show the particles on screen, controlled by `CusomRenderVertexShader` and `CustomRenderFragmentShader` parameters.

The shader set in those 2 parameters will COMPLETELY REPLACED the default shader.
### Transform feedback shader
Used to calculate particles position, size and color, controlled by `CustomTransformFeedbackVertexShader` parameter.

The shader set in the parameter will not replaced the default shader, but instead insert itself to the default shader as PLUG.

Currently, only one PLUG is supported:
- `void PLUG_update_after()`: Will be called after the particle's finished it's calculations. This is mainly used to modify the particle after all calculations are done. See the `Snow` effect in `gallery` demo for example.