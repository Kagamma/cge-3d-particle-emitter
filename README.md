# cge-3d-particle-emitter
GPU-based 3D Particle System for Castle Game Engine

### Notes ###

Due to emitter uses direct OpenGL calls instead of CGE's own renderer, only a few of TCastleScene's features are supported at the moment:

- DistanceCulling works based on Position.
- BoundingBox and frustum culling works. The emitter doesn't calculate bounding box so you need to set up bounding box manually via Effect.BBox property, either by code or by using particle editor.
- Visible works.
- The component registers to TRenderStatistics as 1 Shape / 1 Scene.
- Instancing, by putting the same emitter to multiple TCastleTransform nodes works. It doesn't use GPU instancing at the moment.
- Use StartEmitting instead of ProcessEvents for start / stop emitting.
- Particle's position is affected by emitter's transformation. If you only want to move emitter only, use Position property instead.

License: MIT.

The editor source code is licensed under GNU v2 due to some of its code is borrowed from view3dscene.
