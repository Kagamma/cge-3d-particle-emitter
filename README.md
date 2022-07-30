GPU-based Particle System for Castle Game Engine

### Notes ###

Due to emitter uses direct OpenGL calls instead of CGE's own renderer, only a few of TCastleScene's features are supported at the moment:

- DistanceCulling works based on Position.
- BoundingBox and frustum culling works. The emitter doesn't calculate bounding box so you need to set up bounding box manually via Effect.BBox property, either by code or by using castle-editor.
- Visible works.
- The component registers to TRenderStatistics as 1 Shape / 1 Scene.
- Instancing, by putting the same emitter to multiple TCastleTransform nodes works. It doesn't use GPU instancing at the moment. Note that it is necessary to set  AllowsInstancing = True for this to work.
- If AllowsInstancing = False, then particle's position is independent from emitter's transformation.
- Use ProcessEvents for start / stop emitting.
- AnimateWhenOnlyVisible = False will stop update particle if emitter's boundingbox is outside of view frustum.

License: MIT.
