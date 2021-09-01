unit CastleParticleEmitter;

{$coperators on}
{$macro on}
{$define nl:=+ LineEnding +}

{$ifdef ANDROID}{$define GLES}{$endif}
{$ifdef iOS}{$define GLES}{$endif}

interface

uses
  Classes, SysUtils, fpjsonrtti,
  {$ifdef GLES}
  CastleGLES20, // This wont work. We need GLES3 header
  {$else}
  GL, GLExt,
  {$endif}
  {$ifdef CASTLE_DESIGN_MODE}
  PropEdits, CastlePropEdits, CastleDebugTransform,
  {$endif}
  fpjson, jsonparser,
  CastleTransform, CastleScene, CastleComponentSerialize, CastleColors, CastleBoxes,
  CastleVectors, CastleRenderContext, Generics.Collections, CastleGLImages, CastleLog,
  CastleUtils, CastleApplicationProperties, CastleGLShaders, CastleClassUtils,
  X3DNodes, CastleShapes, CastleGLUtils, CastleViewport, CastleImages, CastleRectangles;

type
  TCastleParticleBlendMode = (
    pbmZero,
    pbmOne,
    pbmSrcColor,
    pbmOneMinusSrcColor,
    pbmSrcAlpha,
    pbmOneMinusSrcAlpha,
    pbmDstAlpha,
    pbmOneMinusDstAlpha,
    pbmDstColor,
    pbmOneMinusDstColor
  );

  TCastleParticleSourceType = (
    pstBox,
    pstSpheroid,
    pstBoxSurface,
    pstSpheroidSurface
  );

const
  CastleParticleBlendValues: array [TCastleParticleBlendMode] of Integer = (
    0, 1, 768, 769, 770, 771, 772, 773, 774, 775
  );
  CastleParticleSourceValues: array [TCastleParticleSourceType] of Integer = (
    0, 1, 2, 3
  );

type
  { Particle struct to hold current particle settings. }
  PCastleParticle = ^TCastleParticle;
  TCastleParticle = packed record
    Position: TVector4;
    TimeToLive: TVector2;
    Life, AnchorCount,
    Size,
    SizeDelta,
    Rotation,
    RotationDelta: Single;
    Color,
    ColorDelta: TVector4;
    StartPos: TVector3;
    Velocity: TVector4;
    Direction: TVector3;
    Translate: TVector3;
    RotationXY: TVector4;
  end;

  PCastleParticleMesh = ^TCastleParticleMesh;
  TCastleParticleMesh = packed record
    Vertex: TVector3;
    Texcoord: TVector2;
    Normal: TVector3;
  end;

  TCastleParticleViewport = class(TCastleViewport)
  strict private
    FImage: TDrawableImage;
    FVisible: Boolean;
    FTextureHeight,
    FTextureWidth: Integer;
  protected
    procedure RenderFromViewEverything(const RenderingCamera: TRenderingCamera); override;
    procedure ReconstructBuffer;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Image: TDrawableImage read FImage;
  published
    property Visible: Boolean read FVisible write FVisible default True;
    property TextureWidth: Integer read FTextureHeight write FTextureHeight default 256;
    property TextureHeight: Integer read FTextureWidth write FTextureWidth default 256;
  end;

  TCastleParticleEffectAnchorItem = class(TCollectionItem)
  strict private
    FTimeNormalized,
    FSize,
    FSizeVariance: Single;
    FColor,
    FColorVariance: TVector4;
    FColorPersistent,
    FColorVariancePersistent: TCastleColorPersistent;
    procedure SetColorForPersistent(const AValue: TVector4);
    function GetColorForPersistent: TVector4;
    procedure SetColorVarianceForPersistent(const AValue: TVector4);
    function GetColorVarianceForPersistent: TVector4;
    procedure SetTimeNormalized(const AValue: Single);
  public
    constructor Create(AClass: TCollection); override;
    destructor Destroy; override;
    property Color: TVector4 read FColor write FColor;
    property ColorVariance: TVector4 read FColorVariance write FColorVariance;
  published
    property TimeNormalized: Single read FTimeNormalized write SetTimeNormalized default 1;
    property Size: Single read FSize write FSize default 1;
    property SizeVariance: Single read FSizeVariance write FSizeVariance;
    property ColorPersistent: TCastleColorPersistent read FColorPersistent;
    property ColorVariancePersistent: TCastleColorPersistent read FColorVariancePersistent;
  end;

  TCastleParticleEffect = class(TCastleComponent)
  private
    FMesh,
    FTexture: String;
    FSourceType: TCastleParticleSourceType;
    FBlendFuncSource,
    FBlendFuncDestination: TCastleParticleBlendMode;
    FMaxParticles: Integer;
    FLifeSpan,
    FLifeSpanVariance,
    FSize,
    FSizeVariance,
    FSpeed,
    FSpeedVariance,
    FDuration: Single;
    FDirectionVariance: Single;
    FRotation,
    FRotationVariance,
    FRotationSpeed,
    FRotationSpeedVariance,
    FSourcePosition,
    FSourcePositionVariance,
    FDirection,
    FGravity: TVector3;
    FColor,
    FColorVariance: TVector4;
    FBoundingBoxMinPersistent,
    FBoundingBoxMaxPersistent,
    FRotationPersistent,
    FRotationVariancePersistent,
    FRotationSpeedPersistent,
    FRotationSpeedVariancePersistent,
    FSourcePositionPersistent,
    FSourcePositionVariancePersistent,
    FDirectionPersistent,
    FGravityPersistent: TCastleVector3Persistent;
    FColorPersistent,
    FColorVariancePersistent: TCastleColorPersistent;
    FRadial,
    FRadialVariance: Single;
    FTextureViewport: TCastleParticleViewport;
    FBBox: TBox3D;
    FAnchors: TCollection;
    procedure SetBoundingBoxMinForPersistent(const AValue: TVector3);
    function GetBoundingBoxMinForPersistent: TVector3;
    procedure SetBoundingBoxMaxForPersistent(const AValue: TVector3);
    function GetBoundingBoxMaxForPersistent: TVector3;
    procedure SetRotationForPersistent(const AValue: TVector3);
    function GetRotationForPersistent: TVector3;
    procedure SetRotationVarianceForPersistent(const AValue: TVector3);
    function GetRotationVarianceForPersistent: TVector3;
    procedure SetRotationSpeedForPersistent(const AValue: TVector3);
    function GetRotationSpeedForPersistent: TVector3;
    procedure SetRotationSpeedVarianceForPersistent(const AValue: TVector3);
    function GetRotationSpeedVarianceForPersistent: TVector3;
    procedure SetSourcePositionForPersistent(const AValue: TVector3);
    function GetSourcePositionForPersistent: TVector3;
    procedure SetSourcePositionVarianceForPersistent(const AValue: TVector3);
    function GetSourcePositionVarianceForPersistent: TVector3;
    procedure SetDirectionForPersistent(const AValue: TVector3);
    function GetDirectionForPersistent: TVector3;
    procedure SetGravityForPersistent(const AValue: TVector3);
    function GetGravityForPersistent: TVector3;
    procedure SetColorForPersistent(const AValue: TVector4);
    function GetColorForPersistent: TVector4;
    procedure SetColorVarianceForPersistent(const AValue: TVector4);
    function GetColorVarianceForPersistent: TVector4;
    procedure SetTextureViewport(const AValue: TCastleParticleViewport);
    procedure SetMesh(const AValue: String);
    procedure SetTexture(const AValue: String);
    procedure SetMaxParticle(const AValue: Integer);
    procedure SetDuration(const AValue: Single);
  protected
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    IsColliable: Boolean;
    IsNeedRefresh: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const AURL: String; const IsTexturePathRelative: Boolean = False);
    procedure Save(const AURL: String; const IsTexturePathRelative: Boolean = False);

    property Rotation: TVector3 read FRotation write FRotation;
    property RotationVariance: TVector3 read FRotationVariance write FRotationVariance;
    property RotationSpeed: TVector3 read FRotationSpeed write FRotationSpeed;
    property RotationSpeedVariance: TVector3 read FRotationSpeedVariance write FRotationSpeedVariance;
    property SourcePosition: TVector3 read FSourcePosition write FSourcePosition;
    property SourcePositionVariance: TVector3 read FSourcePositionVariance write FSourcePositionVariance;
    property Direction: TVector3 read FDirection write FDirection;
    property Gravity: TVector3 read FGravity write FGravity;
    property Color: TVector4 read FColor write FColor;
    property ColorVariance: TVector4 read FColorVariance write FColorVariance;
    property BBox: TBox3D read FBBox write FBBox;
  published
    property Mesh: String read FMesh write SetMesh;
    property Texture: String read FTexture write SetTexture;
    property SourceType: TCastleParticleSourceType read FSourceType write FSourceType default pstBox;
    property BlendFuncSource: TCastleParticleBlendMode read FBlendFuncSource write FBlendFuncSource default pbmOne;
    property BlendFuncDestination: TCastleParticleBlendMode read FBlendFuncDestination write FBlendFuncDestination default pbmOne;
    property MaxParticles: Integer read FMaxParticles write SetMaxParticle default 100;
    property LifeSpan: Single read FLifeSpan write FLifeSpan default 1;
    property LifeSpanVariance: Single read FLifeSpanVariance write FLifeSpanVariance default 0.5;
    property Size: Single read FSize write FSize default 1;
    property SizeVariance: Single read FSizeVariance write FSizeVariance;
    property Speed: Single read FSpeed write FSpeed default 3;
    property SpeedVariance: Single read FSpeedVariance write FSpeedVariance default 1;
    property Duration: Single read FDuration write SetDuration default -1;
    property DirectionVariance: Single read FDirectionVariance write FDirectionVariance default 0.4;
    property Radial: Single read FRadial write FRadial;
    property RadialVariance: Single read FRadialVariance write FRadialVariance;
    property TextureViewport: TCastleParticleViewport read FTextureViewport write SetTextureViewport;
    property BoundingBoxMinPersistent: TCastleVector3Persistent read FBoundingBoxMinPersistent;
    property BoundingBoxMaxPersistent: TCastleVector3Persistent read FBoundingBoxMaxPersistent;
    property RotationPersistent: TCastleVector3Persistent read FRotationPersistent write FRotationPersistent;
    property RotationVariancePersistent: TCastleVector3Persistent read FRotationVariancePersistent write FRotationVariancePersistent;
    property RotationSpeedPersistent: TCastleVector3Persistent read FRotationSpeedPersistent write FRotationSpeedPersistent;
    property RotationSpeedVariancePersistent: TCastleVector3Persistent read FRotationSpeedVariancePersistent write FRotationSpeedVariancePersistent;
    property SourcePositionPersistent: TCastleVector3Persistent read FSourcePositionPersistent;
    property SourcePositionVariancePersistent: TCastleVector3Persistent read FSourcePositionVariancePersistent;
    property DirectionPersistent: TCastleVector3Persistent read FDirectionPersistent;
    property GravityPersistent: TCastleVector3Persistent read FGravityPersistent;
    property ColorPersistent: TCastleColorPersistent read FColorPersistent;
    property ColorVariancePersistent: TCastleColorPersistent read FColorVariancePersistent;
    property Anchors: TCollection read FAnchors;
  end;

  TCastleParticleAttractor = class(TCastleTransform)
  strict private
    FAttraction: Single;
  protected
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Attraction: Single read FAttraction write FAttraction;
  end;

  TCastleParticleEmitter = class(TCastleTransform)
  strict private
    Texture: GLuint;

    VAOs,
    VAOMeshes,
    VBOs: array[0..1] of GLuint;
    VBOMesh, VBOMeshIndices: GLuint;
    CurrentBuffer: GLuint;
    Particles: packed array of TCastleParticle;
    ParticleMesh: packed array of TCastleParticleMesh;
    ParticleMeshIndices: packed array of GLuint;

    FStartEmitting: Boolean;
    FEffect: TCastleParticleEffect;
    FParticleCount: Integer;
    FSecondsPassed: Single;
    FIsUpdated: Boolean;
    { Countdown before remove the emitter }
    FCountdownTillRemove,
    { The value is in miliseconds. Set it to -1 for infinite emitting, 0 to
      stop the emitter and positive value for cooldown. }
    FEmissionTime: Single;
    { When this is set to true, the emitter will automatically freed after
      all particles destroyed. }
    FReleaseWhenDone: Boolean;
    { Bypass GLContext problem }
    FIsGLContextInitialized: Boolean;
    FIsNeedRefresh: Boolean;
    FDistanceCulling: Single;
    FIsDrawn: Boolean;
    FAllowsWriteToDepthBuffer: Boolean;
    { If true, the particle still perform update even when being culled }
    FAllowsUpdateWhenCulled: Boolean;
    { If true, this instance can be used in multiple transform nodes }
    FAllowsInstancing: Boolean;
    { If true, spawn all particles at once (burst) }
    FBurst: Boolean;
    { If true, smooth texture }
    FSmoothTexture: Boolean;
    FEnableFog,
    FTimePlaying: Boolean;
    FTimePlayingSpeed: Single;
    FColorList,
    FColorVarianceList: TVector4List;
    FAnchorList,
    FSizeList,
    FSizeVarianceList: TSingleList;
    FAttractor: TCastleParticleAttractor;
    {$ifdef CASTLE_DESIGN_MODE}
    FDebugBox: TDebugBox;
    {$endif}
    procedure InternalLoadMesh;
    procedure InternalRefreshEffect;
    procedure SetStartEmitting(V: Boolean);
    procedure SetBurst(V: Boolean);
    procedure SetSmoothTexture(V: Boolean);
    procedure SetAttractor(const AValue: TCastleParticleAttractor);
  protected
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure LocalRender(const Params: TRenderParams); override;
    procedure LoadEffect(const AEffect: TCastleParticleEffect);
    procedure GLContextOpen; virtual;
    procedure GLContextClose; override;
    procedure RefreshEffect;
    function LocalBoundingBox: TBox3D; override;
  published
    property Effect: TCastleParticleEffect read FEffect write LoadEffect;
    property Attractor: TCastleParticleAttractor read FAttractor write SetAttractor;
    { If true, the emitter will start emitting }
    property StartEmitting: Boolean read FStartEmitting write SetStartEmitting default True;
    property DistanceCulling: Single read FDistanceCulling write FDistanceCulling default 0;
    property AllowsWriteToDepthBuffer: Boolean read FAllowsWriteToDepthBuffer write FAllowsWriteToDepthBuffer default False;
    property AllowsUpdateWhenCulled: Boolean read FAllowsUpdateWhenCulled write FAllowsUpdateWhenCulled default True;
    property AllowsInstancing: Boolean read FAllowsInstancing write FAllowsInstancing default False;
    property EnableFog: Boolean read FEnableFog write FEnableFog default False;
    property SmoothTexture: Boolean read FSmoothTexture write SetSmoothTexture default True;
    property Burst: Boolean read FBurst write SetBurst default False;
    property TimePlaying: Boolean read FTimePlaying write FTimePlaying default True;
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed default 1.0;
  end;

function CastleParticleBlendValueToBlendMode(const AValue: Integer): TCastleParticleBlendMode;

implementation

uses
  CastleDownload, CastleURIUtils, Math;

const
  TransformVertexShaderSourceMultipleInstances: String =
'#version 330'nl
'layout(location = 0) in vec4 inPosition;'nl
'layout(location = 1) in vec4 inTimeToLive;'nl
'layout(location = 2) in vec4 inSizeRotation;'nl
'layout(location = 3) in vec4 inColor;'nl
'layout(location = 4) in vec4 inColorDelta;'nl
'layout(location = 5) in vec3 inStartPos;'nl
'layout(location = 6) in vec4 inVelocity;'nl
'layout(location = 7) in vec3 inDirection;'nl
'layout(location = 8) in vec3 inTranslate;'nl
'layout(location = 9) in vec4 inRotationXY;'nl

'out vec4 outPosition;'nl
'out vec4 outTimeToLive;'nl
'out vec4 outSizeRotation;'nl
'out vec4 outColor;'nl
'out vec4 outColorDelta;'nl
'out vec3 outStartPos;'nl
'out vec4 outVelocity;'nl
'out vec3 outDirection;'nl
'out vec3 outTranslate;'nl
'out vec4 outRotationXY;'nl

'struct Effect {'nl
'  int sourceType;'nl
'  float particleLifeSpan;'nl
'  float particleLifeSpanVariance;'nl
'  float Size;'nl
'  float SizeVariance;'nl
'  float anchorSize[5];'nl
'  float anchorSizeVariance[5];'nl
'  float maxRadius;'nl
'  float maxRadiusVariance;'nl
'  float minRadius;'nl
'  float minRadiusVariance;'nl
'  float rotatePerSecond;'nl
'  float rotatePerSecondVariance;'nl
'  vec3 rotation;'nl
'  vec3 rotationVariance;'nl
'  vec3 rotationSpeed;'nl
'  vec3 rotationSpeedVariance;'nl
'  float speed;'nl
'  float speedVariance;'nl
'  float radial;'nl
'  float radialVariance;'nl
'  float anchor[5];'nl
'  vec3 sourcePosition;'nl
'  vec3 sourcePositionVariance;'nl
'  vec3 gravity;'nl
'  vec3 direction;'nl
'  float directionVariance;'nl
'  vec4 Color;'nl
'  vec4 ColorVariance;'nl
'  vec4 anchorColor[5];'nl
'  vec4 anchorColorVariance[5];'nl
'  vec4 attractor;'nl
'  int anchorCount;'nl
'  int attractorCount;'nl
'  int maxParticles;'nl
'  int isColliable;'nl
'};'nl
'uniform Effect effect;'nl
'uniform float emissionTime;'nl
'uniform float deltaTime;'nl

'float rnd() {'nl
'  outPosition.w = fract(sin(outPosition.w + outPosition.x + outVelocity.x + outVelocity.y + outColor.x + deltaTime) * 43758.5453123);'nl
'  return outPosition.w;'nl
'}'nl

'vec3 rotate(vec3 v, float angle, vec3 axis) {'nl
'  axis = normalize(axis);'nl
'  float c = cos(angle);'nl
'  float s = sin(angle);'nl
'  vec3 r = vec3('nl
'      v.x * (axis.x * axis.x * (1.0 - c) + c)'nl
'    + v.y * (axis.x * axis.y * (1.0 - c) - axis.z * s)'nl
'    + v.z * (axis.x * axis.z * (1.0 - c) + axis.y * s),'nl
'      v.x * (axis.y * axis.x * (1.0 - c) + axis.z * s)'nl
'    + v.y * (axis.y * axis.y * (1.0 - c) + c)'nl
'    + v.z * (axis.y * axis.z * (1.0 - c) - axis.x * s),'nl
'      v.x * (axis.z * axis.x * (1.0 - c) - axis.y * s)'nl
'    + v.y * (axis.z * axis.y * (1.0 - c) + axis.x * s)'nl
'    + v.z * (axis.z * axis.z * (1.0 - c) + c)'nl
'  );'nl
'  return r;'nl
'}'nl

'void initParticle() {'nl
'  outPosition = inPosition;'nl
'  outTimeToLive = inTimeToLive;'nl
'  outSizeRotation = inSizeRotation;'nl
'  outColor = inColor;'nl
'  outColorDelta = inColorDelta;'nl
'  outStartPos = inStartPos;'nl
'  outVelocity = inVelocity;'nl
'  outDirection = inDirection;'nl
'  outTranslate = inTranslate;'nl
'  outRotationXY = inRotationXY;'nl
'}'nl

'void emitParticle() {'nl
'  outTimeToLive.z = effect.particleLifeSpan + effect.particleLifeSpanVariance * (rnd() * 2.0 - 1.0);'nl // Life
'  outTimeToLive.x = outTimeToLive.z;'nl
'  outTimeToLive.y = outTimeToLive.z * (effect.anchorCount > 1 ? effect.anchor[1] : 0.99);'nl
'  outTimeToLive.w = 1;'nl // current anchor
'  float invLifeSpan = 1.0 / outTimeToLive.x;'nl
'  float invTimeRemaining = 1.0 / outTimeToLive.y;'nl
'  outTimeToLive.y = outTimeToLive.z - outTimeToLive.y;'nl
'  vec3 vrpos = vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  if (effect.sourceType == 1) {'nl
'    vrpos = vec3(rnd(), rnd(), rnd()) * normalize(vrpos);'nl
'    outPosition.xyz = effect.sourcePosition + effect.sourcePositionVariance * vrpos;'nl
'  } else if (effect.sourceType == 2) {'nl
'    float face = rnd();'nl
'    if (face < 1.0 / 3.0)'nl
'      vrpos = vec3(sign(vrpos.x), vrpos.y, vrpos.z);'nl
'    else if (face < 2.0 / 3.0)'nl
'      vrpos = vec3(vrpos.x, sign(vrpos.y), vrpos.z);'nl
'    else'nl
'      vrpos = vec3(vrpos.x, vrpos.y, sign(vrpos.z));'nl
'    outPosition.xyz = effect.sourcePosition + effect.sourcePositionVariance * vrpos;'nl
'  } else if (effect.sourceType == 3) {'nl
'    outPosition.xyz = effect.sourcePosition + effect.sourcePositionVariance * normalize(vrpos);'nl
'  } else {'nl
'    outPosition.xyz = effect.sourcePosition + effect.sourcePositionVariance * vrpos;'nl
'  }'nl
'  outTranslate = vec3(0.0);'nl // Do nothing
'  outStartPos = vec3(0.0);'nl // Do nothing
'  outDirection = effect.direction;'nl
'  vec3 cd = normalize(cross(outDirection, outDirection.zxy));'nl
'  float angle = effect.directionVariance * (rnd() * 2.0 - 1.0);'nl
'  vec3 vrdir = rotate(outDirection, angle, cd);'nl
'  vrdir = rotate(vrdir, rnd() * 2.0 * 3.14159265359, outDirection);'nl
'  vec3 vspeed = vec3('nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0),'nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0),'nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0));'nl
'  outVelocity = vec4(vrdir * vspeed, effect.radial + effect.radialVariance * (rnd() * 2.0 - 1.0));'nl

'  outColor = effect.anchorColor[0] + effect.anchorColorVariance[0] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  vec4 middleColor = effect.anchorCount == 1 ? outColor : effect.anchorColor[1] + effect.anchorColorVariance[1] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outColorDelta = (middleColor - outColor) * invTimeRemaining;'nl
'  float startSize = max(0.0001, effect.anchorSize[0] + effect.anchorSizeVariance[0] * (rnd() * 2.0 - 1.0));'nl
'  float finishSize = effect.anchorCount == 1 ? startSize : max(0.0001, effect.anchorSize[1] + effect.anchorSizeVariance[1] * (rnd() * 2.0 - 1.0));'nl
'  outSizeRotation.xy = vec2(startSize, (finishSize - startSize) * invTimeRemaining);'nl

'  outSizeRotation.z = effect.rotation.z + effect.rotationVariance.z * (rnd() * 2.0 - 1.0);'nl
'  outSizeRotation.w = effect.rotationSpeed.z + effect.rotationSpeedVariance.z * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.x = effect.rotation.x + effect.rotationVariance.x * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.y = effect.rotationSpeed.x + effect.rotationSpeedVariance.x * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.z = effect.rotation.y + effect.rotationVariance.y * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.w = effect.rotationSpeed.y + effect.rotationSpeedVariance.y * (rnd() * 2.0 - 1.0);'nl
'}'nl

'void updateParticle() {'nl
'  float timeBetweenParticle = max(deltaTime, effect.particleLifeSpan / effect.maxParticles);'nl
'  if (outTimeToLive.x <= 0.0 && emissionTime == 0.0) {'nl
'    outTimeToLive.x = (rnd() - 1.0) * effect.particleLifeSpan;'nl
'  }'nl
'  if (outTimeToLive.x == 0.0 && emissionTime != 0.0) {'nl
'    emitParticle();'nl
'  } else if (outTimeToLive.x < 0.0) {'nl
'    outTimeToLive.x = min(0.0, outTimeToLive.x + timeBetweenParticle);'nl
'    return;'nl
'  }'nl
'  outColor += outColorDelta * deltaTime;'nl
'  if ((outTimeToLive.x >= outTimeToLive.y) && (outTimeToLive.x - deltaTime < outTimeToLive.y)) {'nl
'    int a = int(outTimeToLive.w) + 1;'nl
'    outTimeToLive.w = a;'nl // current anchor
'    if (a < effect.anchorCount) {'nl
'      outTimeToLive.y = outTimeToLive.z * effect.anchor[a];'nl
'      float invTimeRemaining = 1.0 / (outTimeToLive.y - outTimeToLive.z * effect.anchor[a - 1]);'nl
'      outTimeToLive.y = outTimeToLive.z - outTimeToLive.y;'nl
'      vec4 finishColor = effect.anchorColor[a] + effect.anchorColorVariance[a] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'      outColorDelta = (finishColor - outColor) * invTimeRemaining;'nl
'      float finishSize = max(0.0001, effect.anchorSize[a] + effect.anchorSizeVariance[a] * (rnd() * 2.0 - 1.0));'nl
'      outSizeRotation.y = (finishSize - outSizeRotation.x) * invTimeRemaining;'nl
'    } else {'nl
'      outColorDelta = vec4(0.0);'nl
'      outSizeRotation.y = 0.0;'nl
'      outTimeToLive.y = outTimeToLive.z;'nl
'    }'nl
'  }'nl
'  outTimeToLive.x = max(0.0, outTimeToLive.x - deltaTime);'nl
'  outVelocity.xyz = rotate(outVelocity.xyz, outVelocity.w * deltaTime, outDirection) + effect.gravity * deltaTime;'nl
'  if (effect.attractorCount > 0) {'nl
'    vec3 a = outPosition.xyz - effect.attractor.xyz;'nl
'    outVelocity.xyz += a * effect.attractor.w;'nl
'  }'nl
'  outPosition.xyz = rotate(outPosition.xyz, outVelocity.w * deltaTime, outDirection) + outVelocity.xyz * deltaTime;'nl
'  outSizeRotation.x += outSizeRotation.y * deltaTime;'nl
'  outSizeRotation.z += outSizeRotation.w * deltaTime;'nl
'  outRotationXY.x += outRotationXY.y * deltaTime;'nl
'  outRotationXY.z += outRotationXY.w * deltaTime;'nl
'}'nl

'void main() {'nl
'  initParticle();'nl
'  updateParticle();'nl
'}';

  TransformVertexShaderSourceSingleInstance: String =
'#version 330'nl
'layout(location = 0) in vec4 inPosition;'nl
'layout(location = 1) in vec4 inTimeToLive;'nl
'layout(location = 2) in vec4 inSizeRotation;'nl
'layout(location = 3) in vec4 inColor;'nl
'layout(location = 4) in vec4 inColorDelta;'nl
'layout(location = 5) in vec3 inStartPos;'nl
'layout(location = 6) in vec4 inVelocity;'nl
'layout(location = 7) in vec3 inDirection;'nl
'layout(location = 8) in vec3 inTranslate;'nl
'layout(location = 9) in vec4 inRotationXY;'nl

'out vec4 outPosition;'nl
'out vec4 outTimeToLive;'nl
'out vec4 outSizeRotation;'nl
'out vec4 outColor;'nl
'out vec4 outColorDelta;'nl
'out vec3 outStartPos;'nl
'out vec4 outVelocity;'nl
'out vec3 outDirection;'nl
'out vec3 outTranslate;'nl
'out vec4 outRotationXY;'nl

'struct Effect {'nl
'  int sourceType;'nl
'  float particleLifeSpan;'nl
'  float particleLifeSpanVariance;'nl
'  float Size;'nl
'  float SizeVariance;'nl
'  float anchorSize[5];'nl
'  float anchorSizeVariance[5];'nl
'  float maxRadius;'nl
'  float maxRadiusVariance;'nl
'  float minRadius;'nl
'  float minRadiusVariance;'nl
'  float rotatePerSecond;'nl
'  float rotatePerSecondVariance;'nl
'  vec3 rotation;'nl
'  vec3 rotationVariance;'nl
'  vec3 rotationSpeed;'nl
'  vec3 rotationSpeedVariance;'nl
'  float speed;'nl
'  float speedVariance;'nl
'  float radial;'nl
'  float radialVariance;'nl
'  float anchor[5];'nl
'  vec3 sourcePosition;'nl
'  vec3 sourcePositionVariance;'nl
'  vec3 gravity;'nl
'  vec3 direction;'nl
'  float directionVariance;'nl
'  vec4 Color;'nl
'  vec4 ColorVariance;'nl
'  vec4 anchorColor[5];'nl
'  vec4 anchorColorVariance[5];'nl
'  vec4 attractor;'nl
'  int anchorCount;'nl
'  int attractorCount;'nl
'  int maxParticles;'nl
'  int isColliable;'nl
'};'nl
'uniform Effect effect;'nl
'uniform mat4 mMatrix;'nl
'uniform float emissionTime;'nl
'uniform float deltaTime;'nl

'float rnd() {'nl
'  outPosition.w = fract(sin(outPosition.w + outPosition.x + outVelocity.x + outVelocity.y + outColor.x + deltaTime) * 43758.5453123);'nl
'  return outPosition.w;'nl
'}'nl

'vec3 rotate(vec3 v, float angle, vec3 axis) {'nl
'  axis = normalize(axis);'nl
'  float c = cos(angle);'nl
'  float s = sin(angle);'nl
'  vec3 r = vec3('nl
'      v.x * (axis.x * axis.x * (1.0 - c) + c)'nl
'    + v.y * (axis.x * axis.y * (1.0 - c) - axis.z * s)'nl
'    + v.z * (axis.x * axis.z * (1.0 - c) + axis.y * s),'nl
'      v.x * (axis.y * axis.x * (1.0 - c) + axis.z * s)'nl
'    + v.y * (axis.y * axis.y * (1.0 - c) + c)'nl
'    + v.z * (axis.y * axis.z * (1.0 - c) - axis.x * s),'nl
'      v.x * (axis.z * axis.x * (1.0 - c) - axis.y * s)'nl
'    + v.y * (axis.z * axis.y * (1.0 - c) + axis.x * s)'nl
'    + v.z * (axis.z * axis.z * (1.0 - c) + c)'nl
'  );'nl
'  return r;'nl
'}'nl

'void initParticle() {'nl
'  outPosition = inPosition;'nl
'  outTimeToLive = inTimeToLive;'nl
'  outSizeRotation = inSizeRotation;'nl
'  outColor = inColor;'nl
'  outColorDelta = inColorDelta;'nl
'  outStartPos = inStartPos;'nl
'  outVelocity = inVelocity;'nl
'  outDirection = inDirection;'nl
'  outTranslate = inTranslate;'nl
'  outRotationXY = inRotationXY;'nl
'}'nl

'void emitParticle() {'nl
'  mat3 rMatrix = mat3(mMatrix);'nl
'  outTimeToLive.z = effect.particleLifeSpan + effect.particleLifeSpanVariance * (rnd() * 2.0 - 1.0);'nl // Life
'  outTimeToLive.x = outTimeToLive.z;'nl
'  outTimeToLive.y = outTimeToLive.z * (effect.anchorCount > 1 ? effect.anchor[1] : 0.99);'nl
'  outTimeToLive.w = 1;'nl // current anchor
'  float invLifeSpan = 1.0 / outTimeToLive.x;'nl
'  float invTimeRemaining = 1.0 / outTimeToLive.y;'nl
'  outTimeToLive.y = outTimeToLive.z - outTimeToLive.y;'nl
'  vec3 scale = vec3('nl
'    length(vec3(mMatrix[0][0], mMatrix[0][1], mMatrix[0][2])),'nl
'    length(vec3(mMatrix[1][0], mMatrix[1][1], mMatrix[1][2])),'nl
'    length(vec3(mMatrix[2][0], mMatrix[2][1], mMatrix[2][2]))'nl
'  );'nl
'  vec3 vrpos = vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  if (effect.sourceType == 1) {'nl
'    vrpos = vec3(rnd(), rnd(), rnd()) * normalize(vrpos);'nl
'    outStartPos = rMatrix * (effect.sourcePosition + scale * effect.sourcePositionVariance * vrpos);'nl
'  } else if (effect.sourceType == 2) {'nl
'    float face = rnd();'nl
'    if (face < 1.0 / 3.0)'nl
'      vrpos = vec3(sign(vrpos.x), vrpos.y, vrpos.z);'nl
'    else if (face < 2.0 / 3.0)'nl
'      vrpos = vec3(vrpos.x, sign(vrpos.y), vrpos.z);'nl
'    else'nl
'      vrpos = vec3(vrpos.x, vrpos.y, sign(vrpos.z));'nl
'    outStartPos = rMatrix * (effect.sourcePosition + scale * effect.sourcePositionVariance * vrpos);'nl
'  } else if (effect.sourceType == 3) {'nl
'    outStartPos = rMatrix * (effect.sourcePosition + scale * effect.sourcePositionVariance * normalize(vrpos));'nl
'  } else {'nl
'    outStartPos = rMatrix * (effect.sourcePosition + scale * effect.sourcePositionVariance * vrpos);'nl
'  }'nl
'  outTranslate = vec3(mMatrix[3][0], mMatrix[3][1], mMatrix[3][2]);'nl
'  outPosition.xyz = outTranslate + outStartPos;'nl
'  outDirection = rMatrix * effect.direction;'nl
'  vec3 cd = normalize(cross(outDirection, outDirection.zxy));'nl
'  float angle = effect.directionVariance * (rnd() * 2.0 - 1.0);'nl
'  vec3 vrdir = rotate(outDirection, angle, cd);'nl
'  vrdir = rotate(vrdir, rnd() * 2.0 * 3.14159265359, outDirection);'nl
'  vec3 vspeed = vec3('nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0),'nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0),'nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0));'nl
'  outVelocity = vec4(vrdir * vspeed, effect.radial + effect.radialVariance * (rnd() * 2.0 - 1.0));'nl

'  outColor = effect.anchorColor[0] + effect.anchorColorVariance[0] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  vec4 middleColor = effect.anchorCount == 1 ? outColor : effect.anchorColor[1] + effect.anchorColorVariance[1] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outColorDelta = (middleColor - outColor) * invTimeRemaining;'nl
'  float startSize = max(0.0001, effect.anchorSize[0] + effect.anchorSizeVariance[0] * (rnd() * 2.0 - 1.0));'nl
'  float finishSize = effect.anchorCount == 1 ? startSize : max(0.0001, effect.anchorSize[1] + effect.anchorSizeVariance[1] * (rnd() * 2.0 - 1.0));'nl
'  outSizeRotation.xy = vec2(startSize, (finishSize - startSize) * invTimeRemaining);'nl

'  outSizeRotation.z = effect.rotation.z + effect.rotationVariance.z * (rnd() * 2.0 - 1.0);'nl
'  outSizeRotation.w = effect.rotationSpeed.z + effect.rotationSpeedVariance.z * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.x = effect.rotation.x + effect.rotationVariance.x * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.y = effect.rotationSpeed.x + effect.rotationSpeedVariance.x * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.z = effect.rotation.y + effect.rotationVariance.y * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.w = effect.rotationSpeed.y + effect.rotationSpeedVariance.y * (rnd() * 2.0 - 1.0);'nl
'}'nl

'void updateParticle() {'nl
'  float timeBetweenParticle = max(deltaTime, effect.particleLifeSpan / effect.maxParticles);'nl
'  if (outTimeToLive.x <= 0.0 && emissionTime == 0.0) {'nl
'    outTimeToLive.x = (rnd() - 1.0) * effect.particleLifeSpan;'nl
'  }'nl
'  if (outTimeToLive.x == 0.0 && emissionTime != 0.0) {'nl
'    emitParticle();'nl
'  } else if (outTimeToLive.x < 0.0) {'nl
'    outTimeToLive.x = min(0.0, outTimeToLive.x + timeBetweenParticle);'nl
'    return;'nl
'  }'nl
'  outColor += outColorDelta * deltaTime;'nl
'  if ((outTimeToLive.x >= outTimeToLive.y) && (outTimeToLive.x - deltaTime < outTimeToLive.y)) {'nl
'    int a = int(outTimeToLive.w) + 1;'nl
'    outTimeToLive.w = a;'nl // current anchor
'    if (a < effect.anchorCount) {'nl
'      outTimeToLive.y = outTimeToLive.z * effect.anchor[a];'nl
'      float invTimeRemaining = 1.0 / (outTimeToLive.y - outTimeToLive.z * effect.anchor[a - 1]);'nl
'      outTimeToLive.y = outTimeToLive.z - outTimeToLive.y;'nl
'      vec4 finishColor = effect.anchorColor[a] + effect.anchorColorVariance[a] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'      outColorDelta = (finishColor - outColor) * invTimeRemaining;'nl
'      float finishSize = max(0.0001, effect.anchorSize[a] + effect.anchorSizeVariance[a] * (rnd() * 2.0 - 1.0));'nl
'      outSizeRotation.y = (finishSize - outSizeRotation.x) * invTimeRemaining;'nl
'    } else {'nl
'      outColorDelta = vec4(0.0);'nl
'      outSizeRotation.y = 0.0;'nl
'      outTimeToLive.y = outTimeToLive.z;'nl
'    }'nl
'  }'nl
'  outTimeToLive.x = max(0.0, outTimeToLive.x - deltaTime);'nl
'  outVelocity.xyz = rotate(outVelocity.xyz, outVelocity.w * deltaTime, outDirection) + effect.gravity * deltaTime;'nl
'  if (effect.attractorCount > 0) {'nl
'    vec3 a = outPosition.xyz - effect.attractor.xyz;'nl
'    outVelocity.xyz += a * effect.attractor.w;'nl
'  }'nl
'  outStartPos = rotate(outStartPos, outVelocity.w * deltaTime, outDirection) + outVelocity.xyz * deltaTime;'nl
'  outPosition.xyz = outStartPos + outTranslate;'nl
'  outSizeRotation.x += outSizeRotation.y * deltaTime;'nl
'  outSizeRotation.z += outSizeRotation.w * deltaTime;'nl
'  outRotationXY.x += outRotationXY.y * deltaTime;'nl
'  outRotationXY.z += outRotationXY.w * deltaTime;'nl
'}'nl

'void main() {'nl
'  initParticle();'nl
'  updateParticle();'nl
'}';

  VertexShaderSourceQuad: String =
'#version 330'nl
'layout(location = 0) in vec4 inPosition;'nl
'layout(location = 1) in vec4 inTimeToLive;'nl
'layout(location = 2) in vec4 inSizeRotation;'nl
'layout(location = 3) in vec4 inColor;'nl
'layout(location = 9) in vec4 inRotationXY;'nl
'layout(location = 13) in vec3 inVertex;'nl
'layout(location = 14) in vec2 inTexcoord;'nl

'out vec2 fragTexCoord;'nl
'out vec4 fragColor;'nl
'out float fragFogCoord;'nl

'uniform mat4 vOrMvMatrix;'nl
'uniform mat4 pMatrix;'nl
'uniform float scaleX;'nl
'uniform float scaleY;'nl
'uniform float scaleZ;'nl

'mat3 createRotate(vec3 p) {'nl
'  float cr = cos(p.x);'nl
'  float sr = sin(p.x);'nl
'  float cp = cos(p.y);'nl
'  float sp = sin(p.y);'nl
'  float cy = cos(p.z);'nl
'  float sy = sin(p.z);'nl
'  return mat3('nl
'    cp * cy,'nl
'    cp * sy,'nl
'    - sp,'nl
'    sr * sp * cy - cr * sy,'nl
'    sr * sp * sy + cr * cy,'nl
'    sr * cp,'nl
'    cr * sp * cy + sr * sy,'nl
'    cr * sp * sy - sr * cy,'nl
'    cr * cp'nl
'  );'nl
'}'nl

'void main() {'nl
'  if (inTimeToLive.x > 0.0) {'nl
'    fragTexCoord = inTexcoord;'nl
'    fragColor = inColor;'nl
'    vec3 center = vec3(vOrMvMatrix * vec4(inPosition.xyz, 1.0)).xyz;'nl
'    mat3 m = createRotate(vec3(inRotationXY.x, inRotationXY.z, inSizeRotation.z));'nl
'    vec4 p = vec4(m * (inVertex * vec3(scaleX, scaleY, scaleZ) * vec3(inSizeRotation.x)) + center, 1.0);'nl
'    fragFogCoord = abs(p.z / p.w);'nl
'    gl_Position = pMatrix * p;'nl
'  } else'nl
'    gl_Position = vec4(-1.0, -1.0, -1.0, 1.0);'nl // Discard this vertex by making it outside of clip plane
'}';

  FragmentShaderSourceQuad: String =
'#version 330'nl
'precision lowp float;'nl
'in vec2 fragTexCoord;'nl
'in vec4 fragColor;'nl
'in float fragFogCoord;'nl

'out vec4 outColor;'nl

'uniform sampler2D baseColor;'nl
'uniform int fogEnable;'nl
'uniform float fogEnd;'nl
'uniform vec3 fogColor;'nl

'void main() {'nl
'  outColor = texture(baseColor, fragTexCoord) * fragColor;'nl
'  if (fogEnable == 1) {'nl
'    float fogFactor = (fogEnd - fragFogCoord) / fogEnd;'nl
'    outColor.rgb = mix(fogColor, outColor.rgb, clamp(fogFactor, 0.0, 1.0));'nl
'  }'nl
'  outColor.rgb *= outColor.a;'nl
'}';

  VertexShaderSourceMesh: String =
'#version 330'nl
'layout(location = 0) in vec4 inPosition;'nl
'layout(location = 1) in vec4 inTimeToLive;'nl
'layout(location = 2) in vec4 inSizeRotation;'nl
'layout(location = 3) in vec4 inColor;'nl
'layout(location = 9) in vec4 inRotationXY;'nl
'layout(location = 13) in vec3 inVertex;'nl
'layout(location = 14) in vec2 inTexcoord;'nl

'out vec2 fragTexCoord;'nl
'out vec4 fragColor;'nl
'out float fragFogCoord;'nl

'uniform mat4 vOrMvMatrix;'nl
'uniform mat4 pMatrix;'nl
'uniform float scaleX;'nl
'uniform float scaleY;'nl
'uniform float scaleZ;'nl

'mat3 createRotate(vec3 p) {'nl
'  float cr = cos(p.x);'nl
'  float sr = sin(p.x);'nl
'  float cp = cos(p.y);'nl
'  float sp = sin(p.y);'nl
'  float cy = cos(p.z);'nl
'  float sy = sin(p.z);'nl
'  return mat3('nl
'    cp * cy,'nl
'    cp * sy,'nl
'    - sp,'nl
'    sr * sp * cy - cr * sy,'nl
'    sr * sp * sy + cr * cy,'nl
'    sr * cp,'nl
'    cr * sp * cy + sr * sy,'nl
'    cr * sp * sy - sr * cy,'nl
'    cr * cp'nl
'  );'nl
'}'nl

'void main() {'nl
'  if (inTimeToLive.x > 0.0) {'nl
'    fragTexCoord = inTexcoord;'nl
'    fragColor = inColor;'nl
'    mat3 m = createRotate(vec3(inRotationXY.x, inRotationXY.z, inSizeRotation.z));'nl
'    vec4 p = vOrMvMatrix * vec4(m * inVertex * vec3(scaleX, scaleY, scaleZ) * vec3(inSizeRotation.x) + inPosition.xyz, 1.0);'nl
'    fragFogCoord = abs(p.z / p.w);'nl
'    gl_Position = pMatrix * p;'nl
'  } else'nl
'    gl_Position = vec4(-1.0, -1.0, -1.0, 1.0);'nl // Discard this vertex by making it outside of clip plane
'}';

  FragmentShaderSourceMesh: String =
'#version 330'nl
'precision lowp float;'nl
'in vec2 fragTexCoord;'nl
'in vec4 fragColor;'nl
'in float fragFogCoord;'nl

'out vec4 outColor;'nl

'uniform sampler2D baseColor;'nl
'uniform int fogEnable;'nl
'uniform float fogEnd;'nl
'uniform vec3 fogColor;'nl

'void main() {'nl
'  outColor = texture(baseColor, fragTexCoord) * fragColor;'nl
'  if (fogEnable == 1) {'nl
'    float fogFactor = (fogEnd - fragFogCoord) / fogEnd;'nl
'    outColor.rgb = mix(fogColor, outColor.rgb, clamp(fogFactor, 0.0, 1.0));'nl
'  }'nl
'  outColor.rgb *= outColor.a;'nl
'}';

  Varyings: array[0..9] of PChar = (
    'outPosition',
    'outTimeToLive',
    'outSizeRotation',
    'outColor',
    'outColorDelta',
    'outStartPos',
    'outVelocity',
    'outDirection',
    'outTranslate',
    'outRotationXY'
  );

  // Built-in particle vertices & texcoords
  BuiltInVertexArray: packed array[0..5] of TVector3 = (
    (Data: (-0.5, -0.5, 0)), (Data: (0.5, -0.5, 0)), (Data: (0.5, 0.5, 0)),
    (Data: (-0.5, -0.5, 0)), (Data: (0.5, 0.5, 0)), (Data: (-0.5, 0.5, 0))
  );
  BuiltInTexcoordArray: packed array[0..5] of TVector2 = (
    (Data: (0, 0)), (Data: (1, 0)), (Data: (1, 1)),
    (Data: (0, 0)), (Data: (1, 1)), (Data: (0, 1))
  );

var
  IsCheckedForUsable: Boolean = False;
  TransformFeedbackProgram: TGLSLProgram = nil;
  TransformFeedbackProgramSingleInstance: TGLSLProgram = nil;
  TransformFeedbackProgramMultipleInstances: TGLSLProgram = nil;
  RenderProgram: TGLSLProgram = nil;
  RenderProgramMesh: TGLSLProgram = nil;
  RenderProgramQuad: TGLSLProgram = nil;

{ Call when OpenGL context is closed }
procedure FreeGLContext;
begin
  if TransformFeedbackProgramSingleInstance <> nil then
  begin
    FreeAndNil(TransformFeedbackProgramSingleInstance);
    FreeAndNil(TransformFeedbackProgramMultipleInstances);
    FreeAndNil(RenderProgramQuad);
    FreeAndNil(RenderProgramMesh);
  end;
end;

function CreateVec3Persistent(const G: TGetVector3Event; const S: TSetVector3Event; const ADefaultValue: TVector3): TCastleVector3Persistent;
begin
  Result := TCastleVector3Persistent.Create;
  Result.InternalGetValue := G;
  Result.InternalSetValue := S;
  Result.InternalDefaultValue := ADefaultValue;
end;

function CreateColorPersistent(const G: TGetVector4Event; const S: TSetVector4Event; const ADefaultValue: TVector4): TCastleColorPersistent;
begin
  Result := TCastleColorPersistent.Create;
  Result.InternalGetValue := G;
  Result.InternalSetValue := S;
  Result.InternalDefaultValue := ADefaultValue;
end;

procedure TCastleParticleViewport.RenderFromViewEverything(const RenderingCamera: TRenderingCamera);
var
  SR: TRectangle;
begin
  Self.ReconstructBuffer;
  //
  Self.FImage.RenderToImageBegin;
  inherited;
  Self.FImage.RenderToImageEnd;
  // Final result
  if Self.Visible then
  begin
    SR := Self.RenderRect.Round;
    Self.FImage.Draw(0, 0, SR.Width, SR.Height);
  end;
end;

function TCastleParticleViewport.PropertySections(const PropertyName: String): TPropertySections;
begin
  case PropertyName of
    'TextureWidth',
    'TextureHeight',
    'Visible':
      Result := [psBasic];
    else
      Result := inherited PropertySections(PropertyName);
  end;
end;

constructor TCastleParticleViewport.Create(AOwner: TComponent);
begin
  inherited;
  Self.FImage := nil;
  Self.Visible := True;
  Self.TextureWidth := 256;
  Self.TextureHeight := 256;
end;

destructor TCastleParticleViewport.Destroy;
begin
  FreeAndNil(Self.FImage);
  inherited;
end;

procedure TCastleParticleViewport.ReconstructBuffer;
begin
  // Reconstruct the buffers when width/height change
  if (Self.FImage = nil) or (Self.FImage.Width <> Self.FTextureWidth) or (Self.FImage.Height <> Self.FTextureHeight) then
  begin
    if Self.FImage <> nil then
    begin
      FreeAndNil(Self.FImage);
    end;
    Self.FImage := TDrawableImage.Create(TRGBAlphaImage.Create(Self.FTextureWidth, Self.FTextureHeight), True, True);
    Self.FImage.RepeatS := True;
    Self.FImage.RepeatT := True;
  end;
end;

procedure TCastleParticleEffect.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Self.FTextureViewport) then
  begin
    Self.FTextureViewport := nil;
    Self.IsNeedRefresh := True;
  end;
end;

// ---------------------------------

procedure TCastleParticleEffectAnchorItem.SetColorForPersistent(const AValue: TVector4);
begin
  Self.FColor := AValue;
end;

function TCastleParticleEffectAnchorItem.GetColorForPersistent: TVector4;
begin
  Result := Self.FColor;
end;

procedure TCastleParticleEffectAnchorItem.SetColorVarianceForPersistent(const AValue: TVector4);
begin
  Self.FColorVariance := AValue;
end;

function TCastleParticleEffectAnchorItem.GetColorVarianceForPersistent: TVector4;
begin
  Result := Self.FColorVariance;
end;

procedure TCastleParticleEffectAnchorItem.SetTimeNormalized(const AValue: Single);
begin
  Self.FTimeNormalized := Min(1, Max(0, AValue));
end;

constructor TCastleParticleEffectAnchorItem.Create(AClass: TCollection);
begin
  inherited;
  Self.FColor := Vector4(1, 1, 1, 1);
  Self.FSize := 1;
  Self.FColorPersistent := CreateColorPersistent(
    @Self.GetColorForPersistent,
    @Self.SetColorForPersistent,
    Self.FColor
  );
  Self.FColorVariancePersistent := CreateColorPersistent(
    @Self.GetColorVarianceForPersistent,
    @Self.SetColorVarianceForPersistent,
    Self.FColorVariance
  );
  Self.FTimeNormalized := 1;
end;

destructor TCastleParticleEffectAnchorItem.Destroy;
begin
  FreeAndNil(Self.FColorPersistent);
  FreeAndNil(Self.FColorVariancePersistent);
  inherited;
end;

// ---------------------------------

procedure TCastleParticleEffect.SetBoundingBoxMinForPersistent(const AValue: TVector3);
begin
  Self.FBBox := Box3D(AValue, Self.FBBox.Data[1]);
end;

function TCastleParticleEffect.GetBoundingBoxMinForPersistent: TVector3;
begin
  Result := Self.FBBox.Data[0];
end;

procedure TCastleParticleEffect.SetBoundingBoxMaxForPersistent(const AValue: TVector3);
begin
  Self.FBBox := Box3D(Self.FBBox.Data[0], AValue);
end;

function TCastleParticleEffect.GetBoundingBoxMaxForPersistent: TVector3;
begin
  Result := Self.FBBox.Data[1];
end;

procedure TCastleParticleEffect.SetRotationForPersistent(const AValue: TVector3);
begin
  Self.FRotation := AValue;
end;

function TCastleParticleEffect.GetRotationForPersistent: TVector3;
begin
  Result := Self.FRotation;
end;

procedure TCastleParticleEffect.SetRotationVarianceForPersistent(const AValue: TVector3);
begin
  Self.FRotationVariance := AValue;
end;

function TCastleParticleEffect.GetRotationVarianceForPersistent: TVector3;
begin
  Result := Self.FRotationVariance;
end;

procedure TCastleParticleEffect.SetRotationSpeedForPersistent(const AValue: TVector3);
begin
  Self.FRotationSpeed := AValue;
end;

function TCastleParticleEffect.GetRotationSpeedForPersistent: TVector3;
begin
  Result := Self.FRotationSpeed;
end;

procedure TCastleParticleEffect.SetRotationSpeedVarianceForPersistent(const AValue: TVector3);
begin
  Self.FRotationSpeedVariance := AValue;
end;

function TCastleParticleEffect.GetRotationSpeedVarianceForPersistent: TVector3;
begin
  Result := Self.FRotationSpeedVariance;
end;

procedure TCastleParticleEffect.SetSourcePositionForPersistent(const AValue: TVector3);
begin
  Self.FSourcePosition := AValue;
end;

function TCastleParticleEffect.GetSourcePositionForPersistent: TVector3;
begin
  Result := Self.FSourcePosition;
end;

procedure TCastleParticleEffect.SetSourcePositionVarianceForPersistent(const AValue: TVector3);
begin
  Self.FSourcePositionVariance := AValue;
end;

function TCastleParticleEffect.GetSourcePositionVarianceForPersistent: TVector3;
begin
  Result := FSourcePositionVariance;
end;

procedure TCastleParticleEffect.SetDirectionForPersistent(const AValue: TVector3);
begin
  Self.FDirection := AValue.Normalize;
end;

function TCastleParticleEffect.GetDirectionForPersistent: TVector3;
begin
  Result := Self.FDirection;
end;

procedure TCastleParticleEffect.SetGravityForPersistent(const AValue: TVector3);
begin
  Self.FGravity := AValue;
end;

function TCastleParticleEffect.GetGravityForPersistent: TVector3;
begin
  Result := Self.FGravity;
end;

procedure TCastleParticleEffect.SetColorForPersistent(const AValue: TVector4);
begin
  Self.FColor := AValue;
end;

function TCastleParticleEffect.GetColorForPersistent: TVector4;
begin
  Result := Self.FColor;
end;

procedure TCastleParticleEffect.SetColorVarianceForPersistent(const AValue: TVector4);
begin
  Self.FColorVariance := AValue;
end;

function TCastleParticleEffect.GetColorVarianceForPersistent: TVector4;
begin
  Result := Self.FColorVariance;
end;

procedure TCastleParticleEffect.SetTextureViewport(const AValue: TCastleParticleViewport);
begin
  Self.FTextureViewport := AValue;
  Self.IsNeedRefresh := True;
  if AValue <> nil then
  begin
    AValue.FreeNotification(Self);
  end;
end;

procedure TCastleParticleEffect.SetMesh(const AValue: String);
begin
  Self.FMesh := AValue;
  Self.IsNeedRefresh := True;
end;

procedure TCastleParticleEffect.SetTexture(const AValue: String);
begin
  Self.FTexture := AValue;
  Self.IsNeedRefresh := True;
end;

procedure TCastleParticleEffect.SetMaxParticle(const AValue: Integer);
begin
  Self.FMaxParticles := Max(AValue, 0);
  Self.IsNeedRefresh := True;
end;

procedure TCastleParticleEffect.SetDuration(const AValue: Single);
begin
  Self.FDuration := AValue;
  Self.IsNeedRefresh := True;
end;

function TCastleParticleEffect.PropertySections(const PropertyName: String): TPropertySections;
begin
  case PropertyName of
    'Tag':
      Result := inherited PropertySections(PropertyName);
    else
      Result := [psBasic];
  end;
end;

constructor TCastleParticleEffect.Create(AOwner: TComponent);
begin
  inherited;
  Self.BBox := TBox3D.Empty;
  Self.FTexture := '';
  Self.FMaxParticles := 100;
  Self.FDuration := -1;
  Self.FLifeSpan := 1;
  Self.FLifeSpanVariance := 0.5;
  Self.FColor := Vector4(1, 1, 1, 1);
  Self.FSize := 1;
  Self.FSourcePositionVariance := Vector3(0.02, 0.02, 0.02);
  Self.FDirection := Vector3(0, 1, 0);
  Self.FDirectionVariance := 0.4;
  Self.FSpeed := 3;
  Self.FSpeedVariance := 1;
  Self.FBlendFuncSource := pbmOne;
  Self.FBlendFuncDestination := pbmOne;
  Self.FSourceType := pstBox;
  //
  Self.FBoundingBoxMinPersistent := CreateVec3Persistent(
    @Self.GetBoundingBoxMinForPersistent,
    @Self.SetBoundingBoxMinForPersistent,
    Self.BBox.Data[0]
  );
  Self.FBoundingBoxMaxPersistent := CreateVec3Persistent(
    @Self.GetBoundingBoxMaxForPersistent,
    @Self.SetBoundingBoxMaxForPersistent,
    Self.BBox.Data[1]
  );
  Self.FRotationPersistent := CreateVec3Persistent(
    @Self.GetRotationForPersistent,
    @Self.SetRotationForPersistent,
    Self.FRotation
  );
  Self.FRotationVariancePersistent := CreateVec3Persistent(
    @Self.GetRotationVarianceForPersistent,
    @Self.SetRotationVarianceForPersistent,
    Self.FRotationVariance
  );
  Self.FRotationSpeedPersistent := CreateVec3Persistent(
    @Self.GetRotationSpeedForPersistent,
    @Self.SetRotationSpeedForPersistent,
    Self.FRotation
  );
  Self.FRotationSpeedVariancePersistent := CreateVec3Persistent(
    @Self.GetRotationSpeedVarianceForPersistent,
    @Self.SetRotationSpeedVarianceForPersistent,
    Self.FRotationSpeedVariance
  );
  Self.FSourcePositionPersistent := CreateVec3Persistent(
    @Self.GetSourcePositionForPersistent,
    @Self.SetSourcePositionForPersistent,
    Self.FSourcePosition
  );
  Self.FSourcePositionVariancePersistent := CreateVec3Persistent(
    @Self.GetSourcePositionVarianceForPersistent,
    @Self.SetSourcePositionVarianceForPersistent,
    Self.FSourcePositionVariance
  );
  Self.FDirectionPersistent := CreateVec3Persistent(
    @Self.GetDirectionForPersistent,
    @Self.SetDirectionForPersistent,
    Self.FDirection
  );
  Self.FGravityPersistent := CreateVec3Persistent(
    @Self.GetGravityForPersistent,
    @Self.SetGravityForPersistent,
    Self.FGravity
  );
  Self.FColorPersistent := CreateColorPersistent(
    @Self.GetColorForPersistent,
    @Self.SetColorForPersistent,
    Self.FColor
  );
  Self.FColorVariancePersistent := CreateColorPersistent(
    @Self.GetColorVarianceForPersistent,
    @Self.SetColorVarianceForPersistent,
    Self.FColorVariance
  );
  Self.FAnchors := TCollection.Create(TCastleParticleEffectAnchorItem);
end;

destructor TCastleParticleEffect.Destroy;
begin
  FreeAndNil(Self.FAnchors);
  FreeAndNil(Self.FBoundingBoxMinPersistent);
  FreeAndNil(Self.FBoundingBoxMaxPersistent);
  FreeAndNil(Self.FRotationPersistent);
  FreeAndNil(Self.FRotationVariancePersistent);
  FreeAndNil(Self.FRotationSpeedPersistent);
  FreeAndNil(Self.FRotationSpeedVariancePersistent);
  FreeAndNil(Self.FSourcePositionPersistent);
  FreeAndNil(Self.FSourcePositionVariancePersistent);
  FreeAndNil(Self.FDirectionPersistent);
  FreeAndNil(Self.FGravityPersistent);
  FreeAndNil(Self.FColorPersistent);
  FreeAndNil(Self.FColorVariancePersistent);
  inherited;
end;

procedure TCastleParticleEffect.Load(const AURL: String; const IsTexturePathRelative: Boolean = False);
var
  MS: TStream;
  DeStreamer: TJSONDeStreamer;
  SS: TStringStream;
begin
  MS := Download(AURL);
  SS := TStringStream.Create('');
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    MS.Position := 0;
    SS.CopyFrom(MS, MS.Size);
    DeStreamer.JSONToObject(SS.DataString, Self);
    if IsTexturePathRelative then
      Self.Texture := ExtractURIPath(AURL) + Self.Texture;
  finally
    FreeAndNil(DeStreamer);
    FreeAndNil(SS);
    FreeAndNil(MS);
  end;
end;

procedure TCastleParticleEffect.Save(const AURL: String; const IsTexturePathRelative: Boolean = False);
var
  FS: TFileStream;
  Streamer: TJSONStreamer;
  SS: TStringStream;
  S: String;
begin
  FS := URLSaveStream(AURL) as TFileStream;
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoUseFormatString];
    if IsTexturePathRelative then
    begin
      S := Self.Texture;
      Self.Texture := ExtractFileName(Self.Texture);
    end;
    SS := TStringStream.Create(Streamer.ObjectToJSONString(Self));
    if IsTexturePathRelative then
      Self.Texture := S;
    try
      SS.Position := 0;
      FS.CopyFrom(SS, SS.Size);
    finally
      FreeAndNil(SS);
    end;
  finally
    FreeAndNil(Streamer);
    FreeAndNil(FS);
  end;
end;

// ---------------------------------

procedure TCastleParticleEmitter.SetStartEmitting(V: Boolean);
begin
  Self.FStartEmitting := V;
  if V and Assigned(FEffect) then
    Self.FCountdownTillRemove := Self.FEffect.LifeSpan + Self.FEffect.LifeSpanVariance;
end;

procedure TCastleParticleEmitter.SetBurst(V: Boolean);
begin
  Self.FBurst := V;
  Self.FIsNeedRefresh := True;
end;

procedure TCastleParticleEmitter.SetSmoothTexture(V: Boolean);
begin
  Self.FSmoothTexture := V;
  Self.FIsNeedRefresh := True;
end;

constructor TCastleParticleEmitter.Create(AOwner: TComponent);
{$ifdef CASTLE_DESIGN_MODE}
var
  DebugScene: TCastleScene;
  MainRoot: TX3DRootNode;
{$endif}
begin
  inherited;
  Self.FIsUpdated := False;
  Self.Texture := 0;
  Self.FSecondsPassed := 0;
  Self.Scale := Vector3(1, 1, 1);
  Self.FIsGLContextInitialized := False;
  Self.FIsNeedRefresh := False;
  Self.FAllowsUpdateWhenCulled := True;
  Self.FAllowsInstancing := False;
  Self.FBurst := False;
  Self.FSmoothTexture := True;
  Self.FStartEmitting := True;
  FTimePlaying := true;
  FTimePlayingSpeed := 1.0;
  Self.FColorList := TVector4List.Create;
  Self.FColorVarianceList := TVector4List.Create;
  Self.FSizeList := TSingleList.Create;
  Self.FSizeVarianceList := TSingleList.Create;
  Self.FAnchorList := TSingleList.Create;
  Self.FColorList.Capacity := 5;
  Self.FColorVarianceList.Capacity := 5;
  Self.FSizeList.Capacity := 5;
  Self.FSizeVarianceList.Capacity := 5;
  Self.FAnchorList.Capacity := 5;
  {$ifdef CASTLE_DESIGN_MODE}
  DebugScene := TCastleScene.Create(Self);
  DebugScene.SetTransient;
  DebugScene.Pickable := False;
  Self.FDebugBox := TDebugBox.Create(Self);
  Self.FDebugBox.Color := Vector4(0, 1, 0, 0.5);
  MainRoot := TX3DRootNode.Create;
  MainRoot.AddChildren(Self.FDebugBox.Root);
  DebugScene.Load(MainRoot, True);
  Self.Add(DebugScene);
  {$endif}
end;

destructor TCastleParticleEmitter.Destroy;
begin
  Self.FAnchorList.Free;
  Self.FColorList.Free;
  Self.FColorVarianceList.Free;
  Self.FSizeList.Free;
  Self.FSizeVarianceList.Free;
  inherited;
end;

procedure TCastleParticleEmitter.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  I: Integer;
  S: Single;
  AnchorItem: TCastleParticleEffectAnchorItem;
  AttractorCount: Integer = 0;
  AttractorValue: TVector4;
  M: TMatrix4;
begin
  inherited;
  if (not Self.Exists) or (not Assigned(Self.FEffect)) then
    Exit;
  Self.GLContextOpen;
  if Self.FIsNeedRefresh or Self.FEffect.IsNeedRefresh then
    Self.InternalRefreshEffect;
  //if not Self.ProcessEvents then
  //  Exit;

  Self.FSecondsPassed := SecondsPassed;

  if not Self.FIsUpdated then
  begin
    if (FEmissionTime > 0) or (FEmissionTime = -1) then
    begin
      if FEmissionTime > 0 then
        if Self.TimePlaying then
          FEmissionTime := Max(0, FEmissionTime - SecondsPassed * Self.TimePlayingSpeed)
        else
          FEmissionTime := Max(0, FEmissionTime - SecondsPassed);
    end;

    if not Self.FStartEmitting then
    begin
      if Self.TimePlaying then
        Self.FCountdownTillRemove := Self.FCountdownTillRemove - SecondsPassed * Self.TimePlayingSpeed
      else
        Self.FCountdownTillRemove := Self.FCountdownTillRemove - SecondsPassed;
    end;

    if ((not Self.FAllowsUpdateWhenCulled) and Self.FIsDrawn) or Self.FAllowsUpdateWhenCulled then
    begin
      if Self.AllowsInstancing then
        TransformFeedbackProgram := TransformFeedbackProgramMultipleInstances
      else
        TransformFeedbackProgram := TransformFeedbackProgramSingleInstance;
      glEnable(GL_RASTERIZER_DISCARD);
      TransformFeedbackProgram.Enable;
      if Self.TimePlaying then
        TransformFeedbackProgram.Uniform('deltaTime').SetValue(Self.FSecondsPassed * Self.TimePlayingSpeed)
      else
        TransformFeedbackProgram.Uniform('deltaTime').SetValue(Self.FSecondsPassed);
      if Self.FStartEmitting then
        TransformFeedbackProgram.Uniform('emissionTime').SetValue(Self.FEmissionTime)
      else
        TransformFeedbackProgram.Uniform('emissionTime').SetValue(0);
      if not Self.AllowsInstancing then
      begin
        TransformFeedbackProgram.Uniform('mMatrix').SetValue(Self.WorldTransform);
        // World attractor
        if Self.FAttractor <> nil then
        begin
          M := Self.FAttractor.WorldTransform;
          AttractorValue := Vector4(Vector3(M[3,0], M[3,1], M[3,2]), -Self.FAttractor.Attraction);
          TransformFeedbackProgram.Uniform('effect.attractorCount').SetValue(1);
          TransformFeedbackProgram.Uniform('effect.attractor').SetValue(AttractorValue);
        end else
          TransformFeedbackProgram.Uniform('effect.attractorCount').SetValue(0);
      end else
      begin
        // Local attractor
        for I := 0 to Self.Count - 1 do
        begin
          if Self.Items[I].Exists and Self.Items[I].Visible and (Self.Items[I] is TCastleParticleAttractor) then
          begin
            AttractorValue := Vector4(Self.Items[I].Translation, -TCastleParticleAttractor(Self.Items[I]).Attraction);
            AttractorCount := 1;
            Break;
          end;
        end;
        TransformFeedbackProgram.Uniform('effect.attractorCount').SetValue(AttractorCount);
        TransformFeedbackProgram.Uniform('effect.attractor').SetValue(AttractorValue);
      end;
      TransformFeedbackProgram.Uniform('effect.sourceType').SetValue(CastleParticleSourceValues[Self.FEffect.SourceType]);
      TransformFeedbackProgram.Uniform('effect.sourcePosition').SetValue(Self.FEffect.SourcePosition);
      TransformFeedbackProgram.Uniform('effect.sourcePositionVariance').SetValue(Self.FEffect.SourcePositionVariance);
      TransformFeedbackProgram.Uniform('effect.maxParticles').SetValue(Self.FEffect.MaxParticles);

      // Build list of anchors
      Self.FAnchorList.Count := 0;
      Self.FColorList.Count := 0;
      Self.FColorVarianceList.Count := 0;
      Self.FSizeList.Count := 0;
      Self.FSizeVarianceList.Count := 0;
      Self.FAnchorList.Add(0);
      Self.FSizeList.Add(Self.FEffect.Size);
      Self.FSizeVarianceList.Add(Self.FEffect.SizeVariance);
      Self.FColorList.Add(Self.FEffect.Color);
      Self.FColorVarianceList.Add(Self.FEffect.ColorVariance);
      for I := 0 to Self.FEffect.Anchors.Count - 1 do
      begin
        // We limit number of anchors to 4 at the moment
        if I >= 4 then Break;
        AnchorItem := TCastleParticleEffectAnchorItem(Self.FEffect.Anchors.Items[I]);
        Self.FAnchorList.Add(Min(0.99, Max(0.01, AnchorItem.TimeNormalized)));
        Self.FSizeList.Add(AnchorItem.Size);
        Self.FSizeVarianceList.Add(AnchorItem.SizeVariance);
        Self.FColorList.Add(AnchorItem.Color);
        Self.FColorVarianceList.Add(AnchorItem.ColorVariance);
      end;

      TransformFeedbackProgram.Uniform('effect.anchorCount').SetValue(Self.FAnchorList.Count);
      TransformFeedbackProgram.Uniform('effect.anchor').SetValue(Self.FAnchorList);
      TransformFeedbackProgram.Uniform('effect.anchorColor').SetValue(Self.FColorList);
      TransformFeedbackProgram.Uniform('effect.anchorColorVariance').SetValue(Self.FColorVarianceList);
      TransformFeedbackProgram.Uniform('effect.anchorSize').SetValue(Self.FSizeList);
      TransformFeedbackProgram.Uniform('effect.anchorSizeVariance').SetValue(Self.FSizeVarianceList);

      TransformFeedbackProgram.Uniform('effect.particleLifeSpan').SetValue(Self.FEffect.LifeSpan);
      TransformFeedbackProgram.Uniform('effect.particleLifeSpanVariance').SetValue(Self.FEffect.LifeSpanVariance);
      TransformFeedbackProgram.Uniform('effect.rotation').SetValue(Self.FEffect.Rotation);
      TransformFeedbackProgram.Uniform('effect.rotationVariance').SetValue(Self.FEffect.RotationVariance);
      TransformFeedbackProgram.Uniform('effect.rotationSpeed').SetValue(Self.FEffect.RotationSpeed);
      TransformFeedbackProgram.Uniform('effect.rotationSpeedVariance').SetValue(Self.FEffect.RotationSpeedVariance);
      TransformFeedbackProgram.Uniform('effect.direction').SetValue(Self.FEffect.Direction);
      TransformFeedbackProgram.Uniform('effect.directionVariance').SetValue(Self.FEffect.DirectionVariance);
      TransformFeedbackProgram.Uniform('effect.speed').SetValue(Self.FEffect.Speed);
      TransformFeedbackProgram.Uniform('effect.speedVariance').SetValue(Self.FEffect.SpeedVariance);
      TransformFeedbackProgram.Uniform('effect.gravity').SetValue(Self.FEffect.Gravity);
      TransformFeedbackProgram.Uniform('effect.radial').SetValue(Self.FEffect.Radial);
      TransformFeedbackProgram.Uniform('effect.radialVariance').SetValue(Self.FEffect.RadialVariance);
      glBindVertexArray(Self.VAOs[CurrentBuffer]);
      glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, Self.VBOs[(CurrentBuffer + 1) mod 2]);
      glBeginTransformFeedback(GL_POINTS);
      glDrawArrays(GL_POINTS, 0, Self.FEffect.MaxParticles);
      glEndTransformFeedback();
      glDisable(GL_RASTERIZER_DISCARD);
      glBindVertexArray(0); // Just in case :)
      CurrentBuffer := (CurrentBuffer + 1) mod 2;
    end;
  end;

  RemoveMe := rtNone;
  if Self.FReleaseWhenDone then
  begin
    if (Self.FEmissionTime = 0) then
    begin
      if not Self.FIsUpdated then
      begin
        if Self.TimePlaying then
          Self.FCountdownTillRemove := Self.FCountdownTillRemove - SecondsPassed * Self.TimePlayingSpeed
        else
          Self.FCountdownTillRemove := Self.FCountdownTillRemove - SecondsPassed;
      end;
      if (Self.FCountdownTillRemove <= 0) then
      begin
        RemoveMe := rtRemoveAndFree;
      end;
    end;
  end;
  Self.FIsUpdated := True;
  Self.FIsDrawn := False;
end;

procedure TCastleParticleEmitter.LocalRender(const Params: TRenderParams);
var
  BoundingBoxMin, BoundingBoxMax,
  RenderCameraPosition: TVector3;
  RelativeBBox: TBox3D;
  M: TMatrix4;
  IndicesCount: Integer;
  Fog: TFogFunctionality;
begin
  inherited;
  Self.FIsUpdated := False;
  if (not Self.Exists) or (not Assigned(Self.FEffect)) then
    Exit;
  if not Self.FIsGLContextInitialized then
    Exit;
  if (not Self.Visible) or Params.InShadow or (Params.StencilTest > 0) then
    Exit;
  if not Params.Transparent then
    Exit;
  if (not Self.FStartEmitting) and (Self.FCountdownTillRemove <= 0) then
    Exit;
  if not Self.ExcludeFromStatistics then
    Inc(Params.Statistics.ScenesVisible);
  if DistanceCulling > 0 then
  begin
    RenderCameraPosition := Params.InverseTransform^.MultPoint(Params.RenderingCamera.Position);
    if RenderCameraPosition.Length > DistanceCulling + LocalBoundingBox.Radius then
      Exit;
  end;
  if not Self.FEffect.BBox.IsEmpty then
  begin
    RelativeBBox := Box3D(
      Self.FEffect.BBox.Data[0],
      Self.FEffect.BBox.Data[1]
    );
    if not Params.Frustum^.Box3DCollisionPossibleSimple(RelativeBBox) then
      Exit;
  end;
  Self.FIsDrawn := True;
  if not Self.ExcludeFromStatistics then
  begin
    Inc(Params.Statistics.ShapesVisible);
    Inc(Params.Statistics.ShapesRendered);
    Inc(Params.Statistics.ScenesRendered);
  end;

  // Draw particles
  if Self.AllowsInstancing then
    TransformFeedbackProgram := TransformFeedbackProgramMultipleInstances
  else
    TransformFeedbackProgram := TransformFeedbackProgramSingleInstance;
  if Self.FEffect.Mesh = '' then
    RenderProgram := RenderProgramQuad
  else
    RenderProgram := RenderProgramMesh;
  if not Self.FAllowsWriteToDepthBuffer then
    glDepthMask(GL_FALSE)
  else
    glDepthMask(GL_TRUE);
  // Get global fog
  if Self.FEnableFog and (Params.GlobalFog <> nil) then
    Fog := (Params.GlobalFog as TFogNode).Functionality(TFogFunctionality) as TFogFunctionality
  else
    Fog := nil;
  //
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(CastleParticleBlendValues[Self.FEffect.BlendFuncSource], CastleParticleBlendValues[Self.FEffect.BlendFuncDestination]);
  RenderProgram.Enable;
  RenderProgram.Uniform('scaleX').SetValue(Vector3(Params.Transform^[0,0], Params.Transform^[0,1], Params.Transform^[0,2]).Length);
  RenderProgram.Uniform('scaleY').SetValue(Vector3(Params.Transform^[1,0], Params.Transform^[1,1], Params.Transform^[1,2]).Length);
  RenderProgram.Uniform('scaleZ').SetValue(Vector3(Params.Transform^[2,0], Params.Transform^[2,1], Params.Transform^[2,2]).Length);
  if (Fog <> nil) then
  begin
    // TODO: More type of fog
    RenderProgram.Uniform('fogEnable').SetValue(1);
    RenderProgram.Uniform('fogEnd').SetValue(Fog.VisibilityRange);
    RenderProgram.Uniform('fogColor').SetValue(Fog.Color);
  end else
  begin
    RenderProgram.Uniform('fogEnable').SetValue(0);
  end;
  if Self.AllowsInstancing then
  begin
    M := Params.RenderingCamera.Matrix * Params.Transform^;
    RenderProgram.Uniform('vOrMvMatrix').SetValue(M);
  end else
  begin
    RenderProgram.Uniform('vOrMvMatrix').SetValue(Params.RenderingCamera.Matrix);
  end;
  RenderProgram.Uniform('pMatrix').SetValue(RenderContext.ProjectionMatrix);
  glBindVertexArray(Self.VAOMeshes[CurrentBuffer]);
  glActiveTexture(GL_TEXTURE0);
  if Self.FEffect.TextureViewport = nil then
    glBindTexture(GL_TEXTURE_2D, Self.Texture)
  else
  if Self.FEffect.TextureViewport.Image <> nil then
    glBindTexture(GL_TEXTURE_2D, Self.FEffect.TextureViewport.Image.Texture);
  IndicesCount := Length(Self.ParticleMeshIndices);
  if IndicesCount = 0 then
    glDrawArraysInstanced(GL_TRIANGLES, 0, Length(Self.ParticleMesh), Self.FEffect.MaxParticles)
  else
    glDrawElementsInstanced(GL_TRIANGLES, IndicesCount, GL_UNSIGNED_INT, nil, Self.FEffect.MaxParticles);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindVertexArray(0);
  // Render boundingbox in editor
  {$ifdef CASTLE_DESIGN_MODE}
  Self.FDebugBox.Box := Self.FEffect.BBox;
  {$endif}
  glDisable(GL_BLEND);
  // Which pass is this?
  glDisable(GL_DEPTH_TEST);
  if not Self.FAllowsWriteToDepthBuffer then
    glDepthMask(GL_TRUE);
end;

procedure TCastleParticleEmitter.LoadEffect(const AEffect: TCastleParticleEffect);
begin
  Self.FEffect := AEffect;
  if AEffect <> nil then
  begin
    AEffect.FreeNotification(Self);
  end;
  RefreshEffect;
end;

procedure TCastleParticleEmitter.SetAttractor(const AValue: TCastleParticleAttractor);
begin
  Self.FAttractor := AValue;
  if AValue <> nil then
  begin
    AValue.FreeNotification(Self);
  end;
end;

function TCastleParticleEmitter.PropertySections(const PropertyName: String): TPropertySections;
begin
  case PropertyName of
    'Burst',
    'Attractor',
    'AllowsInstancing',
    'StartEmitting',
    'Effect':
      Result := [psBasic];
    else
      Result := inherited PropertySections(PropertyName);
  end;
end;

procedure TCastleParticleEmitter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Self.FEffect then
      Self.LoadEffect(nil)
    else
    if AComponent = Self.FAttractor then
      Self.FAttractor := nil;
  end;
end;

procedure TCastleParticleEmitter.GLContextOpen;
var
  V: Integer;
begin
  // Safeguard
  if not ApplicationProperties.IsGLContextOpen then Exit;
  if Self.FIsGLContextInitialized then Exit;

  if not IsCheckedForUsable then
  begin
    // Check maximum number of vertex attributes
    glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, @V);
    WritelnLog('GL_MAX_VERTEX_ATTRIBS: ' + IntToStr(V));
    if V < 16 then
      raise Exception.Create('TCastleParticleEmitter requires GL_MAX_VERTEX_ATTRIBS at least 16');
    IsCheckedForUsable := True;
  end;

  if TransformFeedbackProgramSingleInstance = nil then
  begin
    TransformFeedbackProgramSingleInstance := TGLSLProgram.Create;
    TransformFeedbackProgramSingleInstance.AttachVertexShader(TransformVertexShaderSourceSingleInstance);
    TransformFeedbackProgramSingleInstance.SetTransformFeedbackVaryings(Varyings);
    TransformFeedbackProgramSingleInstance.Link;

    TransformFeedbackProgramMultipleInstances := TGLSLProgram.Create;
    TransformFeedbackProgramMultipleInstances.AttachVertexShader(TransformVertexShaderSourceMultipleInstances);
    TransformFeedbackProgramMultipleInstances.SetTransformFeedbackVaryings(Varyings);
    TransformFeedbackProgramMultipleInstances.Link;

    RenderProgramQuad := TGLSLProgram.Create;
    RenderProgramQuad.AttachVertexShader(VertexShaderSourceQuad);
    RenderProgramQuad.AttachFragmentShader(FragmentShaderSourceQuad);
    RenderProgramQuad.Link;

    RenderProgramMesh := TGLSLProgram.Create;
    RenderProgramMesh.AttachVertexShader(VertexShaderSourceMesh);
    RenderProgramMesh.AttachFragmentShader(FragmentShaderSourceMesh);
    RenderProgramMesh.Link;

    ApplicationProperties.OnGLContextClose.Add(@FreeGLContext);
  end;

  glGenBuffers(2, @Self.VBOs);
  glGenVertexArrays(2, @Self.VAOs);
  glGenBuffers(1, @Self.VBOMesh);
  glGenBuffers(1, @Self.VBOMeshIndices);
  glGenVertexArrays(2, @Self.VAOMeshes);
  Self.FIsGLContextInitialized := True;
end;

procedure TCastleParticleEmitter.GLContextClose;
begin
  if Self.FIsGLContextInitialized then
  begin
    glDeleteBuffers(1, @Self.VBOMesh);
    glDeleteBuffers(1, @Self.VBOMeshIndices);
    glDeleteVertexArrays(2, @Self.VAOMeshes);
    glDeleteBuffers(2, @Self.VBOs);
    glDeleteVertexArrays(2, @Self.VAOs);
    glFreeTexture(Self.Texture);
    Self.FIsGLContextInitialized := False;
  end;
  inherited;
end;

procedure TCastleParticleEmitter.InternalLoadMesh;
var
  I: Integer;
  Scene: TCastleScene;
  ShapeList: TShapeList;
  Shape: TShape;
  ShapeNode: TShapeNode;
  IndexedTriangleSetNode: TIndexedTriangleSetNode;
  CoordNode: TCoordinateNode;
  TexcoordNode: TTextureCoordinateNode;
  VertexList, NormalList: TVector3List;
  TexcoordList: TVector2List;
  IndexList: TLongIntList;
begin
  SetLength(Self.ParticleMeshIndices, 0);
  SetLength(Self.ParticleMesh, 0);
  // Load built-in mesh if Mesh is empty
  if (Self.FEffect.Mesh = '') or (not URIFileExists(Self.FEffect.Mesh)) then
  begin
    SetLength(Self.ParticleMesh, Length(BuiltInVertexArray));
    // Generate built-in mesh
    for I := 0 to High(BuiltInVertexArray) do
    begin
      Self.ParticleMesh[I].Vertex := BuiltInVertexArray[I];
      Self.ParticleMesh[I].Texcoord := BuiltInTexcoordArray[I];
    end;
  end else
  begin
    Scene := TCastleScene.Create(nil);
    Scene.URL := Self.FEffect.Mesh;
    try
      try
        // We only care the first shape
        ShapeList := Scene.Shapes.TraverseList(True);
        if ShapeList.Count = 0 then
          raise Exception.Create('No mesh found in this model');
        Shape := ShapeList.Items[0];
        ShapeNode := Shape.Node as TShapeNode;
        //
        IndexedTriangleSetNode := ShapeNode.FindNode(TIndexedTriangleSetNode, False) as TIndexedTriangleSetNode;
        CoordNode := ShapeNode.FindNode(TCoordinateNode, False) as TCoordinateNode;
        TexcoordNode := ShapeNode.FindNode(TTextureCoordinateNode, False) as TTextureCoordinateNode;
        VertexList := CoordNode.FdPoint.Items;
        TexcoordList := TexcoordNode.FdPoint.Items;
        NormalList := Shape.NormalsSmooth(False, True);
        IndexList := IndexedTriangleSetNode.FdIndex.Items;
        if (VertexList.Count <> TexcoordList.Count) or (VertexList.Count <> NormalList.Count) then
          raise Exception.Create('Invalid mesh data');
        //
        SetLength(Self.ParticleMesh, VertexList.Count);
        for I := 0 to VertexList.Count - 1 do
        begin
          Self.ParticleMesh[I].Vertex := VertexList[I];
          Self.ParticleMesh[I].Texcoord := TexcoordList[I];
          Self.ParticleMesh[I].Normal := NormalList[I];
        end;
        SetLength(Self.ParticleMeshIndices, IndexList.Count);
        for I := 0 to IndexList.Count - 1 do
          Self.ParticleMeshIndices[I] := IndexList[I];
      except
        on E: Exception do
        begin
          WritelnWarning('CastleParticleEmitter', E.Message);
        end;
      end;
    finally
      FreeAndNil(Scene);
    end;
  end;
end;

procedure TCastleParticleEmitter.InternalRefreshEffect;
var
  I: Integer;
begin
  if Self.FEffect = nil then
    Exit;
  // Only process if texture exists
  if (not URIFileExists(Self.FEffect.Texture)) and (Self.Effect.TextureViewport = nil) then
    Exit;

  Self.FEmissionTime := Self.FEffect.Duration;
  Self.FParticleCount := Self.FEffect.MaxParticles;
  Self.FCountdownTillRemove := Self.FEffect.LifeSpan + Self.FEffect.LifeSpanVariance;
  SetLength(Self.Particles, Self.FEffect.MaxParticles);
  Self.InternalLoadMesh;

  if Self.FEffect.LifeSpan = 0 then
    Self.FEffect.LifeSpan := 0.001;

  glFreeTexture(Self.Texture);
  if Self.FEffect.TextureViewport = nil then
  begin
    if Self.FSmoothTexture then
      Self.Texture := LoadGLTexture(
        Self.FEffect.Texture,
        TextureFilter(minLinear, magLinear),
        Texture2DClampToEdge
      )
    else
      Self.Texture := LoadGLTexture(
        Self.FEffect.Texture,
        TextureFilter(minNearest, magNearest),
        Texture2DClampToEdge
      );
  end;

  // Generate initial lifecycle
  for I := 0 to Self.FEffect.MaxParticles - 1 do
  begin
    with Self.Particles[I] do
    begin
      if Self.FBurst then
        TimeToLive.X := 0.005
      else
        TimeToLive.X := Random * (Self.FEffect.LifeSpan + Self.FEffect.LifeSpanVariance);
      // Position.W is being used as random seed
      Position := Vector4(Random, Random, Random, Random);
      Direction := Vector3(1, 0, 0);
    end;
  end;

  // Drawing VAO
  glBindBuffer(GL_ARRAY_BUFFER, Self.VBOMesh);
  glBufferData(GL_ARRAY_BUFFER, Length(Self.ParticleMesh) * SizeOf(TCastleParticleMesh), @Self.ParticleMesh[0], GL_STATIC_DRAW);
  if Length(Self.ParticleMeshIndices) > 0 then
  begin
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Self.VBOMeshIndices);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(Self.ParticleMeshIndices) * SizeOf(GLuint), @Self.ParticleMeshIndices[0], GL_STATIC_DRAW);
  end;
  Self.CurrentBuffer := 0;
  for I := 0 to 1 do
  begin
    // Transform & feedback VAO
    glBindVertexArray(Self.VAOs[I]);

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOs[I]);
    glBufferData(GL_ARRAY_BUFFER, Self.FEffect.MaxParticles * SizeOf(TCastleParticle), @Self.Particles[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(16));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(32));
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(48));
    glEnableVertexAttribArray(4);
    glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(64));
    glEnableVertexAttribArray(5);
    glVertexAttribPointer(5, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(80));
    glEnableVertexAttribArray(6);
    glVertexAttribPointer(6, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(92));
    glEnableVertexAttribArray(7);
    glVertexAttribPointer(7, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(108));
    glEnableVertexAttribArray(8);
    glVertexAttribPointer(8, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(120));
    glEnableVertexAttribArray(9);
    glVertexAttribPointer(9, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(132));

    // Instancing VAO
    glBindVertexArray(Self.VAOMeshes[I]);

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOs[I]);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(0));
    glVertexAttribDivisor(0, 1);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(16));
    glVertexAttribDivisor(1, 1);
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(32));
    glVertexAttribDivisor(2, 1);
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(48));
    glVertexAttribDivisor(3, 1);
    glEnableVertexAttribArray(9);
    glVertexAttribPointer(9, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), Pointer(132));
    glVertexAttribDivisor(9, 1);

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOMesh);

    glEnableVertexAttribArray(13);
    glVertexAttribPointer(13, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticleMesh), Pointer(0));
    glEnableVertexAttribArray(14);
    glVertexAttribPointer(14, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticleMesh), Pointer(12));
    glEnableVertexAttribArray(15);
    glVertexAttribPointer(15, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticleMesh), Pointer(20));

    if Length(Self.ParticleMeshIndices) > 0 then
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Self.VBOMeshIndices);

    glBindVertexArray(0);
  end;
  SetLength(Self.Particles, 0);
  Self.FIsNeedRefresh := False;
  Self.FEffect.IsNeedRefresh := False;
end;

procedure TCastleParticleEmitter.RefreshEffect;
begin
  Self.FIsNeedRefresh := True;
end;

function TCastleParticleEmitter.LocalBoundingBox: TBox3D;
begin
  if GetExists and (Self.FEffect <> nil) then
  begin
    if not Self.FEffect.BBox.IsEmpty then
      Result := Box3D(
        Self.FEffect.BBox.Data[0],
        Self.FEffect.BBox.Data[1]
      )
    else
      Result := Self.FEffect.BBox;
  end else
    Result := TBox3D.Empty;
  Result.Include(inherited LocalBoundingBox);
end;

function CastleParticleBlendValueToBlendMode(const AValue: Integer): TCastleParticleBlendMode;
begin
  case AValue of
    0: Result := pbmZero;
    1: Result := pbmOne;
    768: Result := pbmSrcColor;
    769: Result := pbmOneMinusSrcColor;
    770: Result := pbmSrcAlpha;
    771: Result := pbmOneMinusSrcAlpha;
    772: Result := pbmDstAlpha;
    773: Result := pbmOneMinusDstAlpha;
    774: Result := pbmDstColor;
    775: Result := pbmOneMinusDstColor;
  end;
end;

function TCastleParticleAttractor.PropertySections(const PropertyName: String): TPropertySections;
begin
  case PropertyName of
    'Attraction':
      Result := [psBasic];
    else
      Result := inherited PropertySections(PropertyName);
  end;
end;

initialization
  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleParticleEffect,
    'Texture', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleParticleEffect,
    'Mesh', TSceneURLPropertyEditor);
  {$endif}
  RegisterSerializableComponent(TCastleParticleEmitter, 'Particle Emitter');
  RegisterSerializableComponent(TCastleParticleAttractor, 'Particle Attractor');
  RegisterSerializableComponent(TCastleParticleEffect, 'Particle Effect');
  RegisterSerializableComponent(TCastleParticleViewport, 'Viewport (Particle)');

end.
