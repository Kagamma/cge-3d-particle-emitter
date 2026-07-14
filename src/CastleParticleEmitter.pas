unit CastleParticleEmitter;

{$coperators on}
{$macro on}
{$define nl:=+ LineEnding +}

{$ifdef ANDROID}{$define GLES}{$endif}
{$ifdef iOS}{$define GLES}{$endif}
{$ifdef WASI}
  {$define CastleGLES := CastleInternalWebGL}
  {$define GLES}
{$endif}
{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, fpjsonrtti, md5,
  {$ifdef GLES}
  CastleGLES,
  {$else}
  CastleGL,
  {$endif}
  {$ifdef CASTLE_DESIGN_MODE}
  PropEdits, CastlePropEdits, CastleDebugTransform,
  {$endif}
  {$ifdef WASI}
  Job.JS, CastleInternalJobWeb,
  {$endif}
  CastleTransform, CastleScene, CastleComponentSerialize, CastleColors, CastleBoxes,
  CastleVectors, CastleRenderContext, Generics.Collections, CastleGLImages, CastleLog,
  CastleUtils, CastleApplicationProperties, CastleGLShaders, CastleClassUtils,
  X3DNodes, CastleShapes, CastleGLUtils, CastleViewport, CastleImages, CastleRectangles,
  CastleRenderOptions;

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
    pstSpheroidSurface,
    pstCylinderSurface
  );

  TCastleParticleAttractorType = (
    patDistance,
    patGravityPoint
  );

  TCastleParticleRotationType = (
    prtDefault,
    prtPreviousPosition
  );

const
  CastleParticleBlendValues: array [TCastleParticleBlendMode] of Integer = (
    2, 3, 5, 8, 0, 1, 6, 9, 4, 7
  );
  CastleParticleSourceValues: array [TCastleParticleSourceType] of Integer = (
    0, 1, 2, 3, 4
  );
  CastleParticleAttractorType: array [TCastleParticleAttractorType] of Integer = (
    0, 1
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
    PreviousPosition: TVector4;
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

  TCastleParticleEffectUBO = packed record
    {$ifdef WASI}
    AnchorCount: Integer;
    Anchor,
    AnchorSize,
    AnchorSizeVariance: TSingleList;
    AnchorColor,
    AnchorColorVariance: TVector4List;
    TextureAsSourcePositionSize: Integer;
    {$else}
    Rotation: TVector3;
    LifeSpan: Single;
    RotationVariance: TVector3;
    LifeSpanVariance: Single;
    RotationSpeed: TVector3;
    Speed: Single;
    RotationSpeedVariance: TVector3;
    SpeedVariance: Single;
    SourcePosition: TVector3;
    Radial: Single;
    SourcePositionVariance: TVector3;
    RadialVariance: Single;
    Gravity: TVector3;
    DirectionVariance: Single;
    Direction: TVector3;
    SourceType: Integer;
    SourcePositionLocalVariance: TVector3;
    AnchorCount: Integer;
    Anchor,
    AnchorSize,
    AnchorSizeVariance: array[0..7] of Single;
    AnchorColor,
    AnchorColorVariance: array[0..4] of TVector4;
    MaxParticles,
    TextureAsSourcePositionSize: Integer;
    {$endif}
  end;

  TCastleParticleEffect = class(TCastleComponent)
  private
    FBillboard: Boolean;
    FMesh,
    FTexture,
    FMeshAsSourcePosition: String;
    FSourceType: TCastleParticleSourceType;
    FBlendFuncSource,
    FBlendFuncDestination: TCastleParticleBlendMode;
    FRotationType: TCastleParticleRotationType;
    FMaxParticles: Integer;
    FLifeSpan,
    FLifeSpanVariance,
    FSize,
    FSizeVariance,
    FSpeed,
    FSpeedVariance,
    FDuration,
    FDirectionVariance: Single;
    FRotation,
    FRotationVariance,
    FRotationSpeed,
    FRotationSpeedVariance,
    FSourcePosition,
    FSourcePositionVariance,
    FSourcePositionLocalVariance,
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
    FSourcePositionLocalVariancePersistent,
    FDirectionPersistent,
    FGravityPersistent: TCastleVector3Persistent;
    FColorPersistent,
    FColorVariancePersistent: TCastleColorPersistent;
    FRadial,
    FRadialVariance: Single;
    FTextureViewport: TCastleParticleViewport;
    FBBox: TBox3D;
    FAnchors: TCollection;
    FVertex_0,
    FVertex_1,
    FVertex_2,
    FVertex_3: TVector3;
    FVertexPersistent_0,
    FVertexPersistent_1,
    FVertexPersistent_2,
    FVertexPersistent_3: TCastleVector3Persistent;

    FCustomTransformFeedbackVertexShader,
    FCustomRenderVertexShader,
    FCustomRenderFragmentShader: TStrings;

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
    procedure SetSourcePositionLocalVarianceForPersistent(const AValue: TVector3);
    function GetSourcePositionLocalVarianceForPersistent: TVector3;
    procedure SetDirectionForPersistent(const AValue: TVector3);
    function GetDirectionForPersistent: TVector3;
    procedure SetGravityForPersistent(const AValue: TVector3);
    function GetGravityForPersistent: TVector3;
    procedure SetColorForPersistent(const AValue: TVector4);
    function GetColorForPersistent: TVector4;
    procedure SetColorVarianceForPersistent(const AValue: TVector4);
    function GetColorVarianceForPersistent: TVector4;
    procedure SetVertex0ForPersistent(const AValue: TVector3);
    function GetVertex0ForPersistent: TVector3;
    procedure SetVertex1ForPersistent(const AValue: TVector3);
    function GetVertex1ForPersistent: TVector3;
    procedure SetVertex2ForPersistent(const AValue: TVector3);
    function GetVertex2ForPersistent: TVector3;
    procedure SetVertex3ForPersistent(const AValue: TVector3);
    function GetVertex3ForPersistent: TVector3;
    procedure SetTextureViewport(const AValue: TCastleParticleViewport);
    procedure SetMesh(const AValue: String);
    procedure SetMeshAsSourcePosition(const AValue: String);
    procedure SetTexture(const AValue: String);
    procedure SetMaxParticle(const AValue: Integer);
    procedure SetDuration(const AValue: Single);
    procedure SetLifeSpan(const AValue: Single);
    procedure SetLifeSpanVariance(const AValue: Single);
    procedure SetCustomTransformFeedbackVertexShader(const V: TStrings);
    procedure SetCustomRenderVertexShader(const V: TStrings);
    procedure SetCustomRenderFragmentShader(const V: TStrings);
  protected
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    IsColliable: Boolean;
    IsNeedRefresh: Boolean;
    IsNeedRecompile: Boolean;
    IsChanged: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const AURL: String; const IsTexturePathRelative: Boolean = False);
    procedure Save(const AURL: String; const IsTexturePathRelative: Boolean = False);
    procedure Changed;

    property Rotation: TVector3 read FRotation write FRotation;
    property RotationVariance: TVector3 read FRotationVariance write FRotationVariance;
    property RotationSpeed: TVector3 read FRotationSpeed write FRotationSpeed;
    property RotationSpeedVariance: TVector3 read FRotationSpeedVariance write FRotationSpeedVariance;
    property SourcePosition: TVector3 read FSourcePosition write FSourcePosition;
    property SourcePositionVariance: TVector3 read FSourcePositionVariance write FSourcePositionVariance;
    property SourcePositionLocalVariance: TVector3 read FSourcePositionLocalVariance write FSourcePositionLocalVariance;
    property Direction: TVector3 read FDirection write FDirection;
    property Gravity: TVector3 read FGravity write FGravity;
    property Color: TVector4 read FColor write FColor;
    property ColorVariance: TVector4 read FColorVariance write FColorVariance;
    property BBox: TBox3D read FBBox write FBBox;
    property Vertex_0: TVector3 read FVertex_0 write FVertex_0;
    property Vertex_1: TVector3 read FVertex_1 write FVertex_1;
    property Vertex_2: TVector3 read FVertex_2 write FVertex_2;
    property Vertex_3: TVector3 read FVertex_3 write FVertex_3;
  published
    property Billboard: Boolean read FBillboard write FBillboard default True;
    property Mesh: String read FMesh write SetMesh;
    property MeshAsSourcePosition: String read FMeshAsSourcePosition write SetMeshAsSourcePosition;
    property Texture: String read FTexture write SetTexture;
    property SourceType: TCastleParticleSourceType read FSourceType write FSourceType default pstBox;
    property RotationType: TCastleParticleRotationType read FRotationType write FRotationType default prtDefault;
    property BlendFuncSource: TCastleParticleBlendMode read FBlendFuncSource write FBlendFuncSource default pbmOne;
    property BlendFuncDestination: TCastleParticleBlendMode read FBlendFuncDestination write FBlendFuncDestination default pbmOne;
    property MaxParticles: Integer read FMaxParticles write SetMaxParticle default 100;
    property LifeSpan: Single read FLifeSpan write SetLifeSpan default 1;
    property LifeSpanVariance: Single read FLifeSpanVariance write SetLifeSpanVariance default 0.5;
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
    property SourcePositionLocalVariancePersistent: TCastleVector3Persistent read FSourcePositionLocalVariancePersistent;
    property DirectionPersistent: TCastleVector3Persistent read FDirectionPersistent;
    property GravityPersistent: TCastleVector3Persistent read FGravityPersistent;
    property ColorPersistent: TCastleColorPersistent read FColorPersistent;
    property ColorVariancePersistent: TCastleColorPersistent read FColorVariancePersistent;
    property Anchors: TCollection read FAnchors;
    property Vertex_0Persistent: TCastleVector3Persistent read FVertexPersistent_0;
    property Vertex_1Persistent: TCastleVector3Persistent read FVertexPersistent_1;
    property Vertex_2Persistent: TCastleVector3Persistent read FVertexPersistent_2;
    property Vertex_3Persistent: TCastleVector3Persistent read FVertexPersistent_3;

    property CustomTransformFeedbackVertexShader: TStrings read FCustomTransformFeedbackVertexShader write SetCustomTransformFeedbackVertexShader;
    property CustomRenderVertexShader: TStrings read FCustomRenderVertexShader write SetCustomRenderVertexShader;
    property CustomRenderFragmentShader: TStrings read FCustomRenderFragmentShader write SetCustomRenderFragmentShader;
  end;

  TCastleParticleAttractor = class(TCastleTransform)
  strict private
    FAttraction: Single;
    FKillDistance: Single;
    FAttractorType: TCastleParticleAttractorType;
  protected
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Attraction: Single read FAttraction write FAttraction;
    property KillDistance: Single read FKillDistance write FKillDistance default -1;
    property AttactorType: TCastleParticleAttractorType read FAttractorType write FAttractorType default patDistance;
  end;

  PGLVertexArrayObject = ^TGLVertexArrayObject;
  PGLBuffer = ^TGLBuffer;
  PGLTexture = ^TGLTexture;

  TCastleParticleEmitter = class(TCastleTransform)
  strict private
    Texture,
    TextureAsSourcePosition: TGLTexture;

    LocalRenderProgramQuad,
    LocalRenderProgramMesh,
    LocalTransformFeedbackProgramSingleInstance,
    LocalTransformFeedbackProgramMultipleInstances: TGLSLProgram;
    VAOTnFs,
    VAOMeshes: array[0..1] of TGLVertexArrayObject;
    VBOTnFs: array[0..1] of TGLBuffer;
    VBOMesh, VBOMeshIndices: TGLBuffer;
    {$ifndef WASI}
    UBO: TGLBuffer;
    {$endif}
    CurrentBuffer: TGLuint;
    Particles: packed array of TCastleParticle;
    ParticleMesh: packed array of TCastleParticleMesh;
    ParticleMeshIndices: packed array of TGLushort;

    FStartEmitting: Boolean;
    FEffect: TCastleParticleEffect;
    FEffectUBO: TCastleParticleEffectUBO;
    FParticleCount: Integer;
    FIsUpdated: Boolean;
    FTime,
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
    FIsEffectChanged,
    FTimePlaying: Boolean;
    FDeltaTime,
    FTimePlayingSpeed: Single;
    FAttractorList: TVector4List;
    FAttractorKillDistanceList: TSingleList;
    FAttractorTypeList: TInt32List;
    //
    FCameraMatrix: TMatrix4;
    FGlobalFog: TFogNode;
    {$ifdef CASTLE_DESIGN_MODE}
    FDebugBox: TDebugBox;
    {$endif}
    procedure InternalLoadMesh;
    procedure InternalLoadMeshAsSourcePosition;
    procedure InternalRefreshEffect;
    procedure SetStartEmitting(const V: Boolean);
    procedure SetBurst(const V: Boolean);
    procedure SetSmoothTexture(const V: Boolean);
    procedure SetAllowsInstancing(const V: Boolean);
    procedure InternalRender(const Transformation: TTransformation; const PassParams: TRenderOnePassParams);
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
    function Clone(const AOwner: TComponent): TCastleParticleEmitter;
    property StartEmitting: Boolean read FStartEmitting write SetStartEmitting;
    property AllowsUpdateWhenCulled: Boolean read FAllowsUpdateWhenCulled write FAllowsUpdateWhenCulled;
  published
    property Effect: TCastleParticleEffect read FEffect write LoadEffect;
    { If true, the emitter will start emitting }
    property ProcessEvents: Boolean read FStartEmitting write SetStartEmitting default True;
    property DistanceCulling: Single read FDistanceCulling write FDistanceCulling default 0;
    property AllowsWriteToDepthBuffer: Boolean read FAllowsWriteToDepthBuffer write FAllowsWriteToDepthBuffer default False;
    property AllowsInstancing: Boolean read FAllowsInstancing write SetAllowsInstancing default False;
    property AnimateWhenOnlyVisible: Boolean read FAllowsUpdateWhenCulled write FAllowsUpdateWhenCulled default True;
    property EnableFog: Boolean read FEnableFog write FEnableFog default False;
    property SmoothTexture: Boolean read FSmoothTexture write SetSmoothTexture default True;
    property Burst: Boolean read FBurst write SetBurst default False;
    property TimePlaying: Boolean read FTimePlaying write FTimePlaying default True;
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed default 1.0;
    property DeltaTime: Single read FDeltaTime write FDeltaTime default 0;
    property ReleaseWhenDone: Boolean read FReleaseWhenDone write FReleaseWhenDone default False;
  end;

function CastleParticleBlendValueToBlendMode(const AValue: Integer): TCastleParticleBlendMode;

implementation

uses
  CastleDownload, CastleURIUtils, Math;

const
  CommonTransformVertexFunctions =
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
'}';

  CommonVertexFunctions =
'mat3 createLookup(vec3 d) {'nl
'  vec3 w = vec3(0.0, 1.0, 0.0);'nl
'  vec3 r = normalize(cross(d, w));'nl
'  vec3 u = normalize(cross(r, d));'nl
'  return mat3('nl
'    r.xyz,'nl
'    u.xyz,'nl
'    d.xyz'nl
'  );'nl
'}'nl

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
'}';

  TransformVertexShaderSourceMultipleInstances_Part1 =
{$ifdef GLES}
'#version 300 es'nl
{$else}
'#version 330'nl
{$endif}

'#define SOURCE_BOX 0'nl
'#define SOURCE_SPHEROID 1'nl
'#define SOURCE_BOXSURFACE 2'nl
'#define SOURCE_SPHEROIDSURFACE 3'nl
'#define SOURCE_CYLINDERSURFACE 4'nl

'#define ATTRACTOR_DISTANCE 0'nl
'#define ATTRACTOR_GRAVITY_POINT 1'nl

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
'layout(location = 10) in vec4 inPreviousPosition;'nl

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
'out vec4 outPreviousPosition;'nl

{$ifdef WASI}
'uniform vec3 rotation;'nl
'uniform float particleLifeSpan;'nl
'uniform vec3 rotationVariance;'nl
'uniform float particleLifeSpanVariance;'nl
'uniform vec3 rotationSpeed;'nl
'uniform float speed;'nl
'uniform vec3 rotationSpeedVariance;'nl
'uniform float speedVariance;'nl
'uniform vec3 sourcePosition;'nl
'uniform float radial;'nl
'uniform vec3 sourcePositionVariance;'nl
'uniform float radialVariance;'nl
'uniform vec3 gravity;'nl
'uniform float directionVariance;'nl
'uniform vec3 direction;'nl
'uniform int sourceType;'nl
'uniform vec3 sourcePositionLocalVariance;'nl
'uniform int anchorCount;'nl
'uniform float anchor[8];'nl
'uniform float anchorSize[8];'nl
'uniform float anchorSizeVariance[8];'nl
'uniform vec4 anchorColor[5];'nl
'uniform vec4 anchorColorVariance[5];'nl
'uniform int maxParticles;'nl
'uniform int textureAsSourcePositionSize;'nl
{$else}
'layout(packed) uniform Effect {'nl
'  vec3 rotation;'nl
'  float particleLifeSpan;'nl
'  vec3 rotationVariance;'nl
'  float particleLifeSpanVariance;'nl
'  vec3 rotationSpeed;'nl
'  float speed;'nl
'  vec3 rotationSpeedVariance;'nl
'  float speedVariance;'nl
'  vec3 sourcePosition;'nl
'  float radial;'nl
'  vec3 sourcePositionVariance;'nl
'  float radialVariance;'nl
'  vec3 gravity;'nl
'  float directionVariance;'nl
'  vec3 direction;'nl
'  int sourceType;'nl
'  vec3 sourcePositionLocalVariance;'nl
'  int anchorCount;'nl
'  float anchor[8];'nl
'  float anchorSize[8];'nl
'  float anchorSizeVariance[8];'nl
'  vec4 anchorColor[5];'nl
'  vec4 anchorColorVariance[5];'nl
'  int maxParticles;'nl
'  int textureAsSourcePositionSize;'nl
'};'nl
{$endif}

'uniform vec4 attractors[4];'nl
'uniform float attractorKillDistances[4];'nl
'uniform int attractorCount;'nl
'uniform int attractorType[4];'nl
'uniform float time;'nl
'uniform float emissionTime;'nl
'uniform float deltaTime;'nl
'uniform sampler2D textureAsSourcePosition;'nl

CommonTransformVertexFunctions nl

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
'  outPreviousPosition = inPreviousPosition;'nl
'}'nl

'void emitParticle() {'nl
'  outTimeToLive.z = particleLifeSpan + particleLifeSpanVariance * (rnd() * 2.0 - 1.0);'nl // Life
'  outTimeToLive.x = outTimeToLive.z;'nl
'  outTimeToLive.y = outTimeToLive.z * (anchorCount > 1 ? anchor[1] : 0.99);'nl
'  outTimeToLive.w = 1.0;'nl // current anchor
'  float invLifeSpan = 1.0 / outTimeToLive.x;'nl
'  float invTimeRemaining = 1.0 / outTimeToLive.y;'nl
'  outTimeToLive.y = outTimeToLive.z - outTimeToLive.y;'nl
'  if (textureAsSourcePositionSize == 0) {'nl
'    vec3 vrpos = vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'    if (sourceType == SOURCE_SPHEROID) {'nl
'      vrpos = vec3(rnd(), rnd(), rnd()) * normalize(vrpos);'nl
'      outPosition.xyz = sourcePosition + sourcePositionVariance * vrpos;'nl
'    } else if (sourceType == SOURCE_BOXSURFACE) {'nl
'      float face = rnd();'nl
'      if (face < 1.0 / 3.0)'nl
'        vrpos = vec3(sign(vrpos.x), vrpos.y, vrpos.z);'nl
'      else if (face < 2.0 / 3.0)'nl
'        vrpos = vec3(vrpos.x, sign(vrpos.y), vrpos.z);'nl
'      else'nl
'        vrpos = vec3(vrpos.x, vrpos.y, sign(vrpos.z));'nl
'      outPosition.xyz = sourcePosition + sourcePositionVariance * vrpos;'nl
'    } else if (sourceType == SOURCE_SPHEROIDSURFACE) {'nl
'      outPosition.xyz = sourcePosition + sourcePositionVariance * normalize(vrpos);'nl
'    } else if (sourceType == SOURCE_CYLINDERSURFACE) {'nl
'      outPosition.xyz = sourcePosition + sourcePositionVariance * vec3(normalize(vrpos.xz), vrpos.y).xzy;'nl
'    } else {'nl
'      outPosition.xyz = sourcePosition + sourcePositionVariance * vrpos;'nl
'    }'nl
'  } else {'nl
'    outPosition.xyz = texelFetch(textureAsSourcePosition, ivec2(floor(rnd() * float(textureAsSourcePositionSize)), 0), 0).xyz * sourcePositionVariance;'nl
'  }'nl
'  outPreviousPosition.xyz = outPosition.xyz;'nl
'  outPreviousPosition.w = rnd();'nl
'  outPosition.xyz += sourcePositionLocalVariance * normalize(vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0));'nl
'  outTranslate = vec3(0.0);'nl // Do nothing
'  outStartPos = vec3(0.0);'nl // Do nothing
'  outDirection = direction;'nl
'  vec3 cd = normalize(cross(outDirection, outDirection.zxy));'nl
'  float angle = directionVariance * (rnd() * 2.0 - 1.0);'nl
'  vec3 vrdir = rotate(outDirection, angle, cd);'nl
'  vrdir = rotate(vrdir, rnd() * 2.0 * 3.14159265359, outDirection);'nl
'  vec3 vspeed = vec3('nl
'    speed + speedVariance * (rnd() * 2.0 - 1.0),'nl
'    speed + speedVariance * (rnd() * 2.0 - 1.0),'nl
'    speed + speedVariance * (rnd() * 2.0 - 1.0));'nl
'  outVelocity = vec4(vrdir * vspeed, radial + radialVariance * (rnd() * 2.0 - 1.0));'nl

'  outColor = anchorColor[0] + anchorColorVariance[0] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  vec4 middleColor = anchorCount == 1 ? outColor : anchorColor[1] + anchorColorVariance[1] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outColorDelta = (middleColor - outColor) * invTimeRemaining;'nl
'  float startSize = max(0.0001, anchorSize[0] + anchorSizeVariance[0] * (rnd() * 2.0 - 1.0));'nl
'  float finishSize = anchorCount == 1 ? startSize : max(0.0001, anchorSize[1] + anchorSizeVariance[1] * (rnd() * 2.0 - 1.0));'nl
'  outSizeRotation.xy = vec2(startSize, (finishSize - startSize) * invTimeRemaining);'nl

'  outSizeRotation.z = rotation.z + rotationVariance.z * (rnd() * 2.0 - 1.0);'nl
'  outSizeRotation.w = rotationSpeed.z + rotationSpeedVariance.z * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.x = rotation.x + rotationVariance.x * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.y = rotationSpeed.x + rotationSpeedVariance.x * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.z = rotation.y + rotationVariance.y * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.w = rotationSpeed.y + rotationSpeedVariance.y * (rnd() * 2.0 - 1.0);'nl
'}'+ LineEnding;

  TransformVertexShaderSourceMultipleInstances_Part2 =
'void updateParticle() {'nl
'  float timeBetweenParticle = max(deltaTime, particleLifeSpan / float(maxParticles));'nl
'  if (outTimeToLive.x <= 0.0 && emissionTime == 0.0) {'nl
'    outTimeToLive.x = (rnd() - 1.0) * particleLifeSpan;'nl
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
'    outTimeToLive.w = float(a);'nl // current anchor
'    if (a < anchorCount) {'nl
'      outTimeToLive.y = outTimeToLive.z * anchor[a];'nl
'      float invTimeRemaining = 1.0 / (outTimeToLive.y - outTimeToLive.z * anchor[a - 1]);'nl
'      outTimeToLive.y = outTimeToLive.z - outTimeToLive.y;'nl
'      vec4 finishColor = anchorColor[a] + anchorColorVariance[a] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'      outColorDelta = (finishColor - outColor) * invTimeRemaining;'nl
'      float finishSize = max(0.0001, anchorSize[a] + anchorSizeVariance[a] * (rnd() * 2.0 - 1.0));'nl
'      outSizeRotation.y = (finishSize - outSizeRotation.x) * invTimeRemaining;'nl
'    } else {'nl
'      outColorDelta = vec4(0.0);'nl
'      outSizeRotation.y = 0.0;'nl
'      outTimeToLive.y = outTimeToLive.z;'nl
'    }'nl
'  }'nl
'  outTimeToLive.x = max(0.0, outTimeToLive.x - deltaTime);'nl
'  outVelocity.xyz = rotate(outVelocity.xyz, outVelocity.w * deltaTime, outDirection) + gravity * deltaTime;'nl
'  for (int i = 0; i < attractorCount; i++) {'nl
'    if (attractorKillDistances[i] >= 0.0) {'nl
'      float dist = distance(attractors[i].xyz, outPosition.xyz);'nl
'      if (dist <= attractorKillDistances[i]) {'nl
'        outTimeToLive.x = 0.0;'nl
'      }'nl
'    }'nl
'    vec3 a = outPosition.xyz - attractors[i].xyz;'nl
'    if (attractorType[i] == ATTRACTOR_GRAVITY_POINT) {'nl
'      float force = 6.674 * attractors[i].w / length(a);'nl
'      outVelocity.xyz += normalize(a) * force;'nl
'    } else {'nl
'      outVelocity.xyz += a * attractors[i].w;'nl
'    }'nl
'  }'nl
'  outPreviousPosition.xyz = outPosition.xyz;'nl
'  outPosition.xyz = rotate(outPosition.xyz, outVelocity.w * deltaTime, outDirection) + outVelocity.xyz * deltaTime;'nl
'  outSizeRotation.x += outSizeRotation.y * deltaTime;'nl
'  outSizeRotation.z += outSizeRotation.w * deltaTime;'nl
'  outRotationXY.x += outRotationXY.y * deltaTime;'nl
'  outRotationXY.z += outRotationXY.w * deltaTime;'nl
'  PLUG_update_after();'nl
'}'nl

'void main() {'nl
'  initParticle();'nl
'  updateParticle();'nl
'}';

  TransformVertexShaderSourceMultipleInstances: String =
    TransformVertexShaderSourceMultipleInstances_Part1 +
    'void PLUG_update_after(){}' + LineEnding +
    TransformVertexShaderSourceMultipleInstances_Part2;

  TransformVertexShaderSourceSingleInstance_Part1 =
{$ifdef GLES}
'#version 300 es'nl
{$else}
'#version 330'nl
{$endif}

'#define SOURCE_BOX 0'nl
'#define SOURCE_SPHEROID 1'nl
'#define SOURCE_BOXSURFACE 2'nl
'#define SOURCE_SPHEROIDSURFACE 3'nl
'#define SOURCE_CYLINDERSURFACE 4'nl

'#define ATTRACTOR_DISTANCE 0'nl
'#define ATTRACTOR_GRAVITY_POINT 1'nl

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
'layout(location = 10) in vec4 inPreviousPosition;'nl

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
'out vec4 outPreviousPosition;'nl

{$ifdef WASI}
'uniform vec3 rotation;'nl
'uniform float particleLifeSpan;'nl
'uniform vec3 rotationVariance;'nl
'uniform float particleLifeSpanVariance;'nl
'uniform vec3 rotationSpeed;'nl
'uniform float speed;'nl
'uniform vec3 rotationSpeedVariance;'nl
'uniform float speedVariance;'nl
'uniform vec3 sourcePosition;'nl
'uniform float radial;'nl
'uniform vec3 sourcePositionVariance;'nl
'uniform float radialVariance;'nl
'uniform vec3 gravity;'nl
'uniform float directionVariance;'nl
'uniform vec3 direction;'nl
'uniform int sourceType;'nl
'uniform vec3 sourcePositionLocalVariance;'nl
'uniform int anchorCount;'nl
'uniform float anchor[8];'nl
'uniform float anchorSize[8];'nl
'uniform float anchorSizeVariance[8];'nl
'uniform vec4 anchorColor[5];'nl
'uniform vec4 anchorColorVariance[5];'nl
'uniform int maxParticles;'nl
'uniform int textureAsSourcePositionSize;'nl
{$else}
'layout(packed) uniform Effect {'nl
'  vec3 rotation;'nl
'  float particleLifeSpan;'nl
'  vec3 rotationVariance;'nl
'  float particleLifeSpanVariance;'nl
'  vec3 rotationSpeed;'nl
'  float speed;'nl
'  vec3 rotationSpeedVariance;'nl
'  float speedVariance;'nl
'  vec3 sourcePosition;'nl
'  float radial;'nl
'  vec3 sourcePositionVariance;'nl
'  float radialVariance;'nl
'  vec3 gravity;'nl
'  float directionVariance;'nl
'  vec3 direction;'nl
'  int sourceType;'nl
'  vec3 sourcePositionLocalVariance;'nl
'  int anchorCount;'nl
'  float anchor[8];'nl
'  float anchorSize[8];'nl
'  float anchorSizeVariance[8];'nl
'  vec4 anchorColor[5];'nl
'  vec4 anchorColorVariance[5];'nl
'  int maxParticles;'nl
'  int textureAsSourcePositionSize;'nl
'};'nl
{$endif}

'uniform vec4 attractors[4];'nl
'uniform float attractorKillDistances[4];'nl
'uniform int attractorCount;'nl
'uniform int attractorType[4];'nl
'uniform mat4 mMatrix;'nl
'uniform float time;'nl
'uniform float emissionTime;'nl
'uniform float deltaTime;'nl
'uniform sampler2D textureAsSourcePosition;'nl

CommonTransformVertexFunctions nl

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
'  outPreviousPosition = inPreviousPosition;'nl
'}'nl

'void emitParticle() {'nl
'  mat3 rMatrix = mat3(mMatrix);'nl
'  outTimeToLive.z = particleLifeSpan + particleLifeSpanVariance * (rnd() * 2.0 - 1.0);'nl // Life
'  outTimeToLive.x = outTimeToLive.z;'nl
'  outTimeToLive.y = outTimeToLive.z * (anchorCount > 1 ? anchor[1] : 0.99);'nl
'  outTimeToLive.w = 1.0;'nl // current anchor
'  float invLifeSpan = 1.0 / outTimeToLive.x;'nl
'  float invTimeRemaining = 1.0 / outTimeToLive.y;'nl
'  outTimeToLive.y = outTimeToLive.z - outTimeToLive.y;'nl
'  vec3 scale = vec3('nl
'    length(vec3(mMatrix[0][0], mMatrix[0][1], mMatrix[0][2])),'nl
'    length(vec3(mMatrix[1][0], mMatrix[1][1], mMatrix[1][2])),'nl
'    length(vec3(mMatrix[2][0], mMatrix[2][1], mMatrix[2][2]))'nl
'  );'nl
'  vec3 vrpos = vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  if (textureAsSourcePositionSize == 0) {'nl
'    if (sourceType == SOURCE_SPHEROID) {'nl
'      vrpos = vec3(rnd(), rnd(), rnd()) * normalize(vrpos);'nl
'      outStartPos = rMatrix * (sourcePosition + scale * sourcePositionVariance * vrpos);'nl
'    } else if (sourceType == SOURCE_BOXSURFACE) {'nl
'      float face = rnd();'nl
'      if (face < 1.0 / 3.0)'nl
'        vrpos = vec3(sign(vrpos.x), vrpos.y, vrpos.z);'nl
'      else if (face < 2.0 / 3.0)'nl
'        vrpos = vec3(vrpos.x, sign(vrpos.y), vrpos.z);'nl
'      else'nl
'        vrpos = vec3(vrpos.x, vrpos.y, sign(vrpos.z));'nl
'      outStartPos = rMatrix * (sourcePosition + scale * sourcePositionVariance * vrpos);'nl
'    } else if (sourceType == SOURCE_SPHEROIDSURFACE) {'nl
'      outStartPos = rMatrix * (sourcePosition + scale * sourcePositionVariance * normalize(vrpos));'nl
'    } else if (sourceType == SOURCE_CYLINDERSURFACE) {'nl
'      outStartPos = rMatrix * (sourcePosition + scale * sourcePositionVariance * vec3(normalize(vrpos.xz), vrpos.y).xzy);'nl
'    } else {'nl
'      outStartPos = rMatrix * (sourcePosition + scale * sourcePositionVariance * vrpos);'nl
'    }'nl
'  } else {'nl
'    outStartPos = rMatrix * (sourcePosition + texelFetch(textureAsSourcePosition, ivec2(floor(rnd() * float(textureAsSourcePositionSize)), 0), 0).xyz * sourcePositionVariance);'nl
'  }'nl
'  outStartPos += sourcePositionLocalVariance * normalize(vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0));'nl
'  outTranslate = vec3(mMatrix[3][0], mMatrix[3][1], mMatrix[3][2]);'nl
'  outPosition.xyz = outTranslate + outStartPos;'nl
'  outPreviousPosition.xyz = outStartPos.xyz;'nl
'  outPreviousPosition.w = rnd();'nl
'  outDirection = rMatrix * direction;'nl
'  vec3 cd = normalize(cross(outDirection, outDirection.zxy));'nl
'  float angle = directionVariance * (rnd() * 2.0 - 1.0);'nl
'  vec3 vrdir = rotate(outDirection, angle, cd);'nl
'  vrdir = rotate(vrdir, rnd() * 2.0 * 3.14159265359, outDirection);'nl
'  vec3 vspeed = vec3('nl
'    speed + speedVariance * (rnd() * 2.0 - 1.0),'nl
'    speed + speedVariance * (rnd() * 2.0 - 1.0),'nl
'    speed + speedVariance * (rnd() * 2.0 - 1.0));'nl
'  outVelocity = vec4(vrdir * vspeed, radial + radialVariance * (rnd() * 2.0 - 1.0));'nl

'  outColor = anchorColor[0] + anchorColorVariance[0] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  vec4 middleColor = anchorCount == 1 ? outColor : anchorColor[1] + anchorColorVariance[1] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outColorDelta = (middleColor - outColor) * invTimeRemaining;'nl
'  float startSize = max(0.0001, anchorSize[0] + anchorSizeVariance[0] * (rnd() * 2.0 - 1.0));'nl
'  float finishSize = anchorCount == 1 ? startSize : max(0.0001, anchorSize[1] + anchorSizeVariance[1] * (rnd() * 2.0 - 1.0));'nl
'  outSizeRotation.xy = vec2(startSize, (finishSize - startSize) * invTimeRemaining);'nl

'  outSizeRotation.z = rotation.z + rotationVariance.z * (rnd() * 2.0 - 1.0);'nl
'  outSizeRotation.w = rotationSpeed.z + rotationSpeedVariance.z * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.x = rotation.x + rotationVariance.x * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.y = rotationSpeed.x + rotationSpeedVariance.x * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.z = rotation.y + rotationVariance.y * (rnd() * 2.0 - 1.0);'nl
'  outRotationXY.w = rotationSpeed.y + rotationSpeedVariance.y * (rnd() * 2.0 - 1.0);'nl
'}' + LineEnding;

  TransformVertexShaderSourceSingleInstance_Part2 =
'void updateParticle() {'nl
'  float timeBetweenParticle = max(deltaTime, particleLifeSpan / float(maxParticles));'nl
'  if (outTimeToLive.x <= 0.0 && emissionTime == 0.0) {'nl
'    outTimeToLive.x = (rnd() - 1.0) * particleLifeSpan;'nl
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
'    outTimeToLive.w = float(a);'nl // current anchor
'    if (a < anchorCount) {'nl
'      outTimeToLive.y = outTimeToLive.z * anchor[a];'nl
'      float invTimeRemaining = 1.0 / (outTimeToLive.y - outTimeToLive.z * anchor[a - 1]);'nl
'      outTimeToLive.y = outTimeToLive.z - outTimeToLive.y;'nl
'      vec4 finishColor = anchorColor[a] + anchorColorVariance[a] * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'      outColorDelta = (finishColor - outColor) * invTimeRemaining;'nl
'      float finishSize = max(0.0001, anchorSize[a] + anchorSizeVariance[a] * (rnd() * 2.0 - 1.0));'nl
'      outSizeRotation.y = (finishSize - outSizeRotation.x) * invTimeRemaining;'nl
'    } else {'nl
'      outColorDelta = vec4(0.0);'nl
'      outSizeRotation.y = 0.0;'nl
'      outTimeToLive.y = outTimeToLive.z;'nl
'    }'nl
'  }'nl
'  outTimeToLive.x = max(0.0, outTimeToLive.x - deltaTime);'nl
'  outVelocity.xyz = rotate(outVelocity.xyz, outVelocity.w * deltaTime, outDirection) + gravity * deltaTime;'nl
'  for (int i = 0; i < attractorCount; i++) {'nl
'    if (attractorKillDistances[i] >= 0.0) {'nl
'      float dist = distance(attractors[i].xyz + outTranslate, outPosition.xyz);'nl
'      if (dist <= attractorKillDistances[i]) {'nl
'        outTimeToLive.x = 0.0;'nl
'      }'nl
'    }'nl
'    vec3 a = outPosition.xyz - (attractors[i].xyz + outTranslate);'nl
'    if (attractorType[i] == ATTRACTOR_GRAVITY_POINT) {'nl
'      float force = 6.674 * attractors[i].w / length(a);'nl
'      outVelocity.xyz += normalize(a) * force;'nl
'    } else {'nl
'      outVelocity.xyz += a * attractors[i].w;'nl
'    }'nl
'  }'nl
'  outStartPos = rotate(outStartPos, outVelocity.w * deltaTime, outDirection) + outVelocity.xyz * deltaTime;'nl
'  outPreviousPosition.xyz = outPosition.xyz;'nl
'  outPosition.xyz = outStartPos + outTranslate;'nl
'  outSizeRotation.x += outSizeRotation.y * deltaTime;'nl
'  outSizeRotation.z += outSizeRotation.w * deltaTime;'nl
'  outRotationXY.x += outRotationXY.y * deltaTime;'nl
'  outRotationXY.z += outRotationXY.w * deltaTime;'nl
'  PLUG_update_after();'nl
'}'nl

'void main() {'nl
'  initParticle();'nl
'  updateParticle();'nl
'}';

  TransformVertexShaderSourceSingleInstance: String =
    TransformVertexShaderSourceSingleInstance_Part1 +
    'void PLUG_update_after(){}' + LineEnding +
    TransformVertexShaderSourceSingleInstance_Part2;

  VertexShaderSourceQuad_Part1 =
{$ifdef GLES}
'#version 300 es'nl
{$else}
'#version 330'nl
{$endif}

'#define ROTATION_DEFAULT 0'nl
'#define ROTATION_PREVIOUS_POSITION 1'nl

'layout(location = 0) in vec4 inPosition;'nl
'layout(location = 1) in vec4 inTimeToLive;'nl
'layout(location = 2) in vec4 inSizeRotation;'nl
'layout(location = 3) in vec4 inColor;'nl
'layout(location = 9) in vec4 inRotationXY;'nl
'layout(location = 10) in vec4 inPreviousPosition;'nl
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
'uniform int rotationType;'nl
'uniform float time;'nl

CommonVertexFunctions nl'';

  VertexShaderSourceQuad_Part2 =
'void main() {'nl
'  if (inTimeToLive.x > 0.0) {'nl
'    vec2 texCoord = inTexcoord;'nl
'    PLUG_texture_coord(texCoord);'nl
'    fragTexCoord = texCoord;'nl
'    fragColor = inColor;'nl
'    vec3 center = vec3(vOrMvMatrix * vec4(inPosition.xyz, 1.0)).xyz;'nl
'    float angle = inSizeRotation.z;'nl
'    if (rotationType == ROTATION_PREVIOUS_POSITION) {'nl
'      vec3 centerScreen = vec4(pMatrix * vec4(center, 1.0)).xyz;'nl
'      vec3 centerPrevious = vec3(vOrMvMatrix * vec4(inPreviousPosition.xyz, 1.0)).xyz;'nl
'      vec3 centerPreviousScreen = vec4(pMatrix * vec4(centerPrevious, 1.0)).xyz;'nl
'      vec3 centerDelta = centerPreviousScreen - centerScreen;'nl
'      angle = atan(centerDelta.y, centerDelta.x);'nl
'    }'nl
'    mat3 m = createRotate(vec3(inRotationXY.x, inRotationXY.z, angle));'nl
'    vec3 obj_vert = inVertex;'nl
'    PLUG_vertex_object_space(obj_vert);'nl
'    vec4 p = vec4(m * (obj_vert * vec3(scaleX, scaleY, scaleZ) * vec3(inSizeRotation.x)) + center, 1.0);'nl
'    fragFogCoord = abs(p.z / p.w);'nl
'    gl_Position = pMatrix * p;'nl
'  } else'nl
'    gl_Position = vec4(-1.0, -1.0, -1.0, 1.0);'nl // Discard this vertex by making it outside of clip plane
'}';

  VertexShaderSourceQuad: String =
    VertexShaderSourceQuad_Part1 +
    'void PLUG_vertex_object_space(inout vec3 vertex_object){}' + LineEnding +
    'void PLUG_texture_coord(inout vec2 texture_coord){}' + LineEnding +
    VertexShaderSourceQuad_Part2;

  FragmentShaderSourceQuad_Part1 =
{$ifdef GLES}
'#version 300 es'nl
{$else}
'#version 330'nl
{$endif}
'precision lowp float;'nl
'in vec2 fragTexCoord;'nl
'in vec4 fragColor;'nl
'in float fragFogCoord;'nl

'out vec4 outColor;'nl

'uniform sampler2D baseColor;'nl
'uniform int fogEnable;'nl
'uniform float fogEnd;'nl
'uniform vec3 fogColor;'nl
'uniform float time;'nl '';

  FragmentShaderSourceQuad_Part2 =
'void main() {'nl
'  vec4 color = fragColor;'nl
'  PLUG_color(color);'nl
'  vec4 texture_color = texture(baseColor, fragTexCoord);'nl
'  PLUG_texture_color(texture_color);'nl
'  outColor = texture_color * color;'nl
'  if (fogEnable == 1) {'nl
'    float fogFactor = (fogEnd - fragFogCoord) / fogEnd;'nl
'    outColor.rgb = mix(fogColor, outColor.rgb, clamp(fogFactor, 0.0, 1.0));'nl
'  }'nl
'  outColor.rgb *= outColor.a;'nl
'}';

  FragmentShaderSourceQuad: String =
    FragmentShaderSourceQuad_Part1 +
    'void PLUG_color(inout vec4 color){}' + LineEnding +
    'void PLUG_texture_color(inout vec4 texture_color){}' + LineEnding +
    FragmentShaderSourceQuad_Part2;

  VertexShaderSourceMesh_Part1 =
{$ifdef GLES}
'#version 300 es'nl
{$else}
'#version 330'nl
{$endif}

'#define ROTATION_DEFAULT 0'nl
'#define ROTATION_PREVIOUS_POSITION 1'nl

'layout(location = 0) in vec4 inPosition;'nl
'layout(location = 1) in vec4 inTimeToLive;'nl
'layout(location = 2) in vec4 inSizeRotation;'nl
'layout(location = 3) in vec4 inColor;'nl
'layout(location = 9) in vec4 inRotationXY;'nl
'layout(location = 10) in vec4 inPreviousPosition;'nl
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
'uniform int rotationType;'nl
'uniform float time;'nl

CommonVertexFunctions nl'';

  VertexShaderSourceMesh_Part2 =
'void main() {'nl
'  if (inTimeToLive.x > 0.0) {'nl
'    vec2 texCoord = inTexcoord;'nl
'    PLUG_texture_coord(texCoord);'nl
'    fragTexCoord = texCoord;'nl
'    fragColor = inColor;'nl
'    mat3 m;'nl
'    if (rotationType == ROTATION_PREVIOUS_POSITION) {'nl
'      vec3 posDelta = normalize(inPreviousPosition.xyz - inPosition.xyz);'nl
'      m = createLookup(posDelta);'nl
'    } else {'nl
'      m = createRotate(vec3(inRotationXY.x, inRotationXY.z, inSizeRotation.z));'nl
'    }'nl
'    vec3 obj_vert = inVertex;'nl
'    PLUG_vertex_object_space(obj_vert);'nl
'    vec4 p = vOrMvMatrix * vec4(m * obj_vert * vec3(scaleX, scaleY, scaleZ) * vec3(inSizeRotation.x) + inPosition.xyz, 1.0);'nl
'    fragFogCoord = abs(p.z / p.w);'nl
'    gl_Position = pMatrix * p;'nl
'  } else'nl
'    gl_Position = vec4(-1.0, -1.0, -1.0, 1.0);'nl // Discard this vertex by making it outside of clip plane
'}';

  VertexShaderSourceMesh: String =
    VertexShaderSourceMesh_Part1 +
    'void PLUG_vertex_object_space(inout vec3 vertex_object){}' + LineEnding +
    'void PLUG_texture_coord(inout vec2 texture_coord){}' + LineEnding +
    VertexShaderSourceMesh_Part2;

  FragmentShaderSourceMesh_Part1 =
{$ifdef GLES}
'#version 300 es'nl
{$else}
'#version 330'nl
{$endif}
'precision lowp float;'nl
'in vec2 fragTexCoord;'nl
'in vec4 fragColor;'nl
'in float fragFogCoord;'nl

'out vec4 outColor;'nl

'uniform sampler2D baseColor;'nl
'uniform int fogEnable;'nl
'uniform float fogEnd;'nl
'uniform vec3 fogColor;'nl
'uniform float time;'nl'';

  FragmentShaderSourceMesh_Part2 =
'void main() {'nl
'  vec4 color = fragColor;'nl
'  PLUG_color(color);'nl
'  vec4 texture_color = texture(baseColor, fragTexCoord);'nl
'  PLUG_texture_color(texture_color);'nl
'  outColor = texture_color * color;'nl
'  if (fogEnable == 1) {'nl
'    float fogFactor = (fogEnd - fragFogCoord) / fogEnd;'nl
'    outColor.rgb = mix(fogColor, outColor.rgb, clamp(fogFactor, 0.0, 1.0));'nl
'  }'nl
'  outColor.rgb *= outColor.a;'nl
'}';

  FragmentShaderSourceMesh: String =
    FragmentShaderSourceMesh_Part1 +
    'void PLUG_color(inout vec4 color){}' + LineEnding +
    'void PLUG_texture_color(inout vec4 texture_color){}' + LineEnding +
    FragmentShaderSourceMesh_Part2;

{$ifdef GLES}
  FragmentShaderSourceDummy: String =
'#version 300 es'nl
'precision lowp float;'nl
'out vec4 outColor;'nl
'void main() {'nl
'  outColor = vec4(0.0);'nl
'}';
{$endif}

  Varyings: array[0..10] of String = (
    'outPosition',
    'outTimeToLive',
    'outSizeRotation',
    'outColor',
    'outColorDelta',
    'outStartPos',
    'outVelocity',
    'outDirection',
    'outTranslate',
    'outRotationXY',
    'outPreviousPosition'
  );

  BuiltInTexcoordArray: packed array[0..5] of TVector2 = (
    (X: 0; Y: 0), (X: 1; Y: 0), (X: 1; Y: 1),
    (X: 0; Y: 0), (X: 1; Y: 1), (X: 0; Y: 1)
  );

type
  TCastleParticleShaders = record
    Shader1, Shader2: TGLSLProgram;
  end;
  TCastleParticleShaderCache = TDictionary<String, TCastleParticleShaders>;

var
  ShaderCache: TCastleParticleShaderCache;

var
  IsCheckedForUsable: Boolean = False;
  TransformFeedbackProgram: TGLSLProgram = nil;
  TransformFeedbackProgramSingleInstance: TGLSLProgram = nil;
  TransformFeedbackProgramMultipleInstances: TGLSLProgram = nil;
  RenderProgram: TGLSLProgram = nil;
  RenderProgramMesh: TGLSLProgram = nil;
  RenderProgramQuad: TGLSLProgram = nil;

{$ifdef WASI}
procedure glGenBuffers(Size: TGLuint; V: PGLBuffer);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
  begin
    V^ := glCreateBuffer();
    Inc(V);
  end;
end;

procedure glDeleteBuffers(Size: TGLuint; V: PGLBuffer);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
  begin
    glDeleteBuffer(V^);
    Inc(V);
  end;
end;

procedure glGenVertexArrays(Size: TGLuint; V: PGLVertexArrayObject);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
  begin
    V^ := glCreateVertexArray();
    Inc(V);
  end;
end;

procedure glDeleteVertexArrays(Size: TGLuint; V: PGLVertexArrayObject);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
  begin
    glDeleteVertexArray(V^);
    Inc(V);
  end;
end;

procedure glGenTextures(Size: TGLuint; V: PGLTexture);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
  begin
    V^ := glCreateTexture();
    Inc(V);
  end;
end;

procedure glDeleteTextures(Size: TGLuint; V: PGLTexture);
var
  I: Integer;
begin
  for I := 0 to Size - 1 do
  begin
    glDeleteTexture(V^);
    Inc(V);
  end;
end;
{$endif}

{ Call when OpenGL context is closed }
procedure FreeGLContext;
var
  Key: String;
  Shaders: TCastleParticleShaders;
begin
  if TransformFeedbackProgramSingleInstance <> nil then
  begin
    FreeAndNil(TransformFeedbackProgramSingleInstance);
    FreeAndNil(TransformFeedbackProgramMultipleInstances);
    FreeAndNil(RenderProgramQuad);
    FreeAndNil(RenderProgramMesh);
  end;
  //
  if ShaderCache.Count > 0 then
    for Key in ShaderCache.Keys do
    begin
      Shaders := ShaderCache[Key];
      Shaders.Shader1.Free;
      Shaders.Shader2.Free;
    end;
  ShaderCache.Clear;
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
  if (PropertyName = 'TextureWidth')
    or (PropertyName = 'TextureHeight')
    or (PropertyName = 'Visible') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
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
    Self.GetColorForPersistent,
    Self.SetColorForPersistent,
    Self.FColor
  );
  Self.FColorVariancePersistent := CreateColorPersistent(
    Self.GetColorVarianceForPersistent,
    Self.SetColorVarianceForPersistent,
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

procedure TCastleParticleEffect.SetSourcePositionLocalVarianceForPersistent(const AValue: TVector3);
begin
  Self.FSourcePositionLocalVariance := AValue;
end;

function TCastleParticleEffect.GetSourcePositionLocalVarianceForPersistent: TVector3;
begin
  Result := FSourcePositionLocalVariance;
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

procedure TCastleParticleEffect.SetVertex0ForPersistent(const AValue: TVector3);
begin
  Self.FVertex_0 := AValue;
  Self.IsNeedRefresh := True;
end;

function TCastleParticleEffect.GetVertex0ForPersistent: TVector3;
begin
  Result := Self.FVertex_0;
end;

procedure TCastleParticleEffect.SetVertex1ForPersistent(const AValue: TVector3);
begin
  Self.FVertex_1 := AValue;
  Self.IsNeedRefresh := True;
end;

function TCastleParticleEffect.GetVertex1ForPersistent: TVector3;
begin
  Result := Self.FVertex_1;
end;

procedure TCastleParticleEffect.SetVertex2ForPersistent(const AValue: TVector3);
begin
  Self.FVertex_2 := AValue;
  Self.IsNeedRefresh := True;
end;

function TCastleParticleEffect.GetVertex2ForPersistent: TVector3;
begin
  Result := Self.FVertex_2;
end;

procedure TCastleParticleEffect.SetVertex3ForPersistent(const AValue: TVector3);
begin
  Self.FVertex_3 := AValue;
  Self.IsNeedRefresh := True;
end;

function TCastleParticleEffect.GetVertex3ForPersistent: TVector3;
begin
  Result := Self.FVertex_3;
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

procedure TCastleParticleEffect.SetMeshAsSourcePosition(const AValue: String);
begin
  Self.FMeshAsSourcePosition := AValue;
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

procedure TCastleParticleEffect.SetLifeSpan(const AValue: Single);
begin
  Self.FLifeSpan := AValue;
  Self.IsNeedRefresh := True;
end;

procedure TCastleParticleEffect.SetLifeSpanVariance(const AValue: Single);
begin
  Self.FLifeSpanVariance := AValue;
  Self.IsNeedRefresh := True;
end;

procedure TCastleParticleEffect.SetCustomTransformFeedbackVertexShader(const V: TStrings);
begin
  Self.CustomTransformFeedbackVertexShader.Assign(V);
  Self.IsNeedRefresh := True;
  Self.IsNeedRecompile := True;
end;

procedure TCastleParticleEffect.SetCustomRenderVertexShader(const V: TStrings);
begin
  Self.CustomRenderVertexShader.Assign(V);
  Self.IsNeedRefresh := True;
  Self.IsNeedRecompile := True;
end;

procedure TCastleParticleEffect.SetCustomRenderFragmentShader(const V: TStrings);
begin
  Self.CustomRenderFragmentShader.Assign(V);
  Self.IsNeedRefresh := True;
  Self.IsNeedRecompile := True;
end;

function TCastleParticleEffect.PropertySections(const PropertyName: String): TPropertySections;
begin
  if PropertyName = 'Tag' then
    Result := inherited PropertySections(PropertyName)
  else
    Result := [psBasic];
end;

constructor TCastleParticleEffect.Create(AOwner: TComponent);
begin
  inherited;
  Self.FBillboard := True;
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
  Self.FRotationType := prtDefault;
  Self.FSourceType := pstBox;
  Self.FVertex_0 := Vector3(-0.5, -0.5, 0);
  Self.FVertex_1 := Vector3( 0.5, -0.5, 0);
  Self.FVertex_2 := Vector3( 0.5,  0.5, 0);
  Self.FVertex_3 := Vector3(-0.5,  0.5, 0);
  //
  Self.FBoundingBoxMinPersistent := CreateVec3Persistent(
    Self.GetBoundingBoxMinForPersistent,
    Self.SetBoundingBoxMinForPersistent,
    Self.BBox.Data[0]
  );
  Self.FBoundingBoxMaxPersistent := CreateVec3Persistent(
    Self.GetBoundingBoxMaxForPersistent,
    Self.SetBoundingBoxMaxForPersistent,
    Self.BBox.Data[1]
  );
  Self.FRotationPersistent := CreateVec3Persistent(
    Self.GetRotationForPersistent,
    Self.SetRotationForPersistent,
    Self.FRotation
  );
  Self.FBillboard := True;
  Self.FRotationVariancePersistent := CreateVec3Persistent(
    Self.GetRotationVarianceForPersistent,
    Self.SetRotationVarianceForPersistent,
    Self.FRotationVariance
  );
  Self.FRotationSpeedPersistent := CreateVec3Persistent(
    Self.GetRotationSpeedForPersistent,
    Self.SetRotationSpeedForPersistent,
    Self.FRotation
  );
  Self.FRotationSpeedVariancePersistent := CreateVec3Persistent(
    Self.GetRotationSpeedVarianceForPersistent,
    Self.SetRotationSpeedVarianceForPersistent,
    Self.FRotationSpeedVariance
  );
  Self.FSourcePositionPersistent := CreateVec3Persistent(
    Self.GetSourcePositionForPersistent,
    Self.SetSourcePositionForPersistent,
    Self.FSourcePosition
  );
  Self.FSourcePositionVariancePersistent := CreateVec3Persistent(
    Self.GetSourcePositionVarianceForPersistent,
    Self.SetSourcePositionVarianceForPersistent,
    Self.FSourcePositionVariance
  );
  Self.FSourcePositionLocalVariancePersistent := CreateVec3Persistent(
    Self.GetSourcePositionLocalVarianceForPersistent,
    Self.SetSourcePositionLocalVarianceForPersistent,
    Self.FSourcePositionLocalVariance
  );
  Self.FDirectionPersistent := CreateVec3Persistent(
    Self.GetDirectionForPersistent,
    Self.SetDirectionForPersistent,
    Self.FDirection
  );
  Self.FGravityPersistent := CreateVec3Persistent(
    Self.GetGravityForPersistent,
    Self.SetGravityForPersistent,
    Self.FGravity
  );
  Self.FColorPersistent := CreateColorPersistent(
    Self.GetColorForPersistent,
    Self.SetColorForPersistent,
    Self.FColor
  );
  Self.FColorVariancePersistent := CreateColorPersistent(
    Self.GetColorVarianceForPersistent,
    Self.SetColorVarianceForPersistent,
    Self.FColorVariance
  );
  Self.FVertexPersistent_0 := CreateVec3Persistent(
    Self.GetVertex0ForPersistent,
    Self.SetVertex0ForPersistent,
    Self.FVertex_0
  );
  Self.FVertexPersistent_1 := CreateVec3Persistent(
    Self.GetVertex1ForPersistent,
    Self.SetVertex1ForPersistent,
    Self.FVertex_1
  );
  Self.FVertexPersistent_2 := CreateVec3Persistent(
    Self.GetVertex2ForPersistent,
    Self.SetVertex2ForPersistent,
    Self.FVertex_2
  );
  Self.FVertexPersistent_3 := CreateVec3Persistent(
    Self.GetVertex3ForPersistent,
    Self.SetVertex3ForPersistent,
    Self.FVertex_3
  );
  Self.FAnchors := TCollection.Create(TCastleParticleEffectAnchorItem);
  Self.FCustomTransformFeedbackVertexShader := TStringList.Create;
  Self.FCustomRenderVertexShader := TStringList.Create;
  Self.FCustomRenderFragmentShader := TStringList.Create;
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
  FreeAndNil(Self.FSourcePositionLocalVariancePersistent);
  FreeAndNil(Self.FDirectionPersistent);
  FreeAndNil(Self.FGravityPersistent);
  FreeAndNil(Self.FColorPersistent);
  FreeAndNil(Self.FColorVariancePersistent);
  FreeAndNil(Self.FVertexPersistent_0);
  FreeAndNil(Self.FVertexPersistent_1);
  FreeAndNil(Self.FVertexPersistent_2);
  FreeAndNil(Self.FVertexPersistent_3);
  FreeAndNil(Self.FCustomTransformFeedbackVertexShader);
  FreeAndNil(Self.FCustomRenderVertexShader);
  FreeAndNil(Self.FCustomRenderFragmentShader);
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

procedure TCastleParticleEffect.Changed;
begin
  Self.IsChanged := True;
end;

// ---------------------------------

procedure TCastleParticleEmitter.SetStartEmitting(const V: Boolean);
begin
  Self.FStartEmitting := V;
  if V and (FEffect <> nil) then
    Self.FCountdownTillRemove := Self.FEffect.LifeSpan + Self.FEffect.LifeSpanVariance;
end;

procedure TCastleParticleEmitter.SetBurst(const V: Boolean);
begin
  Self.FBurst := V;
  Self.FIsNeedRefresh := True;
end;

procedure TCastleParticleEmitter.SetSmoothTexture(const V: Boolean);
begin
  Self.FSmoothTexture := V;
  Self.FIsNeedRefresh := True;
end;

procedure TCastleParticleEmitter.SetAllowsInstancing(const V: Boolean);
begin
  Self.FAllowsInstancing := V;
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
  Self.Texture := GLObjectNone;
  Self.TextureAsSourcePosition := GLObjectNone;
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
  Self.FAttractorList := TVector4List.Create;
  Self.FAttractorKillDistanceList := TSingleList.Create;
  Self.FAttractorTypeList := TInt32List.Create;
  Self.FAttractorList.Count := 4;
  Self.FAttractorTypeList.Count := 4;
  Self.FAttractorKillDistanceList.Count := 4;
  Self.FDeltaTime := 0;
  Self.FTime := 0;

  {$ifdef WASI}
  Self.FEffectUBO.Anchor := TSingleList.Create;
  Self.FEffectUBO.Anchor.Count := 8;
  Self.FEffectUBO.AnchorSize := TSingleList.Create;
  Self.FEffectUBO.AnchorSize.Count := 8;
  Self.FEffectUBO.AnchorSizeVariance := TSingleList.Create;
  Self.FEffectUBO.AnchorSizeVariance.Count := 8;
  Self.FEffectUBO.AnchorColor := TVector4List.Create;
  Self.FEffectUBO.AnchorColor.Count := 5;
  Self.FEffectUBO.AnchorColorVariance := TVector4List.Create;
  Self.FEffectUBO.AnchorColorVariance.Count := 5;
  {$endif}

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
  {$ifdef WASI}
  Self.FEffectUBO.Anchor.Free;
  Self.FEffectUBO.AnchorSize.Free;
  Self.FEffectUBO.AnchorSizeVariance.Free;
  Self.FEffectUBO.AnchorColor.Free;
  Self.FEffectUBO.AnchorColorVariance.Free;
  {$endif}
  Self.FAttractorTypeList.Free;
  Self.FAttractorList.Free;
  Self.FAttractorKillDistanceList.Free;
  inherited;
end;

procedure TCastleParticleEmitter.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  I, AnchorCount, AttractorCount: Integer;
  RealSecondsPassed: Single;
  AnchorItem: TCastleParticleEffectAnchorItem;
begin
  inherited;
  if (not Self.Exists) or (Self.FEffect = nil) then
    Exit;
  Self.GLContextOpen;
  if Self.FIsNeedRefresh or Self.FEffect.IsNeedRefresh or Self.FEffect.IsNeedRecompile then
    Self.InternalRefreshEffect;

  if Self.FDeltaTime <= 0 then
    RealSecondsPassed := SecondsPassed
  else
    RealSecondsPassed := Self.FDeltaTime;
  if Self.TimePlaying then
    Self.FTime := Self.FTime + RealSecondsPassed * Self.TimePlayingSpeed
  else
    Self.FTime := Self.FTime + RealSecondsPassed;

  if not Self.FIsUpdated then
  begin
    if (FEmissionTime > 0) or (FEmissionTime = -1) then
    begin
      if FEmissionTime > 0 then
        if Self.TimePlaying then
          FEmissionTime := Max(0, FEmissionTime - RealSecondsPassed * Self.TimePlayingSpeed)
        else
          FEmissionTime := Max(0, FEmissionTime - RealSecondsPassed);
    end;

    if not Self.FStartEmitting then
    begin
      if Self.TimePlaying then
        Self.FCountdownTillRemove := Self.FCountdownTillRemove - RealSecondsPassed * Self.TimePlayingSpeed
      else
        Self.FCountdownTillRemove := Self.FCountdownTillRemove - RealSecondsPassed;
    end;

    if ((not Self.FAllowsUpdateWhenCulled) and Self.FIsDrawn) or Self.FAllowsUpdateWhenCulled then
    begin
      if Self.AllowsInstancing then
        TransformFeedbackProgram := Self.LocalTransformFeedbackProgramMultipleInstances
      else
        TransformFeedbackProgram := Self.LocalTransformFeedbackProgramSingleInstance;
      glEnable(GL_RASTERIZER_DISCARD);
      TransformFeedbackProgram.Enable;
      TransformFeedbackProgram.Uniform('time').SetValue(Self.FTime);
      if Self.TimePlaying then
        TransformFeedbackProgram.Uniform('deltaTime').SetValue(RealSecondsPassed * Self.TimePlayingSpeed)
      else
        TransformFeedbackProgram.Uniform('deltaTime').SetValue(RealSecondsPassed);
      if Self.FStartEmitting then
        TransformFeedbackProgram.Uniform('emissionTime').SetValue(Self.FEmissionTime)
      else
        TransformFeedbackProgram.Uniform('emissionTime').SetValue(0);
      if not Self.AllowsInstancing then
      begin
        TransformFeedbackProgram.Uniform('mMatrix').SetValue(Self.WorldTransform);
      end;
      // Build list of attractor
      AttractorCount := 0;
      for I := 0 to Self.Count - 1 do
      begin
        if AttractorCount >= 4 then Break;
        if Self.Items[I].Exists and Self.Items[I].Visible and (Self.Items[I] is TCastleParticleAttractor) then
        begin
          Self.FAttractorList[AttractorCount] := Vector4(Self.Items[I].Translation, -TCastleParticleAttractor(Self.Items[I]).Attraction);
          Self.FAttractorKillDistanceList[AttractorCount] := (TCastleParticleAttractor(Self.Items[I]).KillDistance);
          Self.FAttractorTypeList[AttractorCount] := CastleParticleAttractorType[TCastleParticleAttractor(Self.Items[I]).AttactorType];
          Inc(AttractorCount);
        end;
      end;
      TransformFeedbackProgram.Uniform('attractorCount').SetValue(AttractorCount);
      TransformFeedbackProgram.Uniform('attractors').SetValue(Self.FAttractorList);
      TransformFeedbackProgram.Uniform('attractorKillDistances').SetValue(Self.FAttractorKillDistanceList);
      TransformFeedbackProgram.Uniform('attractorType').SetValue(Self.FAttractorTypeList);

      // We want editor to always update UBO
      {$ifdef CASTLE_DESIGN_MODE}
        Self.FIsEffectChanged := True;
      {$endif}
      {$ifdef WASI}
        Self.FIsEffectChanged := True;
      {$endif}
      // We only update UBO if Effect.IsChanged = True
      if Self.FEffect.IsChanged or Self.FIsEffectChanged then
      begin
        // Build list of anchors
        AnchorCount := 1;
        Self.FEffectUBO.Anchor[0] := 0;
        Self.FEffectUBO.AnchorSize[0] := Self.FEffect.Size;
        Self.FEffectUBO.AnchorSizeVariance[0] := Self.FEffect.SizeVariance;
        Self.FEffectUBO.AnchorColor[0] := Self.FEffect.Color;
        Self.FEffectUBO.AnchorColorVariance[0] := Self.FEffect.ColorVariance;
        for I := 0 to Self.FEffect.Anchors.Count - 1 do
        begin
          // We limit number of anchors to 4 at the moment
          if I >= 4 then Break;
          AnchorItem := TCastleParticleEffectAnchorItem(Self.FEffect.Anchors.Items[I]);
          Self.FEffectUBO.Anchor[AnchorCount] := Min(0.99, Max(0.01, AnchorItem.TimeNormalized));
          Self.FEffectUBO.AnchorSize[AnchorCount] := AnchorItem.Size;
          Self.FEffectUBO.AnchorSizeVariance[AnchorCount] := AnchorItem.SizeVariance;
          Self.FEffectUBO.AnchorColor[AnchorCount] := AnchorItem.Color;
          Self.FEffectUBO.AnchorColorVariance[AnchorCount] := AnchorItem.ColorVariance;
          Inc(AnchorCount);
        end;
        {$ifdef WASI}
        TransformFeedbackProgram.Uniform('anchorCount').SetValue(AnchorCount);
        TransformFeedbackProgram.Uniform('anchor').SetValue(Self.FEffectUBO.Anchor);
        TransformFeedbackProgram.Uniform('anchorSize').SetValue(Self.FEffectUBO.AnchorSize);
        TransformFeedbackProgram.Uniform('anchorSizeVariance').SetValue(Self.FEffectUBO.AnchorSizeVariance);
        TransformFeedbackProgram.Uniform('anchorColor').SetValue(Self.FEffectUBO.AnchorColor);
        TransformFeedbackProgram.Uniform('anchorColorVariance').SetValue(Self.FEffectUBO.AnchorColorVariance);
        TransformFeedbackProgram.Uniform('textureAsSourcePositionSize').SetValue(Self.FEffectUBO.TextureAsSourcePositionSize);

        TransformFeedbackProgram.Uniform('sourceType').SetValue(TGLint(CastleParticleSourceValues[Self.FEffect.SourceType]));
        TransformFeedbackProgram.Uniform('sourcePosition').SetValue(Self.FEffect.SourcePosition);
        TransformFeedbackProgram.Uniform('sourcePositionVariance').SetValue(Self.FEffect.SourcePositionVariance);
        TransformFeedbackProgram.Uniform('sourcePositionLocalVariance').SetValue(Self.FEffect.SourcePositionLocalVariance);
        TransformFeedbackProgram.Uniform('maxParticles').SetValue(Self.FEffect.MaxParticles);
        TransformFeedbackProgram.Uniform('particleLifeSpan').SetValue(Self.FEffect.LifeSpan);
        TransformFeedbackProgram.Uniform('particleLifeSpanVariance').SetValue(Self.FEffect.LifeSpanVariance);
        TransformFeedbackProgram.Uniform('rotation').SetValue(Self.FEffect.Rotation);
        TransformFeedbackProgram.Uniform('rotationVariance').SetValue(Self.FEffect.RotationVariance);
        TransformFeedbackProgram.Uniform('rotationSpeed').SetValue(Self.FEffect.RotationSpeed);
        TransformFeedbackProgram.Uniform('rotationSpeedVariance').SetValue(Self.FEffect.RotationSpeedVariance);
        TransformFeedbackProgram.Uniform('direction').SetValue(Self.FEffect.Direction);
        TransformFeedbackProgram.Uniform('directionVariance').SetValue(Self.FEffect.DirectionVariance);
        TransformFeedbackProgram.Uniform('speed').SetValue(Self.FEffect.Speed);
        TransformFeedbackProgram.Uniform('speedVariance').SetValue(Self.FEffect.SpeedVariance);
        TransformFeedbackProgram.Uniform('gravity').SetValue(Self.FEffect.Gravity);
        TransformFeedbackProgram.Uniform('radial').SetValue(Self.FEffect.Radial);
        TransformFeedbackProgram.Uniform('radialVariance').SetValue(Self.FEffect.RadialVariance);
        {$else}
        Self.FEffectUBO.AnchorCount := AnchorCount;
        Self.FEffectUBO.SourceType := CastleParticleSourceValues[Self.FEffect.SourceType];
        Self.FEffectUBO.SourcePosition := Self.FEffect.SourcePosition;
        Self.FEffectUBO.SourcePositionVariance := Self.FEffect.SourcePositionVariance;
        Self.FEffectUBO.SourcePositionLocalVariance := Self.FEffect.SourcePositionLocalVariance;
        Self.FEffectUBO.MaxParticles := Self.FEffect.MaxParticles;
        Self.FEffectUBO.LifeSpan := Self.FEffect.LifeSpan;
        Self.FEffectUBO.LifeSpanVariance := Self.FEffect.LifeSpanVariance;
        Self.FEffectUBO.Rotation := Self.FEffect.Rotation;
        Self.FEffectUBO.RotationVariance := Self.FEffect.RotationVariance;
        Self.FEffectUBO.RotationSpeed := Self.FEffect.RotationSpeed;
        Self.FEffectUBO.RotationSpeedVariance := Self.FEffect.RotationSpeedVariance;
        Self.FEffectUBO.Direction := Self.FEffect.Direction;
        Self.FEffectUBO.DirectionVariance := Self.FEffect.DirectionVariance;
        Self.FEffectUBO.Speed := Self.FEffect.Speed;
        Self.FEffectUBO.SpeedVariance := Self.FEffect.SpeedVariance;
        Self.FEffectUBO.Gravity := Self.FEffect.Gravity;
        Self.FEffectUBO.Radial := Self.FEffect.Radial;
        Self.FEffectUBO.RadialVariance := Self.FEffect.RadialVariance;

        glBindBuffer(GL_UNIFORM_BUFFER, Self.UBO);
        glBufferSubData(GL_UNIFORM_BUFFER, 0, SizeOf(TCastleParticleEffectUBO), @Self.FEffectUBO);
        glBindBuffer(GL_UNIFORM_BUFFER, GLObjectNone);
        {$endif}
        Self.FIsEffectChanged := False;
      end;

      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, Self.TextureAsSourcePosition);
      glBindVertexArray(Self.VAOTnFs[Self.CurrentBuffer]);
      {$ifndef WASI}
      glBindBufferBase(GL_UNIFORM_BUFFER, 0, Self.UBO);
      {$endif}
      glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, Self.VBOTnFs[(Self.CurrentBuffer + 1) mod 2]);
      glBeginTransformFeedback(GL_POINTS);
      glDrawArrays(GL_POINTS, 0, Self.FEffect.MaxParticles);
      glEndTransformFeedback();
      glDisable(GL_RASTERIZER_DISCARD);
      glBindVertexArray(GLObjectNone);
      glBindTexture(GL_TEXTURE_2D, GLObjectNone);
      CurrentBuffer := (Self.CurrentBuffer + 1) mod 2;
    end;
  end;

  RemoveMe := rtNone;
  if Self.FReleaseWhenDone {$ifdef CASTLE_DESIGN_MODE}and (InternalCastleApplicationMode in [appSimulation, appSimulationPaused]){$endif} then
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
  RenderCameraPosition: TVector3;
  RelativeBBox: TBox3D;
begin
  inherited;
  Self.FIsUpdated := False;
  if (not Self.Exists) or (Self.FEffect = nil) then
    Exit;
  if not Self.FIsGLContextInitialized then
    Exit;
  // We set it here, because 1 effect can be used by multiple emitters
  Self.FEffect.IsNeedRefresh := False;
  Self.FEffect.IsChanged := False;
  //
  if not Self.Visible then
    Exit;
  if (not Self.FStartEmitting) and (Self.FCountdownTillRemove <= 0) then
    Exit;
  //if not Self.ExcludeFromStatistics then
    Inc(Params.Statistics.ScenesVisible);
  if DistanceCulling > 0 then
  begin
    RenderCameraPosition := Params.Transformation^.InverseTransform.MultPoint(Params.RenderingCamera.Camera.Position);
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
  //if not Self.ExcludeFromStatistics then
  begin
    Inc(Params.Statistics.ShapesVisible);
    Inc(Params.Statistics.ShapesRendered);
    Inc(Params.Statistics.ScenesRendered);
  end;
  Self.FGlobalFog := TFogNode(Params.GlobalFog);
  Self.FCameraMatrix := Params.RenderingCamera.Matrix;

  Params.AddRenderEvent(Self.InternalRender);
end;

procedure TCastleParticleEmitter.InternalRender(const Transformation: TTransformation; const PassParams: TRenderOnePassParams);
var
  Fog: TFogFunctionality;
  M: TMatrix4;
  IndicesCount: Integer;
  PreviousProgram: TGLSLProgram;
  OldDepthTest, OldDepthBufferUpdate: Boolean;
begin
  if PassParams.DisableShadowVolumeCastingLights or
     (not PassParams.UsingBlending) or
     PassParams.InsideStencilTest then
    exit;

  PreviousProgram := RenderContext.CurrentProgram;

  // Draw particles
  if Self.AllowsInstancing then
    TransformFeedbackProgram := Self.LocalTransformFeedbackProgramMultipleInstances
  else
    TransformFeedbackProgram := Self.LocalTransformFeedbackProgramSingleInstance;
  if Self.FEffect.Billboard then
    RenderProgram := Self.LocalRenderProgramQuad
  else
    RenderProgram := Self.LocalRenderProgramMesh;
  OldDepthBufferUpdate := RenderContext.DepthBufferUpdate;
  if not Self.FAllowsWriteToDepthBuffer then
    RenderContext.DepthBufferUpdate := False
  else
    RenderContext.DepthBufferUpdate := True;
  // Get global fog
  if Self.FEnableFog and (Self.FGlobalFog <> nil) then
    Fog := (Self.FGlobalFog as TFogNode).Functionality(TFogFunctionality) as TFogFunctionality
  else
    Fog := nil;
  //
  OldDepthTest := RenderContext.DepthTest;
  RenderContext.DepthTest := True;
  RenderContext.BlendingEnable(TBlendingSourceFactor(CastleParticleBlendValues[Self.FEffect.BlendFuncSource]), TBlendingDestinationFactor(CastleParticleBlendValues[Self.FEffect.BlendFuncDestination]));
  RenderProgram.Enable;
  RenderProgram.Uniform('scaleX').SetValue(Vector3(Transformation.Transform[0,0], Transformation.Transform[0,1], Transformation.Transform[0,2]).Length);
  RenderProgram.Uniform('scaleY').SetValue(Vector3(Transformation.Transform[1,0], Transformation.Transform[1,1], Transformation.Transform[1,2]).Length);
  RenderProgram.Uniform('scaleZ').SetValue(Vector3(Transformation.Transform[2,0], Transformation.Transform[2,1], Transformation.Transform[2,2]).Length);
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
    M := Self.FCameraMatrix * Transformation.Transform;
    RenderProgram.Uniform('vOrMvMatrix').SetValue(M);
  end else
  begin
    RenderProgram.Uniform('vOrMvMatrix').SetValue(Self.FCameraMatrix);
  end;
  RenderProgram.Uniform('pMatrix').SetValue(RenderContext.ProjectionMatrix);
  RenderProgram.Uniform('rotationType').SetValue(Integer(Self.FEffect.RotationType));
  RenderProgram.Uniform('time').SetValue(Self.FTime);
  glBindVertexArray(Self.VAOMeshes[Self.CurrentBuffer]);
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
    glDrawElementsInstanced(GL_TRIANGLES, IndicesCount, GL_UNSIGNED_SHORT, {$ifdef WASI}0{$else}nil{$endif}, Self.FEffect.MaxParticles);
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, GLObjectNone);
  glBindVertexArray(GLObjectNone);
  // Render boundingbox in editor
  {$ifdef CASTLE_DESIGN_MODE}
  Self.FDebugBox.Box := Self.FEffect.BBox;
  {$endif}
  if not Self.FAllowsWriteToDepthBuffer then
    glDepthMask(GL_TRUE);
  if PreviousProgram <> nil then
  begin
    PreviousProgram.Enable;
  end;
  RenderContext.BlendingEnable(bsOne, bdZero);
  RenderContext.DepthTest := OldDepthTest;
  RenderContext.DepthBufferUpdate := OldDepthBufferUpdate;
end;

procedure TCastleParticleEmitter.LoadEffect(const AEffect: TCastleParticleEffect);
begin
  Self.FEffect := AEffect;
  if Self.FEffect <> nil then
  begin
    Self.FEffect.FreeNotification(Self);
    Self.FEffect.IsNeedRecompile := True;
  end;
  RefreshEffect;
end;

function TCastleParticleEmitter.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Burst')
    or (PropertyName = 'AllowsInstancing')
    or (PropertyName = 'StartEmitting')
    or (PropertyName = 'Effect') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleParticleEmitter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Self.FEffect then
      Self.LoadEffect(nil);
  end;
end;

procedure TCastleParticleEmitter.GLContextOpen;
var
  V: TGLuint;
  ProgramId: TGLProgram;
  EffectUniformIndex: TGLuint;
begin
  // Safeguard
  if not ApplicationProperties.IsGLContextOpen then Exit;
  if Self.FIsGLContextInitialized then Exit;

  if not IsCheckedForUsable then
  begin
    // Check maximum number of vertex attributes
    {$ifdef WASI}
      V := glGetParameter(GL_MAX_VERTEX_ATTRIBS);
    {$else}
      glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, @V);
    {$endif}
    WritelnLog('GL_MAX_VERTEX_ATTRIBS: ' + IntToStr(V));
    if V < 16 then
      raise Exception.Create('TCastleParticleEmitter requires GL_MAX_VERTEX_ATTRIBS at least 16');
    // Check GL_MAX_VERTEX_UNIFORM_COMPONENTS
    {$ifdef WASI}
      V := glGetParameter(GL_MAX_VERTEX_UNIFORM_COMPONENTS);
    {$else}
      glGetIntegerv($8B4A, @V);
    {$endif}
    WritelnLog('GL_MAX_VERTEX_UNIFORM_COMPONENTS: ' + IntToStr(V));
    // Check GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
    {$ifdef WASI}
      V := glGetParameter(GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS);
    {$else}
      glGetIntegerv(GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS, @V);
    {$endif}
    WritelnLog('GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS: ' + IntToStr(V));
    IsCheckedForUsable := True;
  end;

  if TransformFeedbackProgramSingleInstance = nil then
  begin
    try
      TransformFeedbackProgramSingleInstance := TGLSLProgram.Create;
      TransformFeedbackProgramSingleInstance.AttachVertexShader(TransformVertexShaderSourceSingleInstance);
      {$ifdef GLES}
        TransformFeedbackProgramSingleInstance.AttachFragmentShader(FragmentShaderSourceDummy);
      {$endif}
      TransformFeedbackProgramSingleInstance.SetTransformFeedbackVaryings(Varyings);
      TransformFeedbackProgramSingleInstance.Link;

      TransformFeedbackProgramMultipleInstances := TGLSLProgram.Create;
      TransformFeedbackProgramMultipleInstances.AttachVertexShader(TransformVertexShaderSourceMultipleInstances);
      {$ifdef GLES}
        TransformFeedbackProgramMultipleInstances.AttachFragmentShader(FragmentShaderSourceDummy);
      {$endif}
      TransformFeedbackProgramMultipleInstances.SetTransformFeedbackVaryings(Varyings);
      TransformFeedbackProgramMultipleInstances.Link;

      // CGE doesn't expose ProgramId, so we work around by quering for it from OpenGL
      {$ifndef WASI}
      TransformFeedbackProgramSingleInstance.Enable;
      glGetIntegerv(GL_CURRENT_PROGRAM, @ProgramId);
      EffectUniformIndex := glGetUniformBlockIndex(ProgramId, 'Effect');
      glUniformBlockBinding(ProgramId, EffectUniformIndex, 0);
      TransformFeedbackProgramSingleInstance.Disable;

      TransformFeedbackProgramMultipleInstances.Enable;
      glGetIntegerv(GL_CURRENT_PROGRAM, @ProgramId);
      EffectUniformIndex := glGetUniformBlockIndex(ProgramId, 'Effect');
      glUniformBlockBinding(ProgramId, EffectUniformIndex, 0);
      TransformFeedbackProgramMultipleInstances.Disable;
      {$endif}

      RenderProgramQuad := TGLSLProgram.Create;
      RenderProgramQuad.AttachVertexShader(VertexShaderSourceQuad);
      RenderProgramQuad.AttachFragmentShader(FragmentShaderSourceQuad);
      RenderProgramQuad.Link;

      RenderProgramMesh := TGLSLProgram.Create;
      RenderProgramMesh.AttachVertexShader(VertexShaderSourceMesh);
      RenderProgramMesh.AttachFragmentShader(FragmentShaderSourceMesh);
      RenderProgramMesh.Link;
    except
      on E: Exception do
        WritelnWarning('CastleParticleEmitter', E.Message);
    end;

    ApplicationProperties.OnGLContextClose.Add(@FreeGLContext);
  end;

  Self.LocalRenderProgramMesh := RenderProgramMesh;
  Self.LocalRenderProgramQuad := RenderProgramQuad;
  Self.LocalTransformFeedbackProgramSingleInstance := TransformFeedbackProgramSingleInstance;
  Self.LocalTransformFeedbackProgramMultipleInstances := TransformFeedbackProgramMultipleInstances;

  glGenBuffers(2, @Self.VBOTnFs);
  glGenVertexArrays(2, @Self.VAOTnFs);
  glGenBuffers(1, @Self.VBOMesh);
  glGenBuffers(1, @Self.VBOMeshIndices);
  glGenVertexArrays(2, @Self.VAOMeshes);
  {$ifndef WASI}
  glGenBuffers(1, @Self.UBO);
  {$endif}
  Self.FIsGLContextInitialized := True;
  Self.FIsNeedRefresh := True;
  //
  if Self.FEffect <> nil then
    Self.FEffect.IsNeedRecompile := True;
end;

procedure TCastleParticleEmitter.GLContextClose;
var
  P: Pointer;
  Size: Integer;
begin
  if Self.FIsGLContextInitialized then
  begin
    if not (csDestroying in Self.ComponentState) then
    begin
      Size := Self.FEffect.MaxParticles * SizeOf(TCastleParticle);
      // Backup current state of particles to RAM
      glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, Self.VBOTnFs[Self.CurrentBuffer]);
      {$ifndef WASI}
      P := glMapBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, Size, GL_MAP_READ_BIT);
      if P <> nil then
      begin
        SetLength(Self.Particles, Self.FEffect.MaxParticles);
        System.Move(P^, Self.Particles[0], Size);
      end else
        WritelnLog('Error while backing up particles''s state to RAM: ' + IntToStr(glGetError()));
      glUnmapBuffer(GL_TRANSFORM_FEEDBACK_BUFFER);
      {$endif}
      glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, GLObjectNone);
    end;
    glDeleteBuffers(1, @Self.VBOMesh);
    glDeleteBuffers(1, @Self.VBOMeshIndices);
    glDeleteVertexArrays(2, @Self.VAOMeshes);
    glDeleteBuffers(2, @Self.VBOTnFs);
    glDeleteVertexArrays(2, @Self.VAOTnFs);
    {$ifndef WASI}
    glDeleteBuffers(1, @Self.UBO);
    {$endif}
    glDeleteTextures(1, @Self.Texture);
    if Self.TextureAsSourcePosition <> GLObjectNone then
      glDeleteTextures(1, @Self.TextureAsSourcePosition);
    Self.FIsGLContextInitialized := False;

    {$ifdef CASTLE_DESIGN_MODE}
      if Self.LocalRenderProgramMesh <> RenderProgramMesh then
        FreeAndNil(Self.LocalRenderProgramMesh);
      if Self.LocalRenderProgramQuad <> RenderProgramQuad then
        FreeAndNil(Self.LocalRenderProgramQuad);
      if Self.LocalTransformFeedbackProgramSingleInstance <> TransformFeedbackProgramSingleInstance then
        FreeAndNil(Self.LocalTransformFeedbackProgramSingleInstance);
      if Self.LocalTransformFeedbackProgramMultipleInstances <> TransformFeedbackProgramMultipleInstances then
        FreeAndNil(Self.LocalTransformFeedbackProgramMultipleInstances);
    {$endif}
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
  IndexList: TInt32List;
begin
  SetLength(Self.ParticleMeshIndices, 0);
  SetLength(Self.ParticleMesh, 0);
  // Load built-in mesh if Mesh is empty
  if (Self.FEffect.Mesh = '') or (not URIFileExists(Self.FEffect.Mesh)) then
  begin
    SetLength(Self.ParticleMesh, 6);
    // Generate built-in mesh
    for I := 0 to 5 do
    begin
      Self.ParticleMesh[I].Texcoord := BuiltInTexcoordArray[I];
    end;
    Self.ParticleMesh[0].Vertex := Self.FEffect.Vertex_0;
    Self.ParticleMesh[1].Vertex := Self.FEffect.Vertex_1;
    Self.ParticleMesh[2].Vertex := Self.FEffect.Vertex_2;
    Self.ParticleMesh[3].Vertex := Self.FEffect.Vertex_0;
    Self.ParticleMesh[4].Vertex := Self.FEffect.Vertex_2;
    Self.ParticleMesh[5].Vertex := Self.FEffect.Vertex_3;
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
        NormalList := Shape.NormalsSmooth(True);
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

procedure TCastleParticleEmitter.InternalLoadMeshAsSourcePosition;
var
  I: Integer;
  Scene: TCastleScene;
  ShapeList: TShapeList;
  Shape: TShape;
  ShapeNode: TShapeNode;
  IndexedTriangleSetNode: TIndexedTriangleSetNode;
  CoordNode: TCoordinateNode;
  VertexList: TVector3List;
  TextureData: packed array of TVector3;
  MaxTextureSize,
  ActualTextureSize: Integer;

  {$ifdef WASI}
  function TextureJSData: IJSTypedArray;
  var
    Len: Cardinal;
    ResultFloat32: IJSFloat32Array;
  begin
    Len := Length(TextureData) * 3;
    ResultFloat32 := TJSFloat32Array.Create(Len);
    ResultFloat32.CopyFromMemory(@TextureData[0], Len);
    Result := ResultFloat32;
  end;
  {$endif}

begin
  if (Self.FEffect.MeshAsSourcePosition = '') or (not URIFileExists(Self.FEffect.MeshAsSourcePosition)) then exit;
  {$ifdef WASI}
    MaxTextureSize := glGetParameter(GL_MAX_TEXTURE_SIZE);
  {$else}
    glGetIntegerv(GL_MAX_TEXTURE_SIZE, @MaxTextureSize);
  {$endif}
  Scene := TCastleScene.Create(nil);
  Scene.URL := Self.FEffect.MeshAsSourcePosition;
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
      VertexList := CoordNode.FdPoint.Items;
      //
      ActualTextureSize := Min(MaxTextureSize, VertexList.Count);
      SetLength(TextureData, ActualTextureSize);
      for I := 0 to VertexList.Count - 1 do
      begin
        TextureData[I] := VertexList[I];
      end;
      Self.FEffectUBO.TextureAsSourcePositionSize := ActualTextureSize;
      // Generate texture
      if Self.TextureAsSourcePosition = GLObjectNone then
        glGenTextures(1, @Self.TextureAsSourcePosition);
      glBindTexture(GL_TEXTURE_2D, Self.TextureAsSourcePosition);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      // TODO: Optimize MaxTextureSize, support actual 2D texture
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB32F, ActualTextureSize, 1, 0, GL_RGB, GL_FLOAT, {$ifdef WASI}TextureJSData{$else}@TextureData[0]{$endif});
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

procedure TCastleParticleEmitter.InternalRefreshEffect;
var
  ProgramId: TGLProgram;
  EffectUniformIndex: TGLuint;
  I: Integer;
  Key, PlugVertex, PlugFragment, Plug1, Plug2, SrcVertex, SrcFragment: String;
  Shaders: TCastleParticleShaders;
begin
  if Self.FEffect = nil then
    Exit;
  // Only process if texture exists
  if (not URIFileExists(Self.FEffect.Texture)) and (Self.Effect.TextureViewport = nil) then
    Exit;

  // Do we need to recompile shaders?
  if Self.FEffect.IsNeedRecompile then
  begin
    try
      Self.FEffect.IsNeedRecompile := False;
      // TransformFeedback shader
      PlugVertex := Trim(Self.FEffect.CustomTransformFeedbackVertexShader.Text);
      if PlugVertex <> '' then
      begin
        Key := MD5Print(MD5String(PlugVertex));
        if ShaderCache.ContainsKey(Key) then
        begin
          Shaders := ShaderCache[Key];
          Self.LocalTransformFeedbackProgramSingleInstance := Shaders.Shader1;
          Self.LocalTransformFeedbackProgramMultipleInstances := Shaders.Shader2;
        end else
        begin
          // Single instance
          SrcVertex := TransformVertexShaderSourceSingleInstance_Part1 + PlugVertex + TransformVertexShaderSourceSingleInstance_Part2;
          {$ifdef CASTLE_DESIGN_MODE}
            if Self.LocalTransformFeedbackProgramSingleInstance <> TransformFeedbackProgramSingleInstance then
              FreeAndNil(Self.LocalTransformFeedbackProgramSingleInstance);
          {$endif}
          Self.LocalTransformFeedbackProgramSingleInstance := TGLSLProgram.Create;
          Self.LocalTransformFeedbackProgramSingleInstance.AttachVertexShader(SrcVertex);
          {$ifdef GLES}
            Self.LocalTransformFeedbackProgramSingleInstance.AttachFragmentShader(FragmentShaderSourceDummy);
          {$endif}
          Self.LocalTransformFeedbackProgramSingleInstance.SetTransformFeedbackVaryings(Varyings);
          Self.LocalTransformFeedbackProgramSingleInstance.Link;

          // Multiple instances
          SrcVertex := TransformVertexShaderSourceMultipleInstances_Part1 + PlugVertex + TransformVertexShaderSourceMultipleInstances_Part2;
          {$ifdef CASTLE_DESIGN_MODE}
            if Self.LocalTransformFeedbackProgramMultipleInstances <> TransformFeedbackProgramMultipleInstances then
              FreeAndNil(Self.LocalTransformFeedbackProgramMultipleInstances);
          {$endif}
          Self.LocalTransformFeedbackProgramMultipleInstances := TGLSLProgram.Create;
          Self.LocalTransformFeedbackProgramMultipleInstances.AttachVertexShader(SrcVertex);
          {$ifdef GLES}
            Self.LocalTransformFeedbackProgramMultipleInstances.AttachFragmentShader(FragmentShaderSourceDummy);
          {$endif}
          Self.LocalTransformFeedbackProgramMultipleInstances.SetTransformFeedbackVaryings(Varyings);
          Self.LocalTransformFeedbackProgramMultipleInstances.Link;

          {$ifndef WASI}
          // CGE doesn't expose ProgramId, so we work around by quering for it from OpenGL
          Self.LocalTransformFeedbackProgramSingleInstance.Enable;
          glGetIntegerv(GL_CURRENT_PROGRAM, @ProgramId);
          EffectUniformIndex := glGetUniformBlockIndex(ProgramId, 'Effect');
          glUniformBlockBinding(ProgramId, EffectUniformIndex, 0);
          Self.LocalTransformFeedbackProgramSingleInstance.Disable;

          Self.LocalTransformFeedbackProgramMultipleInstances.Enable;
          glGetIntegerv(GL_CURRENT_PROGRAM, @ProgramId);
          EffectUniformIndex := glGetUniformBlockIndex(ProgramId, 'Effect');
          glUniformBlockBinding(ProgramId, EffectUniformIndex, 0);
          Self.LocalTransformFeedbackProgramMultipleInstances.Disable;
          {$endif}

          Shaders.Shader1 := Self.LocalTransformFeedbackProgramSingleInstance;
          Shaders.Shader2 := Self.LocalTransformFeedbackProgramMultipleInstances;
          {$ifndef CASTLE_DESIGN_MODE}
            ShaderCache.AddOrSetValue(Key, Shaders);
          {$endif}
        end;
      end else
      begin
        Self.LocalTransformFeedbackProgramSingleInstance := TransformFeedbackProgramSingleInstance;
        Self.LocalTransformFeedbackProgramMultipleInstances := TransformFeedbackProgramMultipleInstances;
      end;
    except
      on E: Exception do
      begin
        WritelnWarning('CastleParticleEmitter', E.Message);
        Self.LocalTransformFeedbackProgramSingleInstance := TransformFeedbackProgramSingleInstance;
        Self.LocalTransformFeedbackProgramMultipleInstances := TransformFeedbackProgramMultipleInstances;
      end;
    end;
    try
      // Render shader
      PlugVertex := Trim(Self.FEffect.CustomRenderVertexShader.Text);
      PlugFragment := Trim(Self.FEffect.CustomRenderFragmentShader.Text);
      if (PlugVertex <> '') or (PlugFragment <> '') then
      begin
        Key := MD5Print(MD5String(PlugVertex + PlugFragment));
        if ShaderCache.ContainsKey(Key) then
        begin
          Shaders := ShaderCache[Key];
          Self.LocalRenderProgramQuad := Shaders.Shader1;
          Self.LocalRenderProgramMesh := Shaders.Shader2;
        end else
        begin
          // Quad
          {$ifdef CASTLE_DESIGN_MODE}
            if Self.LocalRenderProgramQuad <> RenderProgramQuad then
              FreeAndNil(Self.LocalRenderProgramQuad);
          {$endif}
          Self.LocalRenderProgramQuad := TGLSLProgram.Create;
          if PlugVertex <> '' then
          begin
            if PlugVertex.IndexOf('PLUG_vertex_object_space') >= 0 then
              Plug1 := ''
            else
              Plug1 := 'void PLUG_vertex_object_space(inout vec3 vertex_object){}' + LineEnding;
            if PlugVertex.IndexOf('PLUG_texture_coord') >= 0 then
              Plug2 := ''
            else
              Plug2 := 'void PLUG_texture_coord(inout vec2 texture_coord){}' + LineEnding;
            SrcVertex := VertexShaderSourceQuad_Part1 + Plug1 + Plug2 + PlugVertex + VertexShaderSourceQuad_Part2;
            Self.LocalRenderProgramQuad.AttachVertexShader(SrcVertex);
          end else
            Self.LocalRenderProgramQuad.AttachVertexShader(VertexShaderSourceQuad);
          //
          if PlugFragment <> '' then
          begin
            if PlugFragment.IndexOf('PLUG_color') >= 0 then
              Plug1 := ''
            else
              Plug1 := 'void PLUG_color(inout vec4 color){}' + LineEnding;
            if PlugFragment.IndexOf('PLUG_texture_color') >= 0 then
              Plug2 := ''
            else
              Plug2 := 'void PLUG_texture_color(inout vec4 texture_color){}' + LineEnding;
            SrcFragment := FragmentShaderSourceMesh_Part1 + Plug1 + Plug2 + PlugFragment + FragmentShaderSourceMesh_Part2;
            Self.LocalRenderProgramQuad.AttachFragmentShader(SrcFragment)
          end else
            Self.LocalRenderProgramQuad.AttachFragmentShader(FragmentShaderSourceQuad);
          Self.LocalRenderProgramQuad.Link;

          // Mesh
          {$ifdef CASTLE_DESIGN_MODE}
            if Self.LocalRenderProgramMesh <> RenderProgramMesh then
              FreeAndNil(Self.LocalRenderProgramMesh);
          {$endif}
          Self.LocalRenderProgramMesh := TGLSLProgram.Create;
          if PlugVertex <> '' then
          begin
            if PlugVertex.IndexOf('void PLUG_vertex_object_space') >= 0 then
              Plug1 := ''
            else
              Plug1 := 'void PLUG_vertex_object_space(inout vec3 vertex_object){}' + LineEnding;
            if PlugVertex.IndexOf('void PLUG_texture_coord') >= 0 then
              Plug2 := ''
            else
              Plug2 := 'void PLUG_texture_coord(inout vec2 texture_coord){}' + LineEnding;
            SrcVertex := VertexShaderSourceMesh_Part1 + Plug1 + Plug2 + PlugVertex + VertexShaderSourceMesh_Part2;
            Self.LocalRenderProgramMesh.AttachVertexShader(SrcVertex);
          end else
            Self.LocalRenderProgramMesh.AttachVertexShader(VertexShaderSourceMesh);
          //
          if PlugFragment <> '' then
          begin
            if PlugFragment.IndexOf('PLUG_color') >= 0 then
              Plug1 := ''
            else
              Plug1 := 'void PLUG_color(inout vec4 color){}' + LineEnding;
            if PlugFragment.IndexOf('PLUG_texture_color') >= 0 then
              Plug2 := ''
            else
              Plug2 := 'void PLUG_texture_color(inout vec4 texture_color){}' + LineEnding;
            SrcFragment := FragmentShaderSourceMesh_Part1 + Plug1 + Plug2 + PlugFragment + FragmentShaderSourceMesh_Part2;
            Self.LocalRenderProgramMesh.AttachFragmentShader(SrcFragment)
          end else
            Self.LocalRenderProgramMesh.AttachFragmentShader(FragmentShaderSourceMesh);
          Self.LocalRenderProgramMesh.Link;

          Shaders.Shader1 := Self.LocalRenderProgramQuad;
          Shaders.Shader2 := Self.LocalRenderProgramMesh;
          {$ifndef CASTLE_DESIGN_MODE}
            ShaderCache.AddOrSetValue(Key, Shaders);
          {$endif}
        end;
      end else
      begin
        Self.LocalRenderProgramQuad := RenderProgramQuad;
        Self.LocalRenderProgramMesh := RenderProgramMesh;
      end;
    except
      on E: Exception do
      begin
        WritelnWarning('CastleParticleEmitter', E.Message);
        Self.LocalRenderProgramQuad := RenderProgramQuad;
        Self.LocalRenderProgramMesh := RenderProgramMesh;
      end;
    end;
  end;

  Self.FEmissionTime := Self.FEffect.Duration;
  Self.FParticleCount := Self.FEffect.MaxParticles;
  Self.FCountdownTillRemove := Self.FEffect.LifeSpan + Self.FEffect.LifeSpanVariance;
  Self.InternalLoadMesh;

  if Self.FEffect.LifeSpan = 0 then
    Self.FEffect.LifeSpan := 0.001;

  glDeleteTextures(1, @Self.Texture);
  if Self.TextureAsSourcePosition <> GLObjectNone then
  begin
    glDeleteTextures(1, @Self.TextureAsSourcePosition);
    Self.TextureAsSourcePosition := GLObjectNone;
  end;
  Self.FEffectUBO.TextureAsSourcePositionSize := 0;
  Self.InternalLoadMeshAsSourcePosition;

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

  { Here we only generate particles's data if the length of the array is 0.
    Otherwise it's already contains previous data from VRAM. }
  if Length(Self.Particles) = 0 then
  begin
    // Generate initial lifecycle
    SetLength(Self.Particles, Self.FEffect.MaxParticles);
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
  end;

  // UBO
  if Self.AllowsInstancing then
    TransformFeedbackProgram := TransformFeedbackProgramMultipleInstances
  else
    TransformFeedbackProgram := TransformFeedbackProgramSingleInstance;

  {$ifndef WASI}
  // Create UBO
  glBindBuffer(GL_UNIFORM_BUFFER, Self.UBO);
  glBufferData(GL_UNIFORM_BUFFER, SizeOf(TCastleParticleEffectUBO), @Self.FEffectUBO, GL_STATIC_DRAW);
  glBindBuffer(GL_UNIFORM_BUFFER, GLObjectNone);
  {$endif}

  // Drawing VAO
  glBindBuffer(GL_ARRAY_BUFFER, Self.VBOMesh);
  glBufferData(GL_ARRAY_BUFFER, Length(Self.ParticleMesh) * SizeOf(TCastleParticleMesh), @Self.ParticleMesh[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, GLObjectNone);
  if Length(Self.ParticleMeshIndices) > 0 then
  begin
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Self.VBOMeshIndices);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(Self.ParticleMeshIndices) * SizeOf(TGLushort), @Self.ParticleMeshIndices[0], GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, GLObjectNone);
  end;
  Self.CurrentBuffer := 0;
  for I := 0 to 1 do
  begin
    // Transform & feedback VAO
    glBindVertexArray(Self.VAOTnFs[I]);

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOTnFs[I]);
    glBufferData(GL_ARRAY_BUFFER, Self.FEffect.MaxParticles * SizeOf(TCastleParticle), @Self.Particles[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(16));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(32));
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(48));
    glEnableVertexAttribArray(4);
    glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(64));
    glEnableVertexAttribArray(5);
    glVertexAttribPointer(5, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(80));
    glEnableVertexAttribArray(6);
    glVertexAttribPointer(6, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(92));
    glEnableVertexAttribArray(7);
    glVertexAttribPointer(7, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(108));
    glEnableVertexAttribArray(8);
    glVertexAttribPointer(8, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(120));
    glEnableVertexAttribArray(9);
    glVertexAttribPointer(9, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(132));
    glEnableVertexAttribArray(10);
    glVertexAttribPointer(10, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(148));

    // Instancing VAO
    glBindVertexArray(Self.VAOMeshes[I]);

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOTnFs[I]);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(0));
    glVertexAttribDivisor(0, 1);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(16));
    glVertexAttribDivisor(1, 1);
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(32));
    glVertexAttribDivisor(2, 1);
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(48));
    glVertexAttribDivisor(3, 1);
    glEnableVertexAttribArray(9);
    glVertexAttribPointer(9, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(132));
    glVertexAttribDivisor(9, 1);
    glEnableVertexAttribArray(10);
    glVertexAttribPointer(10, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticle), {$ifndef WASI}Pointer{$endif}(148));
    glVertexAttribDivisor(10, 1);

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOMesh);

    glEnableVertexAttribArray(13);
    glVertexAttribPointer(13, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticleMesh), {$ifndef WASI}Pointer{$endif}(0));
    glEnableVertexAttribArray(14);
    glVertexAttribPointer(14, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticleMesh), {$ifndef WASI}Pointer{$endif}(12));
    glEnableVertexAttribArray(15);
    glVertexAttribPointer(15, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastleParticleMesh), {$ifndef WASI}Pointer{$endif}(20));

    if Length(Self.ParticleMeshIndices) > 0 then
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Self.VBOMeshIndices);

    glBindVertexArray(GLObjectNone);
  end;
  SetLength(Self.Particles, 0);

  Self.FIsNeedRefresh := False;
  Self.FIsEffectChanged := True;
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

constructor TCastleParticleAttractor.Create(AOwner: TComponent);
begin
  inherited;
  Self.FKillDistance := -1;
end;

function TCastleParticleAttractor.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'AttactorType')
    or (PropertyName = 'KillDistance')
    or (PropertyName = 'Attraction') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleParticleEmitter.Clone(const AOwner: TComponent): TCastleParticleEmitter;
begin
  Result := TCastleParticleEmitter.Create(AOwner);
  Result.DistanceCulling := Self.DistanceCulling;
  Result.AllowsWriteToDepthBuffer := Self.AllowsWriteToDepthBuffer;
  Result.AllowsUpdateWhenCulled := Self.AllowsUpdateWhenCulled;
  Result.AllowsInstancing := Self.AllowsInstancing;
  Result.EnableFog := Self.EnableFog;
  Result.SmoothTexture := Self.SmoothTexture;
  Result.Burst := Self.Burst;
  Result.TimePlaying := Self.TimePlaying;
  Result.TimePlayingSpeed := Self.TimePlayingSpeed;
  Result.DeltaTime := Self.DeltaTime;
  // TODO: Should we clone effect?
  Result.Effect := Self.Effect;
end;

initialization
  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleParticleEffect,
    'Texture', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleParticleEffect,
    'Mesh', TSceneURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleParticleEffect,
    'MeshAsSourcePosition', TSceneURLPropertyEditor);
  {$endif}
  RegisterSerializableComponent(TCastleParticleEmitter, ['Particle', 'Particle Emitter']);
  RegisterSerializableComponent(TCastleParticleAttractor, ['Particle', 'Particle Attractor']);
  RegisterSerializableComponent(TCastleParticleEffect, ['Particle', 'Particle Effect']);
  RegisterSerializableComponent(TCastleParticleViewport, ['Particle', 'Viewport (Particle)']);
  ShaderCache := TCastleParticleShaderCache.Create;

finalization
  ShaderCache.Free;

end.
