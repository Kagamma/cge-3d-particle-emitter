unit Castle3DParticleEmitterGPU;

{$coperators on}
{$macro on}
{$define nl:=+ LineEnding +}

{$ifdef ANDROID}{$define GLES}{$endif}
{$ifdef iOS}{$define GLES}{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef GLES}
  CastleGLES20, // This wont work. We need GLES3 header
  {$else}
  GL, GLExt,
  {$endif}
  fpjson, jsonparser,
  CastleTransform, CastleSceneCore, CastleComponentSerialize, CastleColors, CastleBoxes,
  CastleVectors, CastleRenderContext, Generics.Collections, CastleGLImages, CastleLog,
  CastleUtils, CastleApplicationProperties, CastleGLShaders, CastleClassUtils,
  X3DNodes;

type
  TCastle3DParticleBlendMode = (
    p3dbZero,
    p3dbOne,
    p3dbSrcColor,
    p3dbOneMinusSrcColor,
    p3dbSrcAlpha,
    p3dbOneMinusSrcAlpha,
    p3dbDstAlpha,
    p3dbOneMinusDstAlpha,
    p3dbDstColor,
    p3dbOneMinusDstColor
  );

const
  Castle3DParticleBlendValues: array [TCastle3DParticleBlendMode] of Integer = (
    0, 1, 768, 769, 770, 771, 772, 773, 774, 775
  );

type
  { 2D particle struct to hold current particle settings. }
  PCastle3DParticle = ^TCastle3DParticle;
  TCastle3DParticle = packed record
    Position: TVector4;
    TimeToLive: TVector2;
    Size,
    SizeDelta,
    Rotation,
    RotationDelta: Single;
    Color,
    ColorDelta: TVector4;
    StartPos: TVector3;
    Velocity: TVector4;
    Direction: TVector3;
  end;

  TCastle3DParticleEffect = class(TCastleComponent)
  private
    FTexture: String;
    FBlendFuncSource,
    FBlendFuncDestination: TCastle3DParticleBlendMode;
    FMaxParticles: Integer;
    FParticleLifeSpan,
    FParticleLifeSpanVariance,
    FStartParticleSize,
    FStartParticleSizeVariance,
    FFinishParticleSize,
    FFinishParticleSizeVariance,
    FMiddleAnchor,
    FSpeed,
    FSpeedVariance,
    FRotationStart,
    FRotationStartVariance,
    FRotationEnd,
    FRotationEndVariance,
    FDuration: Single;
    FDirectionVariance: Single;
    FSourcePosition,
    FSourcePositionVariance,
    FDirection,
    FGravity: TVector3;
    FStartColor,
    FStartColorVariance,
    FMiddleColor,
    FMiddleColorVariance,
    FFinishColor,
    FFinishColorVariance: TVector4;
    FBoundingBoxMinPersistent,
    FBoundingBoxMaxPersistent,
    FSourcePositionPersistent,
    FSourcePositionVariancePersistent,
    FDirectionPersistent,
    FGravityPersistent: TCastleVector3Persistent;
    FStartColorPersistent,
    FStartColorVariancePersistent,
    FMiddleColorPersistent,
    FMiddleColorVariancePersistent,
    FFinishColorPersistent,
    FFinishColorVariancePersistent: TCastleColorPersistent;
    FRadial,
    FRadialVariance: Single;
    FBBox: TBox3D;
    procedure SetBoundingBoxMinForPersistent(const AValue: TVector3);
    function GetBoundingBoxMinForPersistent: TVector3;
    procedure SetBoundingBoxMaxForPersistent(const AValue: TVector3);
    function GetBoundingBoxMaxForPersistent: TVector3;
    procedure SetSourcePositionForPersistent(const AValue: TVector3);
    function GetSourcePositionForPersistent: TVector3;
    procedure SetSourcePositionVarianceForPersistent(const AValue: TVector3);
    function GetSourcePositionVarianceForPersistent: TVector3;
    procedure SetDirectionForPersistent(const AValue: TVector3);
    function GetDirectionForPersistent: TVector3;
    procedure SetGravityForPersistent(const AValue: TVector3);
    function GetGravityForPersistent: TVector3;
    procedure SetStartColorForPersistent(const AValue: TVector4);
    function GetStartColorForPersistent: TVector4;
    procedure SetStartColorVarianceForPersistent(const AValue: TVector4);
    function GetStartColorVarianceForPersistent: TVector4;
    procedure SetMiddleColorForPersistent(const AValue: TVector4);
    function GetMiddleColorForPersistent: TVector4;
    procedure SetMiddleColorVarianceForPersistent(const AValue: TVector4);
    function GetMiddleColorVarianceForPersistent: TVector4;
    procedure SetFinishColorForPersistent(const AValue: TVector4);
    function GetFinishColorForPersistent: TVector4;
    procedure SetFinishColorVarianceForPersistent(const AValue: TVector4);
    function GetFinishColorVarianceForPersistent: TVector4;
    procedure SetTexture(const AValue: String);
    procedure SetMaxParticle(const AValue: Integer);
  public
    IsColliable: Boolean;
    IsNeedRefresh: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const AURL: String);
    procedure Save(const AURL: String);

    property SourcePosition: TVector3 read FSourcePosition write FSourcePosition;
    property SourcePositionVariance: TVector3 read FSourcePositionVariance write FSourcePositionVariance;
    property Direction: TVector3 read FDirection write FDirection;
    property Gravity: TVector3 read FGravity write FGravity;
    property StartColor: TVector4 read FStartColor write FStartColor;
    property StartColorVariance: TVector4 read FStartColorVariance write FStartColorVariance;
    property MiddleColor: TVector4 read FMiddleColor write FMiddleColor;
    property MiddleColorVariance: TVector4 read FMiddleColorVariance write FMiddleColorVariance;
    property FinishColor: TVector4 read FFinishColor write FFinishColor;
    property FinishColorVariance: TVector4 read FFinishColorVariance write FFinishColorVariance;
    property BBox: TBox3D read FBBox write FBBox;
  published
    property Texture: String read FTexture write SetTexture;
    property BlendFuncSource: TCastle3DParticleBlendMode read FBlendFuncSource write FBlendFuncSource default p3dbOne;
    property BlendFuncDestination: TCastle3DParticleBlendMode read FBlendFuncDestination write FBlendFuncDestination default p3dbOne;
    property MaxParticles: Integer read FMaxParticles write SetMaxParticle default 100;
    property ParticleLifeSpan: Single read FParticleLifeSpan write FParticleLifeSpan default 1;
    property ParticleLifeSpanVariance: Single read FParticleLifeSpanVariance write FParticleLifeSpanVariance default 0.5;
    property StartParticleSize: Single read FStartParticleSize write FStartParticleSize default 1;
    property StartParticleSizeVariance: Single read FStartParticleSizeVariance write FStartParticleSizeVariance;
    property FinishParticleSize: Single read FFinishParticleSize write FFinishParticleSize default 0.1;
    property FinishParticleSizeVariance: Single read FFinishParticleSizeVariance write FFinishParticleSizeVariance;
    property MiddleAnchor: Single read FMiddleAnchor write FMiddleAnchor default 0.4;
    property Speed: Single read FSpeed write FSpeed default 3;
    property SpeedVariance: Single read FSpeedVariance write FSpeedVariance default 1;
    property RotationStart: Single read FRotationStart write FRotationStart;
    property RotationStartVariance: Single read FRotationStartVariance write FRotationStartVariance;
    property RotationEnd: Single read FRotationEnd write FRotationEnd;
    property RotationEndVariance: Single read FRotationEndVariance write FRotationEndVariance;
    property Duration: Single read FDuration write FDuration default -1;
    property DirectionVariance: Single read FDirectionVariance write FDirectionVariance default 0.4;
    property Radial: Single read FRadial write FRadial;
    property RadialVariance: Single read FRadialVariance write FRadialVariance;
    property BoundingBoxMinPersistent: TCastleVector3Persistent read FBoundingBoxMinPersistent;
    property BoundingBoxMaxPersistent: TCastleVector3Persistent read FBoundingBoxMaxPersistent;
    property SourcePositionPersistent: TCastleVector3Persistent read FSourcePositionPersistent;
    property SourcePositionVariancePersistent: TCastleVector3Persistent read FSourcePositionVariancePersistent;
    property DirectionPersistent: TCastleVector3Persistent read FDirectionPersistent;
    property GravityPersistent: TCastleVector3Persistent read FGravityPersistent;
    property StartColorPersistent: TCastleColorPersistent read FStartColorPersistent;
    property StartColorVariancePersistent: TCastleColorPersistent read FStartColorVariancePersistent;
    property MiddleColorPersistent: TCastleColorPersistent read FMiddleColorPersistent;
    property MiddleColorVariancePersistent: TCastleColorPersistent read FMiddleColorVariancePersistent;
    property FinishColorPersistent: TCastleColorPersistent read FFinishColorPersistent;
    property FinishColorVariancePersistent: TCastleColorPersistent read FFinishColorVariancePersistent;
  end;

  TCastle3DParticleEmitterGPU = class(TCastleSceneCore)
  strict private
    Texture: GLuint;

    VAOs,
    VBOs: array[0..1] of GLuint;
    CurrentBuffer: GLuint;
    Particles: packed array of TCastle3DParticle;

    FURL: String;
    FStartEmitting: Boolean;
    FEffect: TCastle3DParticleEffect;
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
    FPosition: TVector3;
    { Bypass GLContext problem }
    FIsGLContextInitialized: Boolean;
    FIsNeedRefresh: Boolean;
    FDistanceCulling: Single;
    procedure InternalRefreshEffect;
    procedure SetStartEmitting(V: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure LocalRender(const Params: TRenderParams); override;
    { Save effect to a file }
    procedure SaveEffect(const AURL: String); overload;
    { Init a new FEffect and load settings from .json file. }
    procedure LoadEffect(const AURL: String); overload;
    procedure LoadEffect(const AEffect: TCastle3DParticleEffect);
    procedure GLContextOpen; virtual;
    procedure GLContextClose; override;
    procedure RefreshEffect;
    function LocalBoundingBox: TBox3D; override;
    { Move the position of emitter only }
    property Position: TVector3 read FPosition write FPosition;
  published
    { URL of a .p3d file. This will call LoadEffect to load particle effect }
    property URL: String read FURL write LoadEffect;
    property Effect: TCastle3DParticleEffect read FEffect write LoadEffect;
    { If true, the emitter will start emitting }
    property StartEmitting: Boolean read FStartEmitting write SetStartEmitting default False;
    property DistanceCulling: Single read FDistanceCulling write FDistanceCulling default 0;
  end;

function Castle3DParticleBlendValueToBlendMode(const AValue: Integer): TCastle3DParticleBlendMode;

implementation

uses
  CastleDownload, CastleURIUtils, Math;

const
  TransformVertexShaderSource: String =
'#version 330'nl
'layout(location = 0) in vec4 inPosition;'nl
'layout(location = 1) in vec2 inTimeToLive;'nl
'layout(location = 2) in vec4 inSizeRotation;'nl
'layout(location = 3) in vec4 inColor;'nl
'layout(location = 4) in vec4 inColorDelta;'nl
'layout(location = 5) in vec3 inStartPos;'nl
'layout(location = 6) in vec4 inVelocity;'nl
'layout(location = 7) in vec3 inDirection;'nl

'out vec4 outPosition;'nl
'out vec2 outTimeToLive;'nl
'out vec4 outSizeRotation;'nl
'out vec4 outColor;'nl
'out vec4 outColorDelta;'nl
'out vec3 outStartPos;'nl
'out vec4 outVelocity;'nl
'out vec3 outDirection;'nl

'struct Effect {'nl
'  float particleLifeSpan;'nl
'  float particleLifeSpanVariance;'nl
'  float startParticleSize;'nl
'  float startParticleSizeVariance;'nl
'  float finishParticleSize;'nl
'  float finishParticleSizeVariance;'nl
'  float maxRadius;'nl
'  float maxRadiusVariance;'nl
'  float minRadius;'nl
'  float minRadiusVariance;'nl
'  float rotatePerSecond;'nl
'  float rotatePerSecondVariance;'nl
'  float rotationStart;'nl
'  float rotationStartVariance;'nl
'  float rotationEnd;'nl
'  float rotationEndVariance;'nl
'  float speed;'nl
'  float speedVariance;'nl
'  float radial;'nl
'  float radialVariance;'nl
'  float middleAnchor;'nl
'  vec3 sourcePosition;'nl
'  vec3 sourcePositionVariance;'nl
'  vec3 gravity;'nl
'  vec3 direction;'nl
'  float directionVariance;'nl
'  vec4 startColor;'nl
'  vec4 startColorVariance;'nl
'  vec4 middleColor;'nl
'  vec4 middleColorVariance;'nl
'  vec4 finishColor;'nl
'  vec4 finishColorVariance;'nl
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
'}'nl

'void emitParticle() {'nl
'  outTimeToLive.x = effect.particleLifeSpan + effect.particleLifeSpanVariance * (rnd() * 2.0 - 1.0);'nl
'  outTimeToLive.y = outTimeToLive.x - outTimeToLive.x * effect.middleAnchor;'nl
'  float invLifeSpan = 1.0 / outTimeToLive.x;'nl
'  outPosition.xyz = effect.sourcePosition + effect.sourcePositionVariance * vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outStartPos = effect.sourcePosition;'nl
'  outColor = effect.startColor + effect.startColorVariance * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  vec4 middleColor = effect.middleColor + effect.middleColorVariance * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outColorDelta = (middleColor - outColor) * (1.0 / (outTimeToLive.x - outTimeToLive.y));'nl
'  vec3 cd = normalize(cross(effect.direction, effect.direction.zxy));'nl
'  float angle = effect.directionVariance * (rnd() * 2.0 - 1.0);'nl
'  vec3 vrdir = rotate(effect.direction, angle, cd);'nl
'  vrdir = rotate(vrdir, rnd() * 2.0 * 3.14159265359, effect.direction);'nl
'  vec3 vspeed = vec3('nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0),'nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0),'nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0));'nl
'  outVelocity = vec4(vrdir * vspeed, effect.radial + effect.radialVariance * (rnd() * 2.0 - 1.0));'nl

'  float startSize = max(0.0001, effect.startParticleSize + effect.startParticleSizeVariance * (rnd() * 2.0 - 1.0));'nl
'  float finishSize = max(0.0001, effect.finishParticleSize + effect.finishParticleSizeVariance * (rnd() * 2.0 - 1.0));'nl
'  outSizeRotation.xy = vec2(startSize, (finishSize - startSize) * invLifeSpan);'nl

'  outSizeRotation.z = effect.rotationStart + effect.rotationStartVariance * (rnd() * 2.0 - 1.0);'nl
'  float endRotation = effect.rotationEnd + effect.rotationEndVariance * (rnd() * 2.0 - 1.0);'nl
'  outSizeRotation.w = (endRotation - outSizeRotation.z) * invLifeSpan;'nl
'  outDirection = effect.direction;'nl
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
'    vec4 finishColor = effect.finishColor + effect.finishColorVariance * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'    outColorDelta = (finishColor - outColor) * (1.0 / outTimeToLive.y);'nl
'  }'nl
'  outTimeToLive.x = max(0.0, outTimeToLive.x - deltaTime);'nl
'  outVelocity.xyz = rotate(outVelocity.xyz, outVelocity.w * deltaTime, outDirection) + effect.gravity * deltaTime;'nl
'  outPosition.xyz = rotate(outPosition.xyz, outVelocity.w * deltaTime, outDirection) + outVelocity.xyz * deltaTime;'nl
'  outSizeRotation.x += outSizeRotation.y * deltaTime;'nl
'  outSizeRotation.z += outSizeRotation.w * deltaTime;'nl
'}'nl

'void main() {'nl
'  initParticle();'nl
'  updateParticle();'nl
'}';

  VertexShaderSource: String =
'#version 330'nl
'layout(location = 0) in vec4 inPosition;'nl
'layout(location = 1) in vec2 inTimeToLive;'nl
'layout(location = 2) in vec4 inSizeRotation;'nl
'layout(location = 3) in vec4 inColor;'nl

'out float geomTimeToLive;'nl
'out vec2 geomSize;'nl
'out vec2 geomRotation;'nl
'out vec4 geomColor;'nl

'uniform mat4 mvMatrix;'nl

'void main() {'nl
'  gl_Position = mvMatrix * vec4(inPosition.xyz, 1.0);'nl
'  geomTimeToLive = inTimeToLive.x;'nl
'  geomSize = inSizeRotation.xy;'nl
'  geomRotation = inSizeRotation.zw;'nl
'  geomColor = inColor;'nl
'}';

  GeometryShaderSource: String =
'#version 330'nl
'layout(points) in;'nl
'layout(triangle_strip, max_vertices = 4) out;'nl

'in float geomTimeToLive[];'nl
'in vec2 geomSize[];'nl
'in vec2 geomRotation[];'nl
'in vec4 geomColor[];'nl

'out vec2 fragTexCoord;'nl
'out vec4 fragColor;'nl

'uniform mat4 pMatrix;'nl
'uniform float scaleFactor;'nl

'void main() {'nl
'  if (geomTimeToLive[0] > 0.0) {'nl
'    fragColor = geomColor[0];'nl

'    float s = sin(geomRotation[0].x);'nl
'    float c = cos(geomRotation[0].x);'nl
'    float sadd = (c + s) * geomSize[0].x * scaleFactor * 0.5;'nl
'    float ssub = (c - s) * geomSize[0].x * scaleFactor * 0.5;'nl
'    vec4 p = gl_in[0].gl_Position;'nl

'    gl_Position = pMatrix * vec4(p.x - ssub, p.y - sadd, p.zw);'nl
'    fragTexCoord = vec2(0.0, 1.0);'nl
'    EmitVertex();'nl
'    gl_Position = pMatrix * vec4(p.x - sadd, p.y + ssub, p.zw);'nl
'    fragTexCoord = vec2(0.0, 0.0);'nl
'    EmitVertex();'nl
'    gl_Position = pMatrix * vec4(p.x + sadd, p.y - ssub, p.zw);'nl
'    fragTexCoord = vec2(1.0, 1.0);'nl
'    EmitVertex();'nl
'    gl_Position = pMatrix * vec4(p.x + ssub, p.y + sadd, p.zw);'nl
'    fragTexCoord = vec2(1.0, 0.0);'nl
'    EmitVertex();'nl

'    EndPrimitive();'nl
'  }'nl
'}';

  FragmentShaderSource: String =
'#version 330'nl
'precision lowp float;'nl
'in vec2 fragTexCoord;'nl
'in vec4 fragColor;'nl

'out vec4 outColor;'nl

'uniform sampler2D baseColor;'nl

'void main() {'nl
'  outColor = texture(baseColor, fragTexCoord) * fragColor;'nl
'  outColor.rgb *= outColor.a;'nl
'}';

  Varyings: array[0..7] of PChar = (
    'outPosition',
    'outTimeToLive',
    'outSizeRotation',
    'outColor',
    'outColorDelta',
    'outStartPos',
    'outVelocity',
    'outDirection'
  );

var
  IsCheckedForUsable: Boolean = False;
  TransformFeedbackProgram: TGLSLProgram = nil;
  RenderProgram: TGLSLProgram = nil;

procedure TCastle3DParticleEffect.SetBoundingBoxMinForPersistent(const AValue: TVector3);
begin
  Self.FBBox := Box3D(AValue, Self.FBBox.Data[1]);
end;

function TCastle3DParticleEffect.GetBoundingBoxMinForPersistent: TVector3;
begin
  Result := Self.FBBox.Data[0];
end;

procedure TCastle3DParticleEffect.SetBoundingBoxMaxForPersistent(const AValue: TVector3);
begin
  Self.FBBox := Box3D(Self.FBBox.Data[0], AValue);
end;

function TCastle3DParticleEffect.GetBoundingBoxMaxForPersistent: TVector3;
begin
  Result := Self.FBBox.Data[1];
end;

procedure TCastle3DParticleEffect.SetSourcePositionForPersistent(const AValue: TVector3);
begin
  Self.FSourcePosition := AValue;
end;

function TCastle3DParticleEffect.GetSourcePositionForPersistent: TVector3;
begin
  Result := Self.FSourcePosition;
end;

procedure TCastle3DParticleEffect.SetSourcePositionVarianceForPersistent(const AValue: TVector3);
begin
  Self.FSourcePositionVariance := AValue;
end;

function TCastle3DParticleEffect.GetSourcePositionVarianceForPersistent: TVector3;
begin
  Result := FSourcePositionVariance;
end;

procedure TCastle3DParticleEffect.SetDirectionForPersistent(const AValue: TVector3);
begin
  Self.FDirection := AValue;
end;

function TCastle3DParticleEffect.GetDirectionForPersistent: TVector3;
begin
  Result := Self.FDirection;
end;

procedure TCastle3DParticleEffect.SetGravityForPersistent(const AValue: TVector3);
begin
  Self.FGravity := AValue;
end;

function TCastle3DParticleEffect.GetGravityForPersistent: TVector3;
begin
  Result := Self.FGravity;
end;

procedure TCastle3DParticleEffect.SetStartColorForPersistent(const AValue: TVector4);
begin
  Self.FStartColor := AValue;
end;

function TCastle3DParticleEffect.GetStartColorForPersistent: TVector4;
begin
  Result := Self.FStartColor;
end;

procedure TCastle3DParticleEffect.SetStartColorVarianceForPersistent(const AValue: TVector4);
begin
  Self.FStartColorVariance := AValue;
end;

function TCastle3DParticleEffect.GetStartColorVarianceForPersistent: TVector4;
begin
  Result := Self.FStartColorVariance;
end;

procedure TCastle3DParticleEffect.SetMiddleColorForPersistent(const AValue: TVector4);
begin
  Self.FMiddleColor := AValue;
end;

function TCastle3DParticleEffect.GetMiddleColorForPersistent: TVector4;
begin
  Result := Self.FMiddleColor;
end;

procedure TCastle3DParticleEffect.SetMiddleColorVarianceForPersistent(const AValue: TVector4);
begin
  Self.FMiddleColorVariance := AValue;
end;

function TCastle3DParticleEffect.GetMiddleColorVarianceForPersistent: TVector4;
begin
  Result := Self.FMiddleColorVariance;
end;

procedure TCastle3DParticleEffect.SetFinishColorForPersistent(const AValue: TVector4);
begin
  Self.FFinishColor := AValue;
end;

function TCastle3DParticleEffect.GetFinishColorForPersistent: TVector4;
begin
  Result := Self.FFinishColor;
end;

procedure TCastle3DParticleEffect.SetFinishColorVarianceForPersistent(const AValue: TVector4);
begin
  Self.FFinishColorVariance := AValue;
end;

function TCastle3DParticleEffect.GetFinishColorVarianceForPersistent: TVector4;
begin
  Result := Self.FFinishColorVariance;
end;

procedure TCastle3DParticleEffect.SetTexture(const AValue: String);
begin
  Self.FTexture := AValue;
  Self.IsNeedRefresh := True;
end;

procedure TCastle3DParticleEffect.SetMaxParticle(const AValue: Integer);
begin
  Self.FMaxParticles := AValue;
  Self.IsNeedRefresh := True;
end;

constructor TCastle3DParticleEffect.Create(AOwner: TComponent);
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
begin
  inherited;
  Self.BBox := TBox3D.Empty;
  Self.FTexture := '';
  Self.FMaxParticles := 100;
  Self.FDuration := -1;
  Self.FParticleLifeSpan := 1;
  Self.FParticleLifeSpanVariance := 0.5;
  Self.FMiddleAnchor := 0.4;
  Self.FStartColor := Vector4(1, 0, 0, 1);
  Self.FMiddleColor := Vector4(1, 0.5, 0, 0.5);
  Self.FFinishColor := Vector4(0.3, 0.3, 0.3, 0.3);
  Self.FStartParticleSize := 1;
  Self.FFinishParticleSize := 0.1;
  Self.FSourcePositionVariance := Vector3(0.02, 0.02, 0.02);
  Self.FDirection := Vector3(0, 1, 0);
  Self.FDirectionVariance := 0.4;
  Self.FSpeed := 3;
  Self.FSpeedVariance := 1;
  Self.FBlendFuncSource := p3dbOne;
  Self.FBlendFuncDestination := p3dbOne;
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
  Self.FStartColorPersistent := CreateColorPersistent(
    @Self.GetStartColorForPersistent,
    @Self.SetStartColorForPersistent,
    Self.FStartColor
  );
  Self.FStartColorVariancePersistent := CreateColorPersistent(
    @Self.GetStartColorVarianceForPersistent,
    @Self.SetStartColorVarianceForPersistent,
    Self.FStartColorVariance
  );
  Self.FMiddleColorPersistent := CreateColorPersistent(
    @Self.GetMiddleColorForPersistent,
    @Self.SetMiddleColorForPersistent,
    Self.FMiddleColor
  );
  Self.FMiddleColorVariancePersistent := CreateColorPersistent(
    @Self.GetMiddleColorVarianceForPersistent,
    @Self.SetMiddleColorVarianceForPersistent,
    Self.FMiddleColorVariance
  );
  Self.FFinishColorPersistent := CreateColorPersistent(
    @Self.GetFinishColorForPersistent,
    @Self.SetFinishColorForPersistent,
    Self.FFinishColor
  );
  Self.FFinishColorVariancePersistent := CreateColorPersistent(
    @Self.GetFinishColorVarianceForPersistent,
    @Self.SetFinishColorVarianceForPersistent,
    Self.FFinishColorVariance
  );
end;

destructor TCastle3DParticleEffect.Destroy;
begin
  FreeAndNil(Self.FBoundingBoxMinPersistent);
  FreeAndNil(Self.FBoundingBoxMaxPersistent);
  FreeAndNil(Self.FSourcePositionPersistent);
  FreeAndNil(Self.FSourcePositionVariancePersistent);
  FreeAndNil(Self.FDirectionPersistent);
  FreeAndNil(Self.FGravityPersistent);
  FreeAndNil(Self.FStartColorPersistent);
  FreeAndNil(Self.FStartColorVariancePersistent);
  FreeAndNil(Self.FMiddleColorPersistent);
  FreeAndNil(Self.FMiddleColorVariancePersistent);
  FreeAndNil(Self.FFinishColorPersistent);
  FreeAndNil(Self.FFinishColorVariancePersistent);
  inherited;
end;

procedure TCastle3DParticleEffect.Load(const AURL: String);
var
  Json, JsonObject: TJSONObject;
  MS: TStream;
  SS: TStringStream;
begin
  MS := Download(AURL);
  SS := TStringStream.Create('');
  try
    MS.Position := 0;
    SS.CopyFrom(MS, MS.Size);
    Json := GetJSON(SS.DataString) as TJSONObject;
    try
      Self.Texture := ExtractURIPath(AURL) + Json.Strings['texture'];
      Self.MaxParticles := Json.Integers['maxParticles'];
      Self.Duration := Json.Floats['duration'];
      Self.ParticleLifeSpan := Json.Floats['particleLifeSpan'];
      Self.ParticleLifeSpanVariance := Json.Floats['particleLifeSpanVariance'];
      Self.StartParticleSize := Json.Floats['startParticleSize'];
      Self.StartParticleSizeVariance := Json.Floats['startParticleSizeVariance'];
      Self.FinishParticleSize := Json.Floats['finishParticleSize'];
      Self.FinishParticleSizeVariance := Json.Floats['finishParticleSizeVariance'];
      Self.BlendFuncSource := Castle3DParticleBlendValueToBlendMode(Json.Integers['blendFuncSource']);
      Self.BlendFuncDestination := Castle3DParticleBlendValueToBlendMode(Json.Integers['blendFuncDestination']);
      Self.MiddleAnchor := Json.Floats['middleAnchor'];
      Self.DirectionVariance := Json.Floats['directionVariance'];
      Self.RotationStart := Json.Integers['rotationStart'];
      Self.RotationStartVariance := Json.Integers['rotationStartVariance'];
      Self.RotationEnd := Json.Integers['rotationFinish'];
      Self.RotationEndVariance := Json.Integers['rotationFinishVariance'];
      Self.Speed := Json.Floats['speed'];
      Self.SpeedVariance := Json.Floats['speedVariance'];
      JsonObject := Json.Objects['startColor'];
      Self.StartColor := Vector4(JsonObject.Floats['red'], JsonObject.Floats['green'], JsonObject.Floats['blue'], JsonObject.Floats['alpha']);
      JsonObject := Json.Objects['startColorVariance'];
      Self.StartColorVariance := Vector4(JsonObject.Floats['red'], JsonObject.Floats['green'], JsonObject.Floats['blue'], JsonObject.Floats['alpha']);
      JsonObject := Json.Objects['middleColor'];
      Self.MiddleColor := Vector4(JsonObject.Floats['red'], JsonObject.Floats['green'], JsonObject.Floats['blue'], JsonObject.Floats['alpha']);
      JsonObject := Json.Objects['middleColorVariance'];
      Self.MiddleColorVariance := Vector4(JsonObject.Floats['red'], JsonObject.Floats['green'], JsonObject.Floats['blue'], JsonObject.Floats['alpha']);
      JsonObject := Json.Objects['finishColor'];
      Self.FinishColor := Vector4(JsonObject.Floats['red'], JsonObject.Floats['green'], JsonObject.Floats['blue'], JsonObject.Floats['alpha']);
      JsonObject := Json.Objects['finishColorVariance'];
      Self.FinishColorVariance := Vector4(JsonObject.Floats['red'], JsonObject.Floats['green'], JsonObject.Floats['blue'], JsonObject.Floats['alpha']);
      JsonObject := Json.Objects['sourcePositionVariance'];
      Self.SourcePositionVariance := Vector3(JsonObject.Floats['x'], JsonObject.Floats['y'], JsonObject.Floats['z']);
      JsonObject := Json.Objects['direction'];
      Self.Direction := Vector3(JsonObject.Floats['x'], JsonObject.Floats['y'], JsonObject.Floats['z']);
      JsonObject := Json.Objects['gravity'];
      Self.Gravity := Vector3(JsonObject.Floats['x'], JsonObject.Floats['y'], JsonObject.Floats['z']);
      JsonObject := Json.Objects['bbox'];
      Self.BBox := Box3D(
        Vector3(JsonObject.Floats['minx'], JsonObject.Floats['miny'], JsonObject.Floats['minz']),
        Vector3(JsonObject.Floats['maxx'], JsonObject.Floats['maxy'], JsonObject.Floats['maxz'])
      );
      // External
      try Self.Radial := Json.Integers['radial']; except Self.Radial := 0; end;
      try Self.RadialVariance := Json.Integers['radialVariance']; except Self.RadialVariance := 0; end;
    finally
      FreeAndNil(Json);
    end;
  finally
    FreeAndNil(MS);
    FreeAndNil(SS);
  end;
end;

procedure TCastle3DParticleEffect.Save(const AURL: String);
var
  Json: TJSONObject;
  FS: TFileStream;
  SS: TStringStream;
  function Vec3Add(V: TVector3; XN, YN, ZN: String): TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.Add(XN, V.X);
    Result.Add(YN, V.Y);
    Result.Add(ZN, V.Z);
  end;
  function Vec4Add(V: TVector4; XN, YN, ZN, WN: String): TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.Add(XN, V.X);
    Result.Add(YN, V.Y);
    Result.Add(ZN, V.Z);
    Result.Add(WN, V.W);
  end;
  function BBoxAdd: TJSONObject;
  begin
    Result := TJSONObject.Create;
    Result.Add('minx', Self.BBox.Data[0].X);
    Result.Add('miny', Self.BBox.Data[0].Y);
    Result.Add('minz', Self.BBox.Data[0].Z);
    Result.Add('maxx', Self.BBox.Data[1].X);
    Result.Add('maxy', Self.BBox.Data[1].Y);
    Result.Add('maxz', Self.BBox.Data[1].Z);
  end;
begin
  Json := TJSONObject.Create;
  FS := URLSaveStream(AURL) as TFileStream;
  try
    Json.Add('texture', ExtractFileName(Self.Texture));
    Json.Add('maxParticles', Self.MaxParticles);
    Json.Add('duration', Self.Duration);
    Json.Add('particleLifeSpan', Self.ParticleLifeSpan);
    Json.Add('particleLifeSpanVariance', Self.ParticleLifeSpanVariance);
    Json.Add('startParticleSize', Self.StartParticleSize);
    Json.Add('startParticleSizeVariance', Self.StartParticleSizeVariance);
    Json.Add('finishParticleSize', Self.FinishParticleSize);
    Json.Add('finishParticleSizeVariance', Self.FinishParticleSizeVariance);
    Json.Add('blendFuncSource', Castle3DParticleBlendValues[Self.BlendFuncSource]);
    Json.Add('blendFuncDestination', Castle3DParticleBlendValues[Self.BlendFuncDestination]);
    Json.Add('middleAnchor', Self.MiddleAnchor);
    Json.Add('directionVariance', Self.DirectionVariance);
    Json.Add('speed', Self.Speed);
    Json.Add('speedVariance', Self.SpeedVariance);
    Json.Add('rotationStart', Self.RotationStart);
    Json.Add('rotationStartVariance', Self.RotationStartVariance);
    Json.Add('rotationFinish', Self.RotationEnd);
    Json.Add('rotationFinishVariance', Self.RotationEndVariance);
    Json.Add('radial', Self.Radial);
    Json.Add('radialVariance', Self.RadialVariance);

    Json.Add('startColor', Vec4Add(Self.StartColor, 'red', 'green', 'blue', 'alpha'));
    Json.Add('startColorVariance', Vec4Add(Self.StartColorVariance, 'red', 'green', 'blue', 'alpha'));
    Json.Add('middleColor', Vec4Add(Self.MiddleColor, 'red', 'green', 'blue', 'alpha'));
    Json.Add('middleColorVariance', Vec4Add(Self.MiddleColorVariance, 'red', 'green', 'blue', 'alpha'));
    Json.Add('finishColor', Vec4Add(Self.FinishColor, 'red', 'green', 'blue', 'alpha'));
    Json.Add('finishColorVariance', Vec4Add(Self.FinishColorVariance, 'red', 'green', 'blue', 'alpha'));
    Json.Add('sourcePositionVariance', Vec3Add(Self.SourcePositionVariance, 'x', 'y', 'z'));
    Json.Add('direction', Vec3Add(Self.Direction, 'x', 'y', 'z'));
    Json.Add('gravity', Vec3Add(Self.Gravity, 'x', 'y', 'z'));
    Json.Add('bbox', BBoxAdd);
    SS := TStringStream.Create(Json.AsJSON);
    try
      SS.Position := 0;
      FS.CopyFrom(SS, SS.Size);
    finally
      FreeAndNil(SS);
    end;
  finally
    FreeAndNil(FS);
    FreeAndNil(Json);
  end;
end;

procedure TCastle3DParticleEmitterGPU.SetStartEmitting(V: Boolean);
begin
  Self.FStartEmitting := V;
  if V and Assigned(FEffect) then
    Self.FCountdownTillRemove := Self.FEffect.ParticleLifeSpan + Self.FEffect.ParticleLifeSpanVariance;
end;

constructor TCastle3DParticleEmitterGPU.Create(AOwner: TComponent);
begin
  inherited;
  Self.FIsUpdated := False;
  Self.Texture := 0;
  Self.FSecondsPassed := 0;
  Self.FPosition := Vector3(0, 0, 0);
  Self.Scale := Vector3(1, 1, 1);
  Self.FIsGLContextInitialized := False;
  Self.FIsNeedRefresh := False;
  Self.ShadowMaps := False;
end;

destructor TCastle3DParticleEmitterGPU.Destroy;
begin
  inherited;
end;

procedure TCastle3DParticleEmitterGPU.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  S: Single;
begin
  inherited;
  if not Assigned(Self.FEffect) then
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
    TransformFeedbackProgram.Uniform('effect.sourcePosition').SetValue(Self.FPosition);
    TransformFeedbackProgram.Uniform('effect.sourcePosition').SetValue(Self.FEffect.SourcePosition);
    TransformFeedbackProgram.Uniform('effect.sourcePositionVariance').SetValue(Self.FEffect.SourcePositionVariance);
    TransformFeedbackProgram.Uniform('effect.maxParticles').SetValue(Self.FEffect.MaxParticles);
    if Self.FEffect.MiddleAnchor = 0 then
      S := 0.01
    else
      S := Self.FEffect.MiddleAnchor;
    TransformFeedbackProgram.Uniform('effect.middleAnchor').SetValue(S);
    TransformFeedbackProgram.Uniform('effect.startColor').SetValue(Self.FEffect.StartColor);
    TransformFeedbackProgram.Uniform('effect.startColorVariance').SetValue(Self.FEffect.StartColorVariance);
    TransformFeedbackProgram.Uniform('effect.middleColor').SetValue(Self.FEffect.MiddleColor);
    TransformFeedbackProgram.Uniform('effect.middleColorVariance').SetValue(Self.FEffect.MiddleColorVariance);
    TransformFeedbackProgram.Uniform('effect.finishColor').SetValue(Self.FEffect.FinishColor);
    TransformFeedbackProgram.Uniform('effect.finishColorVariance').SetValue(Self.FEffect.FinishColorVariance);
    TransformFeedbackProgram.Uniform('effect.particleLifeSpan').SetValue(Self.FEffect.ParticleLifeSpan);
    TransformFeedbackProgram.Uniform('effect.particleLifeSpanVariance').SetValue(Self.FEffect.ParticleLifeSpanVariance);
    TransformFeedbackProgram.Uniform('effect.startParticleSize').SetValue(Self.FEffect.StartParticleSize);
    TransformFeedbackProgram.Uniform('effect.startParticleSizeVariance').SetValue(Self.FEffect.StartParticleSizeVariance);
    TransformFeedbackProgram.Uniform('effect.finishParticleSize').SetValue(Self.FEffect.FinishParticleSize);
    TransformFeedbackProgram.Uniform('effect.finishParticleSizeVariance').SetValue(Self.FEffect.FinishParticleSizeVariance);
    TransformFeedbackProgram.Uniform('effect.rotationStart').SetValue(Self.FEffect.RotationStart);
    TransformFeedbackProgram.Uniform('effect.rotationStartVariance').SetValue(Self.FEffect.RotationStartVariance);
    TransformFeedbackProgram.Uniform('effect.rotationEnd').SetValue(Self.FEffect.RotationEnd);
    TransformFeedbackProgram.Uniform('effect.rotationEndVariance').SetValue(Self.FEffect.RotationEndVariance);
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
end;

procedure TCastle3DParticleEmitterGPU.LocalRender(const Params: TRenderParams);
var
  M: TMatrix4;
  BoundingBoxMin, BoundingBoxMax,
  RenderCameraPosition: TVector3;
  RelativeBBox: TBox3D;
  SX, SY, SZ: Single;
begin
  inherited;
  Self.FIsUpdated := False;
  if not Assigned(Self.FEffect) then
    Exit;
  if not Self.FIsGLContextInitialized then
    Exit;
  if (not Self.Visible) or Params.InShadow or (not Params.Transparent) or (Params.StencilTest > 0) then
    Exit;
  if (not Self.FStartEmitting) and (Self.FCountdownTillRemove <= 0) then
    Exit;
  Inc(Params.Statistics.ScenesVisible);
  if DistanceCulling > 0 then
  begin
    RenderCameraPosition := Params.InverseTransform^.MultPoint(Params.RenderingCamera.Position);
    if (Position - RenderCameraPosition).Length > DistanceCulling + LocalBoundingBox.Radius then
      Exit;
  end;
  if not Self.FEffect.BBox.IsEmpty then
  begin
    RelativeBBox := Box3D(
      Self.FEffect.BBox.Data[0] + Self.Position,
      Self.FEffect.BBox.Data[1] + Self.Position
    );
    if not Params.Frustum^.Box3DCollisionPossibleSimple(RelativeBBox) then
      Exit;
  end;
  Inc(Params.Statistics.ShapesVisible);
  Inc(Params.Statistics.ShapesRendered);
  Inc(Params.Statistics.ScenesRendered);

  M := Params.RenderingCamera.Matrix * Params.Transform^;

  // Draw particles
  glDepthMask(GL_FALSE);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(Castle3DParticleBlendValues[Self.FEffect.BlendFuncSource], Castle3DParticleBlendValues[Self.FEffect.BlendFuncDestination]);
  RenderProgram.Enable;
  SX := Vector3(Params.Transform^[0,0], Params.Transform^[0,1], Params.Transform^[0,2]).Length;
  SY := Vector3(Params.Transform^[1,0], Params.Transform^[1,1], Params.Transform^[1,2]).Length;
  SZ := Vector3(Params.Transform^[2,0], Params.Transform^[2,1], Params.Transform^[2,2]).Length;
  RenderProgram.Uniform('scaleFactor').SetValue((SX + SY + SZ) / 3);
  RenderProgram.Uniform('mvMatrix').SetValue(M);
  RenderProgram.Uniform('pMatrix').SetValue(RenderContext.ProjectionMatrix);
  glBindVertexArray(Self.VAOs[CurrentBuffer]);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Self.Texture);
  glDrawArrays(GL_POINTS, 0, Self.FEffect.MaxParticles);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindVertexArray(0);
  // Render boundingbox in editor
  if CastleDesignMode and (not Self.FEffect.BBox.IsEmptyOrZero) then
  begin
    // OpenGL FFP is enough for visualizing bounding box
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    BoundingBoxMin := Self.FEffect.BBox.Data[0];
    BoundingBoxMax := Self.FEffect.BBox.Data[1];
    RenderProgram.Disable;
    glUseProgram(0);
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrixf(@M);
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(@RenderContext.ProjectionMatrix);
    glBegin(GL_LINES);
      glColor4f(0, 1, 0, 0.5);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMin.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMin.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMin.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMin.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMin.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMin.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMin.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMin.Y, BoundingBoxMin.Z);

      glVertex3f(BoundingBoxMin.X, BoundingBoxMax.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMax.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMax.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMax.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMax.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMax.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMax.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMax.Y, BoundingBoxMin.Z);

      glVertex3f(BoundingBoxMin.X, BoundingBoxMin.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMax.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMin.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMax.Y, BoundingBoxMin.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMin.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMax.X, BoundingBoxMax.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMin.Y, BoundingBoxMax.Z);
      glVertex3f(BoundingBoxMin.X, BoundingBoxMax.Y, BoundingBoxMax.Z);
    glEnd();
  end;
  glDisable(GL_BLEND);
  // Which pass is this?
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_TRUE);
end;

procedure TCastle3DParticleEmitterGPU.SaveEffect(const AURL: String);
begin
  Self.FEffect.Save(AURL);
end;

procedure TCastle3DParticleEmitterGPU.LoadEffect(const AURL: String);
begin
  Self.FEffect := TCastle3DParticleEffect.Create(Self);
  Self.FEffect.Load(AURL);
  FURL := AURL;
  RefreshEffect;
end;

procedure TCastle3DParticleEmitterGPU.LoadEffect(const AEffect: TCastle3DParticleEffect);
begin
  Self.FEffect := AEffect;
  if AEffect <> nil then
  begin
    AEffect.FreeNotification(Self);
  end;
  RefreshEffect;
end;

procedure TCastle3DParticleEmitterGPU.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Self.FEffect) then
  begin
    Self.LoadEffect(nil);
  end;
end;

procedure TCastle3DParticleEmitterGPU.GLContextOpen;
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
    if V < Length(Varyings) then
      raise Exception.Create(Format('TCastle3DParticleEmitterGPU requires GL_MAX_VERTEX_ATTRIBS at least %d', [Length(Varyings)]));
    IsCheckedForUsable := True;
  end;

  if TransformFeedbackProgram = nil then
  begin
    TransformFeedbackProgram := TGLSLProgram.Create;
    TransformFeedbackProgram.AttachVertexShader(TransformVertexShaderSource);
    TransformFeedbackProgram.SetTransformFeedbackVaryings(Varyings);
    TransformFeedbackProgram.Link;

    RenderProgram := TGLSLProgram.Create;
    RenderProgram.AttachVertexShader(VertexShaderSource);
    RenderProgram.AttachGeometryShader(GeometryShaderSource);
    RenderProgram.AttachFragmentShader(FragmentShaderSource);
    RenderProgram.Link;
  end;

  glGenBuffers(2, @Self.VBOs);
  glGenVertexArrays(2, @Self.VAOs);
  Self.FIsGLContextInitialized := True;
end;

procedure TCastle3DParticleEmitterGPU.GLContextClose;
begin
  if Self.FIsGLContextInitialized then
  begin
    glDeleteBuffers(2, @Self.VBOs);
    glDeleteVertexArrays(2, @Self.VAOs);
    glFreeTexture(Self.Texture);
    Self.FIsGLContextInitialized := False;
  end;
  inherited;
end;

procedure TCastle3DParticleEmitterGPU.InternalRefreshEffect;
var
  I: Integer;
begin
  if Self.FEffect = nil then
    Exit;
  // Only process if texture exists
  if not URIFileExists(Self.FEffect.Texture) then
    Exit;

  Self.FEmissionTime := Self.FEffect.Duration;
  Self.FParticleCount := Self.FEffect.MaxParticles;
  Self.FCountdownTillRemove := Self.FEffect.ParticleLifeSpan + Self.FEffect.ParticleLifeSpanVariance;
  SetLength(Self.Particles, Self.FEffect.MaxParticles);

  if Self.FEffect.ParticleLifeSpan = 0 then
    Self.FEffect.ParticleLifeSpan := 0.001;

  glFreeTexture(Self.Texture);
  Self.Texture := LoadGLTexture(
    Self.FEffect.Texture,
    TextureFilter(minLinear, magLinear),
    Texture2DClampToEdge
  );

  // Generate initial lifecycle
  for I := 0 to Self.FEffect.MaxParticles - 1 do
  begin
    with Self.Particles[I] do
    begin
      TimeToLive.X := Random * (Self.FEffect.ParticleLifeSpan + Self.FEffect.ParticleLifeSpanVariance);
      // Position.W is being used as random seed
      Position := Vector4(Random, Random, Random, Random);
      Direction := Vector3(1, 0, 0);
    end;
  end;

  // Drawing VAO
  Self.CurrentBuffer := 0;
  for I := 0 to 1 do
  begin
    glBindVertexArray(Self.VAOs[I]);

    glBindBuffer(GL_ARRAY_BUFFER, Self.VBOs[I]);
    glBufferData(GL_ARRAY_BUFFER, Self.FEffect.MaxParticles * SizeOf(TCastle3DParticle), @Self.Particles[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(16));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(24));
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(40));
    glEnableVertexAttribArray(4);
    glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(56));
    glEnableVertexAttribArray(5);
    glVertexAttribPointer(5, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(72));
    glEnableVertexAttribArray(6);
    glVertexAttribPointer(6, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(84));
    glEnableVertexAttribArray(7);
    glVertexAttribPointer(7, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(100));

    glBindVertexArray(0);
  end;
  SetLength(Self.Particles, 0);
  Self.FIsNeedRefresh := False;
  Self.FEffect.IsNeedRefresh := False;
end;

procedure TCastle3DParticleEmitterGPU.RefreshEffect;
begin
  Self.FIsNeedRefresh := True;
end;

function TCastle3DParticleEmitterGPU.LocalBoundingBox: TBox3D;
begin
  if GetExists and (Self.FEffect <> nil) then
  begin
    if not Self.FEffect.BBox.IsEmpty then
      Result := Box3D(
        Self.FEffect.BBox.Data[0] + Self.Position,
        Self.FEffect.BBox.Data[1] + Self.Position
      )
    else
      Result := Self.FEffect.BBox;
  end else
    Result := TBox3D.Empty;
  Result.Include(inherited LocalBoundingBox);
end;

function Castle3DParticleBlendValueToBlendMode(const AValue: Integer): TCastle3DParticleBlendMode;
begin
  case AValue of
    0: Result := p3dbZero;
    1: Result := p3dbOne;
    768: Result := p3dbSrcColor;
    769: Result := p3dbOneMinusSrcColor;
    770: Result := p3dbSrcAlpha;
    771: Result := p3dbOneMinusSrcAlpha;
    772: Result := p3dbDstAlpha;
    773: Result := p3dbOneMinusDstAlpha;
    774: Result := p3dbDstColor;
    775: Result := p3dbOneMinusDstColor;
  end;
end;

initialization
  RegisterSerializableComponent(TCastle3DParticleEmitterGPU, '3D Particle Emitter (GPU)');
  RegisterSerializableComponent(TCastle3DParticleEffect, '3D Particle Effect');

end.
