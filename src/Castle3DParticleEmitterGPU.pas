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
  CastleTransform, CastleSceneCore, CastleComponentSerialize,
  CastleVectors, CastleRenderContext, Generics.Collections, CastleGLImages, CastleLog,
  CastleUtils, CastleApplicationProperties, CastleGLShaders,
  CastleBoxes,
  X3DNodes;

type
  { 2D particle struct to hold current particle settings. }
  PCastle3DParticle = ^TCastle3DParticle;
  TCastle3DParticle = packed record
    Position: TVector3;
    TimeToLive: TVector2;
    Size,
    SizeDelta,
    Rotation,
    RotationDelta: Single;
    Color,
    ColorDelta: TVector4;
    { Gravity parameters }
    StartPos,
    Velocity: TVector3;
    RadialAcceleration,
    TangentialAcceleration: Single;
    { Radial parameters }
    EmitRadius,
    EmitRadiusDelta,
    EmitRotation,
    EmitRotationDelta: Single;
  end;

  TCastle3DParticleEffect = class
  public
    Texture: String;
    MaxParticles,
    BlendFuncSource,
    BlendFuncDestination: Integer;
    ParticleLifeSpan,
    ParticleLifeSpanVariance,
    StartParticleSize,
    StartParticleSizeVariance,
    FinishParticleSize,
    FinishParticleSizeVariance,
    MiddleAnchor,
    Speed,
    SpeedVariance,
    RotationStart,
    RotationStartVariance,
    RotationEnd,
    RotationEndVariance,
    Duration: Single;
    SourcePosition,
    SourcePositionVariance,
    Direction: TVector3;
    DirectionVariance: Single;
    Gravity: TVector3;
    StartColor,
    StartColorVariance,
    MiddleColor,
    MiddleColorVariance,
    FinishColor,
    FinishColorVariance: TVector4;
    IsColliable: Boolean;
    BBox: TBox3D;
    constructor Create;
    procedure Load(const AURL: String);
    procedure Save(const AURL: String);
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
    FOwnEffect: Boolean;
    FPosition: TVector3;
    { Bypass GLContext problem }
    FIsGLContextInitialized: Boolean;
    FIsNeedRefresh: Boolean;
    procedure InternalRefreshEffect;
    procedure SetStartEmitting(V: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure LocalRender(const Params: TRenderParams); override;
    { Save effect to a file }
    procedure SaveEffect(const AURL: String); overload;
    { This method will free the current Effect if any, init a new FEffect and
      load settings from .json file. }
    procedure LoadEffect(const AURL: String); overload;
    { If AOwnEffect = true the effect will be freed once emitter is freed. }
    procedure LoadEffect(const AEffect: TCastle3DParticleEffect; const AOwnEffect: Boolean = True); overload;
    procedure GLContextOpen; virtual;
    procedure GLContextClose; virtual;
    procedure RefreshEffect;
    function LocalBoundingBox: TBox3D; override;
    { Move the position of emitter only }
    property Position: TVector3 read FPosition write FPosition;
  published
    { URL of a .p3d file. This will call LoadEffect to load particle effect }
    property URL: String read FURL write LoadEffect;
    { If true, the emitter will start emitting }
    property StartEmitting: Boolean read FStartEmitting write SetStartEmitting default False;
  end;

implementation

uses
  CastleDownload, CastleURIUtils, Math;

const
  TransformVertexShaderSource: String =
'#version 330'nl
'layout(location = 0) in vec3 inPosition;'nl
'layout(location = 1) in vec2 inTimeToLive;'nl
'layout(location = 2) in vec2 inSize;'nl
'layout(location = 3) in vec2 inRotation;'nl
'layout(location = 4) in vec4 inColor;'nl
'layout(location = 5) in vec4 inColorDelta;'nl
'layout(location = 6) in vec3 inStartPos;'nl
'layout(location = 7) in vec3 inVelocity;'nl
'layout(location = 8) in vec2 inAcceleration;'nl
'layout(location = 9) in vec2 inEmitRadius;'nl
'layout(location = 10) in vec2 inEmitRotation;'nl

'out vec3 outPosition;'nl
'out vec2 outTimeToLive;'nl
'out vec2 outSize;'nl
'out vec2 outRotation;'nl
'out vec4 outColor;'nl
'out vec4 outColorDelta;'nl
'out vec3 outStartPos;'nl
'out vec3 outVelocity;'nl
'out vec2 outAcceleration;'nl
'out vec2 outEmitRadius;'nl
'out vec2 outEmitRotation;'nl

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
'  outAcceleration.x = fract(sin(outAcceleration.x + outPosition.x + outVelocity.x + outVelocity.y + outColor.x + deltaTime) * 43758.5453123);'nl
'  return outAcceleration.x;'nl
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
'  outSize = inSize;'nl
'  outRotation = inRotation;'nl
'  outColor = inColor;'nl
'  outColorDelta = inColorDelta;'nl
'  outStartPos = inStartPos;'nl
'  outVelocity = inVelocity;'nl
'  outAcceleration = inAcceleration;'nl
'  outEmitRadius = inEmitRadius;'nl
'  outEmitRotation = inEmitRotation;'nl
'}'nl

'void emitParticle() {'nl
'  outTimeToLive.x = effect.particleLifeSpan + effect.particleLifeSpanVariance * (rnd() * 2.0 - 1.0);'nl
'  outTimeToLive.y = outTimeToLive.x - outTimeToLive.x * effect.middleAnchor;'nl
'  float invLifeSpan = 1.0 / outTimeToLive.x;'nl
'  outPosition = effect.sourcePosition + effect.sourcePositionVariance * vec3(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outStartPos = effect.sourcePosition;'nl
'  outColor = effect.startColor + effect.startColorVariance * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  vec4 middleColor = effect.middleColor + effect.middleColorVariance * vec4(rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0, rnd() * 2.0 - 1.0);'nl
'  outColorDelta = (middleColor - outColor) * (1.0 / (outTimeToLive.x - outTimeToLive.y));'nl
'  vec3 cd = normalize(cross(effect.direction, effect.direction.zxy));'nl
'  float angle = effect.directionVariance * (rnd() * 2.0 - 1.0);'nl
'  vec3 vrdir = rotate(effect.direction, angle, cd);'nl
'  vrdir = rotate(vrdir, rnd() * 2.0 * 3.1415, effect.direction);'nl
'  vec3 vspeed = vec3('nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0),'nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0),'nl
'    effect.speed + effect.speedVariance * (rnd() * 2.0 - 1.0));'nl
'  outVelocity = vrdir * vspeed;'nl

'  float startSize = max(0.1, effect.startParticleSize + effect.startParticleSizeVariance * (rnd() * 2.0 - 1.0));'nl
'  float finishSize = max(0.1, effect.finishParticleSize + effect.finishParticleSizeVariance * (rnd() * 2.0 - 1.0));'nl
'  outSize = vec2(startSize, (finishSize - startSize) * invLifeSpan);'nl

'  outRotation.x = effect.rotationStart + effect.rotationStartVariance * (rnd() * 2.0 - 1.0);'nl
'  float endRotation = effect.rotationEnd + effect.rotationEndVariance * (rnd() * 2.0 - 1.0);'nl
'  outRotation.y = (endRotation - outRotation.x) * invLifeSpan;'nl
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
'  outVelocity += effect.gravity * deltaTime;'nl
'  outPosition += outVelocity * deltaTime;'nl
'  outSize.x += outSize.y * deltaTime;'nl
'  outRotation.x += outRotation.y * deltaTime;'nl
'}'nl

'void main() {'nl
'  initParticle();'nl
'  updateParticle();'nl
'}';

  VertexShaderSource: String =
'#version 330'nl
'layout(location = 0) in vec3 inPosition;'nl
'layout(location = 1) in vec2 inTimeToLive;'nl
'layout(location = 2) in vec2 inSize;'nl
'layout(location = 3) in vec2 inRotation;'nl
'layout(location = 4) in vec4 inColor;'nl

'out float geomTimeToLive;'nl
'out vec2 geomSize;'nl
'out vec2 geomRotation;'nl
'out vec4 geomColor;'nl

'uniform mat4 mvMatrix;'nl

'void main() {'nl
'  gl_Position = mvMatrix * vec4(inPosition, 1.0);'nl
'  geomTimeToLive = inTimeToLive.x;'nl
'  geomSize = inSize;'nl
'  geomRotation = inRotation;'nl
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

'void main() {'nl
'  if (geomTimeToLive[0] > 0.0) {'nl
'    fragColor = geomColor[0];'nl

'    float s = sin(geomRotation[0].x);'nl
'    float c = cos(geomRotation[0].x);'nl
'    float sadd = (c + s) * geomSize[0].x * 0.5;'nl
'    float ssub = (c - s) * geomSize[0].x * 0.5;'nl
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

  Varyings: array[0..10] of PChar = (
    'outPosition',
    'outTimeToLive',
    'outSize',
    'outRotation',
    'outColor',
    'outColorDelta',
    'outStartPos',
    'outVelocity',
    'outAcceleration',
    'outEmitRadius',
    'outEmitRotation'
  );

var
  IsCheckedForUsable: Boolean = False;
  TransformFeedbackProgram: TGLSLProgram = nil;
  RenderProgram: TGLSLProgram = nil;

constructor TCastle3DParticleEffect.Create;
begin
  inherited;
  Self.BBox := TBox3D.Empty;
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
      Self.BlendFuncSource := Json.Integers['blendFuncSource'];
      Self.BlendFuncDestination := Json.Integers['blendFuncDestination'];
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
  Json, JsonObject: TJSONObject;
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
  FS := TFileStream.Create(AURL, fmCreate);
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
    Json.Add('blendFuncSource', Self.BlendFuncSource);
    Json.Add('blendFuncDestination', Self.BlendFuncDestination);
    Json.Add('middleAnchor', Self.MiddleAnchor);
    Json.Add('directionVariance', Self.DirectionVariance);
    Json.Add('speed', Self.Speed);
    Json.Add('speedVariance', Self.SpeedVariance);
    Json.Add('rotationStart', Self.RotationStart);
    Json.Add('rotationStartVariance', Self.RotationStartVariance);
    Json.Add('rotationFinish', Self.RotationEnd);
    Json.Add('rotationFinishVariance', Self.RotationEndVariance);

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
  Self.FOwnEffect := False;
  Self.Scale := Vector3(1, 1, 1);
  Self.FIsGLContextInitialized := False;
  Self.FIsNeedRefresh := False;
  Self.ShadowMaps := False;
end;

destructor TCastle3DParticleEmitterGPU.Destroy;
begin
  if Self.FOwnEffect and Assigned(FEffect) then
    FEffect.Free;
  Self.GLContextClose;
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
  if Self.FIsNeedRefresh then
    Self.InternalRefreshEffect;
  //if not Self.ProcessEvents then
  //  Exit;

  Self.FSecondsPassed := SecondsPassed;

  if not Self.FIsUpdated then
  begin
    if (FEmissionTime > 0) or (FEmissionTime = -1) then
    begin
      if FEmissionTime > 0 then
        FEmissionTime := Max(0, FEmissionTime - SecondsPassed);
    end;

    if not Self.FStartEmitting then
    begin
      Self.FCountdownTillRemove := Self.FCountdownTillRemove - SecondsPassed;
    end;

    glEnable(GL_RASTERIZER_DISCARD);
    TransformFeedbackProgram.Enable;
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
        Self.FCountdownTillRemove := Self.FCountdownTillRemove - SecondsPassed;
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
  Inc(Params.Statistics.ShapesVisible);
  Inc(Params.Statistics.ScenesVisible);
  if not Self.FEffect.BBox.IsEmpty then
    if not Params.Frustum^.Box3DCollisionPossibleSimple(Self.FEffect.BBox) then
      Exit;
  Inc(Params.Statistics.ShapesRendered);
  Inc(Params.Statistics.ScenesRendered);

  M := Params.RenderingCamera.Matrix * Params.Transform^;

  // Draw particles
  glDepthMask(GL_FALSE);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(Self.FEffect.BlendFuncSource, Self.FEffect.BlendFuncDestination);
  RenderProgram.Enable;
  RenderProgram.Uniform('mvMatrix').SetValue(M);
  RenderProgram.Uniform('pMatrix').SetValue(RenderContext.ProjectionMatrix);
  glBindVertexArray(Self.VAOs[CurrentBuffer]);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, Self.Texture);
  glDrawArrays(GL_POINTS, 0, Self.FEffect.MaxParticles);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindVertexArray(0);
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
  if Self.FOwnEffect and Assigned(FEffect) then
    FreeAndNil(FEffect);
  FEffect := TCastle3DParticleEffect.Create;
  FEffect.Load(AURL);
  FURL := AURL;
  Self.FOwnEffect := True;
  RefreshEffect;
end;

procedure TCastle3DParticleEmitterGPU.LoadEffect(const AEffect: TCastle3DParticleEffect;
    const AOwnEffect: Boolean = True);
begin
  if Self.FOwnEffect and Assigned(FEffect) then
    FreeAndNil(FEffect);
  FEffect := AEffect;
  Self.FOwnEffect := AOwnEffect;
  RefreshEffect;
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
    if V < 12 then
      raise Exception.Create('TCastle3DParticleEmitterGPU requires GL_MAX_VERTEX_ATTRIBS at least 12');
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

  // Map uniform
  glGenBuffers(2, @Self.VBOs);
  glGenVertexArrays(2, @Self.VAOs);
  Self.FIsGLContextInitialized := True;
end;

procedure TCastle3DParticleEmitterGPU.GLContextClose;
begin
  if not Self.FIsGLContextInitialized then Exit;
  glDeleteBuffers(2, @Self.VBOs);
  glDeleteVertexArrays(2, @Self.VAOs);
  glFreeTexture(Self.Texture);
  Self.FIsGLContextInitialized := False;
end;

procedure TCastle3DParticleEmitterGPU.InternalRefreshEffect;
var
  I: Integer;
begin
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
      Position := Vector3(Random, Random, Random);
      // Take advantage of unused RadialAcceleration for initial seed
      RadialAcceleration := Random;
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
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(12));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(20));
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(28));
    glEnableVertexAttribArray(4);
    glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(36));
    glEnableVertexAttribArray(5);
    glVertexAttribPointer(5, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(52));
    glEnableVertexAttribArray(6);
    glVertexAttribPointer(6, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(68));
    glEnableVertexAttribArray(7);
    glVertexAttribPointer(7, 3, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(80));
    glEnableVertexAttribArray(8);
    glVertexAttribPointer(8, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(84));
    glEnableVertexAttribArray(9);
    glVertexAttribPointer(9, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(100));
    glEnableVertexAttribArray(10);
    glVertexAttribPointer(10, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastle3DParticle), Pointer(108));

    glBindVertexArray(0);
  end;
  SetLength(Self.Particles, 0);
  Self.FIsNeedRefresh := False;
end;

procedure TCastle3DParticleEmitterGPU.RefreshEffect;
begin
  Self.FIsNeedRefresh := True;
end;

function TCastle3DParticleEmitterGPU.LocalBoundingBox: TBox3D;
begin
  if GetExists then
    Result := Self.FEffect.BBox
  else
    Result := TBox3D.Empty;
  Result.Include(inherited LocalBoundingBox);
end;

initialization
  RegisterSerializableComponent(TCastle3DParticleEmitterGPU, '3D Particle Emitter (GPU)');

end.