unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleBoxes,
  CastleTransform, X3DNodes, CastleColors, Castle3DParticleEmitterGPU;

type
  // Borrowed from view3dscene
  TBoundingBoxScene = class(TCastleScene)
  strict private
    TransformNode: TTransformNode;
    Box: TBoxNode;
    Shape: TShapeNode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateBox(const ABox: TBox3D);
  end;

  TStateMain = class(TUIState)
  private
    Viewport: TCastleViewport;
    LabelFps: TCastleLabel;
    EditTexture: TCastleEdit;
    EditMiddleAnchor: TCastleFloatEdit;
    EditStartColorRed,
    EditStartColorGreen,
    EditStartColorBlue,
    EditStartColorAlpha,
    EditStartColorVarianceRed,
    EditStartColorVarianceGreen,
    EditStartColorVarianceBlue,
    EditStartColorVarianceAlpha,
    EditMiddleColorRed,
    EditMiddleColorGreen,
    EditMiddleColorBlue,
    EditMiddleColorAlpha,
    EditMiddleColorVarianceRed,
    EditMiddleColorVarianceGreen,
    EditMiddleColorVarianceBlue,
    EditMiddleColorVarianceAlpha,
    EditFinishColorRed,
    EditFinishColorGreen,
    EditFinishColorBlue,
    EditFinishColorAlpha,
    EditFinishColorVarianceRed,
    EditFinishColorVarianceGreen,
    EditFinishColorVarianceBlue,
    EditFinishColorVarianceAlpha,
    EditDirectionX,
    EditDirectionY,
    EditDirectionZ,
    EditDirectionVariance,
    EditSpeed,
    EditSpeedVariance,
    EditRadial,
    EditRadialVariance,
    EditPositionVarianceX,
    EditPositionVarianceY,
    EditPositionVarianceZ,
    EditGravityX,
    EditGravityY,
    EditGravityZ,
    EditDuration,
    EditLifeSpan,
    EditLifeSpanVariance,
    EditStartParticleSize,
    EditStartParticleSizeVariance,
    EditFinishParticleSize,
    EditFinishParticleSizeVariance,
    EditStartRotation,
    EditStartRotationVariance,
    EditFinishRotation,
    EditFinishRotationVariance,
    EditBBoxX1,
    EditBBoxY1,
    EditBBoxZ1,
    EditBBoxX2,
    EditBBoxY2,
    EditBBoxZ2: TCastleFloatEdit;
    EditMaxParticles,
    EditBlendSource,
    EditBlendDestination: TCastleIntegerEdit;
    ButtonSave,
    ButtonLoad,
    ButtonTextureOpen,
    ButtonApply: TCastleButton;
    Effect: TCastle3DParticleEffect;
    Emitter: TCastle3DParticleEmitterGPU;
    BoundingBoxScene: TBoundingBoxScene;
    procedure EffectToUI;
    procedure UIToEffect;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonTextureOpenClick(Sender: TObject);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure Render; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleWindow, X3DLoad, GameInitialize;

constructor TBoundingBoxScene.Create(AOwner: TComponent);
var
  Root: TX3DRootNode;
  Material: TUnlitMaterialNode;
begin
  inherited;

  Collides := false;
  Pickable := false;
  CastShadowVolumes := false;
  ExcludeFromStatistics := true;
  { Otherwise bbox from previous scene would affect AssignDefaultCamera
    and AssignDefaultNavigation calls done right after new scene is loaded. }
  InternalExcludeFromParentBoundingVolume := true;

  Box := TBoxNode.Create;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := GreenRGB;
  Material.Transparency := 0.5;

  Shape := TShapeNode.Create;
  Shape.Geometry := Box;
  Shape.Shading := shWireframe;
  Shape.Material := Material;
  Shape.Appearance.ShadowCaster := false;

  TransformNode := TTransformNode.Create;
  TransformNode.AddChildren(Shape);

  Root := TX3DRootNode.Create;
  Root.AddChildren(TransformNode);

  Load(Root, true);
end;

procedure TBoundingBoxScene.UpdateBox(const ABox: TBox3D);
begin
  Shape.Render := not ABox.IsEmpty;
  if Shape.Render then
  begin
    TransformNode.Translation := ABox.Center;
    Box.Size := ABox.Size;
  end;
end;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  Viewport := UiOwner.FindRequiredComponent('Viewport') as TCastleViewport;
  LabelFps := UiOwner.FindRequiredComponent('LabelFps') as TCastleLabel;

  EditTexture := UiOwner.FindRequiredComponent('EditTexture') as TCastleEdit;
  EditMiddleAnchor := UiOwner.FindRequiredComponent('EditMiddleAnchor') as TCastleFloatEdit;
  EditStartColorRed := UiOwner.FindRequiredComponent('EditStartColorRed') as TCastleFloatEdit;
  EditStartColorGreen := UiOwner.FindRequiredComponent('EditStartColorGreen') as TCastleFloatEdit;
  EditStartColorBlue := UiOwner.FindRequiredComponent('EditStartColorBlue') as TCastleFloatEdit;
  EditStartColorAlpha := UiOwner.FindRequiredComponent('EditStartColorAlpha') as TCastleFloatEdit;
  EditStartColorVarianceRed := UiOwner.FindRequiredComponent('EditStartColorVarianceRed') as TCastleFloatEdit;
  EditStartColorVarianceGreen := UiOwner.FindRequiredComponent('EditStartColorVarianceGreen') as TCastleFloatEdit;
  EditStartColorVarianceBlue := UiOwner.FindRequiredComponent('EditStartColorVarianceBlue') as TCastleFloatEdit;
  EditStartColorVarianceAlpha := UiOwner.FindRequiredComponent('EditStartColorVarianceAlpha') as TCastleFloatEdit;
  EditMiddleColorRed := UiOwner.FindRequiredComponent('EditMiddleColorRed') as TCastleFloatEdit;
  EditMiddleColorGreen := UiOwner.FindRequiredComponent('EditMiddleColorGreen') as TCastleFloatEdit;
  EditMiddleColorBlue := UiOwner.FindRequiredComponent('EditMiddleColorBlue') as TCastleFloatEdit;
  EditMiddleColorAlpha := UiOwner.FindRequiredComponent('EditMiddleColorAlpha') as TCastleFloatEdit;
  EditMiddleColorVarianceRed := UiOwner.FindRequiredComponent('EditMiddleColorVarianceRed') as TCastleFloatEdit;
  EditMiddleColorVarianceGreen := UiOwner.FindRequiredComponent('EditMiddleColorVarianceGreen') as TCastleFloatEdit;
  EditMiddleColorVarianceBlue := UiOwner.FindRequiredComponent('EditMiddleColorVarianceBlue') as TCastleFloatEdit;
  EditMiddleColorVarianceAlpha := UiOwner.FindRequiredComponent('EditMiddleColorVarianceAlpha') as TCastleFloatEdit;
  EditFinishColorRed := UiOwner.FindRequiredComponent('EditFinishColorRed') as TCastleFloatEdit;
  EditFinishColorGreen := UiOwner.FindRequiredComponent('EditFinishColorGreen') as TCastleFloatEdit;
  EditFinishColorBlue := UiOwner.FindRequiredComponent('EditFinishColorBlue') as TCastleFloatEdit;
  EditFinishColorAlpha := UiOwner.FindRequiredComponent('EditFinishColorAlpha') as TCastleFloatEdit;
  EditFinishColorVarianceRed := UiOwner.FindRequiredComponent('EditFinishColorVarianceRed') as TCastleFloatEdit;
  EditFinishColorVarianceGreen := UiOwner.FindRequiredComponent('EditFinishColorVarianceGreen') as TCastleFloatEdit;
  EditFinishColorVarianceBlue := UiOwner.FindRequiredComponent('EditFinishColorVarianceBlue') as TCastleFloatEdit;
  EditFinishColorVarianceAlpha := UiOwner.FindRequiredComponent('EditFinishColorVarianceAlpha') as TCastleFloatEdit;
  EditDirectionX := UiOwner.FindRequiredComponent('EditDirectionX') as TCastleFloatEdit;
  EditDirectionY := UiOwner.FindRequiredComponent('EditDirectionY') as TCastleFloatEdit;
  EditDirectionZ := UiOwner.FindRequiredComponent('EditDirectionZ') as TCastleFloatEdit;
  EditDirectionVariance := UiOwner.FindRequiredComponent('EditDirectionVariance') as TCastleFloatEdit;
  EditSpeed := UiOwner.FindRequiredComponent('EditSpeed') as TCastleFloatEdit;
  EditSpeedVariance := UiOwner.FindRequiredComponent('EditSpeedVariance') as TCastleFloatEdit;
  EditRadial := UiOwner.FindRequiredComponent('EditRadial') as TCastleFloatEdit;
  EditRadialVariance := UiOwner.FindRequiredComponent('EditRadialVariance') as TCastleFloatEdit;
  EditPositionVarianceX := UiOwner.FindRequiredComponent('EditPositionVarianceX') as TCastleFloatEdit;
  EditPositionVarianceY := UiOwner.FindRequiredComponent('EditPositionVarianceY') as TCastleFloatEdit;
  EditPositionVarianceZ := UiOwner.FindRequiredComponent('EditPositionVarianceZ') as TCastleFloatEdit;
  EditGravityX := UiOwner.FindRequiredComponent('EditGravityX') as TCastleFloatEdit;
  EditGravityY := UiOwner.FindRequiredComponent('EditGravityY') as TCastleFloatEdit;
  EditGravityZ := UiOwner.FindRequiredComponent('EditGravityZ') as TCastleFloatEdit;
  EditDuration := UiOwner.FindRequiredComponent('EditDuration') as TCastleFloatEdit;
  EditLifeSpan := UiOwner.FindRequiredComponent('EditLifeSpan') as TCastleFloatEdit;
  EditLifeSpanVariance := UiOwner.FindRequiredComponent('EditLifeSpanVariance') as TCastleFloatEdit;
  EditStartParticleSize := UiOwner.FindRequiredComponent('EditStartParticleSize') as TCastleFloatEdit;
  EditStartParticleSizeVariance := UiOwner.FindRequiredComponent('EditStartParticleSizeVariance') as TCastleFloatEdit;
  EditFinishParticleSize := UiOwner.FindRequiredComponent('EditFinishParticleSize') as TCastleFloatEdit;
  EditFinishParticleSizeVariance := UiOwner.FindRequiredComponent('EditFinishParticleSizeVariance') as TCastleFloatEdit;
  EditStartRotation := UiOwner.FindRequiredComponent('EditStartRotation') as TCastleFloatEdit;
  EditStartRotationVariance := UiOwner.FindRequiredComponent('EditStartRotationVariance') as TCastleFloatEdit;
  EditFinishRotation := UiOwner.FindRequiredComponent('EditFinishRotation') as TCastleFloatEdit;
  EditFinishRotationVariance := UiOwner.FindRequiredComponent('EditFinishRotationVariance') as TCastleFloatEdit;
  EditBBoxX1 := UiOwner.FindRequiredComponent('EditBBoxX1') as TCastleFloatEdit;
  EditBBoxY1 := UiOwner.FindRequiredComponent('EditBBoxY1') as TCastleFloatEdit;
  EditBBoxZ1 := UiOwner.FindRequiredComponent('EditBBoxZ1') as TCastleFloatEdit;
  EditBBoxX2 := UiOwner.FindRequiredComponent('EditBBoxX2') as TCastleFloatEdit;
  EditBBoxY2 := UiOwner.FindRequiredComponent('EditBBoxY2') as TCastleFloatEdit;
  EditBBoxZ2 := UiOwner.FindRequiredComponent('EditBBoxZ2') as TCastleFloatEdit;
  EditMaxParticles := UiOwner.FindRequiredComponent('EditMaxParticles') as TCastleIntegerEdit;
  EditBlendSource := UiOwner.FindRequiredComponent('EditBlendSrc') as TCastleIntegerEdit;
  EditBlendDestination := UiOwner.FindRequiredComponent('EditBlendDst') as TCastleIntegerEdit;
  ButtonSave := UiOwner.FindRequiredComponent('ButtonSave') as TCastleButton;
  ButtonLoad := UiOwner.FindRequiredComponent('ButtonLoad') as TCastleButton;
  ButtonApply := UiOwner.FindRequiredComponent('ButtonApply') as TCastleButton;
  ButtonTextureOpen := UiOwner.FindRequiredComponent('ButtonTextureOpen') as TCastleButton;

  ButtonApply.OnClick := @ButtonApplyClick;
  ButtonSave.OnClick := @ButtonSaveClick;
  ButtonLoad.OnClick := @ButtonLoadClick;
  ButtonTextureOpen.OnClick := @ButtonTextureOpenClick;

  Effect := TCastle3DParticleEffect.Create(Self);
  Effect.Load('castle-data:/default.json');
  Emitter := TCastle3DParticleEmitterGPU.Create(Self);
  Emitter.LoadEffect(Effect);
  Effect.Texture := 'data/default.png';
  Emitter.StartEmitting := True;
  Viewport.Items.Add(Emitter);
  EffectToUI;

  BoundingBoxScene := TBoundingBoxScene.Create(Self);
  BoundingBoxScene.UpdateBox(Effect.BBox);
  Viewport.Items.Add(BoundingBoxScene);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  if Event.IsKey(keyEnter) then
  begin
    UIToEffect;
    Emitter.RefreshEffect;
    Exit(True);
  end;
end;

procedure TStateMain.EffectToUI;
begin
  EditTexture.Text := Effect.Texture;
  EditMiddleAnchor.Value := Effect.MiddleAnchor;
  EditStartColorRed.Value := Effect.StartColor.X;
  EditStartColorGreen.Value := Effect.StartColor.Y;
  EditStartColorBlue.Value := Effect.StartColor.Z;
  EditStartColorAlpha.Value := Effect.StartColor.W;
  EditStartColorVarianceRed.Value := Effect.StartColorVariance.X;
  EditStartColorVarianceGreen.Value := Effect.StartColorVariance.Y;
  EditStartColorVarianceBlue.Value := Effect.StartColorVariance.Z;
  EditStartColorVarianceAlpha.Value := Effect.StartColorVariance.W;
  EditMiddleColorRed.Value := Effect.MiddleColor.X;
  EditMiddleColorGreen.Value := Effect.MiddleColor.Y;
  EditMiddleColorBlue.Value := Effect.MiddleColor.Z;
  EditMiddleColorAlpha.Value := Effect.MiddleColor.W;
  EditMiddleColorVarianceRed.Value := Effect.MiddleColorVariance.X;
  EditMiddleColorVarianceGreen.Value := Effect.MiddleColorVariance.X;
  EditMiddleColorVarianceBlue.Value := Effect.MiddleColorVariance.Z;
  EditMiddleColorVarianceAlpha.Value := Effect.MiddleColorVariance.W;
  EditFinishColorRed.Value := Effect.FinishColor.X;
  EditFinishColorGreen.Value := Effect.FinishColor.Y;
  EditFinishColorBlue.Value := Effect.FinishColor.Z;
  EditFinishColorAlpha.Value := Effect.FinishColor.W;
  EditFinishColorVarianceRed.Value := Effect.FinishColorVariance.X;
  EditFinishColorVarianceGreen.Value := Effect.FinishColorVariance.Y;
  EditFinishColorVarianceBlue.Value := Effect.FinishColorVariance.Z;
  EditFinishColorVarianceAlpha.Value := Effect.FinishColorVariance.W;
  EditDirectionX.Value := Effect.Direction.X;
  EditDirectionY.Value := Effect.Direction.Y;
  EditDirectionZ.Value := Effect.Direction.Z;
  EditDirectionVariance.Value := Effect.DirectionVariance;
  EditSpeed.Value := Effect.Speed;
  EditSpeedVariance.Value := Effect.SpeedVariance;
  EditRadial.Value := Effect.Radial;
  EditRadialVariance.Value := Effect.RadialVariance;
  EditPositionVarianceX.Value := Effect.SourcePositionVariance.X;
  EditPositionVarianceY.Value := Effect.SourcePositionVariance.Y;
  EditPositionVarianceZ.Value := Effect.SourcePositionVariance.Z;
  EditGravityX.Value := Effect.Gravity.X;
  EditGravityY.Value := Effect.Gravity.Y;
  EditGravityZ.Value := Effect.Gravity.Z;
  EditDuration.Value := Effect.Duration;
  EditLifeSpan.Value := Effect.ParticleLifeSpan;
  EditLifeSpanVariance.Value := Effect.ParticleLifeSpanVariance;
  EditStartParticleSize.Value := Effect.StartParticleSize;
  EditStartParticleSizeVariance.Value := Effect.StartParticleSizeVariance;
  EditFinishParticleSize.Value := Effect.FinishParticleSize;
  EditFinishParticleSizeVariance.Value := Effect.FinishParticleSizeVariance;
  EditStartRotation.Value := Effect.RotationStart;
  EditStartRotationVariance.Value := Effect.RotationStartVariance;
  EditFinishRotation.Value := Effect.RotationEnd;
  EditFinishRotationVariance.Value := Effect.RotationEndVariance;
  EditBBoxX1.Value := Effect.BBox.Data[0].X;
  EditBBoxY1.Value := Effect.BBox.Data[0].Y;
  EditBBoxZ1.Value := Effect.BBox.Data[0].Z;
  EditBBoxX2.Value := Effect.BBox.Data[1].X;
  EditBBoxY2.Value := Effect.BBox.Data[1].Y;
  EditBBoxZ2.Value := Effect.BBox.Data[1].Z;
  EditMaxParticles.Value := Effect.MaxParticles;
  EditBlendSource.Value := Effect.BlendFuncSource;
  EditBlendDestination.Value := Effect.BlendFuncDestination;
end;

procedure TStateMain.UIToEffect;
begin
  Effect.Texture := EditTexture.Text;
  Effect.MiddleAnchor := EditMiddleAnchor.Value;
  Effect.StartColor := Vector4(
    EditStartColorRed.Value,
    EditStartColorGreen.Value,
    EditStartColorBlue.Value,
    EditStartColorAlpha.Value
  );
  Effect.StartColorVariance := Vector4(
    EditStartColorVarianceRed.Value,
    EditStartColorVarianceGreen.Value,
    EditStartColorVarianceBlue.Value,
    EditStartColorVarianceAlpha.Value
  );
  Effect.MiddleColor := Vector4(
    EditMiddleColorRed.Value,
    EditMiddleColorGreen.Value,
    EditMiddleColorBlue.Value,
    EditMiddleColorAlpha.Value
  );
  Effect.MiddleColorVariance := Vector4(
    EditMiddleColorVarianceRed.Value,
    EditMiddleColorVarianceGreen.Value,
    EditMiddleColorVarianceBlue.Value,
    EditMiddleColorVarianceAlpha.Value
  );
  Effect.FinishColor := Vector4(
    EditFinishColorRed.Value,
    EditFinishColorGreen.Value,
    EditFinishColorBlue.Value,
    EditFinishColorAlpha.Value
  );
  Effect.FinishColorVariance := Vector4(
    EditFinishColorVarianceRed.Value,
    EditFinishColorVarianceGreen.Value,
    EditFinishColorVarianceBlue.Value,
    EditFinishColorVarianceAlpha.Value
  );
  Effect.Direction := Vector3(
    EditDirectionX.Value,
    EditDirectionY.Value,
    EditDirectionZ.Value
  ).Normalize;
  Effect.DirectionVariance := EditDirectionVariance.Value;
  Effect.Speed := EditSpeed.Value;
  Effect.SpeedVariance := EditSpeedVariance.Value;
  Effect.Radial := EditRadial.Value;
  Effect.RadialVariance := EditRadialVariance.Value;
  Effect.SourcePositionVariance := Vector3(
    EditPositionVarianceX.Value,
    EditPositionVarianceY.Value,
    EditPositionVarianceZ.Value
  );
  Effect.Gravity := Vector3(
    EditGravityX.Value,
    EditGravityY.Value,
    EditGravityZ.Value
  );
  Effect.BBox := Box3D(
    Vector3(EditBBoxX1.Value, EditBBoxY1.Value, EditBBoxZ1.Value),
    Vector3(EditBBoxX2.Value, EditBBoxY2.Value, EditBBoxZ2.Value)
  );
  Effect.Duration := EditDuration.Value;
  Effect.ParticleLifeSpan := EditLifeSpan.Value;
  Effect.ParticleLifeSpanVariance := EditLifeSpanVariance.Value;
  Effect.StartParticleSize := EditStartParticleSize.Value;
  Effect.StartParticleSizeVariance := EditStartParticleSizeVariance.Value;
  Effect.FinishParticleSize := EditFinishParticleSize.Value;
  Effect.FinishParticleSizeVariance := EditFinishParticleSizeVariance.Value;
  Effect.RotationStart := EditStartRotation.Value;
  Effect.RotationStartVariance := EditStartRotationVariance.Value;
  Effect.RotationEnd := EditFinishRotation.Value;
  Effect.RotationEndVariance := EditFinishRotationVariance.Value;
  Effect.MaxParticles := EditMaxParticles.Value;
  Effect.BlendFuncSource := EditBlendSource.Value;
  Effect.BlendFuncDestination := EditBlendDestination.Value;
  BoundingBoxScene.UpdateBox(Effect.BBox);
end;

function ExtractRelativeDataPath(URL: String): String;
begin
  URL := StringReplace(URL, 'file:///', '', [rfReplaceAll]);
  URL := StringReplace(URL, '\', '/', [rfReplaceAll]);
  Result := URL;
end;

procedure TStateMain.ButtonSaveClick(Sender: TObject);
var
  URL: String;
begin
  if Window.FileDialog('Save Particle', URL, False, '*.json') then
  begin
    UIToEffect;
    Effect.Save(ExtractRelativeDataPath(URL));
  end;
end;

procedure TStateMain.ButtonLoadClick(Sender: TObject);
var
  URL: String;
begin
  if Window.FileDialog('Open Particle', URL, True, '*.json') then
  begin
    Effect.Load(URL);
    EffectToUI;
    BoundingBoxScene.UpdateBox(Effect.BBox);
    Emitter.RefreshEffect;
  end;
end;

procedure TStateMain.ButtonApplyClick(Sender: TObject);
begin
  UIToEffect;
  Emitter.RefreshEffect;
end;

procedure TStateMain.Render;
begin
  
end;

procedure TStateMain.ButtonTextureOpenClick(Sender: TObject);
var
  URL: String;
begin
  if Window.FileDialog('Open Image', URL, True, '*.*') then
  begin
    EditTexture.Text := URL;
  end;
end;

end.
