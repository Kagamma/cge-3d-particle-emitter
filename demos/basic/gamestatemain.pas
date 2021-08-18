{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleBoxes,
  CastleCameras,
  CastleParticleEmitter;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    Viewport: TCastleViewport;
    ExamineNavigation1: TCastleExamineNavigation;
    ButtonFire,
    ButtonFireflies,
    ButtonDustDevil,
    ButtonFountain: TCastleButton;
    Emitter: TCastleParticleEmitter;
    procedure ButtonFireClick(Sender: TObject);
    procedure ButtonFirefliesClick(Sender: TObject);
    procedure ButtonFountainClick(Sender: TObject);
    procedure ButtonDustDevilClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.ButtonFireClick(Sender: TObject);
begin
  Emitter.LoadEffect('castle-data:/fire.json');
end;

procedure TStateMain.ButtonFirefliesClick(Sender: TObject);
begin
  Emitter.LoadEffect('castle-data:/fireflies.json');
end;

procedure TStateMain.ButtonFountainClick(Sender: TObject);
begin
  Emitter.LoadEffect('castle-data:/fountain.json');
end;

procedure TStateMain.ButtonDustDevilClick(Sender: TObject);
begin
  Emitter.LoadEffect('castle-data:/dustdevil.json');
end;

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;

  Viewport := DesignedComponent('Viewport') as TCastleViewport;
  ExamineNavigation1 := DesignedComponent('ExamineNavigation1') as TCastleExamineNavigation;
  ButtonFire := DesignedComponent('ButtonFire') as TCastleButton;
  ButtonFireflies := DesignedComponent('ButtonFireflies') as TCastleButton;
  ButtonFountain := DesignedComponent('ButtonFountain') as TCastleButton;
  ButtonDustDevil := DesignedComponent('ButtonDustDevil') as TCastleButton;

  ButtonFire.OnClick := @ButtonFireClick;
  ButtonFireflies.OnClick := @ButtonFirefliesClick;
  ButtonFountain.OnClick := @ButtonFountainClick;
  ButtonDustDevil.OnClick := @ButtonDustDevilClick;

  Emitter := TCastleParticleEmitter.Create(Self);
  Emitter.LoadEffect('castle-data:/fire.json');
  Emitter.StartEmitting := True;
  Viewport.Items.Add(Emitter);

  { It is necessary to assign ModelBox to have sensible movement/scaling
    possible by TCastleExamineNavigation.
    It is easiest to hardcode it to some "approximate" size of world.
    It only determines how fast you move/scale world by dragging
    with various mouse buttons, it doesn't have to reflect actual world. }
  ExamineNavigation1.ModelBox := BoundingBox3DFromSphere(TVector3.Zero, 1);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
