{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleBoxes,
  Castle3DParticleEmitterGPU;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state_main.castle-user-interface. }
    LabelFps: TCastleLabel;
    Viewport: TCastleViewport;
    ButtonFire,
    ButtonFireflies,
    ButtonDustDevil,
    ButtonFountain: TCastleButton;
    Emitter: TCastle3DParticleEmitterGPU;
    procedure ButtonFireClick(Sender: TObject);
    procedure ButtonFirefliesClick(Sender: TObject);
    procedure ButtonFountainClick(Sender: TObject);
    procedure ButtonDustDevilClick(Sender: TObject);
  public
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

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  LabelFps := UiOwner.FindRequiredComponent('LabelFps') as TCastleLabel;

  Viewport := UiOwner.FindRequiredComponent('Viewport') as TCastleViewport;
  ButtonFire := UiOwner.FindRequiredComponent('ButtonFire') as TCastleButton;
  ButtonFireflies := UiOwner.FindRequiredComponent('ButtonFireflies') as TCastleButton;
  ButtonFountain := UiOwner.FindRequiredComponent('ButtonFountain') as TCastleButton;
  ButtonDustDevil := UiOwner.FindRequiredComponent('ButtonDustDevil') as TCastleButton;

  ButtonFire.OnClick := @ButtonFireClick;
  ButtonFireflies.OnClick := @ButtonFirefliesClick;
  ButtonFountain.OnClick := @ButtonFountainClick;
  ButtonDustDevil.OnClick := @ButtonDustDevilClick;

  Emitter := TCastle3DParticleEmitterGPU.Create(Self);
  Emitter.LoadEffect('castle-data:/fire.json');
  Emitter.StartEmitting := True;
  Viewport.Items.Add(Emitter);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
