unit BCEditor.Editor.TokenInfo.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorTokenInfoColors = class(TPersistent)
  strict private
    FBackground: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clWindow;
  end;

implementation

constructor TBCEditorTokenInfoColors.Create;
begin
  inherited;

  FBackground := clWindow;
end;

procedure TBCEditorTokenInfoColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTokenInfoColors then
  with ASource as TBCEditorTokenInfoColors do
  begin
    Self.FBackground := FBackground;
  end
  else
    inherited Assign(ASource);
end;

end.
