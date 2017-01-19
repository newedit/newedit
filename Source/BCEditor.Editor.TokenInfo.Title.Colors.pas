unit BCEditor.Editor.TokenInfo.Title.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorTokenInfoTitleColors = class(TPersistent)
  strict private
    FBackground: TColor;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clWindow;
  end;

implementation

constructor TBCEditorTokenInfoTitleColors.Create;
begin
  inherited;

  FBackground := clWindow;
end;

procedure TBCEditorTokenInfoTitleColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTokenInfoTitleColors then
  with ASource as TBCEditorTokenInfoTitleColors do
  begin
    Self.FBackground := FBackground;
  end
  else
    inherited Assign(ASource);
end;
end.
