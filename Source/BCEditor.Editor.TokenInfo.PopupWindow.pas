unit BCEditor.Editor.TokenInfo.PopupWindow;

interface

uses
  System.Classes, System.Types, Vcl.Graphics, BCEditor.Editor.PopupWindow;

type
  TBCEditorTokenInfoPopupWindow = class(TBCEditorPopupWindow)
  strict private
    FBitmapBuffer: Vcl.Graphics.TBitmap;
    FContent: TStrings;
    FTitleContent: TStrings;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    procedure Execute(const APoint: TPoint);
    property Content: TStrings read FContent write FContent;
    property TitleContent: TStrings read FTitleContent write FTitleContent;
  end;

implementation

uses
  BCEditor.Editor.TokenInfo;

constructor TBCEditorTokenInfoPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FContent := TStringList.Create;
  FTitleContent := TStringList.Create;

  FBitmapBuffer := Vcl.Graphics.TBitmap.Create;

  Width := 300;
  Height := 300;
end;

destructor TBCEditorTokenInfoPopupWindow.Destroy;
begin
  FContent.Free;
  FTitleContent.Free;
  FBitmapBuffer.Free;

  inherited Destroy;
end;

procedure TBCEditorTokenInfoPopupWindow.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTokenInfo then
  with ASource as TBCEditorTokenInfo do
  begin
    // TODO

  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTokenInfoPopupWindow.Execute(const APoint: TPoint);
begin
  // TODO: ParseContent;
  Show(APoint);
end;

procedure TBCEditorTokenInfoPopupWindow.Paint;
begin
  with FBitmapBuffer do
  begin
    Canvas.Brush.Color := clRed;
    Height := 0;
    Width := ClientWidth;
    Height := ClientHeight;
  end;
  Canvas.Draw(0, 0, FBitmapBuffer);
end;

end.
