unit BCEditor.Editor.TokenInfo.PopupWindow;

{
  Supported tags in contents:

  <a href="Reference">Link text</a>
  <b>Bold text</b>
  <i>Italic text</i>
}

interface

uses
  System.Classes, System.Types, Vcl.Graphics, BCEditor.Lines, BCEditor.Editor.PopupWindow, BCEditor.Editor.TokenInfo;

type
  TBCEditorTokenInfoEvent = procedure(ASender: TObject; const AToken: string; AContent: TBCEditorLines; ATitleContent: TBCEditorLines; var AShowInfo: Boolean) of object;

  TBCEditorTokenInfoPopupWindow = class(TBCEditorPopupWindow)
  strict private
    FBitmapBuffer: Vcl.Graphics.TBitmap;
    FContent: TBCEditorLines;
    FContentTextTokensList: TList;
    FTitleContent: TBCEditorLines;
    FTitleContentTextTokensList: TList;
    FTokenInfo: TBCEditorTokenInfo;
    procedure ParseText(AText: TBCEditorLines; ATokens: TList; AFont: TFont);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    procedure Execute(const APoint: TPoint);
    property Content: TBCEditorLines read FContent write FContent;
    property TitleContent: TBCEditorLines read FTitleContent write FTitleContent;
  end;

implementation

uses
  BCEditor.Consts;

type
  TBCEditorTokenInfoTextStyle = (tsBold, tsItalic, tsReference);
  TBCEditorTokenInfoTextStyles = set of TBCEditorTokenInfoTextStyle;

  TBCEditorTokenInfoTextToken = record
    Value: string;
    Styles: TBCEditorTokenInfoTextStyles;
    Rect: TRect;
    Reference: string;
  end;
  PBCEditorTokenInfoTextToken = ^TBCEditorTokenInfoTextToken;

{ TBCEditorTokenInfoPopupWindow }

constructor TBCEditorTokenInfoPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FContent := TBCEditorLines.Create(nil);
  FContentTextTokensList := TList.Create;

  FTitleContent := TBCEditorLines.Create(nil);
  FTitleContentTextTokensList := TList.Create;

  FBitmapBuffer := Vcl.Graphics.TBitmap.Create;

  // TODO remove these
  Width := 300;
  Height := 300;
end;

destructor TBCEditorTokenInfoPopupWindow.Destroy;
var
  LIndex: Integer;
begin
  FContent.Free;

  for LIndex := FContentTextTokensList.Count - 1 downto 0 do
    Dispose(PBCEditorTokenInfoTextToken(FContentTextTokensList.Items[LIndex]));
  FContentTextTokensList.Clear;
  FContentTextTokensList.Free;

  FTitleContent.Free;

  for LIndex := FTitleContentTextTokensList.Count - 1 downto 0 do
    Dispose(PBCEditorTokenInfoTextToken(FTitleContentTextTokensList.Items[LIndex]));
  FTitleContentTextTokensList.Clear;
  FTitleContentTextTokensList.Free;

  FBitmapBuffer.Free;

  inherited Destroy;
end;

procedure TBCEditorTokenInfoPopupWindow.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTokenInfo then
  begin
    FTokenInfo := ASource as TBCEditorTokenInfo;
    with FTokenInfo do
    begin
      // TODO

    end
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTokenInfoPopupWindow.Execute(const APoint: TPoint);
begin
  ParseText(FContent, FContentTextTokensList, FTokenInfo.Font);
  ParseText(FTitleContent, FTitleContentTextTokensList, FTokenInfo.Title.Font);

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

procedure TBCEditorTokenInfoPopupWindow.ParseText(AText: TBCEditorLines; ATokens: TList; AFont: TFont);
const
  CTOKEN_REFERENCE = 0;
  CTOKEN_BOLD = 1;
  CTOKEN_ITALIC = 2;
var
  LIndex: Integer;
  LPText, LPToken, LPBookmark: PChar;
  LPTextToken: PBCEditorTokenInfoTextToken;
  LCurrentValue: string;
  LCurrentStyles: TBCEditorTokenInfoTextStyles;
  LCurrentRect: TRect;
  LCurrentReference: string;
  LTextHeight: Integer;
  LMaxWidth: Integer;
  LOpenTokens: array [0..2] of string;
  LCloseTokens: array [0..2] of string;

  procedure AddTokens;
  begin
    LOpenTokens[CTOKEN_REFERENCE] := '<A HREF="';
    LOpenTokens[CTOKEN_BOLD] := '<B>';
    LOpenTokens[CTOKEN_ITALIC] := '<I>';
    LCloseTokens[CTOKEN_REFERENCE] := '</A>';
    LCloseTokens[CTOKEN_BOLD] := '</B>';
    LCloseTokens[CTOKEN_ITALIC] := '</I>';
  end;

  procedure ClearCurrentValue;
  begin
    LCurrentValue := '';
    LCurrentStyles := [];
    LCurrentReference := '';
  end;

  procedure AddTextToken;
  begin
    New(LPTextToken);
    LPTextToken^.Value := LCurrentValue;
    LPTextToken^.Styles := LCurrentStyles;
    LCurrentRect.Right := LCurrentRect.Left + FBitmapBuffer.Canvas.TextWidth(LCurrentValue);
    if LCurrentRect.Right > LMaxWidth then
      LMaxWidth := LCurrentRect.Right;
    LPTextToken^.Rect := LCurrentRect;
    LPTextToken^.Reference := LCurrentReference;
    ATokens.Add(LPTextToken);
    ClearCurrentValue;
    LCurrentRect.Left := LCurrentRect.Right;
  end;

  procedure NextLine;
  begin
    AddTextToken;
    LCurrentRect.Left := 0;
    Inc(LCurrentRect.Top, LTextHeight);
    Inc(LCurrentRect.Bottom, LTextHeight);
  end;

begin
  AddTokens;
  FBitmapBuffer.Canvas.Font.Assign(AFont);
  LMaxWidth := 0;
  LTextHeight := FBitmapBuffer.Canvas.TextHeight('X');
  LCurrentRect.Left := 0;
  LCurrentRect.Top := 0;
  LCurrentRect.Bottom := LTextHeight;
  LPText := PChar(AText.Text);
  ClearCurrentValue;
  while LPText^ <> BCEDITOR_NONE_CHAR do
  begin
    if LPText^ = '<' then
    begin
      for LIndex := 0 to Length(LOpenTokens) - 1 do
      begin
        LPToken := PChar(LOpenTokens[LIndex]);
        LPBookmark := LPText;
        while (LPText^ <> BCEDITOR_NONE_CHAR) and (LPToken^ <> BCEDITOR_NONE_CHAR) and (UpCase(LPText^) = LPToken^) do
        begin
          Inc(LPText);
          Inc(LPToken);
        end;
        if LPToken^ = BCEDITOR_NONE_CHAR then
        begin
          if LCurrentValue <> '' then
            AddTextToken;

          case LIndex of
            CTOKEN_REFERENCE:
              begin
                while (LPText^ <> BCEDITOR_NONE_CHAR) and (LPText^ <> '"') do
                begin
                  LCurrentReference := LCurrentReference + LPText^;
                  Inc(LPText);
                end;
                Inc(LPText); // '>'
                Include(LCurrentStyles, tsReference);
              end;
            CTOKEN_BOLD:
              Include(LCurrentStyles, tsBold);
            CTOKEN_ITALIC:
              Include(LCurrentStyles, tsItalic);
          end;
          Break;
        end
        else
          LPText := LPBookmark;
      end;

      for LIndex := 0 to Length(LCloseTokens) - 1 do
      begin
        LPToken := PChar(LCloseTokens[LIndex]);
        LPBookmark := LPText;
        while (LPText^ <> BCEDITOR_NONE_CHAR) and (LPToken^ <> BCEDITOR_NONE_CHAR) and (UpCase(LPText^) = LPToken^) do
        begin
          Inc(LPText);
          Inc(LPToken);
        end;
        if LPToken^ = BCEDITOR_NONE_CHAR then
        begin
          if LCurrentValue <> '' then
            AddTextToken;
          Break;
        end
        else
          LPText := LPBookmark;
      end;
    end;

    if LPText^ = BCEDITOR_CARRIAGE_RETURN then
    begin
      if LCurrentValue <> '' then
        AddTextToken;
      NextLine;
      Inc(LPText, 2);
      Continue;
    end;

    LCurrentValue := LCurrentValue + LPText^;
    Inc(LPText);
  end;
end;

end.
