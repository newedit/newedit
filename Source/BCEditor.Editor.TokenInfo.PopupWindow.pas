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
    FMaxHeight: Integer;
    FMaxWidth: Integer;
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
  BCEditor.Types, BCEditor.Consts, System.UITypes;

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
      if not (tioAutoSize in Options) then
      begin
        Self.Width := Width;
        Self.Height := Height;
      end;
    end
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTokenInfoPopupWindow.Execute(const APoint: TPoint);
begin
  FMaxHeight := 0;
  FMaxWidth := 0;
  ParseText(FContent, FContentTextTokensList, FTokenInfo.Font);
  ParseText(FTitleContent, FTitleContentTextTokensList, FTokenInfo.Title.Font);

  if tioAutoSize in FTokenInfo.Options then
  begin
    // TODO if height goes over the bottom, show scroll bar
    Height := FMaxHeight;
    Width := FMaxWidth;
  end;

  Show(APoint);
end;

procedure TBCEditorTokenInfoPopupWindow.Paint;
var
  LIndex: Integer;
  LPTextToken: PBCEditorTokenInfoTextToken;
  LPreviousStyles: TBCEditorTokenInfoTextStyles;
  LLeft, LTop, LPreviousTop: Integer;
  LText: string;

  procedure PaintToken;
  begin
    Canvas.Font.Style := [];
    if tsBold in LPTextToken^.Styles then
      Canvas.Font.Style :=  Canvas.Font.Style + [fsBold];
    if tsItalic in LPTextToken^.Styles then
      Canvas.Font.Style :=  Canvas.Font.Style + [fsItalic];
    if tsReference in LPTextToken^.Styles then
      Canvas.Font.Style :=  Canvas.Font.Style + [fsUnderline];
    Canvas.TextOut(LLeft, LTop, LText);
  end;

begin
  with FBitmapBuffer do
  begin
    Canvas.Brush.Color := FTokenInfo.Colors.Background;
    Height := 0;
    Width := ClientWidth;
    Height := ClientHeight;
    LLeft := 0;
    LTop := 0;

    if FTitleContentTextTokensList.Count > 0 then
    begin
      Canvas.Brush.Color := FTokenInfo.Title.Colors.Background;

      LPTextToken := PBCEditorTokenInfoTextToken(FTitleContentTextTokensList[0]);
      LPreviousStyles := LPTextToken^.Styles;
      LPreviousTop := LPTextToken^.Rect.Top;
      for LIndex := 0 to FTitleContentTextTokensList.Count - 1 do
      begin
        Canvas.Font.Assign(FTokenInfo.Title.Font);
        if tsReference in LPTextToken^.Styles then
          Canvas.Font.Color := FTokenInfo.Title.Colors.Reference;

        LPTextToken := PBCEditorTokenInfoTextToken(FTitleContentTextTokensList[LIndex]);
        if (LPreviousStyles <> LPTextToken^.Styles) or (LPreviousTop <> LPTextToken^.Rect.Top) then
        begin
          PaintToken;
          LText := '';
          LLeft := LPTextToken^.Rect.Right;
          LTop := LPTextToken^.Rect.Top;
        end;
        LPreviousStyles := LPTextToken^.Styles;
        LPreviousTop :=  LPTextToken^.Rect.Top;
        LText := LText + LPTextToken^.Value;
      end;
    end;

    if FContentTextTokensList.Count > 0 then
    begin
      Canvas.Brush.Color := FTokenInfo.Colors.Background;
      Canvas.Font.Assign(FTokenInfo.Font);
      for LIndex := 0 to FContentTextTokensList.Count - 1 do
      begin


      end;
    end;
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
    if LCurrentRect.Right > FMaxWidth then
      FMaxWidth := LCurrentRect.Right;
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
    Inc(FMaxHeight, LTextHeight);
    Inc(LCurrentRect.Top, LTextHeight);
    Inc(LCurrentRect.Bottom, LTextHeight);
  end;

begin
  AddTokens;
  FBitmapBuffer.Canvas.Font.Assign(AFont);
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
