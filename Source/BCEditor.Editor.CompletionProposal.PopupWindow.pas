unit BCEditor.Editor.CompletionProposal.PopupWindow;

interface

uses
  Winapi.Messages, System.Classes, System.Types, Vcl.Forms, Vcl.Controls, Vcl.Graphics, BCEditor.Utils,
  BCEditor.Types, BCEditor.Editor.CompletionProposal.Columns, BCEditor.Editor.PopupWindow,
  BCEditor.Editor.CompletionProposal;

{$if defined(USE_VCL_STYLES)}
const
  CM_UPDATE_VCLSTYLE_SCROLLBARS= CM_BASE + 2050;
{$endif}

type
  TBCEditorValidateEvent = procedure(ASender: TObject; Shift: TShiftState; EndToken: Char) of object;

  TBCEditorCompletionProposalPopupWindow = class(TBCEditorPopupWindow)
  strict private
    FAdjustCompletionStart: Boolean;
    FItemIndexArray: array of Integer;
    FBitmapBuffer: TBitmap;
    FCaseSensitive: Boolean;
    FCompletionProposal: TBCEditorCompletionProposal;
    FCompletionStart: Integer;
    FSelectedLine: Integer;
    FCurrentString: string;
    FFiltered: Boolean;
    FFormWidth: Integer;
    FItemHeight: Integer;
    FMargin: Integer;
    FMouseWheelAccumulator: Integer;
    FOnValidate: TBCEditorValidateEvent;
    FTopLine: Integer;
    function GetItems: TStrings;
    procedure AddKeyHandlers;
    procedure EditorKeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure EditorKeyPress(ASender: TObject; var AKey: Char);
    procedure HandleDblClick(ASender: TObject);
    procedure HandleOnValidate(ASender: TObject; AShift: TShiftState; AEndToken: Char);
    procedure MoveSelectedLine(ALineCount: Integer);
    procedure RemoveKeyHandlers;
    procedure SetCurrentString(const AValue: string);
    procedure SetTopLine(const AValue: Integer);
    procedure UpdateScrollBar;
    procedure WMMouseWheel(var AMessage: TMessage); message WM_MOUSEWHEEL;
    procedure WMVScroll(var AMessage: TWMScroll); message WM_VSCROLL;
  protected
    function CanResize(var AWidth, AHeight: Integer): Boolean; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Hide; override;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetCurrentInput: string;
    procedure Assign(ASource: TPersistent); override;
    procedure Execute(const ACurrentString: string; X, Y: Integer);
    property CurrentString: string read FCurrentString write SetCurrentString;
    property Items: TStrings read GetItems;
    property TopLine: Integer read FTopLine write SetTopLine;
    property OnValidate: TBCEditorValidateEvent read FOnValidate write FOnValidate;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, System.UITypes, BCEditor.Editor.Base, BCEditor.Editor.KeyCommands,
  BCEditor.Editor.Utils, BCEditor.Consts, System.Math, Vcl.Dialogs
  {$if defined(USE_VCL_STYLES) or not defined(USE_VCL_STYLES) and not defined(USE_ALPHASKINS)}, Vcl.Themes{$endif};

constructor TBCEditorCompletionProposalPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Visible := False;

  AddKeyHandlers;

  FBitmapBuffer := Vcl.Graphics.TBitmap.Create;
  FFiltered := False;
  FCaseSensitive := False;

  FItemHeight := 0;
  FMargin := 2;

  OnValidate := HandleOnValidate;
  OnDblClick := HandleDblClick;
end;

destructor TBCEditorCompletionProposalPopupWindow.Destroy;
begin
  RemoveKeyHandlers;
  FBitmapBuffer.Free;
  SetLength(FItemIndexArray, 0);

  inherited Destroy;
end;

procedure TBCEditorCompletionProposalPopupWindow.Hide;
begin
  RemoveKeyHandlers;

  inherited Hide;
end;

procedure TBCEditorCompletionProposalPopupWindow.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal then
  begin
    FCompletionProposal := ASource as TBCEditorCompletionProposal;
    with FCompletionProposal do
    begin
      Self.FCaseSensitive := cpoCaseSensitive in Options;
      Self.FFiltered := cpoFiltered in Options;
      Self.FBitmapBuffer.Canvas.Font.Assign(Font);
      Self.FItemHeight := TextHeight(FBitmapBuffer.Canvas, 'X');
      Self.FFormWidth := Width;
    end
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposalPopupWindow.AddKeyHandlers;
var
  LEditor: TBCBaseEditor;
begin
  LEditor := Owner as TBCBaseEditor;
  if Assigned(LEditor) then
  begin
    LEditor.AddKeyPressHandler(EditorKeyPress);
    LEditor.AddKeyDownHandler(EditorKeyDown);
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.RemoveKeyHandlers;
var
  LEditor: TBCBaseEditor;
begin
  LEditor := Owner as TBCBaseEditor;
  if Assigned(LEditor) then
  begin
    LEditor.RemoveKeyPressHandler(EditorKeyPress);
    LEditor.RemoveKeyDownHandler(EditorKeyDown);
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.EditorKeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
var
  LChar: Char;
  LEditor: TBCBaseEditor;
  LTextCaretPosition: TBCEditorTextPosition;
begin
  LEditor := nil;
  if Assigned(Owner) then
    LEditor := Owner as TBCBaseEditor;
  case AKey of
    VK_RETURN, VK_TAB:
      if Assigned(OnValidate) then
        OnValidate(Self, AShift, BCEDITOR_NONE_CHAR);
    VK_ESCAPE:
      Hide;
    VK_LEFT:
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);
          if Assigned(LEditor) then
            LEditor.CommandProcessor(ecLeft, BCEDITOR_NONE_CHAR, nil);
        end
        else
        begin
          if Assigned(LEditor) then
            LEditor.CommandProcessor(ecLeft, BCEDITOR_NONE_CHAR, nil);
          Hide;
        end;
      end;
    VK_RIGHT:
      begin
        if Assigned(LEditor) then
          with LEditor do
          begin
            LTextCaretPosition := TextCaretPosition;
            if LTextCaretPosition.Char <= Length(LEditor.Lines[LTextCaretPosition.Line]) then
              LChar := LEditor.Lines[LTextCaretPosition.Line][LTextCaretPosition.Char]
            else
              LChar := BCEDITOR_SPACE_CHAR;

            if IsWordBreakChar(LChar) then
              Self.Hide
            else
              CurrentString := FCurrentString + LChar;

            CommandProcessor(ecRight, BCEDITOR_NONE_CHAR, nil);
          end;
      end;
    VK_PRIOR:
      MoveSelectedLine(-FCompletionProposal.VisibleLines);
    VK_NEXT:
      MoveSelectedLine(FCompletionProposal.VisibleLines);
    VK_END:
      TopLine := Length(FItemIndexArray) - 1;
    VK_HOME:
      TopLine := 0;
    VK_UP:
      if ssCtrl in AShift then
        FSelectedLine := 0
      else
        MoveSelectedLine(-1);
    VK_DOWN:
      if ssCtrl in AShift then
        FSelectedLine := Length(FItemIndexArray) - 1
      else
        MoveSelectedLine(1);
    VK_BACK:
      if AShift = [] then
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);

          if Assigned(LEditor) then
            LEditor.CommandProcessor(ecBackspace, BCEDITOR_NONE_CHAR, nil);
        end
        else
        begin
          if Assigned(LEditor) then
            LEditor.CommandProcessor(ecBackspace, BCEDITOR_NONE_CHAR, nil);

          Hide;
        end;
      end;
    VK_DELETE:
      if Assigned(LEditor) then
        LEditor.CommandProcessor(ecDeleteChar, BCEDITOR_NONE_CHAR, nil);
  end;
  AKey := 0;
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.EditorKeyPress(ASender: TObject; var AKey: Char);
begin
  case AKey of
    BCEDITOR_CARRIAGE_RETURN, BCEDITOR_ESCAPE:
      Hide;
    BCEDITOR_SPACE_CHAR .. High(Char):
      begin
        if not (cpoAutoInvoke in FCompletionProposal.Options) then
          if (Owner as TBCBaseEditor).IsWordBreakChar(AKey) and Assigned(OnValidate) then
            if AKey = BCEDITOR_SPACE_CHAR then
              OnValidate(Self, [], BCEDITOR_NONE_CHAR);
        CurrentString := FCurrentString + AKey;
        if (cpoAutoInvoke in FCompletionProposal.Options) and (Length(FItemIndexArray) = 0) or
          (Pos(AKey, FCompletionProposal.CloseChars) <> 0) then
          Hide
        else
        if Assigned(OnKeyPress) then
          OnKeyPress(Self, AKey);
      end;
    BCEDITOR_BACKSPACE_CHAR:
      with Owner as TBCBaseEditor do
        CommandProcessor(ecChar, AKey, nil);
  end;
  Invalidate;
end;

function TBCEditorCompletionProposalPopupWindow.CanResize(var AWidth, AHeight: Integer): Boolean;
var
  LVisibleLines: Integer;
begin
  Result := True;

  if FItemHeight <> 0 then
  begin
    LVisibleLines := AHeight div FItemHeight;
    if LVisibleLines < 1 then
      LVisibleLines := 1;
  end
  else
    LVisibleLines := 0;

  FCompletionProposal.VisibleLines := LVisibleLines;
end;

procedure TBCEditorCompletionProposalPopupWindow.Resize;
begin
  inherited;

  if FItemHeight <> 0 then
    FCompletionProposal.VisibleLines := ClientHeight div FItemHeight;

  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.Paint;
var
  LIndex, LColumnIndex: Integer;
  LColumnWidth, LItemIndex: Integer;
begin
  FBitmapBuffer.Width := ClientWidth;
  FBitmapBuffer.Height := ClientHeight;
  with FBitmapBuffer do
  begin
    Canvas.Brush.Color := FCompletionProposal.Colors.Background;
    Winapi.Windows.ExtTextOut(Canvas.Handle, 0, 0, ETO_OPAQUE, ClientRect, '', 0, nil);
    for LIndex := 0 to Min(FCompletionProposal.VisibleLines, Length(FItemIndexArray) - 1) do
    begin
      if LIndex + TopLine >= Length(FItemIndexArray) then
        Break;

      if LIndex + TopLine = FSelectedLine then
      begin
        Canvas.Brush.Color := FCompletionProposal.Colors.SelectedBackground;
        Canvas.Pen.Color := FCompletionProposal.Colors.SelectedBackground;
        Canvas.Rectangle(0, FItemHeight * LIndex, ClientWidth, FItemHeight * (LIndex + 1));
        Canvas.Font.Color := FCompletionProposal.Colors.SelectedText;
      end
      else
      begin
        Canvas.Brush.Color := FCompletionProposal.Colors.Background;
        Canvas.Pen.Color := FCompletionProposal.Colors.Background;
        Canvas.Font.Color := FCompletionProposal.Colors.Foreground;
      end;
      LColumnWidth := 0;
      for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
      begin
        LItemIndex := FItemIndexArray[TopLine + LIndex];
        if LItemIndex < FCompletionProposal.Columns[LColumnIndex].ItemList.Count then
          Canvas.TextOut(FMargin + LColumnWidth, FItemHeight * LIndex, FCompletionProposal.Columns[LColumnIndex].ItemList[LItemIndex]);
        LColumnWidth := LColumnWidth + FCompletionProposal.Columns[LColumnIndex].Width;
      end;
    end;
  end;
  Canvas.Draw(0, 0, FBitmapBuffer);
end;

procedure TBCEditorCompletionProposalPopupWindow.MoveSelectedLine(ALineCount: Integer);
begin
  FSelectedLine := MinMax(FSelectedLine + ALineCount, 0, Length(FItemIndexArray) - 1);
  if FSelectedLine >= TopLine + FCompletionProposal.VisibleLines then
    TopLine := FSelectedLine - FCompletionProposal.VisibleLines + 1;
  if FSelectedLine < TopLine then
    TopLine := FSelectedLine;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetCurrentString(const AValue: string);

  function MatchItem(AIndex: Integer): Boolean;
  var
    LCompareString: string;
  begin
    LCompareString := Copy(GetItems[AIndex], 1, Length(AValue));

    if FCaseSensitive then
      Result := WideCompareStr(LCompareString, AValue) = 0
    else
      Result := WideCompareText(LCompareString, AValue) = 0;
  end;

  procedure RecalcList;
  var
    LIndex, LIndex2, LItemsCount: Integer;
  begin
    LIndex2 := 0;
    LItemsCount := GetItems.Count;
    SetLength(FItemIndexArray, 0);
    SetLength(FItemIndexArray, LItemsCount);
    for LIndex := 0 to LItemsCount - 1 do
      if MatchItem(LIndex) then
      begin
        FItemIndexArray[LIndex2] := LIndex;
        Inc(LIndex2);
      end;
    SetLength(FItemIndexArray, LIndex2);
  end;

var
  LIndex: Integer;
begin
  FCurrentString := AValue;

  if FFiltered then
  begin
    RecalcList;
    TopLine := 0;
    Repaint;
  end
  else
  begin
    LIndex := 0;
    while (LIndex < Items.Count) and (not MatchItem(LIndex)) do
      Inc(LIndex);

    if LIndex < Items.Count then
      TopLine := LIndex
    else
      TopLine := 0;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetTopLine(const AValue: Integer);
var
  LDelta: Integer;
  LClientRect: TRect;
begin
  if TopLine <> AValue then
  begin
    LDelta := TopLine - AValue;
    FTopLine := AValue;
    LClientRect := ClientRect;
    if Abs(LDelta) < FCompletionProposal.VisibleLines then
      ScrollWindow(Handle, 0, FItemHeight * LDelta, @LClientRect, @LClientRect)
    else
      Invalidate;
    UpdateScrollBar;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.WMMouseWheel(var AMessage: TMessage);
var
  Delta: Integer;
  WheelClicks: Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  if GetKeyState(VK_CONTROL) >= 0 then
    Delta := Mouse.WheelScrollLines
  else
    Delta := FCompletionProposal.VisibleLines;

  Inc(FMouseWheelAccumulator, Integer(AMessage.wParamHi));
  WheelClicks := FMouseWheelAccumulator div WHEEL_DELTA;
  FMouseWheelAccumulator := FMouseWheelAccumulator mod WHEEL_DELTA;
  if (Delta = Integer(WHEEL_PAGESCROLL)) or (Delta > FCompletionProposal.VisibleLines) then
    Delta := FCompletionProposal.VisibleLines;

  TopLine := TopLine - (Delta * WheelClicks);
end;

procedure TBCEditorCompletionProposalPopupWindow.Execute(const ACurrentString: string; X, Y: Integer);
var
  LPoint: TPoint;

  procedure CalculateFormPlacement;
  var
    LWidth: Integer;
    LHeight: Integer;
  begin
    LPoint.X := X - TextWidth(FBitmapBuffer.Canvas, ACurrentString);
    LPoint.Y := Y;

    LWidth := FFormWidth;
    LHeight := FItemHeight * FCompletionProposal.VisibleLines + 2;

    if LPoint.X + LWidth > Screen.DesktopWidth then
    begin
      LPoint.X := Screen.DesktopWidth - LWidth - 5;
      if LPoint.X < 0 then
        LPoint.X := 0;
    end;

    if LPoint.Y + LHeight > Screen.DesktopHeight then
    begin
      LPoint.Y := LPoint.Y - LHeight - (Owner as TBCBaseEditor).LineHeight - 2;
      if LPoint.Y < 0 then
        LPoint.Y := 0;
    end;

    Width := LWidth;
    Height := LHeight;
  end;

  procedure CalculateColumnWidths;
  var
    LColumnIndex, LIndex: Integer;
    LMaxWidth, LTempWidth, LAutoWidthCount, LWidthSum: Integer;
    LItems: TStrings;
    LProposalColumn: TBCEditorProposalColumn;
  begin
    if FCompletionProposal.Columns.Count = 1 then
    begin
      LProposalColumn := FCompletionProposal.Columns[0];
      if LProposalColumn.AutoWidth then
        LProposalColumn.Width := Width;
      Exit;
    end;

    LAutoWidthCount := 0;
    LWidthSum := 0;
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
    begin
      LProposalColumn := FCompletionProposal.Columns[LColumnIndex];
      if LProposalColumn.AutoWidth then
      begin
        LItems := LProposalColumn.ItemList;
        LMaxWidth := 0;
        for LIndex := 0 to LItems.Count - 1 do
        begin
          LTempWidth := TextWidth(FBitmapBuffer.Canvas, LItems[LIndex]);
          if LTempWidth > LMaxWidth then
            LMaxWidth := LTempWidth;
        end;
        LProposalColumn.Width := LMaxWidth;
        LWidthSum := LWidthSum + LMaxWidth;
        Inc(LAutoWidthCount);
      end;
    end;

    LMaxWidth := (Width - LWidthSum - GetSystemMetrics(SM_CYHSCROLL)) div LAutoWidthCount;
    if LMaxWidth > 0 then
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
    begin
      LProposalColumn := FCompletionProposal.Columns[LColumnIndex];
      if LProposalColumn.AutoWidth then
        LProposalColumn.Width := LProposalColumn.Width + LMaxWidth;
    end;
  end;

var
  LIndex, LCount: Integer;
begin
  LCount := GetItems.Count;
  SetLength(FItemIndexArray, 0);
  SetLength(FItemIndexArray, LCount);
  for LIndex := 0 to LCount - 1 do
    FItemIndexArray[LIndex] := LIndex;

  if Length(FItemIndexArray) > 0 then
  begin
    CalculateFormPlacement;
    CalculateColumnWidths;
    CurrentString := ACurrentString;
    if Length(FItemIndexArray) > 0 then
    begin
      UpdateScrollBar;
      Show(LPoint);
    end;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.HandleOnValidate(ASender: TObject; AShift: TShiftState; AEndToken: Char);
var
  LEditor: TBCBaseEditor;
  LValue, LLine: string;
  LTextPosition: TBCEditorTextPosition;
begin
  if not Assigned(Owner) then
    Exit;
  LEditor := Owner as TBCBaseEditor;
  with LEditor do
  begin
    BeginUpdate;
    BeginUndoBlock;
    try
      LTextPosition := TextCaretPosition;
      if FAdjustCompletionStart then
        FCompletionStart := GetTextPosition(FCompletionStart, LTextPosition.Line).Char;

      if not SelectionAvailable then
      begin
        SelectionBeginPosition := GetTextPosition(FCompletionStart, LTextPosition.Line);
        if AEndToken = BCEDITOR_NONE_CHAR then
        begin
          LLine := Lines[LTextPosition.Line];
          if (Length(LLine) >= LTextPosition.Char) and IsWordBreakChar(LLine[LTextPosition.Char]) then
            SelectionEndPosition := LTextPosition
          else
            SelectionEndPosition := GetTextPosition(WordEnd.Char, LTextPosition.Line)
        end
        else
          SelectionEndPosition := LTextPosition;
      end;

      if FSelectedLine < Length(FItemIndexArray) then
        LValue := GetItems[FItemIndexArray[FSelectedLine]]
      else
        LValue := SelectedText;

      if SelectedText <> LValue then
        SelectedText := LValue;

      if CanFocus then
        SetFocus;
      EnsureCursorPositionVisible;
      TextCaretPosition := SelectionEndPosition;
      SelectionBeginPosition := TextCaretPosition;
    finally
      EndUndoBlock;
      EndUpdate;
    end;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.HandleDblClick(ASender: TObject);
begin
  if Assigned(OnValidate) then
    OnValidate(Self, [], BCEDITOR_NONE_CHAR);
  Hide;
end;

function TBCEditorCompletionProposalPopupWindow.GetCurrentInput: string;
var
  LIndex: Integer;
  LLineText: string;
  LEditor: TBCBaseEditor;
  LTextCaretPosition: TBCEditorTextPosition;
begin
  Result := '';
  LEditor := Owner as TBCBaseEditor;

  LTextCaretPosition := LEditor.TextCaretPosition;

  LLineText := LEditor.Lines[LTextCaretPosition.Line];
  LIndex := LTextCaretPosition.Char - 1;
  if LIndex <= Length(LLineText) then
  begin
    FAdjustCompletionStart := False;
    while (LIndex > 0) and (LLineText[LIndex] > BCEDITOR_SPACE_CHAR) and not LEditor.IsWordBreakChar(LLineText[LIndex]) do
      Dec(LIndex);

    FCompletionStart := LIndex + 1;
    Result := Copy(LLineText, FCompletionStart, LTextCaretPosition.Char - FCompletionStart);
  end
  else
  begin
    FAdjustCompletionStart := True;
    FCompletionStart := LTextCaretPosition.Char;
  end;
end;

function TBCEditorCompletionProposalPopupWindow.GetItems: TStrings;
begin
  Result := FCompletionProposal.Columns[FCompletionProposal.CompletionColumnIndex].ItemList;
end;

procedure TBCEditorCompletionProposalPopupWindow.UpdateScrollBar;
var
  LScrollInfo: TScrollInfo;
begin
  LScrollInfo.cbSize := SizeOf(ScrollInfo);
  LScrollInfo.fMask := SIF_ALL;
  LScrollInfo.fMask := LScrollInfo.fMask or SIF_DISABLENOSCROLL;

  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);

  LScrollInfo.nMin := 0;
  LScrollInfo.nMax := Max(0, GetItems.Count - 2);
  LScrollInfo.nPage := FCompletionProposal.VisibleLines;
  LScrollInfo.nPos := TopLine;

  ShowScrollBar(Handle, SB_VERT, (LScrollInfo.nMin = 0) or (LScrollInfo.nMax > FCompletionProposal.VisibleLines));
  SetScrollInfo(Handle, SB_VERT, LScrollInfo, True);

  if GetItems.Count <= FCompletionProposal.VisibleLines then
    EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH)
  else
  begin
    EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
    if TopLine <= 0 then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_UP)
    else
    if TopLine + FCompletionProposal.VisibleLines >= GetItems.Count then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_DOWN);
  end;

  if Visible then
    SendMessage(Handle, WM_SETREDRAW, -1, 0);

{$if defined(USE_VCL_STYLES)}
  Perform(CM_UPDATE_VCLSTYLE_SCROLLBARS, 0, 0);
{$endif}
end;

procedure TBCEditorCompletionProposalPopupWindow.WMVScroll(var AMessage: TWMScroll);
begin
  Invalidate;
  AMessage.Result := 0;

  case AMessage.ScrollCode of
    SB_TOP:
      TopLine := 0;
    SB_BOTTOM:
      TopLine := GetItems.Count - 1;
    SB_LINEDOWN:
      TopLine := Min(GetItems.Count - FCompletionProposal.VisibleLines, TopLine + 1);
    SB_LINEUP:
      TopLine := Max(0, TopLine - 1);
    SB_PAGEDOWN:
      TopLine := Min(GetItems.Count - FCompletionProposal.VisibleLines, TopLine + FCompletionProposal.VisibleLines);
    SB_PAGEUP:
      TopLine := Max(0, TopLine - FCompletionProposal.VisibleLines);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      //begin  TODO
        //if GetItems.Count > BCEDITOR_MAX_SCROLL_RANGE then
        //  TopLine := MulDiv(FCompletionProposal.VisibleLines + GetItems.Count - 1, AMessage.Pos, BCEDITOR_MAX_SCROLL_RANGE)
        //else
      TopLine := AMessage.Pos;
      //end;
  end;
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  FSelectedLine := Max(0, TopLine + (Y div FItemHeight));
  inherited MouseDown(AButton, AShift, X, Y);
  Refresh;
end;

end.
