unit BCEditor.TextDrawer;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, System.Math, System.Types, System.UITypes,
  BCEditor.Types;

const
  CFontStyleCount = Ord(High(TFontStyle)) + 1;
  CFontStyleCombineCount = 1 shl CFontStyleCount;

type
  TBCEditorStockFontPatterns = 0 .. CFontStyleCombineCount - 1;

  TBCEditorFontData = record
    Style: TFontStyles;
    Handle: HFont;
    CharAdvance: Integer;
    CharHeight: Integer;
  end;
  PBCEditorFontData = ^TBCEditorFontData;

  TBCEditorFontsData = array [TBCEditorStockFontPatterns] of TBCEditorFontData;

  TBCEditorSharedFontsInfo = record
    RefCount: Integer;
    LockCount: Integer;
    BaseFont: TFont;
    BaseLogFont: TLogFont;
    //IsTrueType: Boolean;
    FontsData: TBCEditorFontsData;
  end;

  PBCEditorSharedFontsInfo = ^TBCEditorSharedFontsInfo;

  { TBCEditorFontsInfoManager }

  TBCEditorFontsInfoManager = class(TObject)
  strict private
    FFontsInfo: TList;
    function FindFontsInfo(const ALogFont: TLogFont): PBCEditorSharedFontsInfo;
    function CreateFontsInfo(ABaseFont: TFont; const ALogFont: TLogFont): PBCEditorSharedFontsInfo;
    procedure DestroyFontHandles(ASharedFontsInfo: PBCEditorSharedFontsInfo);
    procedure RetrieveLogFontForComparison(ABaseFont: TFont; var ALogFont: TLogFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LockFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
    procedure UnLockFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
    function GetFontsInfo(ABaseFont: TFont): PBCEditorSharedFontsInfo;
    procedure ReleaseFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
  end;

  TBCEditorFontStock = class(TObject)
  strict private
    FCurrentFont: HFont;
    FCurrentStyle: TFontStyles;
    FHandle: HDC;
    FHandleRefCount: Integer;
    FPSharedFontsInfo: PBCEditorSharedFontsInfo;
    FUsingFontHandles: Boolean;
    FPCurrentFontData: PBCEditorFontData;
    FBaseLogFont: TLogFont;
    function GetBaseFont: TFont;
    //function GetIsTrueType: Boolean;
  protected
    function CalculateFontAdvance(AHandle: HDC; ACharHeight: PInteger): Integer;
    function GetCharAdvance: Integer;
    function GetCharHeight: Integer;
    function GetFontData(AIndex: Integer): PBCEditorFontData;
    function InternalGetHandle: HDC;
    function InternalCreateFont(AStyle: TFontStyles): HFont;
    procedure InternalReleaseDC(AValue: HDC);
    procedure ReleaseFontsInfo;
    procedure SetBaseFont(AValue: TFont);
    procedure SetStyle(AValue: TFontStyles);
    procedure UseFontHandles;
    property FontData[AIndex: Integer]: PBCEditorFontData read GetFontData;
    property FontsInfo: PBCEditorSharedFontsInfo read FPSharedFontsInfo;
  public
    constructor Create(AInitialFont: TFont);
    destructor Destroy; override;

    procedure ReleaseFontHandles; virtual;
    property BaseFont: TFont read GetBaseFont;
    property Style: TFontStyles read FCurrentStyle write SetStyle;
    property FontHandle: HFont read FCurrentFont;
    property CharAdvance: Integer read GetCharAdvance;
    property CharHeight: Integer read GetCharHeight;
    //property IsTrueType: Boolean read GetIsTrueType;
  end;

  EBCEditorFontStockException = class(Exception);

  { TBCEditorTextDrawer }

  TBCEditorTextDrawer = class(TObject)
  strict private
    FBackgroundColor: TColor;
    FCharHeight: Integer;
    FCharWidth: Integer;
    FCalcExtentBaseStyle: TFontStyles;
    FCharABCWidthCache: array [0 .. 127] of TABC;
    //FCharExtra: Integer;
    FColor: TColor;
    FCurrentFont: HFont;
    FDrawingCount: Integer;
    FExtTextOutDistance: PIntegerArray;
    FExtTextOutDistanceLength: Integer;
    FFontStock: TBCEditorFontStock;
    FHandle: HDC;
    FSaveHandle: Integer;
    FStockBitmap: TBitmap;
  protected
    function GetCachedABCWidth(AChar: Cardinal; var AABC: TABC): Boolean;
    //procedure AfterStyleSet; virtual;
    //procedure DoSetCharExtra(AValue: Integer); virtual;
    procedure FlushCharABCWidthCache;
    property DrawingCount: Integer read FDrawingCount;
    property FontStock: TBCEditorFontStock read FFontStock;
    //property StockHandle: HDC read FHandle;
  public
    constructor Create(ACalcExtentBaseStyle: TFontStyles; ABaseFont: TFont);
    destructor Destroy; override;

    function GetCharCount(AChar: PChar): Integer;
    //function GetCharHeight: Integer; virtual;
    //function GetCharWidth: Integer; virtual;
    //function TextExtent(const Text: string): TSize;
    procedure BeginDrawing(AHandle: HDC);
    procedure EndDrawing;
    procedure ExtTextOut(X, Y: Integer; AOptions: Longint; var ARect: TRect; AText: PChar; ALength: Integer);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBaseFont(AValue: TFont);
    procedure SetBaseStyle(const AValue: TFontStyles);
    //procedure SetCharExtra(AValue: Integer); virtual;
    procedure SetForegroundColor(AValue: TColor);
    procedure SetStyle(AValue: TFontStyles);
    //procedure TextOut(X, Y: Integer; AText: PChar; ALength: Integer); virtual;
    //property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    //property BaseFont: TFont write SetBaseFont;
    //property BaseStyle: TFontStyles write SetBaseStyle;
    //property CharExtra: Integer read FCharExtra write SetCharExtra;
    //property CharHeight: Integer read GetCharHeight;
    //property CharWidth: Integer read GetCharWidth;
    //property ForegroundColor: TColor write SetForegroundColor;
    //property Style: TFontStyles write SetStyle;
    property CharHeight: Integer read FCharHeight;
    property CharWidth: Integer read FCharWidth;
    //property Handle: HDC read FHandle write FHandle;
  end;

  EBCEditorTextDrawerException = class(Exception);

function GetFontsInfoManager: TBCEditorFontsInfoManager;

implementation

uses
  BCEditor.Language, System.Character;

var
  GFontsInfoManager: TBCEditorFontsInfoManager;

function GetFontsInfoManager: TBCEditorFontsInfoManager;
begin
  if not Assigned(GFontsInfoManager) then
    GFontsInfoManager := TBCEditorFontsInfoManager.Create;
  Result := GFontsInfoManager;
end;

{ TFontsInfoManager }

procedure TBCEditorFontsInfoManager.LockFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
begin
  Inc(ASharedFontsInfo^.LockCount);
end;

constructor TBCEditorFontsInfoManager.Create;
begin
  inherited;

  FFontsInfo := TList.Create;
end;

function TBCEditorFontsInfoManager.CreateFontsInfo(ABaseFont: TFont; const ALogFont: TLogFont): PBCEditorSharedFontsInfo;
begin
  New(Result);
  FillChar(Result^, SizeOf(TBCEditorSharedFontsInfo), 0);
  with Result^ do
  try
    BaseFont := TFont.Create;
    BaseFont.Assign(ABaseFont);
    BaseLogFont := ALogFont;
    //IsTrueType := 0 <> (TRUETYPE_FONTTYPE and ALogFont.lfPitchAndFamily);
  except
    Result^.BaseFont.Free;
    Dispose(Result);
    raise;
  end;
end;

procedure TBCEditorFontsInfoManager.UnLockFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
begin
  with ASharedFontsInfo^ do
  begin
    Dec(LockCount);
    if 0 = LockCount then
      DestroyFontHandles(ASharedFontsInfo);
  end;
end;

destructor TBCEditorFontsInfoManager.Destroy;
begin
  GFontsInfoManager := nil;

  if Assigned(FFontsInfo) then
  begin
    while FFontsInfo.Count > 0 do
    begin
      Assert(1 = PBCEditorSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1])^.RefCount);
      ReleaseFontsInfo(PBCEditorSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1]));
    end;
    FFontsInfo.Free;
  end;

  inherited;
end;

procedure TBCEditorFontsInfoManager.DestroyFontHandles(ASharedFontsInfo: PBCEditorSharedFontsInfo);
var
  i: Integer;
  LFontData: TBCEditorFontData;
begin
  with ASharedFontsInfo^ do
  for i := Low(TBCEditorStockFontPatterns) to High(TBCEditorStockFontPatterns) do
  begin
    LFontData := FontsData[i];
    if LFontData.Handle <> 0 then
    begin
      DeleteObject(LFontData.Handle);
      LFontData.Handle := 0;
    end;
  end;
end;

function TBCEditorFontsInfoManager.FindFontsInfo(const ALogFont: TLogFont): PBCEditorSharedFontsInfo;
var
  i: Integer;
begin
  for i := 0 to FFontsInfo.Count - 1 do
  begin
    Result := PBCEditorSharedFontsInfo(FFontsInfo[i]);
    if CompareMem(@(Result^.BaseLogFont), @ALogFont, SizeOf(TLogFont)) then
      Exit;
  end;
  Result := nil;
end;

function TBCEditorFontsInfoManager.GetFontsInfo(ABaseFont: TFont): PBCEditorSharedFontsInfo;
var
  LLogFont: TLogFont;
begin
  Assert(Assigned(ABaseFont));

  RetrieveLogFontForComparison(ABaseFont, LLogFont);
  Result := FindFontsInfo(LLogFont);
  if not Assigned(Result) then
  begin
    Result := CreateFontsInfo(ABaseFont, LLogFont);
    FFontsInfo.Add(Result);
  end;

  if Assigned(Result) then
    Inc(Result^.RefCount);
end;

procedure TBCEditorFontsInfoManager.ReleaseFontsInfo(ASharedFontsInfo: PBCEditorSharedFontsInfo);
begin
  Assert(Assigned(ASharedFontsInfo));

  with ASharedFontsInfo^ do
  begin
    Assert(LockCount < RefCount);
    if RefCount > 1 then
      Dec(RefCount)
    else
    begin
      FFontsInfo.Remove(ASharedFontsInfo);
      BaseFont.Free;
      Dispose(ASharedFontsInfo);
    end;
  end;
end;

procedure TBCEditorFontsInfoManager.RetrieveLogFontForComparison(ABaseFont: TFont; var ALogFont: TLogFont);
var
  LPEnd: PChar;
begin
  GetObject(ABaseFont.Handle, SizeOf(TLogFont), @ALogFont);
  with ALogFont do
  begin
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    LPEnd := StrEnd(lfFaceName);
    FillChar(LPEnd[1], @lfFaceName[high(lfFaceName)] - LPEnd, 0);
  end;
end;

{ TFontStock }

function TBCEditorFontStock.CalculateFontAdvance(AHandle: HDC; ACharHeight: PInteger): Integer;
var
  LTextMetric: TTextMetric;
  LCharInfo: TABC;
  LHasABC: Boolean;
begin
  GetTextMetrics(AHandle, LTextMetric);
  LHasABC := GetCharABCWidths(AHandle, Ord('M'), Ord('M'), LCharInfo);
  if not LHasABC then
  begin
    with LCharInfo do
    begin
      abcA := 0;
      abcB := LTextMetric.tmAveCharWidth;
      abcC := 0;
    end;
    LTextMetric.tmOverhang := 0;
  end;

  with LCharInfo do
    Result := abcA + Integer(abcB) + abcC + LTextMetric.tmOverhang;
  if Assigned(ACharHeight) then
    ACharHeight^ := Abs(LTextMetric.tmHeight)
end;

constructor TBCEditorFontStock.Create(AInitialFont: TFont);
begin
  inherited Create;

  SetBaseFont(AInitialFont);
end;

destructor TBCEditorFontStock.Destroy;
begin
  ReleaseFontsInfo;
  Assert(FHandleRefCount = 0);

  inherited;
end;

function TBCEditorFontStock.GetBaseFont: TFont;
begin
  Result := FPSharedFontsInfo^.BaseFont;
end;

function TBCEditorFontStock.GetCharAdvance: Integer;
begin
  Result := FPCurrentFontData^.CharAdvance;
end;

function TBCEditorFontStock.GetCharHeight: Integer;
begin
  Result := FPCurrentFontData^.CharHeight;
end;

function TBCEditorFontStock.GetFontData(AIndex: Integer): PBCEditorFontData;
begin
  Result := @FPSharedFontsInfo^.FontsData[AIndex];
end;

{function TBCEditorFontStock.GetIsTrueType: Boolean;
begin
  Result := FPSharedFontsInfo^.IsTrueType
end;  }

function TBCEditorFontStock.InternalCreateFont(AStyle: TFontStyles): HFont;
const
  CBolds: array [Boolean] of Integer = (400, 700);
begin
  with FBaseLogFont do
  begin
    lfWeight := CBolds[fsBold in AStyle];
    lfItalic := Ord(BOOL(fsItalic in AStyle));
    lfUnderline := Ord(BOOL(fsUnderline in AStyle));
    lfStrikeOut := Ord(BOOL(fsStrikeOut in AStyle));
  end;
  Result := CreateFontIndirect(FBaseLogFont);
end;

function TBCEditorFontStock.InternalGetHandle: HDC;
begin
  if FHandleRefCount = 0 then
  begin
    Assert(FHandle = 0);
    FHandle := GetDC(0);
  end;
  Inc(FHandleRefCount);
  Result := FHandle;
end;

procedure TBCEditorFontStock.InternalReleaseDC(AValue: HDC);
begin
  Dec(FHandleRefCount);
  if FHandleRefCount <= 0 then
  begin
    Assert((FHandle <> 0) and (FHandle = AValue));
    ReleaseDC(0, FHandle);
    FHandle := 0;
    Assert(FHandleRefCount = 0);
  end;
end;

procedure TBCEditorFontStock.ReleaseFontHandles;
begin
  if FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      UnLockFontsInfo(FPSharedFontsInfo);
      FUsingFontHandles := False;
    end;
end;

procedure TBCEditorFontStock.ReleaseFontsInfo;
begin
  if Assigned(FPSharedFontsInfo) then
    with GetFontsInfoManager do
    begin
      if FUsingFontHandles then
      begin
        UnLockFontsInfo(FPSharedFontsInfo);
        FUsingFontHandles := False;
      end;
      ReleaseFontsInfo(FPSharedFontsInfo);
      FPSharedFontsInfo := nil;
    end;
end;

procedure TBCEditorFontStock.SetBaseFont(AValue: TFont);
var
  LSharedFontsInfo: PBCEditorSharedFontsInfo;
begin
  if Assigned(AValue) then
  begin
    LSharedFontsInfo := GetFontsInfoManager.GetFontsInfo(AValue);
    if LSharedFontsInfo = FPSharedFontsInfo then
      GetFontsInfoManager.ReleaseFontsInfo(LSharedFontsInfo)
    else
    begin
      ReleaseFontsInfo;
      FPSharedFontsInfo := LSharedFontsInfo;
      FBaseLogFont := FPSharedFontsInfo^.BaseLogFont;
      SetStyle(AValue.Style);
    end;
  end
  else
    raise EBCEditorFontStockException.Create(SBCEditorValueMustBeSpecified);
end;

procedure TBCEditorFontStock.SetStyle(AValue: TFontStyles);
var
  LIndex: Integer;
  LHandle: HDC;
  LOldFont: HFont;
  LFontDataPointer: PBCEditorFontData;
begin
  Assert(SizeOf(TFontStyles) = 1);

  LIndex := Byte(AValue);
  Assert(LIndex <= High(TBCEditorStockFontPatterns));

  UseFontHandles;
  LFontDataPointer := FontData[LIndex];
  if FPCurrentFontData = LFontDataPointer then
    Exit;

  FPCurrentFontData := LFontDataPointer;
  with LFontDataPointer^ do
  if Handle <> 0 then
  begin
    FCurrentFont := Handle;
    FCurrentStyle := Style;
    Exit;
  end;

  FCurrentFont := InternalCreateFont(AValue);
  LHandle := InternalGetHandle;
  LOldFont := SelectObject(LHandle, FCurrentFont);

  with FPCurrentFontData^ do
  begin
    Handle := FCurrentFont;
    CharAdvance := CalculateFontAdvance(LHandle, @CharHeight);
  end;

  SelectObject(LHandle, LOldFont);
  InternalReleaseDC(LHandle);
end;

procedure TBCEditorFontStock.UseFontHandles;
begin
  if not FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      LockFontsInfo(FPSharedFontsInfo);
      FUsingFontHandles := True;
    end;
end;

{ TBCEditorTextDrawer }

constructor TBCEditorTextDrawer.Create(ACalcExtentBaseStyle: TFontStyles; ABaseFont: TFont);
begin
  inherited Create;

  FFontStock := TBCEditorFontStock.Create(ABaseFont);
  FStockBitmap := TBitmap.Create;
  FCalcExtentBaseStyle := ACalcExtentBaseStyle;
  SetBaseFont(ABaseFont);
  FColor := clWindowText;
  FBackgroundColor := clWindow;
  FExtTextOutDistanceLength := 0;
end;

destructor TBCEditorTextDrawer.Destroy;
begin
  FStockBitmap.Free;
  FFontStock.Free;
  if Assigned(FExtTextOutDistance) then
  begin
    FreeMem(FExtTextOutDistance);
    FExtTextOutDistance := nil;
  end;

  inherited;
end;

procedure TBCEditorTextDrawer.BeginDrawing(AHandle: HDC);
begin
  if FHandle = AHandle then
    Assert(FHandle <> 0)
  else
  begin
    Assert((FHandle = 0) and (AHandle <> 0) and (FDrawingCount = 0));
    FHandle := AHandle;
    FSaveHandle := SaveDC(AHandle);
    SelectObject(AHandle, FCurrentFont);
    Winapi.Windows.SetTextColor(AHandle, ColorToRGB(FColor));
    Winapi.Windows.SetBkColor(AHandle, ColorToRGB(FBackgroundColor));
    //DoSetCharExtra(FCharExtra);
  end;
  Inc(FDrawingCount);
end;

procedure TBCEditorTextDrawer.EndDrawing;
begin
  Assert(FDrawingCount >= 1);
  Dec(FDrawingCount);
  if FDrawingCount <= 0 then
  begin
    if FHandle <> 0 then
      RestoreDC(FHandle, FSaveHandle);
    FSaveHandle := 0;
    FHandle := 0;
    FDrawingCount := 0;
  end;
end;
       (*
function TBCEditorTextDrawer.GetCharWidth: Integer;
begin
  Result := FBaseCharWidth{ + FCharExtra};
end;

function TBCEditorTextDrawer.GetCharHeight: Integer;
begin
  Result := FBaseCharHeight;
end;    *)

procedure TBCEditorTextDrawer.SetBaseFont(AValue: TFont);
begin
  if Assigned(AValue) then
  begin
    FlushCharABCWidthCache;
    FStockBitmap.Canvas.Font.Assign(AValue);
    FStockBitmap.Canvas.Font.Style := [];
    with FFontStock do
    begin
      SetBaseFont(AValue);
      Style := FCalcExtentBaseStyle;
      FCharWidth := CharAdvance;
      FCharHeight := CharHeight;
    end;
    SetStyle(AValue.Style);
  end
  else
    raise EBCEditorTextDrawerException.Create(SBCEditorValueMustBeSpecified);
end;

procedure TBCEditorTextDrawer.SetBaseStyle(const AValue: TFontStyles);
begin
  if FCalcExtentBaseStyle <> AValue then
  begin
    FlushCharABCWidthCache;
    FCalcExtentBaseStyle := AValue;
    with FFontStock do
    begin
      Style := AValue;
      FCharWidth := CharAdvance;
      FCharHeight := CharHeight;
    end;
  end;
end;

procedure TBCEditorTextDrawer.SetStyle(AValue: TFontStyles);
begin
  with FFontStock do
  begin
    SetStyle(AValue);
    Self.FCurrentFont := FontHandle;
  end;
  //AfterStyleSet;
  if FHandle <> 0 then
    SelectObject(FHandle, FCurrentFont);
end;

procedure TBCEditorTextDrawer.FlushCharABCWidthCache;
begin
  FillChar(FCharABCWidthCache, SizeOf(TABC) * Length(FCharABCWidthCache), 0);
end;

{procedure TBCEditorTextDrawer.AfterStyleSet;
begin
  if FHandle <> 0 then
    SelectObject(FHandle, FCurrentFont);
end;}

function TBCEditorTextDrawer.GetCachedABCWidth(AChar: Cardinal; var AABC: TABC): Boolean;
begin
  if AChar > High(FCharABCWidthCache) then
  begin
    Result := GetCharABCWidthsW(FHandle, AChar, AChar, AABC);
    Exit;
  end;
  AABC := FCharABCWidthCache[AChar];
  if (AABC.abcA or Integer(AABC.abcB) or AABC.abcC) = 0 then
  begin
    Result := GetCharABCWidthsW(FHandle, AChar, AChar, AABC);
    if Result then
      FCharABCWidthCache[AChar] := AABC;
  end
  else
    Result := True;
end;

procedure TBCEditorTextDrawer.SetForegroundColor(AValue: TColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    if FHandle <> 0 then
      SetTextColor(FHandle, ColorToRGB(AValue));
  end;
end;

procedure TBCEditorTextDrawer.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    if FHandle <> 0 then
      Winapi.Windows.SetBkColor(FHandle, ColorToRGB(AValue));
  end;
end;
     {
procedure TBCEditorTextDrawer.SetCharExtra(AValue: Integer);
begin
  if FCharExtra <> AValue then
  begin
    FCharExtra := AValue;
    DoSetCharExtra(FCharExtra);
  end;
end;

procedure TBCEditorTextDrawer.DoSetCharExtra(AValue: Integer);
begin
  if FHandle <> 0 then
    SetTextCharacterExtra(FHandle, AValue);
end;        }
     {
procedure TBCEditorTextDrawer.TextOut(X, Y: Integer; AText: PChar; ALength: Integer);
var
  LTempRect: TRect;
begin
  LTempRect := Rect(X, Y, X, Y);

  Winapi.Windows.ExtTextOut(FHandle, X, Y, 0, @LTempRect, AText, ALength, nil);
end;
      }
function TBCEditorTextDrawer.GetCharCount(AChar: PChar): Integer;
var
  LTextSize: TSize;
  LRemainder: Word;
  LResult: Word;
begin
  { Zero width space ($200B), attached characters and combining marks }
  case Word(AChar^) of
    $0000..$0008,$000B,$000E..$001F,$007F..$009F, $200B, $0300..$034F,$0360..$036F,
    $0483..$0486,$0488..$0489,$0591..$05A1,$05A3..$05B9,$05BB..$05BD,$05BF,
    $05C1..$05C2,$05C4,$064B..$0655,$0670,$06D6..$06E4,$06E7..$06E8,
    $06EA..$06ED,$070F,$0711,$0730..$074A,$07A6..$07B0,$0901..$0903,$093C,
    $093E..$094D,$0951..$0954,$0962..$0963,$0981..$0983,$09BC,$09BE..$09C4,
    $09C7..$09C8,$09CB..$09CD,$09D7,$09E2..$09E3,$0A02,$0A3C,$0A3E..$0A42,
    $0A47..$0A48,$0A4B..$0A4D,$0A70..$0A71,$0A81..$0A83,$0ABC,$0ABE..$0AC5,
    $0AC7..$0AC9,$0ACB..$0ACD,$0B01..$0B03,$0B3C,$0B3E..$0B43,$0B47..$0B48,
    $0B4B..$0B4D,$0B56..$0B57,$0B82,$0BBE..$0BC2,$0BC6..$0BC8,$0BCA..$0BCD,
    $0BD7,$0C01..$0C03,$0C3E..$0C44,$0C46..$0C48,$0C4A..$0C4D,$0C55..$0C56,
    $0C82..$0C83,$0CBE..$0CC4,$0CC6..$0CC8,$0CCA..$0CCD,$0CD5..$0CD6,
    $0D02..$0D03,$0D3E..$0D43,$0D46..$0D48,$0D4A..$0D4D,$0D57,$0D82..$0D83,
    $0DCA,$0DCF..$0DD4,$0DD6,$0DD8..$0DDF,$0DF2..$0DF3,$0E31,$0E34..$0E3A,
    $0E47..$0E4E,$0EB1,$0EB4..$0EB9,$0EBB..$0EBC,$0EC8..$0ECD,$0F18..$0F19,
    $0F35,$0F37,$0F39,$0F3E..$0F3F,$0F71..$0F84,$0F86..$0F87,$0F90..$0F97,
    $0F99..$0FBC,$0FC6,$102C..$1032,$1036..$1039,$1056..$1059,$1160..$11A2,
    $11A8..$11F9,$1712..$1714,$1732..$1734,$1752..$1753,$1772..$1773,
    $17B4..$17D3,$180B..$180E,$18A9,$200C..$200F,$202A..$202E,$206A..$206F,
    $20D0..$20EA,$302A..$302F,$3099..$309A,$FB1E,$FE00..$FE0F,$FE20..$FE23,
    $FFF9..$FFFB:
    Result := 0
  else
    begin
      GetTextExtentPoint32(FStockBitmap.Canvas.Handle, AChar, Length(AChar^), LTextSize);
      DivMod(LTextSize.cx, CharWidth, LResult, LRemainder);
      if LRemainder > 0 then
        Inc(LResult);
      Result := LResult;
    end;
  end;
end;

procedure TBCEditorTextDrawer.ExtTextOut(X, Y: Integer; AOptions: Longint; var ARect: TRect; AText: PChar;
  ALength: Integer);
var
  i, LCharWidth: Integer;
//  LLastChar: Cardinal;
//  LRealCharWidth, LNormalCharWidth: Integer;
//  LCharInfo: TABC;
//  LTextMetricA: TTextMetricA;
  LPChar: PChar;
begin
  LCharWidth := CharWidth;

  if ALength > FExtTextOutDistanceLength then
  begin
    FExtTextOutDistanceLength := ALength;
    ReallocMem(FExtTextOutDistance, ALength * SizeOf(Integer));
  end;

  for i := 0 to ALength - 1 do
  begin
    LPChar := @AText[i];
    if Ord(LPChar^) < 128 then
      FExtTextOutDistance[i] := LCharWidth
    else
      FExtTextOutDistance[i] := GetCharCount(LPChar) * LCharWidth;
  end;

  // TODO: Investigate, if this is needed... Windows 10 seems to work without but how about previous versions.
  { avoid clipping the last pixels of text in italic }
 { if ALength > 0 then
  begin
    LLastChar := Ord(AText[ALength - 1]);
    if LLastChar <> 32 then
    begin
      LNormalCharWidth := FExtTextOutDistance[ALength - 1];
      LRealCharWidth := LNormalCharWidth;

      if GetCachedABCWidth(LLastChar, LCharInfo) then
      begin
        LRealCharWidth := LCharInfo.abcA + Integer(LCharInfo.abcB);
        if LCharInfo.abcC >= 0 then
          Inc(LRealCharWidth, LCharInfo.abcC);
      end
      else
      if LLastChar < Ord(High(AnsiChar)) then
      begin
        GetTextMetricsA(FHandle, LTextMetricA);
        LRealCharWidth := LTextMetricA.tmAveCharWidth + LTextMetricA.tmOverhang;
      end;

      if LRealCharWidth > LNormalCharWidth then
        Inc(ARect.Right, LRealCharWidth - LNormalCharWidth);
      FExtTextOutDistance[ALength - 1] := Max(LRealCharWidth, LNormalCharWidth);
    end;
  end;}

  Winapi.Windows.ExtTextOut(FHandle, X, Y, AOptions, @ARect, AText, ALength, Pointer(FExtTextOutDistance));
end;
    {
function TBCEditorTextDrawer.TextExtent(const Text: string): TSize;
begin
  GetTextExtentPoint32(FStockBitmap.Canvas.Handle, PChar(Text), Length(Text), Result);
end; }

initialization

finalization

if Assigned(GFontsInfoManager) then
  GFontsInfoManager.Free;

end.
