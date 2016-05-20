unit BCEditor.Editor.Caret.MultiEdit;

interface

uses
  System.Classes, BCEditor.Editor.Caret.MultiEdit.Colors;

type
  TBCEditorCaretMultiEdit = class(TPersistent)
  strict private
    FColors: TBCEditorCaretMultiEditColors;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    procedure DoChange;
    procedure SetColors(AValue: TBCEditorCaretMultiEditColors);
    procedure SetEnabled(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TBCEditorCaretMultiEditColors read FColors write SetColors;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

constructor TBCEditorCaretMultiEdit.Create;
begin
  inherited;

  FColors := TBCEditorCaretMultiEditColors.Create;
  FEnabled := False;
end;

destructor TBCEditorCaretMultiEdit.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorCaretMultiEdit.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaretMultiEdit) then
  with ASource as TBCEditorCaretMultiEdit do
  begin
    Self.FColors.Assign(FColors);
    Self.FEnabled := FEnabled;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaretMultiEdit.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaretMultiEdit.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCaretMultiEdit.SetColors(AValue: TBCEditorCaretMultiEditColors);
begin
  FColors.Assign(AValue);
end;

end.
