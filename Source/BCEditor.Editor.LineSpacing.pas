unit BCEditor.Editor.LineSpacing;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types;

type
  TBCEditorLineSpacing = class(TPersistent)
  strict private
    FOnChange: TNotifyEvent;
    FSpacing: Integer;
    procedure DoChange;
    procedure SetSpacing(const AValue: Integer);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Spacing: Integer read FSpacing write SetSpacing default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorLineSpacing }

procedure TBCEditorLineSpacing.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLineSpacing) then
  with ASource as TBCEditorLineSpacing do
  begin
    Self.FSpacing := Spacing;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

constructor TBCEditorLineSpacing.Create;
begin
  inherited;

  FSpacing := 0;
end;

procedure TBCEditorLineSpacing.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLineSpacing.SetSpacing(const AValue: Integer);
begin
  if FSpacing <> AValue then
  begin
    FSpacing := AValue;
    DoChange;
  end;
end;

end.

