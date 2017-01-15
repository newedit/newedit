unit BCEditor.Editor.CompletionProposal.Columns;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorCompletionProposalColumnTitle = class(TPersistent)
  strict private
    FCaption: string;
    FFont: TFont;
    FVisible: Boolean;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Caption: string read FCaption write FCaption;
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write FVisible default False;
  end;

  TBCEditorCompletionProposalColumnItem = class(TCollectionItem)
  strict private
    FImageIndex: Integer;
    FValue: string;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property Value: string read FValue write FValue;
  end;

  TBCEditorCompletionProposalColumnItems = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(AIndex: Integer): TBCEditorCompletionProposalColumnItem;
    procedure SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposalColumnItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    function Add: TBCEditorCompletionProposalColumnItem;
    function FindItemID(AID: Integer): TBCEditorCompletionProposalColumnItem;
    function Insert(AIndex: Integer): TBCEditorCompletionProposalColumnItem;
    property Items[AIndex: Integer]: TBCEditorCompletionProposalColumnItem read GetItem write SetItem; default;
  end;

  TBCEditorCompletionProposalColumn = class(TCollectionItem)
  strict private
    FAutoWidth: Boolean;
    FFont: TFont;
    FItems: TBCEditorCompletionProposalColumnItems;
    FTitle: TBCEditorCompletionProposalColumnTitle;
    FWidth: Integer;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property AutoWidth: Boolean read FAutoWidth write FAutoWidth default True;
    property Font: TFont read FFont write SetFont;
    property Items: TBCEditorCompletionProposalColumnItems read FItems write FItems;
    property Title: TBCEditorCompletionProposalColumnTitle read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth default 0;
  end;

  TBCEditorCompletionProposalColumns = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(AIndex: Integer): TBCEditorCompletionProposalColumn;
    procedure SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposalColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    function Add: TBCEditorCompletionProposalColumn;
    function FindItemID(AID: Integer): TBCEditorCompletionProposalColumn;
    function Insert(AIndex: Integer): TBCEditorCompletionProposalColumn;
    property Items[AIndex: Integer]: TBCEditorCompletionProposalColumn read GetItem write SetItem; default;
  end;

implementation

{ TBCEditorCompletionProposalColumnTitle }

constructor TBCEditorCompletionProposalColumnTitle.Create;
begin
  inherited;

  FVisible := False;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
end;

destructor TBCEditorCompletionProposalColumnTitle.Destroy;
begin
  FFont.Free;

  inherited;
end;

procedure TBCEditorCompletionProposalColumnTitle.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalColumnTitle then
  with ASource as TBCEditorCompletionProposalColumnTitle do
  begin
    Self.FCaption := FCaption;
    Self.FFont.Assign(FFont);
    Self.FVisible := FVisible;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposalColumnTitle.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCompletionProposalColumnItem }

constructor TBCEditorCompletionProposalColumnItem.Create(ACollection: TCollection);
begin
  inherited;

  FImageIndex := -1;
end;

procedure TBCEditorCompletionProposalColumnItem.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalColumnItem then
  with ASource as TBCEditorCompletionProposalColumnItem do
  begin
    Self.FImageIndex := FImageIndex;
    Self.FValue := FValue;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCompletionProposalColumnItems }

constructor TBCEditorCompletionProposalColumnItems.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

  FOwner := AOwner;
end;

function TBCEditorCompletionProposalColumnItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCompletionProposalColumnItems.GetItem(AIndex: Integer): TBCEditorCompletionProposalColumnItem;
begin
  Result := inherited GetItem(AIndex) as TBCEditorCompletionProposalColumnItem;
end;

procedure TBCEditorCompletionProposalColumnItems.SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposalColumnItem);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBCEditorCompletionProposalColumnItems.Add: TBCEditorCompletionProposalColumnItem;
begin
  Result := inherited Add as TBCEditorCompletionProposalColumnItem;
end;

function TBCEditorCompletionProposalColumnItems.FindItemID(AID: Integer): TBCEditorCompletionProposalColumnItem;
begin
  Result := inherited FindItemID(AID) as TBCEditorCompletionProposalColumnItem;
end;

function TBCEditorCompletionProposalColumnItems.Insert(AIndex: Integer): TBCEditorCompletionProposalColumnItem;
begin
  Result := inherited Insert(AIndex) as TBCEditorCompletionProposalColumnItem;
end;

{ TBCEditorCompletionProposalColumn }

constructor TBCEditorCompletionProposalColumn.Create(ACollection: TCollection);
begin
  inherited;
  FAutoWidth := True;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FItems := TBCEditorCompletionProposalColumnItems.Create(Self, TBCEditorCompletionProposalColumnItem);
  FTitle := TBCEditorCompletionProposalColumnTitle.Create;
  FWidth := 0;
end;

destructor TBCEditorCompletionProposalColumn.Destroy;
begin
  FFont.Free;
  FItems.Free;
  FTitle.Free;

  inherited;
end;

procedure TBCEditorCompletionProposalColumn.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposalColumn then
  with ASource as TBCEditorCompletionProposalColumn do
  begin
    Self.FAutoWidth := FAutoWidth;
    Self.FFont.Assign(FFont);
    Self.FItems.Assign(FItems);
    Self.FTitle.Assign(FTitle);
    Self.FWidth := FWidth;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposalColumn.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCompletionProposalColumns }

constructor TBCEditorCompletionProposalColumns.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

  FOwner := AOwner;
end;

function TBCEditorCompletionProposalColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCompletionProposalColumns.GetItem(AIndex: Integer): TBCEditorCompletionProposalColumn;
begin
  Result := inherited GetItem(AIndex) as TBCEditorCompletionProposalColumn;
end;

procedure TBCEditorCompletionProposalColumns.SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposalColumn);
begin
  inherited SetItem(AIndex, AValue);
end;

function TBCEditorCompletionProposalColumns.Add: TBCEditorCompletionProposalColumn;
begin
  Result := inherited Add as TBCEditorCompletionProposalColumn;
end;

function TBCEditorCompletionProposalColumns.FindItemID(AID: Integer): TBCEditorCompletionProposalColumn;
begin
  Result := inherited FindItemID(AID) as TBCEditorCompletionProposalColumn;
end;

function TBCEditorCompletionProposalColumns.Insert(AIndex: Integer): TBCEditorCompletionProposalColumn;
begin
  Result := inherited Insert(AIndex) as TBCEditorCompletionProposalColumn;
end;

end.
