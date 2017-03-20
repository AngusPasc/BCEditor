unit BCEditor.Editor.CompletionProposal;

interface {********************************************************************}

uses
  Classes,
  Graphics, Controls, ImgList,
  BCEditor.Types;

type
  TBCEditorCompletionProposal = class(TPersistent)
  type
    TColumns = class;

    TEvent = procedure(Sender: TObject; AColumns: TColumns; const AInput: string;
      var ACanExecute: Boolean) of object;
    TOptions = set of TBCEditorCompletionProposalOption;
    TSelectedEvent = procedure(Sender: TObject; var ASelectedItem: string) of object;
    TValidateEvent = procedure(ASender: TObject; Shift: TShiftState; EndToken: Char) of object;

    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FForeground: TColor;
      FSelectedBackground: TColor;
      FSelectedText: TColor;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write FBackground default clWindow;
      property Foreground: TColor read FForeground write FForeground default clWindowText;
      property SelectedBackground: TColor read FSelectedBackground write FSelectedBackground default clHighlight;
      property SelectedText: TColor read FSelectedText write FSelectedText default clHighlightText;
    end;

    TTrigger = class(TPersistent)
    strict private
      FChars: string;
      FEnabled: Boolean;
      FInterval: Integer;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Chars: string read FChars write FChars;
      property Enabled: Boolean read FEnabled write FEnabled default False;
      property Interval: Integer read FInterval write FInterval default 1000;
    end;

    TItems = class(TCollection)
    type

      TItem = class(TCollectionItem)
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

    strict private
      FOwner: TPersistent;
      function GetItem(AIndex: Integer): TItem;
      procedure SetItem(AIndex: Integer; AValue: TItem);
    protected
      function GetOwner: TPersistent; override;
    public
      constructor Create(AOwner: TPersistent);
      function Add: TItem;
      function FindItemID(AID: Integer): TItem;
      function Insert(AIndex: Integer): TItem;
      property Items[AIndex: Integer]: TItem read GetItem write SetItem; default;
    end;

    TColumns = class(TCollection)
    type

      TColumn = class(TCollectionItem)
      type

        TTitle = class(TPersistent)
        type

          TColors = class(TPersistent)
          strict private
            FBackground: TColor;
            FBottomBorder: TColor;
            FRightBorder: TColor;
          public
            constructor Create;
            procedure Assign(ASource: TPersistent); override;
          published
            property Background: TColor read FBackground write FBackground default clWindow;
            property BottomBorder: TColor read FBottomBorder write FBottomBorder default clBtnFace;
            property RightBorder: TColor read FRightBorder write FRightBorder default clBtnFace;
          end;

        strict private
          FCaption: string;
          FColors: TTitle.TColors;
          FFont: TFont;
          FVisible: Boolean;
          procedure SetFont(const AValue: TFont);
        public
          constructor Create;
          destructor Destroy; override;
          procedure Assign(ASource: TPersistent); override;
        published
          property Caption: string read FCaption write FCaption;
          property Colors: TTitle.TColors read FColors write FColors;
          property Font: TFont read FFont write SetFont;
          property Visible: Boolean read FVisible write FVisible default False;
        end;

      strict private
        FAutoWidth: Boolean;
        FFont: TFont;
        FItems: TItems;
        FTitle: TTitle;
        FVisible: Boolean;
        FWidth: Integer;
        procedure SetFont(const AValue: TFont);
      public
        constructor Create(ACollection: TCollection); override;
        destructor Destroy; override;
        procedure Assign(ASource: TPersistent); override;
      published
        property AutoWidth: Boolean read FAutoWidth write FAutoWidth default True;
        property Font: TFont read FFont write SetFont;
        property Items: TBCEditorCompletionProposal.TItems read FItems write FItems;
        property Title: TBCEditorCompletionProposal.TColumns.TColumn.TTitle read FTitle write FTitle;
        property Visible: Boolean read FVisible write FVisible default True;
        property Width: Integer read FWidth write FWidth default 0;
      end;

    strict private
      FOwner: TPersistent;
      function GetItem(AIndex: Integer): TColumn;
      procedure SetItem(AIndex: Integer; AValue: TColumn);
    protected
      function GetOwner: TPersistent; override;
    public
      constructor Create(AOwner: TPersistent);
      function Add: TColumn;
      function FindItemID(AID: Integer): TColumn;
      function Insert(AIndex: Integer): TColumn;
      property Items[AIndex: Integer]: TColumn read GetItem write SetItem; default;
    end;

  strict private const
    DefaultCloseChars = '()[]. ';
    DefaultOptions = [cpoAutoConstraints, cpoAddHighlighterKeywords, cpoFiltered,
      cpoParseItemsFromText, cpoUseHighlighterColumnFont];
    DefaultSecondaryShortCut = 0;
    DefaultShortCut = 16416; // (Ctrl+Space)
  strict private
    FCloseChars: string;
    FColors: TBCEditorCompletionProposal.TColors;
    FColumns: TBCEditorCompletionProposal.TColumns;
    FCompletionColumnIndex: Integer;
    FConstraints: TSizeConstraints;
    FEnabled: Boolean;
    FImages: TCustomImageList;
    FOptions: TBCEditorCompletionProposal.TOptions;
    FOwner: TComponent;
    FSecondaryShortCut: TShortCut;
    FShortCut: TShortCut;
    FTrigger: TBCEditorCompletionProposal.TTrigger;
    FVisibleLines: Integer;
    FWidth: Integer;
    procedure SetImages(const AValue: TCustomImageList);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure ChangeScale(M, D: Integer);
    procedure SetOption(const AOption: TBCEditorCompletionProposalOption; const AEnabled: Boolean);
  published
    property CloseChars: string read FCloseChars write FCloseChars;
    property Colors: TBCEditorCompletionProposal.TColors read FColors write FColors;
    property Columns: TBCEditorCompletionProposal.TColumns read FColumns write FColumns;
    property CompletionColumnIndex: Integer read FCompletionColumnIndex write FCompletionColumnIndex default 0;
    property Constraints: TSizeConstraints read FConstraints write FConstraints;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TBCEditorCompletionProposal.TOptions read FOptions write FOptions default DefaultOptions;
    property SecondaryShortCut: TShortCut read FSecondaryShortCut write FSecondaryShortCut default DefaultSecondaryShortCut;
    property ShortCut: TShortCut read FShortCut write FShortCut default DefaultShortCut;
    property Trigger: TBCEditorCompletionProposal.TTrigger read FTrigger write FTrigger;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines default 8;
    property Width: Integer read FWidth write FWidth default 260;
  end;

implementation {***************************************************************}

uses
  Menus;

{ TBCEditorCompletionProposal.TColors *****************************************}

constructor TBCEditorCompletionProposal.TColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FForeground := clWindowText;
  FSelectedBackground := clHighlight;
  FSelectedText := clHighlightText;
end;

procedure TBCEditorCompletionProposal.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal.TColors then
  with ASource as TBCEditorCompletionProposal.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.FSelectedBackground := FSelectedBackground;
    Self.FSelectedText := FSelectedText;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCompletionProposal.TTrigger ****************************************}

constructor TBCEditorCompletionProposal.TTrigger.Create;
begin
  inherited;

  FChars := '.';
  FEnabled := False;
  FInterval := 1000;
end;

procedure TBCEditorCompletionProposal.TTrigger.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal.TTrigger then
  with ASource as TBCEditorCompletionProposal.TTrigger do
  begin
    Self.FChars := FChars;
    Self.FEnabled := FEnabled;
    Self.FInterval := FInterval;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCompletionProposal.TItems.TItem ************************************}

constructor TBCEditorCompletionProposal.TItems.TItem.Create(ACollection: TCollection);
begin
  inherited;

  FImageIndex := -1;
end;

procedure TBCEditorCompletionProposal.TItems.TItem.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal.TItems.TItem then
  with ASource as TBCEditorCompletionProposal.TItems.TItem do
  begin
    Self.FImageIndex := FImageIndex;
    Self.FValue := FValue;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCompletionProposal.TItems ******************************************}

constructor TBCEditorCompletionProposal.TItems.Create(AOwner: TPersistent);
begin
  inherited Create(TItem);

  FOwner := AOwner;
end;

function TBCEditorCompletionProposal.TItems.Add: TBCEditorCompletionProposal.TItems.TItem;
begin
  Result := inherited Add as TBCEditorCompletionProposal.TItems.TItem;
end;

function TBCEditorCompletionProposal.TItems.FindItemID(AID: Integer): TBCEditorCompletionProposal.TItems.TItem;
begin
  Result := inherited FindItemID(AID) as TBCEditorCompletionProposal.TItems.TItem;
end;

function TBCEditorCompletionProposal.TItems.GetItem(AIndex: Integer): TBCEditorCompletionProposal.TItems.TItem;
begin
  Result := inherited GetItem(AIndex) as TBCEditorCompletionProposal.TItems.TItem;
end;

function TBCEditorCompletionProposal.TItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCompletionProposal.TItems.Insert(AIndex: Integer): TBCEditorCompletionProposal.TItems.TItem;
begin
  Result := inherited Insert(AIndex) as TBCEditorCompletionProposal.TItems.TItem;
end;

procedure TBCEditorCompletionProposal.TItems.SetItem(AIndex: Integer; AValue: TBCEditorCompletionProposal.TItems.TItem);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TBCEditorCompletionProposal.TColumns.TColumn.TTitle.TColors *****************}

constructor TBCEditorCompletionProposal.TColumns.TColumn.TTitle.TColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FBottomBorder := clBtnFace;
  FRightBorder := clBtnFace;
end;

procedure TBCEditorCompletionProposal.TColumns.TColumn.TTitle.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal.TColumns.TColumn.TTitle.TColors then
  with ASource as TBCEditorCompletionProposal.TColumns.TColumn.TTitle.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBottomBorder := FBottomBorder;
    Self.FRightBorder := FRightBorder;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCompletionProposal.TColumns.TColumn.TTitle *************************}

constructor TBCEditorCompletionProposal.TColumns.TColumn.TTitle.Create;
begin
  inherited;

  FColors := TColors.Create;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FVisible := False;
end;

destructor TBCEditorCompletionProposal.TColumns.TColumn.TTitle.Destroy;
begin
  FColors.Free;
  FFont.Free;

  inherited;
end;

procedure TBCEditorCompletionProposal.TColumns.TColumn.TTitle.Assign(ASource: TPersistent);
begin
  if ASource is TTitle then
  with ASource as TTitle do
  begin
    Self.FCaption := FCaption;
    Self.FColors.Assign(FColors);
    Self.FFont.Assign(FFont);
    Self.FVisible := FVisible;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposal.TColumns.TColumn.TTitle.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCompletionProposal.TColumns.TColumn ********************************}

constructor TBCEditorCompletionProposal.TColumns.TColumn.Create(ACollection: TCollection);
begin
  inherited;

  FAutoWidth := True;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FItems := TItems.Create(Self);
  FTitle := TTitle.Create;
  FVisible := True;
  FWidth := 0;
end;

destructor TBCEditorCompletionProposal.TColumns.TColumn.Destroy;
begin
  FFont.Free;
  FItems.Free;
  FTitle.Free;

  inherited;
end;

procedure TBCEditorCompletionProposal.TColumns.TColumn.Assign(ASource: TPersistent);
begin
  if ASource is TColumn then
  with ASource as TColumn do
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

procedure TBCEditorCompletionProposal.TColumns.TColumn.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCompletionProposal.TColumns ****************************************}

constructor TBCEditorCompletionProposal.TColumns.Create(AOwner: TPersistent);
begin
  inherited Create(TColumn);

  FOwner := AOwner;
end;

function TBCEditorCompletionProposal.TColumns.Add: TColumn;
begin
  Result := inherited Add as TColumn;
end;

function TBCEditorCompletionProposal.TColumns.FindItemID(AID: Integer): TColumn;
begin
  Result := inherited FindItemID(AID) as TColumn;
end;

function TBCEditorCompletionProposal.TColumns.GetItem(AIndex: Integer): TColumn;
begin
  Result := inherited GetItem(AIndex) as TColumn;
end;

function TBCEditorCompletionProposal.TColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorCompletionProposal.TColumns.Insert(AIndex: Integer): TColumn;
begin
  Result := inherited Insert(AIndex) as TColumn;
end;

procedure TBCEditorCompletionProposal.TColumns.SetItem(AIndex: Integer; AValue: TColumn);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TBCEditorCompletionProposal *************************************************}

constructor TBCEditorCompletionProposal.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FCloseChars := DefaultCloseChars;
  FColors := TColors.Create;
  FColumns := TColumns.Create(Self);
  FColumns.Add; { default column }
  FCompletionColumnIndex := 0;
  FEnabled := True;
  FOptions := DefaultOptions;
  FSecondaryShortCut := DefaultSecondaryShortCut;
  FShortCut := DefaultShortCut;
  FTrigger := TTrigger.Create;
  FVisibleLines := 8;
  FWidth := 260;
  FConstraints := TSizeConstraints.Create(nil);
end;

destructor TBCEditorCompletionProposal.Destroy;
begin
  FColors.Free;
  FTrigger.Free;
  FColumns.Free;
  FConstraints.Free;

  inherited;
end;

procedure TBCEditorCompletionProposal.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal then
  with ASource as TBCEditorCompletionProposal do
  begin
    Self.FCloseChars := FCloseChars;
    Self.FColors.Assign(FColors);
    Self.FColumns.Assign(FColumns);
    Self.FEnabled := FEnabled;
    Self.FImages := FImages;
    Self.FOptions := FOptions;
    Self.FSecondaryShortCut := FSecondaryShortCut;
    Self.FShortCut := FShortCut;
    Self.FTrigger.Assign(FTrigger);
    Self.FVisibleLines := FVisibleLines;
    Self.FWidth := FWidth;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposal.ChangeScale(M, D: Integer);
begin
  FWidth := FWidth * M div D;
end;

function TBCEditorCompletionProposal.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TBCEditorCompletionProposal.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
  end;
end;

procedure TBCEditorCompletionProposal.SetOption(const AOption: TBCEditorCompletionProposalOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

end.
