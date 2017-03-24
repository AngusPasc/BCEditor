unit BCEditor.Editor.CodeFolding;

interface {********************************************************************}

uses
  Classes, SysUtils, Types,
  Graphics, Controls,
  BCEditor.Types, BCEditor.Consts, BCEditor.Editor.Glyph;

type
  TBCEditorCodeFolding = class(TPersistent)
  type
    TAllRanges = class;
    TSkipRegions = class;

    TOptions = set of TBCEditorCodeFoldingOption;
    TChangeEvent = procedure(Event: TBCEditorCodeFoldingChanges) of object;

    TColors = class(TPersistent)
    strict private
      FActiveLineBackground: TColor;
      FBackground: TColor;
      FCollapsedLine: TColor;
      FFoldingLine: TColor;
      FFoldingLineHighlight: TColor;
      FIndent: TColor;
      FIndentHighlight: TColor;
      FOnChange: TChangeEvent;
      procedure DoChange;
      procedure SetActiveLineBackground(const AValue: TColor);
      procedure SetBackground(const AValue: TColor);
      procedure SetCollapsedLine(const AValue: TColor);
      procedure SetFoldingLine(const AValue: TColor);
      procedure SetFoldingLineHighlight(const AValue: TColor);
      procedure SetIndent(const AValue: TColor);
      procedure SetIndentHighlight(const AValue: TColor);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property ActiveLineBackground: TColor read FActiveLineBackground write SetActiveLineBackground default clActiveLineBackground;
      property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
      property CollapsedLine: TColor read FCollapsedLine write SetCollapsedLine default clLeftMarginFontForeground;
      property FoldingLine: TColor read FFoldingLine write SetFoldingLine default clLeftMarginFontForeground;
      property FoldingLineHighlight: TColor read FFoldingLineHighlight write SetFoldingLineHighlight default clLeftMarginFontForeground;
      property Indent: TColor read FIndent write SetIndent default clIndent;
      property IndentHighlight: TColor read FIndentHighlight write SetIndentHighlight default clIndentHighlight;
      property OnChange: TChangeEvent read FOnChange write FOnChange;
    end;

    TRegion = class(TCollection)
    type

      TItem = class(TCollectionItem)
      strict private
        FBeginWithBreakChar: Boolean;
        FBreakCharFollows: Boolean;
        FBreakIfNotFoundBeforeNextRegion: string;
        FCloseAtNextToken: Boolean;
        FCloseToken: string;
        FCloseTokenBeginningOfLine: Boolean;
        FCloseTokenLength: Integer;
        FNoSubs: Boolean;
        FOpenIsClose: Boolean;
        FOpenToken: string;
        FOpenTokenBeginningOfLine: Boolean;
        FOpenTokenBreaksLine: Boolean;
        FOpenTokenCanBeFollowedBy: string;
        FOpenTokenEnd: string;
        FOpenTokenLength: Integer;
        FParentRegionItem: TBCEditorCodeFolding.TRegion.TItem;
        FSharedClose: Boolean;
        FShowGuideLine: Boolean;
        FSkipIfFoundAfterOpenTokenArray: TBCEditorArrayOfString;
        FSkipIfFoundAfterOpenTokenArrayCount: Integer;
        FTokenEndIsPreviousLine: Boolean;
        procedure SetSkipIfFoundAfterOpenTokenArrayCount(const AValue: Integer);
      public
        constructor Create(ACollection: TCollection); override;
        property BeginWithBreakChar: Boolean read FBeginWithBreakChar write FBeginWithBreakChar;
        property BreakCharFollows: Boolean read FBreakCharFollows write FBreakCharFollows default True;
        property BreakIfNotFoundBeforeNextRegion: string read FBreakIfNotFoundBeforeNextRegion write FBreakIfNotFoundBeforeNextRegion;
        property CloseAtNextToken: Boolean read FCloseAtNextToken write FCloseAtNextToken;
        property CloseToken: string read FCloseToken write FCloseToken;
        property CloseTokenBeginningOfLine: Boolean read FCloseTokenBeginningOfLine write FCloseTokenBeginningOfLine default False;
        property CloseTokenLength: Integer read FCloseTokenLength write FCloseTokenLength;
        property NoSubs: Boolean read FNoSubs write FNoSubs default False;
        property OpenIsClose: Boolean read FOpenIsClose write FOpenIsClose default False;
        property OpenToken: string read FOpenToken write FOpenToken;
        property OpenTokenBeginningOfLine: Boolean read FOpenTokenBeginningOfLine write FOpenTokenBeginningOfLine default False;
        property OpenTokenBreaksLine: Boolean read FOpenTokenBreaksLine write FOpenTokenBreaksLine default False;
        property OpenTokenCanBeFollowedBy: string read FOpenTokenCanBeFollowedBy write FOpenTokenCanBeFollowedBy;
        property OpenTokenEnd: string read FOpenTokenEnd write FOpenTokenEnd;
        property OpenTokenLength: Integer read FOpenTokenLength write FOpenTokenLength;
        property ParentRegionItem: TBCEditorCodeFolding.TRegion.TItem read FParentRegionItem write FParentRegionItem;
        property SharedClose: Boolean read FSharedClose write FSharedClose default False;
        property ShowGuideLine: Boolean read FShowGuideLine write FShowGuideLine default True;
        property SkipIfFoundAfterOpenTokenArray: TBCEditorArrayOfString read FSkipIfFoundAfterOpenTokenArray write FSkipIfFoundAfterOpenTokenArray;
        property SkipIfFoundAfterOpenTokenArrayCount: Integer read FSkipIfFoundAfterOpenTokenArrayCount write SetSkipIfFoundAfterOpenTokenArrayCount;
        property TokenEndIsPreviousLine: Boolean read FTokenEndIsPreviousLine write FTokenEndIsPreviousLine default False;
      end;

    strict private
      FCloseToken: string;
      FEscapeChar: Char;
      FFoldTags: Boolean;
      FOpenToken: string;
      FSkipRegions: TSkipRegions;
      FStringEscapeChar: Char;
      function GetItem(AIndex: Integer): TBCEditorCodeFolding.TRegion.TItem;
    public
      constructor Create(AItemClass: TCollectionItemClass);
      destructor Destroy; override;
      function Add(const AOpenToken: string; const ACloseToken: string): TBCEditorCodeFolding.TRegion.TItem;
      function Contains(const AOpenToken: string; const ACloseToken: string): Boolean;
      property CloseToken: string read FCloseToken write FCloseToken;
      property EscapeChar: Char read FEscapeChar write FEscapeChar default BCEDITOR_NONE_CHAR;
      property FoldTags: Boolean read FFoldTags write FFoldTags default False;
      property Items[AIndex: Integer]: TBCEditorCodeFolding.TRegion.TItem read GetItem; default;
      property OpenToken: string read FOpenToken write FOpenToken;
      property SkipRegions: TBCEditorCodeFolding.TSkipRegions read FSkipRegions;
      property StringEscapeChar: Char read FStringEscapeChar write FStringEscapeChar default BCEDITOR_NONE_CHAR;
    end;

    TRegions = array of TRegion;

    TSkipRegions = class(TCollection)
    type

      TItem = class(TCollectionItem)
      type
        TItemType = (ritUnspecified, ritMultiLineString, ritSingleLineString, ritMultiLineComment, ritSingleLineComment);
      strict private
        FCloseToken: string;
        FOpenToken: string;
        FRegionType: TItemType;
        FSkipEmptyChars: Boolean;
        FSkipIfNextCharIsNot: Char;
      public
        property OpenToken: string read FOpenToken write FOpenToken;
        property CloseToken: string read FCloseToken write FCloseToken;
        property RegionType: TItemType read FRegionType write FRegionType;
        property SkipEmptyChars: Boolean read FSkipEmptyChars write FSkipEmptyChars;
        property SkipIfNextCharIsNot: Char read FSkipIfNextCharIsNot write FSkipIfNextCharIsNot default BCEDITOR_NONE_CHAR;
      end;

    strict private
      function GetSkipRegionItem(AIndex: Integer): TItem;
    public
      function Add(const AOpenToken, ACloseToken: string): TItem;
      function Contains(const AOpenToken, ACloseToken: string): Boolean;
      property SkipRegionItems[AIndex: Integer]: TItem read GetSkipRegionItem; default;
    end;

    TRanges = class(TPersistent)
    type

      TRange = class
      strict private
        FAllCodeFoldingRanges: TAllRanges;
        FBeginLine: Integer;
        FCollapsed: Boolean;
        FCollapsedBy: Integer;
        FCollapseMarkRect: TRect;
        FEndLine: Integer;
        FFoldRangeLevel: Integer;
        FIndentLevel: Integer;
        FIsExtraTokenFound: Boolean;
        FParentCollapsed: Boolean;
        FRegionItem: TBCEditorCodeFolding.TRegion.TItem;
        FSubCodeFoldingRanges: TRanges;
        FUndoListed: Boolean;
      public
        constructor Create;
        destructor Destroy; override;
        function Collapsable: Boolean;
        procedure MoveBy(LineCount: Integer);
        procedure MoveChildren(By: Integer);
        procedure SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
        procedure Widen(LineCount: Integer);
        property AllCodeFoldingRanges: TAllRanges read FAllCodeFoldingRanges write FAllCodeFoldingRanges;
        property BeginLine: Integer read FBeginLine write FBeginLine;
        property Collapsed: Boolean read FCollapsed write FCollapsed default False;
        property CollapsedBy: Integer read FCollapsedBy write FCollapsedBy;
        property CollapseMarkRect: TRect read FCollapseMarkRect write FCollapseMarkRect;
        property EndLine: Integer read FEndLine write FEndLine;
        property FoldRangeLevel: Integer read FFoldRangeLevel write FFoldRangeLevel;
        property IndentLevel: Integer read FIndentLevel write FIndentLevel;
        property IsExtraTokenFound: Boolean read FIsExtraTokenFound write FIsExtraTokenFound default False;
        property ParentCollapsed: Boolean read FParentCollapsed write FParentCollapsed;
        property RegionItem: TBCEditorCodeFolding.TRegion.TItem read FRegionItem write FRegionItem;
        property SubCodeFoldingRanges: TRanges read FSubCodeFoldingRanges;
        property UndoListed: Boolean read FUndoListed write FUndoListed default False;
      end;

    strict private
      FList: TList;
      function GetCount: Integer;
      function GetItem(AIndex: Integer): TRange;
    public
      constructor Create;
      destructor Destroy; override;
      function Add(AAllCodeFoldingRanges: TAllRanges; ABeginLine, AIndentLevel, AFoldRangeLevel: Integer;
        ARegionItem: TBCEditorCodeFolding.TRegion.TItem; AEndLine: Integer = 0): TRange;
      procedure Clear;
      property Count: Integer read GetCount;
      property Items[AIndex: Integer]: TRange read GetItem; default;
    end;

    TAllRanges = class(TRanges)
    strict private
      FList: TList;
      function GetAllCount: Integer;
      function GetItem(AIndex: Integer): TRanges.TRange;
      procedure SetItem(AIndex: Integer; Value: TRanges.TRange);
    public
      constructor Create;
      destructor Destroy; override;
      procedure ClearAll;
      procedure Delete(AIndex: Integer); overload;
      procedure Delete(FoldRange: TRanges.TRange); overload;
      procedure SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TRanges.TRange);
      procedure UpdateFoldRanges;
      property AllCount: Integer read GetAllCount;
      property Items[AIndex: Integer]: TRanges.TRange read GetItem write SetItem; default;
      property List: TList read FList;
    end;

    THint = class(TPersistent)
    type

      TColors = class(TPersistent)
      strict private
        FBackground: TColor;
        FBorder: TColor;
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Background: TColor read FBackground write FBackground default clWindow;
        property Border: TColor read FBorder write FBorder default clBtnFace;
      end;

      TIndicator = class(TPersistent)
      type
        TOptions = set of TBCEditorCodeFoldingHintIndicatorOption;

        TColors = class(TPersistent)
        strict private
          FBackground: TColor;
          FBorder: TColor;
          FMark: TColor;
        public
          constructor Create;
          procedure Assign(ASource: TPersistent); override;
        published
          property Background: TColor read FBackground write FBackground default clLeftMarginBackground;
          property Border: TColor read FBorder write FBorder default clLeftMarginFontForeground;
          property Mark: TColor read FMark write FMark default clLeftMarginFontForeground;
        end;

        TPadding = class(Controls.TPadding)
        protected
          class procedure InitDefaults(Margins: TMargins); override;
        published
          property Left default 0;
          property Top default 1;
          property Right default 0;
          property Bottom default 1;
        end;

      strict private const
        DefaultOptions = [hioShowBorder, hioShowMark];
      strict private
        FColors: TIndicator.TColors;
        FGlyph: TBCEditorGlyph;
        FMarkStyle: TBCEditorCodeFoldingHintIndicatorMarkStyle;
        FOptions: TBCEditorCodeFolding.THint.TIndicator.TOptions;
        FPadding: TPadding;
        FVisible: Boolean;
        FWidth: Integer;
        procedure SetGlyph(const AValue: TBCEditorGlyph);
      public
        constructor Create;
        destructor Destroy; override;
        procedure Assign(ASource: TPersistent); override;
      published
        property Colors: TIndicator.TColors read FColors write FColors;
        property Glyph: TBCEditorGlyph read FGlyph write SetGlyph;
        property MarkStyle: TBCEditorCodeFoldingHintIndicatorMarkStyle read FMarkStyle write FMarkStyle default imsThreeDots;
        property Options: TBCEditorCodeFolding.THint.TIndicator.TOptions read FOptions write FOptions default DefaultOptions;
        property Padding: TPadding read FPadding write FPadding;
        property Visible: Boolean read FVisible write FVisible default True;
        property Width: Integer read FWidth write FWidth default 26;
      end;

    strict private
      FColors: THint.TColors;
      FCursor: TCursor;
      FFont: TFont;
      FIndicator: TIndicator;
      FRowCount: Integer;
      FVisible: Boolean;
      procedure SetFont(const AValue: TFont);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
    published
      property Colors: THint.TColors read FColors write FColors;
      property Cursor: TCursor read FCursor write FCursor default crHelp;
      property Font: TFont read FFont write SetFont;
      property Indicator: TIndicator read FIndicator write FIndicator;
      property RowCount: Integer read FRowCount write FRowCount default 40;
      property Visible: Boolean read FVisible write FVisible default True;
    end;

  strict private const
    DefaultOptions = [cfoAutoPadding, cfoAutoWidth, cfoHighlightIndentGuides,
      cfoHighlightMatchingPair, cfoShowIndentGuides, cfoShowTreeLine, cfoUncollapseByHintClick];
  strict private
    FColors: TColors;
    FDelayInterval: Cardinal;
    FHint: THint;
    FMarkStyle: TBCEditorCodeFoldingMarkStyle;
    FMouseOverHint: Boolean;
    FOnChange: TChangeEvent;
    FOptions: TOptions;
    FPadding: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure SetColors(const AValue: TColors);
    procedure SetHint(AValue: THint);
    procedure SetMarkStyle(const AValue: TBCEditorCodeFoldingMarkStyle);
    procedure SetOnChange(AValue: TChangeEvent);
    procedure SetOptions(AValue: TOptions);
    procedure SetPadding(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function GetWidth: Integer;
    procedure SetOption(const AOption: TBCEditorCodeFoldingOption; const AEnabled: Boolean);
    property MouseOverHint: Boolean read FMouseOverHint write FMouseOverHint;
  published
    property Colors: TColors read FColors write SetColors;
    property DelayInterval: Cardinal read FDelayInterval write FDelayInterval default 300;
    property Hint: THint read FHint write SetHint;
    property MarkStyle: TBCEditorCodeFoldingMarkStyle read FMarkStyle write SetMarkStyle default msSquare;
    property Options: TOptions read FOptions write SetOptions default DefaultOptions;
    property Padding: Integer read FPadding write SetPadding default 2;
    property Visible: Boolean read FVisible write SetVisible default False;
    property Width: Integer read FWidth write SetWidth default 14;
    property OnChange: TChangeEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

uses
  Math,
  BCEditor.Utils;

{ TBCEditorCodeFolding.TColors *************************************************}

constructor TBCEditorCodeFolding.TColors.Create;
begin
  inherited;

  FActiveLineBackground := clActiveLineBackground;
  FCollapsedLine := clLeftMarginFontForeground;
  FBackground := clLeftMarginBackground;
  FFoldingLine := clLeftMarginFontForeground;
  FFoldingLineHighlight := clLeftMarginFontForeground;
  FIndent := clIndent;
  FIndentHighlight := clIndentHighlight;
end;

procedure TBCEditorCodeFolding.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TColors then
  with ASource as TColors do
  begin
    Self.FActiveLineBackground := FActiveLineBackground;
    Self.FCollapsedLine := FCollapsedLine;
    Self.FBackground := FBackground;
    Self.FFoldingLine := FFoldingLine;
    Self.FFoldingLineHighlight := FFoldingLineHighlight;
    Self.FIndentHighlight := FIndentHighlight;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFolding.TColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(fcRefresh);
end;

procedure TBCEditorCodeFolding.TColors.SetActiveLineBackground(const AValue: TColor);
begin
  if FActiveLineBackground <> AValue then
  begin
    FActiveLineBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.TColors.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.TColors.SetCollapsedLine(const AValue: TColor);
begin
  if FCollapsedLine <> AValue then
  begin
    FCollapsedLine := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.TColors.SetFoldingLine(const AValue: TColor);
begin
  if FFoldingLine <> AValue then
  begin
    FFoldingLine := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.TColors.SetFoldingLineHighlight(const AValue: TColor);
begin
  if FFoldingLineHighlight <> AValue then
  begin
    FFoldingLineHighlight := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.TColors.SetIndent(const AValue: TColor);
begin
  if FIndent <> AValue then
  begin
    FIndent := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.TColors.SetIndentHighlight(const AValue: TColor);
begin
  if FIndentHighlight <> AValue then
  begin
    FIndentHighlight := AValue;
    DoChange;
  end;
end;

{ TBCEditorCodeFolding.TRegion.TItem ******************************************}

constructor TBCEditorCodeFolding.TRegion.TItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FSkipIfFoundAfterOpenTokenArrayCount := 0;
  FBreakIfNotFoundBeforeNextRegion := '';
  FCloseTokenBeginningOfLine := False;
  FNoSubs := False;
  FOpenIsClose := False;
  FOpenTokenBeginningOfLine := False;
  FOpenTokenBreaksLine := False;
  FSharedClose := False;
  FBreakCharFollows := True;
end;

procedure TBCEditorCodeFolding.TRegion.TItem.SetSkipIfFoundAfterOpenTokenArrayCount(const AValue: Integer);
begin
  FSkipIfFoundAfterOpenTokenArrayCount := AValue;
  SetLength(FSkipIfFoundAfterOpenTokenArray, AValue);
end;

{ TBCEditorCodeFolding.TRegion ************************************************}

constructor TBCEditorCodeFolding.TRegion.Create(AItemClass: TCollectionItemClass);
begin
  inherited Create(AItemClass);

  FSkipRegions := TBCEditorCodeFolding.TSkipRegions.Create(TSkipRegions.TItem);
  FEscapeChar := BCEDITOR_NONE_CHAR;
  FStringEscapeChar := BCEDITOR_NONE_CHAR;
  FFoldTags := False;
end;

destructor TBCEditorCodeFolding.TRegion.Destroy;
begin
  FSkipRegions.Free;

  inherited;
end;

function TBCEditorCodeFolding.TRegion.Add(const AOpenToken: string; const ACloseToken: string): TItem;
begin
  Result := TItem(inherited Add);
  with Result do
  begin
    OpenToken := AOpenToken;
    OpenTokenLength := Length(AOpenToken);
    CloseToken := ACloseToken;
    CloseTokenLength := Length(ACloseToken);
  end;
end;

function TBCEditorCodeFolding.TRegion.Contains(const AOpenToken: string; const ACloseToken: string): Boolean;
var
  LIndex: Integer;
  LItem: TItem;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin
    LItem := Items[LIndex];
    if (LItem.OpenToken = AOpenToken) and (LItem.CloseToken = ACloseToken) then
      Exit(True);
  end;
end;

function TBCEditorCodeFolding.TRegion.GetItem(AIndex: Integer): TItem;
begin
  Result := TItem(inherited Items[AIndex]);
end;

{ TBCEditorCodeFolding.TSkipRegions *******************************************}

function TBCEditorCodeFolding.TSkipRegions.Add(const AOpenToken, ACloseToken: string): TItem;
begin
  Result := TItem(inherited Add);
  with Result do
  begin
    OpenToken := AOpenToken;
    CloseToken := ACloseToken;
  end;
end;

function TBCEditorCodeFolding.TSkipRegions.Contains(const AOpenToken, ACloseToken: string): Boolean;
var
  LIndex: Integer;
  LSkipRegion: TItem;
begin
  Result := False;
  for LIndex := 0 to Count - 1 do
  begin
    LSkipRegion := SkipRegionItems[LIndex];
    if (LSkipRegion.OpenToken = AOpenToken) and (LSkipRegion.CloseToken = ACloseToken) then
      Exit(True);
  end;
end;

function TBCEditorCodeFolding.TSkipRegions.GetSkipRegionItem(AIndex: Integer): TItem;
begin
  Result := TItem(inherited Items[AIndex]);
end;

{ TBCEditorCodeFolding.TRanges.TRange *****************************************}

function TBCEditorCodeFolding.TRanges.TRange.Collapsable: Boolean;
begin
  Result := (FBeginLine < FEndLine) or RegionItem.TokenEndIsPreviousLine and (FBeginLine = FEndLine);
end;

constructor TBCEditorCodeFolding.TRanges.TRange.Create;
begin
  inherited;

  FSubCodeFoldingRanges := TRanges.Create;
  FCollapsed := False;
  FCollapsedBy := -1;
  FIsExtraTokenFound := False;
  FUndoListed := False;
end;

destructor TBCEditorCodeFolding.TRanges.TRange.Destroy;
begin;
  FSubCodeFoldingRanges.Clear;
  FSubCodeFoldingRanges.Free;
  FSubCodeFoldingRanges := nil;

  inherited;
end;

procedure TBCEditorCodeFolding.TRanges.TRange.MoveBy(LineCount: Integer);
begin
  Inc(FBeginLine, LineCount);
  Inc(FEndLine, LineCount);
end;

procedure TBCEditorCodeFolding.TRanges.TRange.MoveChildren(By: Integer);
var
  LCodeFoldingRange: TRange;
  LIndex: Integer;
begin
  for LIndex := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[LIndex];
    if Assigned(LCodeFoldingRange) then
    begin
      LCodeFoldingRange.MoveChildren(By);

      with FAllCodeFoldingRanges.List do
      if LCodeFoldingRange.FParentCollapsed then
        Move(IndexOf(LCodeFoldingRange), IndexOf(LCodeFoldingRange) + By);
    end;
  end;
end;

procedure TBCEditorCodeFolding.TRanges.TRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
var
  LCodeFoldingRange: TRange;
  LIndex: Integer;
begin
  if Assigned(FSubCodeFoldingRanges) then
  for LIndex := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[LIndex];
    LCodeFoldingRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed, ACollapsedBy);

    if (LCodeFoldingRange.FCollapsedBy = -1) or (LCodeFoldingRange.FCollapsedBy = ACollapsedBy) then
    begin
      LCodeFoldingRange.FParentCollapsed := AParentCollapsed;

      if not AParentCollapsed then
        LCodeFoldingRange.FCollapsedBy := -1
      else
        LCodeFoldingRange.FCollapsedBy := ACollapsedBy;
    end;
  end;
end;

procedure TBCEditorCodeFolding.TRanges.TRange.Widen(LineCount: Integer);
begin
  Inc(FEndLine, LineCount);
end;

{ TBCEditorCodeFolding.TRanges ************************************************}

constructor TBCEditorCodeFolding.TRanges.Create;
begin
  inherited;

  FList := TList.Create;
end;

destructor TBCEditorCodeFolding.TRanges.Destroy;
begin
  FList.Clear;
  FList.Free;
  FList := nil;

  inherited;
end;

function TBCEditorCodeFolding.TRanges.Add(AAllCodeFoldingRanges: TAllRanges; ABeginLine, AIndentLevel, AFoldRangeLevel: Integer;
  ARegionItem: TRegion.TItem; AEndLine: Integer): TRange;
begin
  Result := TRange.Create;
  with Result do
  begin
    BeginLine := ABeginLine;
    EndLine := AEndLine;
    IndentLevel := AIndentLevel;
    FoldRangeLevel := AFoldRangeLevel;
    AllCodeFoldingRanges := AAllCodeFoldingRanges;
    RegionItem := ARegionItem;
  end;
  FList.Add(Result);
  AAllCodeFoldingRanges.List.Add(Result);
end;

procedure TBCEditorCodeFolding.TRanges.Clear;
begin
  FList.Clear;
end;

function TBCEditorCodeFolding.TRanges.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBCEditorCodeFolding.TRanges.GetItem(AIndex: Integer): TRange;
begin
  Result := FList[AIndex];
end;

{ TBCEditorCodeFolding.TAllRanges *********************************************}

constructor TBCEditorCodeFolding.TAllRanges.Create;
begin
  inherited;

  FList := TList.Create;
end;

destructor TBCEditorCodeFolding.TAllRanges.Destroy;
begin
  FreeList(FList);

  inherited;
end;

procedure TBCEditorCodeFolding.TAllRanges.ClearAll;
begin
  Clear;
  ClearList(FList);
end;

procedure TBCEditorCodeFolding.TAllRanges.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

procedure TBCEditorCodeFolding.TAllRanges.Delete(FoldRange: TRanges.TRange);
var
  LIndex: Integer;
begin
  for LIndex := 0 to FList.Count - 1 do
  if FList[LIndex] = FoldRange then
  begin
    TRanges.TRange(FList[LIndex]).Free;
    FList[LIndex] := nil;
    FList.Delete(LIndex);
    Break;
  end;
end;

function TBCEditorCodeFolding.TAllRanges.GetAllCount: Integer;
begin
  Result := FList.Count;
end;

function TBCEditorCodeFolding.TAllRanges.GetItem(AIndex: Integer): TRanges.TRange;
begin
  if Cardinal(AIndex) < Cardinal(FList.Count) then
    Result := FList.List[AIndex]
  else
    Result := nil;
end;

procedure TBCEditorCodeFolding.TAllRanges.SetItem(AIndex: Integer; Value: TRanges.TRange);
begin
  FList[AIndex] := Value;
end;

procedure TBCEditorCodeFolding.TAllRanges.SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TRanges.TRange);
var
  LFoldRange: TRanges.TRange;
  LIndex: Integer;
begin
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if LFoldRange = AFoldRange then
      Continue;
    if LFoldRange.BeginLine > AFoldRange.EndLine then
      Break;
    if (LFoldRange.EndLine > AFoldRange.EndLine) and (LFoldRange.EndLine <> AFoldRange.EndLine) then
      LFoldRange.ParentCollapsed := True;
  end;
end;

procedure TBCEditorCodeFolding.TAllRanges.UpdateFoldRanges;
var
  LFoldRange: TRanges.TRange;
  LIndex: Integer;
begin
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if Assigned(LFoldRange) then
      LFoldRange.ParentCollapsed := False;
  end;
  for LIndex := 0 to AllCount - 1 do
  begin
    LFoldRange := GetItem(LIndex);
    if Assigned(LFoldRange) and not LFoldRange.ParentCollapsed then
      SetParentCollapsedOfSubCodeFoldingRanges(LFoldRange);
  end;
end;

{ TBCEditorCodeFolding.THint.TColors ******************************************}

constructor TBCEditorCodeFolding.THint.TColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FBorder := clBtnFace;
end;

procedure TBCEditorCodeFolding.THint.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TColors then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBorder := FBorder;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCodeFolding.THint.TIndicator.TColors *******************************}

constructor TBCEditorCodeFolding.THint.TIndicator.TColors.Create;
begin
  inherited;

  FBackground := clLeftMarginBackground;
  FBorder := clLeftMarginFontForeground;
  FMark := clLeftMarginFontForeground;
end;

procedure TBCEditorCodeFolding.THint.TIndicator.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TColors then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBorder := FBorder;
    Self.FMark := FMark;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCodeFolding.THint.TIndicator.TPadding ******************************}

class procedure TBCEditorCodeFolding.THint.TIndicator.TPadding.InitDefaults(Margins: TMargins);
begin
  with Margins do
  begin
    Left := 0;
    Right := 0;
    Top := 1;
    Bottom := 1;
  end;
end;

{ TBCEditorCodeFolding.THint.TIndicator ***************************************}

constructor TBCEditorCodeFolding.THint.TIndicator.Create;
begin
  inherited;

  FColors := TIndicator.TColors.Create;
  FGlyph := TBCEditorGlyph.Create;
  FPadding := TPadding.Create(nil);
  FGlyph.Visible := False;
  FMarkStyle := imsThreeDots;
  FVisible := True;
  FOptions := DefaultOptions;
  FWidth := 26;
end;

destructor TBCEditorCodeFolding.THint.TIndicator.Destroy;
begin
  FColors.Free;
  FGlyph.Free;
  FPadding.Free;

  inherited;
end;

procedure TBCEditorCodeFolding.THint.TIndicator.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TIndicator) then
  with ASource as TIndicator do
  begin
    Self.FVisible := FVisible;
    Self.FMarkStyle := FMarkStyle;
    Self.FWidth := FWidth;
    Self.FColors.Assign(FColors);
    Self.FGlyph.Assign(FGlyph);
    Self.FPadding.Assign(FPadding);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFolding.THint.TIndicator.SetGlyph(const AValue: TBCEditorGlyph);
begin
  FGlyph.Assign(AValue);
end;

{ TBCEditorCodeFolding.THint **************************************************}

constructor TBCEditorCodeFolding.THint.Create;
begin
  inherited;

  FColors := THint.TColors.Create;
  FIndicator := TIndicator.Create;
  FCursor := crHelp;
  FRowCount := 40;
  FVisible := True;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
end;

destructor TBCEditorCodeFolding.THint.Destroy;
begin
  FColors.Free;
  FIndicator.Free;
  FFont.Free;

  inherited;
end;

procedure TBCEditorCodeFolding.THint.Assign(ASource: TPersistent);
begin
  if ASource is THint then
  with ASource as THint do
  begin
    Self.FColors.Assign(FColors);
    Self.FIndicator.Assign(FIndicator);
    Self.FCursor := FCursor;
    Self.FFont.Assign(FFont);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFolding.THint.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorCodeFolding ********************************************************}

constructor TBCEditorCodeFolding.Create;
begin
  inherited;

  FVisible := False;
  FOptions := DefaultOptions;
  FMarkStyle := msSquare;
  FColors := TColors.Create;
  FHint := THint.Create;
  FPadding := 2;
  FWidth := 14;
  FDelayInterval := 300;

  FMouseOverHint := False;
end;

destructor TBCEditorCodeFolding.Destroy;
begin
  FColors.Free;
  FHint.Free;

  inherited;
end;

procedure TBCEditorCodeFolding.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCodeFolding then
  with ASource as TBCEditorCodeFolding do
  begin
    Self.FVisible := FVisible;
    Self.FOptions := FOptions;
    Self.FColors.Assign(FColors);
    Self.FHint.Assign(FHint);
    Self.FWidth := FWidth;
    if Assigned(Self.OnChange) then
      Self.OnChange(fcRescan);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCodeFolding.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(fcRefresh);
end;

function TBCEditorCodeFolding.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorCodeFolding.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorCodeFolding.SetHint(AValue: THint);
begin
  FHint.Assign(AValue);
end;

procedure TBCEditorCodeFolding.SetMarkStyle(const AValue: TBCEditorCodeFoldingMarkStyle);
begin
  if FMarkStyle <> AValue then
  begin
    FMarkStyle := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.SetOnChange(AValue: TChangeEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := AValue;
end;

procedure TBCEditorCodeFolding.SetOption(const AOption: TBCEditorCodeFoldingOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorCodeFolding.SetOptions(AValue: TOptions);
var
  LRescan: Boolean;
begin
  LRescan := not (cfoFoldMultilineComments in FOptions) and (cfoFoldMultilineComments in AValue) or
    (cfoFoldMultilineComments in FOptions) and not (cfoFoldMultilineComments in AValue);

  FOptions := AValue;
  if LRescan then
    FOnChange(fcRescan)
  else
    DoChange;
end;

procedure TBCEditorCodeFolding.SetPadding(const AValue: Integer);
begin
  if FPadding <> AValue then
  begin
    FPadding := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    if Assigned(FOnChange) then
      FOnChange(fcEnabled);
  end;
end;

procedure TBCEditorCodeFolding.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange;
  end;
end;

end.
