unit BCEditor.Editor.Search;

interface {********************************************************************}

uses
  Classes,
  Controls, Graphics,
  BCEditor.Types, BCEditor.Consts;

type
  TBCEditorSearch = class(TPersistent)
  type
    TChangeEvent = procedure(Event: TBCEditorSearchChanges) of object;
    PItem = ^TItem;
    TItem = record
      BeginTextPosition: TBCEditorTextPosition;
      EndTextPosition: TBCEditorTextPosition;
    end;
    TOptions = set of TBCEditorSearchOption;

    THighlighter = class(TPersistent)
    type

      TColors = class(TPersistent)
      strict private
        FBackground: TColor;
        FBorder: TColor;
        FForeground: TColor;
        FOnChange: TChangeEvent;
        procedure DoChange;
        procedure SetBackground(const AValue: TColor);
        procedure SetBorder(const AValue: TColor);
        procedure SetForeground(const AValue: TColor);
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Background: TColor read FBackground write SetBackground default clSearchHighlighter;
        property Border: TColor read FBorder write SetBorder default clNone;
        property Foreground: TColor read FForeground write SetForeground default clWindowText;
        property OnChange: TChangeEvent read FOnChange write FOnChange;
      end;

    strict private
      FColors: TColors;
      FOnChange: TChangeEvent;
      procedure DoChange;
      procedure SetColors(const AValue: TColors);
      procedure SetOnChange(AValue: TChangeEvent);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
    published
      property Colors: TColors read FColors write SetColors;
      property OnChange: TChangeEvent read FOnChange write SetOnChange;
    end;

    TMap = class(TPersistent)
    type
      TOption = (
        moShowActiveLine
      );
      TOptions = set of TBCEditorSearch.TMap.TOption;

      TColors = class(TPersistent)
      strict private
        FActiveLine: TColor;
        FBackground: TColor;
        FForeground: TColor;
        FOnChange: TChangeEvent;
        procedure SetActiveLine(AValue: TColor);
        procedure SetBackground(AValue: TColor);
        procedure SetForeground(AValue: TColor);
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property ActiveLine: TColor read FActiveLine write SetActiveLine default clSearchMapActiveLine;
        property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
        property Foreground: TColor read FForeground write SetForeground default clSearchHighlighter;
        property OnChange: TChangeEvent read FOnChange write FOnChange;
      end;

    strict private const
      DefaultOptions = [moShowActiveLine];
    strict private
      FAlign: TBCEditorSearchMapAlign;
      FColors: TColors;
      FCursor: TCursor;
      FOnChange: TChangeEvent;
      FOptions: TBCEditorSearch.TMap.TOptions;
      FVisible: Boolean;
      FWidth: Integer;
      procedure DoChange;
      procedure SetAlign(const AValue: TBCEditorSearchMapAlign);
      procedure SetColors(const AValue: TColors);
      procedure SetOnChange(AValue: TChangeEvent);
      procedure SetOptions(const AValue: TBCEditorSearch.TMap.TOptions);
      procedure SetVisible(AValue: Boolean);
      procedure SetWidth(AValue: Integer);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
      function GetWidth: Integer;
    published
      property Align: TBCEditorSearchMapAlign read FAlign write SetAlign default saRight;
      property Colors: TColors read FColors write SetColors;
      property Cursor: TCursor read FCursor write FCursor default crArrow;
      property Options: TBCEditorSearch.TMap.TOptions read FOptions write SetOptions default DefaultOptions;
      property Visible: Boolean read FVisible write SetVisible default False;
      property Width: Integer read FWidth write SetWidth default 5;
      property OnChange: TChangeEvent read FOnChange write SetOnChange;
    end;

    TInSelection = class(TPersistent)
    strict private
      FActive: Boolean;
      FBackground: TColor;
      FOnChange: TChangeEvent;
      FSelectionBeginPosition: TBCEditorTextPosition;
      FSelectionEndPosition: TBCEditorTextPosition;
      procedure DoChange;
      procedure SetActive(AValue: Boolean);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
      property SelectionBeginPosition: TBCEditorTextPosition read FSelectionBeginPosition write FSelectionBeginPosition;
      property SelectionEndPosition: TBCEditorTextPosition read FSelectionEndPosition write FSelectionEndPosition;
    published
      property Active: Boolean read FActive write SetActive default False;
      property Background: TColor read FBackground write FBackground default clSearchInSelectionBackground;
      property OnChange: TChangeEvent read FOnChange write FOnChange;
    end;

  strict private const
    DefaultOptions = [soHighlightResults, soSearchOnTyping, soBeepIfStringNotFound, soShowSearchMatchNotFound];
  strict private
    FEnabled: Boolean;
    FEngine: TBCEditorSearchEngine;
    FHighlighter: THighlighter;
    FInSelection: TInSelection;
    FLines: TList;
    FMap: TMap;
    FOnChange: TChangeEvent;
    FOptions: TOptions;
    FSearchText: string;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetEngine(const AValue: TBCEditorSearchEngine);
    procedure SetHighlighter(const AValue: THighlighter);
    procedure SetInSelection(const AValue: TInSelection);
    procedure SetMap(const AValue: TMap);
    procedure SetOnChange(const AValue: TChangeEvent);
    procedure SetSearchText(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure ClearLines;
    function GetNextSearchItemIndex(const ATextPosition: TBCEditorTextPosition): Integer;
    function GetPreviousSearchItemIndex(const ATextPosition: TBCEditorTextPosition): Integer;
    procedure SetOption(const AOption: TBCEditorSearchOption; const AEnabled: Boolean);
    property Visible: Boolean read FVisible write FVisible;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Engine: TBCEditorSearchEngine read FEngine write SetEngine default seNormal;
    property Highlighter: THighlighter read FHighlighter write SetHighlighter;
    property InSelection: TInSelection read FInSelection write SetInSelection;
    property Lines: TList read FLines write FLines;
    property Map: TMap read FMap write SetMap;
    property Options: TOptions read FOptions write FOptions default DefaultOptions;
    property SearchText: string read FSearchText write SetSearchText;
    property OnChange: TChangeEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

uses
  Math;

{ TBCEditorSearch.THighlighter.TColors ****************************************}

constructor TBCEditorSearch.THighlighter.TColors.Create;
begin
  inherited;

  FBackground := clSearchHighlighter;
  FBorder := clNone;
  FForeground := clWindowText;
end;

procedure TBCEditorSearch.THighlighter.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TColors then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBorder := FBorder;
    Self.FForeground := FForeground;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.THighlighter.TColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scRefresh);
end;

procedure TBCEditorSearch.THighlighter.TColors.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.THighlighter.TColors.SetBorder(const AValue: TColor);
begin
  if FBorder <> AValue then
  begin
    FBorder := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.THighlighter.TColors.SetForeground(const AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    DoChange;
  end;
end;

{ TBCEditorSearch.THighlighter **************************************************}

constructor TBCEditorSearch.THighlighter.Create;
begin
  inherited;

  FColors := TColors.Create;
end;

destructor TBCEditorSearch.THighlighter.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorSearch.THighlighter.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is THighlighter) then
  with ASource as THighlighter do
  begin
    Self.FColors.Assign(Colors);
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.THighlighter.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scRefresh);
end;

procedure TBCEditorSearch.THighlighter.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorSearch.THighlighter.SetOnChange(AValue: TChangeEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := FOnChange;
end;

{ TBCEditorSearch.TMap.TColors ************************************************}

constructor TBCEditorSearch.TMap.TColors.Create;
begin
  inherited;

  FActiveLine := clSearchMapActiveLine;
  FBackground := clLeftMarginBackground;
  FForeground := clSearchHighlighter;
end;

procedure TBCEditorSearch.TMap.TColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TColors) then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.FActiveLine := FActiveLine;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(scRefresh);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.TMap.TColors.SetActiveLine(AValue: TColor);
begin
  if FActiveLine <> AValue then
  begin
    FActiveLine := AValue;
    if Assigned(FOnChange) then
      FOnChange(scRefresh);
  end;
end;

procedure TBCEditorSearch.TMap.TColors.SetBackground(AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    if Assigned(FOnChange) then
      FOnChange(scRefresh);
  end;
end;

procedure TBCEditorSearch.TMap.TColors.SetForeground(AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    if Assigned(FOnChange) then
      FOnChange(scRefresh);
  end;
end;

{ TBCEditorSearch.TMap ********************************************************}

constructor TBCEditorSearch.TMap.Create;
begin
  inherited;

  FAlign := saRight;
  FColors := TColors.Create;
  FOptions := [moShowActiveLine];
  FVisible := False;
  FWidth := 5;
  FCursor := crArrow;
end;

destructor TBCEditorSearch.TMap.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorSearch.TMap.Assign(ASource: TPersistent);
begin
  if ASource is TMap then
  with ASource as TMap do
  begin
    Self.FAlign := FAlign;
    Self.FVisible := FVisible;
    Self.FOptions := Options;
    Self.FWidth := FWidth;
    Self.FColors.Assign(FColors);
    Self.FCursor := FCursor;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.TMap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scSearch);
end;

function TBCEditorSearch.TMap.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorSearch.TMap.SetAlign(const AValue: TBCEditorSearchMapAlign);
begin
  if FAlign <> AValue then
  begin
    FAlign := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.TMap.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorSearch.TMap.SetOnChange(AValue: TChangeEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := FOnChange;
end;

procedure TBCEditorSearch.TMap.SetOptions(const AValue: TBCEditorSearch.TMap.TOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.TMap.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

procedure TBCEditorSearch.TMap.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
    FWidth := AValue;
  DoChange;
end;

{ TBCEditorSearch.TInSelection ************************************************}

constructor TBCEditorSearch.TInSelection.Create;
begin
  inherited;

  FActive := False;
  FBackground := clSearchInSelectionBackground;
end;

procedure TBCEditorSearch.TInSelection.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scInSelectionActive);
end;

procedure TBCEditorSearch.TInSelection.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TInSelection) then
  with ASource as TInSelection do
  begin
    Self.FActive := FActive;
    Self.FBackground := FBackground;
    Self.FSelectionBeginPosition := FSelectionBeginPosition;
    Self.FSelectionEndPosition := FSelectionEndPosition;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.TInSelection.SetActive(AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
    DoChange;
  end;
end;

{ TBCEditorSearch *************************************************************}

constructor TBCEditorSearch.Create;
begin
  inherited;

  FSearchText := '';
  FEngine := seNormal;
  FMap := TBCEditorSearch.TMap.Create;
  FLines := TList.Create;
  FHighlighter := THighlighter.Create;
  FInSelection := TInSelection.Create;
  FOptions := DefaultOptions;
  FEnabled := True;
end;

destructor TBCEditorSearch.Destroy;
begin
  FMap.Free;
  FHighlighter.Free;
  FInSelection.Free;
  ClearLines;
  FLines.Free;
  inherited;
end;

procedure TBCEditorSearch.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSearch) then
  with ASource as TBCEditorSearch do
  begin
    Self.FEnabled := FEnabled;
    Self.FSearchText := FSearchText;
    Self.FEngine := FEngine;
    Self.FOptions := FOptions;
    Self.FMap.Assign(FMap);
    Self.FHighlighter.Assign(FHighlighter);
    Self.FInSelection.Assign(FInSelection);
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSearch.ClearLines;
var
  LIndex: Integer;
begin
  for LIndex := FLines.Count - 1 downto 0 do
    Dispose(PItem(FLines.Items[LIndex]));
  FLines.Clear;
end;

procedure TBCEditorSearch.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scRefresh);
end;

function TBCEditorSearch.GetNextSearchItemIndex(const ATextPosition: TBCEditorTextPosition): Integer;
var
  LHigh: Integer;
  LLow: Integer;
  LMiddle: Integer;
  LSearchItem: PItem;

  function IsTextPositionBetweenSearchItems: Boolean;
  var
    LPreviousSearchItem: PItem;
  begin
    LPreviousSearchItem := PItem(FLines.Items[LMiddle - 1]);

    Result :=
      ( (LPreviousSearchItem^.BeginTextPosition.Line < ATextPosition.Line) or
        (LPreviousSearchItem^.BeginTextPosition.Line = ATextPosition.Line) and (LPreviousSearchItem^.BeginTextPosition.Char < ATextPosition.Char) )
      and
      ( (LSearchItem^.BeginTextPosition.Line > ATextPosition.Line) or
        (LSearchItem^.BeginTextPosition.Line = ATextPosition.Line) and (LSearchItem^.BeginTextPosition.Char >= ATextPosition.Char) );
  end;

  function IsSearchItemGreaterThanTextPosition: Boolean;
  begin
    Result := (LSearchItem^.BeginTextPosition.Line > ATextPosition.Line) or
      (LSearchItem^.BeginTextPosition.Line = ATextPosition.Line) and (LSearchItem^.BeginTextPosition.Char >= ATextPosition.Char)
  end;

  function IsSearchItemLowerThanTextPosition: Boolean;
  begin
    Result := (LSearchItem^.BeginTextPosition.Line < ATextPosition.Line) or
      (LSearchItem^.BeginTextPosition.Line = ATextPosition.Line) and (LSearchItem^.BeginTextPosition.Char < ATextPosition.Char)
  end;

begin
  Result := -1;

  if FLines.Count = 0 then
    Exit;

  LSearchItem := PItem(FLines.Items[0]);
  if IsSearchItemGreaterThanTextPosition then
    Exit(0);

  LHigh := FLines.Count - 1;

  LSearchItem := PItem(FLines.Items[LHigh]);
  if IsSearchItemLowerThanTextPosition then
    Exit;

  LLow := 1;

  while LLow <= LHigh do
  begin
    LMiddle := (LLow + LHigh) div 2;

    LSearchItem := PItem(FLines.Items[LMiddle]);

    if IsTextPositionBetweenSearchItems then
      Exit(LMiddle)
    else
    if IsSearchItemGreaterThanTextPosition then
      LHigh := LMiddle - 1
    else
    if IsSearchItemLowerThanTextPosition then
      LLow := LMiddle + 1
  end;
end;

function TBCEditorSearch.GetPreviousSearchItemIndex(const ATextPosition: TBCEditorTextPosition): Integer;
var
  LHigh: Integer;
  LLow: Integer;
  LMiddle: Integer;
  LSearchItem: PItem;

  function IsTextPositionBetweenSearchItems: Boolean;
  var
    LNextSearchItem: PItem;
  begin
    LNextSearchItem := PItem(FLines.Items[LMiddle + 1]);

    Result :=
      ( (LSearchItem^.EndTextPosition.Line < ATextPosition.Line) or
        (LSearchItem^.EndTextPosition.Line = ATextPosition.Line) and (LSearchItem^.EndTextPosition.Char <= ATextPosition.Char) )
      and
      ( (LNextSearchItem^.EndTextPosition.Line > ATextPosition.Line) or
        (LNextSearchItem^.EndTextPosition.Line = ATextPosition.Line) and (LNextSearchItem^.EndTextPosition.Char > ATextPosition.Char))
  end;

  function IsSearchItemGreaterThanTextPosition: Boolean;
  begin
    Result := (LSearchItem^.EndTextPosition.Line > ATextPosition.Line) or
      (LSearchItem^.EndTextPosition.Line = ATextPosition.Line) and (LSearchItem^.EndTextPosition.Char > ATextPosition.Char)
  end;

  function IsSearchItemLowerThanTextPosition: Boolean;
  begin
    Result := (LSearchItem^.EndTextPosition.Line < ATextPosition.Line) or
      (LSearchItem^.EndTextPosition.Line = ATextPosition.Line) and (LSearchItem^.EndTextPosition.Char <= ATextPosition.Char)
  end;

begin
  Result := -1;

  if FLines.Count = 0 then
    Exit;

  LSearchItem := PItem(FLines.Items[0]);
  if IsSearchItemGreaterThanTextPosition then
    Exit;

  LHigh := FLines.Count - 1;

  LSearchItem := PItem(FLines.Items[LHigh]);
  if IsSearchItemLowerThanTextPosition then
    Exit(LHigh);

  LLow := 0;
  Dec(LHigh);

  while LLow <= LHigh do
  begin
    LMiddle := (LLow + LHigh) div 2;

    LSearchItem := PItem(FLines.Items[LMiddle]);

    if IsTextPositionBetweenSearchItems then
      Exit(LMiddle)
    else
    if IsSearchItemGreaterThanTextPosition then
      LHigh := LMiddle - 1
    else
    if IsSearchItemLowerThanTextPosition then
      LLow := LMiddle + 1
  end;
end;

procedure TBCEditorSearch.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if Assigned(FOnChange) then
      FOnChange(scSearch);
  end;
end;

procedure TBCEditorSearch.SetEngine(const AValue: TBCEditorSearchEngine);
begin
  if FEngine <> AValue then
  begin
    FEngine := AValue;
    if Assigned(FOnChange) then
      FOnChange(scEngineUpdate);
  end;
end;

procedure TBCEditorSearch.SetHighlighter(const AValue: THighlighter);
begin
  FHighlighter.Assign(AValue);
end;

procedure TBCEditorSearch.SetInSelection(const AValue: TInSelection);
begin
  FInSelection.Assign(AValue);
end;

procedure TBCEditorSearch.SetMap(const AValue: TMap);
begin
  FMap.Assign(AValue);
end;

procedure TBCEditorSearch.SetOnChange(const AValue: TChangeEvent);
begin
  FOnChange := AValue;
  FMap.OnChange := FOnChange;
  FHighlighter.OnChange := FOnChange;
  FInSelection.OnChange := FOnChange;
end;

procedure TBCEditorSearch.SetOption(const AOption: TBCEditorSearchOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorSearch.SetSearchText(const AValue: string);
begin
  FSearchText := AValue;
  if Assigned(FOnChange) then
    FOnChange(scSearch);
end;

end.
