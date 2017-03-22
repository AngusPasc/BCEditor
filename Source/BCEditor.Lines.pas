unit BCEditor.Lines;

interface {********************************************************************}

uses
  SysUtils, Classes,
  Graphics, Controls,
  BCEditor.Utils, BCEditor.Consts, BCEditor.Types;

type
  TBCEditorLines = class(TStrings)
  protected type
    TChangeEvent = procedure(Sender: TObject; const Line: Integer) of object;
    TCompare = function(Lines: TBCEditorLines; Line1, Line2: Integer): Integer;

    TLineState = (lsLoaded, lsModified, lsSaved);

    TOption = (loColumns, loTrimTrailingSpaces, loUndoGrouped, loUndoAfterLoad, loUndoAfterSave);
    TOptions = set of TOption;

    TRange = Pointer;

    PLineAttribute = ^TLineAttribute;
    TLineAttribute = packed record
      Background: TColor;
      Foreground: TColor;
      LineState: TLineState;
    end;

    TLine = packed record
      Attribute: TLineAttribute;
      ExpandedLength: Integer;
      Flags: set of (sfHasTabs, sfHasNoTabs);
      Range: TRange;
      Text: string;
    end;
    TLines = array of TLine;

    TState = set of (lsLoading, lsUndo, lsRedo, lsCaretMoved, lsSelChanged, lsTextChanged);

    TUndoList = class(TPersistent)
    type
      TUndoType = (utSelection, utInsert, utReplace, utBackspace, utDelete,
        utClear, utInsertIndent, utDeleteIndent);

      PItem = ^TItem;
      TItem = packed record
        BlockNumber: Integer;
        UndoType: TUndoType;
        CaretPosition: TBCEditorTextPosition;
        SelBeginPosition: TBCEditorTextPosition;
        SelEndPosition: TBCEditorTextPosition;
        SelMode: TBCEditorSelectionMode;
        BeginPosition: TBCEditorTextPosition;
        EndPosition: TBCEditorTextPosition;
        Text: string;
      end;

    strict private
      FBlockNumber: Integer;
      FChanges: Integer;
      FCount: Integer;
      FCurrentBlockNumber: Integer;
      FGroupBreak: Boolean;
      FItems: array of TItem;
      FLines: TBCEditorLines;
      FUpdateCount: Integer;
      function GetItemCount(): Integer; inline;
      function GetItems(const AIndex: Integer): TItem;
      function GetUpdated(): Boolean;
      procedure Grow();
      procedure SetItems(const AIndex: Integer; const AValue: TItem);
    protected
      procedure BeginUpdate();
      procedure Clear();
      constructor Create(const ALines: TBCEditorLines);
      procedure EndUpdate();
      procedure GroupBreak();
      function PeekItem(out Item: PItem): Boolean;
      function PopItem(out Item: PItem): Boolean;
      procedure PushItem(const AUndoType: TUndoType; const ACaretPosition: TBCEditorTextPosition;
        const ASelBeginPosition, ASelEndPosition: TBCEditorTextPosition; const ASelMode: TBCEditorSelectionMode;
        const ABeginPosition, AEndPosition: TBCEditorTextPosition; const AText: string = '';
        const ABlockNumber: Integer = 0); overload;
      property Count: Integer read FCount;
      property Changes: Integer read FChanges;
      property ItemCount: Integer read GetItemCount;
      property Items[const AIndex: Integer]: TItem read GetItems write SetItems;
      property Lines: TBCEditorLines read FLines;
      property Updated: Boolean read GetUpdated;
    public
      procedure Assign(ASource: TPersistent); override;
      property UpdateCount: Integer read FUpdateCount;
    end;

  strict private const
    DefaultOptions = [loUndoGrouped];
  strict private
    FCapacity: Integer;
    FCaretPosition: TBCEditorTextPosition;
    FCaseSensitive: Boolean;
    FCount: Integer;
    FEditor: TCustomControl;
    FLines: TLines;
    FMaxLengthLine: Integer;
    FModified: Boolean;
    FOldCaretPosition: TBCEditorTextPosition;
    FOldSelBeginPosition: TBCEditorTextPosition;
    FOldSelEndPosition: TBCEditorTextPosition;
    FOldUndoListCount: Integer;
    FOnAfterLoad: TNotifyEvent;
    FOnBeforeLoad: TNotifyEvent;
    FOnCaretMoved: TNotifyEvent;
    FOnCleared: TNotifyEvent;
    FOnDeleted: TChangeEvent;
    FOnInserted: TChangeEvent;
    FOnSelChange: TNotifyEvent;
    FOnUpdated: TChangeEvent;
    FOptions: TOptions;
    FReadOnly: Boolean;
    FRedoList: TUndoList;
    FSelBeginPosition: TBCEditorTextPosition;
    FSelEndPosition: TBCEditorTextPosition;
    FSelMode: TBCEditorSelectionMode;
    FSortOrder: TBCEditorSortOrder;
    FState: TState;
    FTabWidth: Integer;
    FUndoList: TUndoList;
    function CalcExpandString(ALine: Integer): string;
    procedure DoDelete(ALine: Integer);
    procedure DoDeleteIndent(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const AIndentText: string; const ASelMode: TBCEditorSelectionMode);
    procedure DoDeleteText(ABeginPosition, AEndPosition: TBCEditorTextPosition);
    procedure DoInsertIndent(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const AIndentText: string; const ASelMode: TBCEditorSelectionMode);
    procedure DoInsert(ALine: Integer; const AText: string);
    function DoInsertText(APosition: TBCEditorTextPosition;
      const AText: string): TBCEditorTextPosition;
    procedure DoPut(ALine: Integer; const AText: string);
    procedure ExchangeItems(ALine1, ALine2: Integer);
    procedure ExecuteUndoRedo(const List: TBCEditorLines.TUndoList);
    function GetAttributes(ALine: Integer): PLineAttribute;
    function GetBOLTextPosition(ALine: Integer): TBCEditorTextPosition; inline;
    function GetCanRedo(): Boolean;
    function GetCanUndo(): Boolean;
    function GetEOFPosition(): TBCEditorTextPosition;
    function GetEOLTextPosition(ALine: Integer): TBCEditorTextPosition;
    function GetExpandedString(ALine: Integer): string;
    function GetExpandedStringLength(ALine: Integer): Integer;
    function GetMaxLength(): Integer;
    function GetRange(ALine: Integer): TRange;
    function GetTextBetween(const ABeginPosition, AEndPosition: TBCEditorTextPosition): string; overload;
    function GetTextBetweenColumn(const ABeginPosition, AEndPosition: TBCEditorTextPosition): string; overload;
    procedure Grow();
    procedure InternalClear(const AClearUndo: Boolean); overload;
    procedure PutAttributes(ALine: Integer; const AValue: PLineAttribute);
    procedure PutRange(ALine: Integer; ARange: TRange);
    procedure SetCaretPosition(const AValue: TBCEditorTextPosition);
    procedure SetModified(const AValue: Boolean);
    procedure SetOptions(const AValue: TOptions);
    procedure SetSelBeginPosition(const AValue: TBCEditorTextPosition);
    procedure SetSelEndPosition(const AValue: TBCEditorTextPosition);
    procedure QuickSort(ALeft, ARight: Integer; ACompare: TCompare);
    property Capacity: Integer read FCapacity write SetCapacity;
  protected
    procedure Backspace(ABeginPosition, AEndPosition: TBCEditorTextPosition);
    procedure ClearUndo();
    function CharIndexToPosition(const ACharIndex: Integer): TBCEditorTextPosition; overload; inline;
    function CharIndexToPosition(const ACharIndex: Integer;
      const ARelativePosition: TBCEditorTextPosition): TBCEditorTextPosition; overload;
    function CompareStrings(const S1, S2: string): Integer; override;
    procedure CustomSort(const ABeginLine, AEndLine: Integer; ACompare: TCompare);
    procedure DeleteIndent(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const AIndentText: string; const ASelMode: TBCEditorSelectionMode);
    procedure DeleteText(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const ASelMode: TBCEditorSelectionMode = smNormal); overload;
    function Get(ALine: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetTextLength(): Integer;
    function GetTextStr(): string; override;
    procedure InsertIndent(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const AIndentText: string; const ASelMode: TBCEditorSelectionMode);
    procedure InsertText(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const AText: string); overload;
    function InsertText(APosition: TBCEditorTextPosition;
      const AText: string): TBCEditorTextPosition; overload;
    function IsPositionInSelection(const APosition: TBCEditorTextPosition): Boolean;
    procedure Put(ALine: Integer; const AText: string); override;
    function PositionToCharIndex(const APosition: TBCEditorTextPosition): Integer;
    procedure Redo(); inline;
    function ReplaceText(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const AText: string): TBCEditorTextPosition;
    procedure SetCapacity(AValue: Integer); override;
    procedure SetTabWidth(const AValue: Integer);
    procedure SetTextStr(const AValue: string); override;
    procedure SetUpdateState(AUpdating: Boolean); override;
    procedure Sort(const ABeginLine, AEndLine: Integer); virtual;
    procedure Undo(); inline;
    procedure UndoGroupBreak();
    property Attributes[ALine: Integer]: PLineAttribute read GetAttributes write PutAttributes;
    property BOLTextPosition[ALine: Integer]: TBCEditorTextPosition read GetBOLTextPosition;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretPosition: TBCEditorTextPosition read FCaretPosition write SetCaretPosition;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property Editor: TCustomControl read FEditor write FEditor;
    property EOFTextPosition: TBCEditorTextPosition read GetEOFPosition;
    property EOLTextPosition[ALine: Integer]: TBCEditorTextPosition read GetEOLTextPosition;
    property ExpandedStringLengths[ALine: Integer]: Integer read GetExpandedStringLength;
    property ExpandedStrings[ALine: Integer]: string read GetExpandedString;
    property Lines: TLines read FLines;
    property MaxLength: Integer read GetMaxLength;
    property Modified: Boolean read FModified write SetModified;
    property OnAfterLoad: TNotifyEvent read FOnAfterLoad write FOnAfterLoad;
    property OnBeforeLoad: TNotifyEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnCaretMoved: TNotifyEvent read FOnCaretMoved write FOnCaretMoved;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
    property OnDeleted: TChangeEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TChangeEvent read FOnInserted write FOnInserted;
    property OnSelChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    property OnUpdated: TChangeEvent read FOnUpdated write FOnUpdated;
    property Options: TOptions read FOptions write SetOptions;
    property Ranges[ALine: Integer]: TRange read GetRange write PutRange;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property RedoList: TUndoList read FRedoList;
    property SelBeginPosition: TBCEditorTextPosition read FSelBeginPosition write SetSelBeginPosition;
    property SelEndPosition: TBCEditorTextPosition read FSelEndPosition write SetSelEndPosition;
    property SelMode: TBCEditorSelectionMode read FSelMode write FSelMode;
    property SortOrder: TBCEditorSortOrder read FSortOrder write FSortOrder;
    property State: TState read FState;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property TextBetween[const ABeginPosition, AEndPosition: TBCEditorTextPosition]: string read GetTextBetween;
    property TextBetweenColumn[const ABeginPosition, AEndPosition: TBCEditorTextPosition]: string read GetTextBetweenColumn;
    property UndoList: TUndoList read FUndoList;
  public
    function Add(const AText: string): Integer; override;
    procedure Clear(); overload; override;
    constructor Create(const AEditor: TCustomControl);
    procedure Delete(ALine: Integer); overload; override;
    destructor Destroy; override;
    procedure Insert(ALine: Integer; const AText: string); override;
    procedure SaveToStream(AStream: TStream; AEncoding: TEncoding = nil); override;
  end;

const
  BOFTextPosition: TBCEditorTextPosition = (Char: 1; Line: 0);
  InvalidTextPosition: TBCEditorTextPosition = (Char: -1; Line: -1);

implementation {***************************************************************}

uses
  Math, StrUtils;

function HasLineBreak(const Text: string): Boolean;
var
  LEndPos: PChar;
  LPos: PChar;
begin
  LPos := PChar(Text); LEndPos := PChar(@Text[Length(Text)]);
  while (LPos <= LEndPos) do
    if (CharInSet(LPos^, [BCEDITOR_LINEFEED, BCEDITOR_CARRIAGE_RETURN])) then
      Exit(True)
    else
      Inc(LPos);
  Result := False;
end;

{ TBCEditorLines.TUndoList ****************************************************}

procedure TBCEditorLines.TUndoList.Assign(ASource: TPersistent);
var
  I: Integer;
begin
  Assert(Assigned(ASource) and (ASource is TBCEditorLines.TUndoList));

  Clear();
  SetLength(FItems, TUndoList(ASource).Count);
  for I := 0 to TUndoList(ASource).Count - 1 do
    FItems[I] := TUndoList(ASource).Items[I];
  FCurrentBlockNumber := TUndoList(ASource).FCurrentBlockNumber;
end;

procedure TBCEditorLines.TUndoList.BeginUpdate();
begin
  if (UpdateCount = 0) then
  begin
    Inc(FBlockNumber);
    FChanges := 0;
    FCurrentBlockNumber := FBlockNumber;
  end;

  Inc(FUpdateCount);
end;

procedure TBCEditorLines.TUndoList.Clear();
begin
  FBlockNumber := 0;
  FCount := 0;
  FGroupBreak := False;
  SetLength(FItems, 0);
end;

constructor TBCEditorLines.TUndoList.Create(const ALines: TBCEditorLines);
begin
  inherited Create();

  FLines := ALines;

  FBlockNumber := 0;
  FCount := 0;
  FUpdateCount := 0;
end;

procedure TBCEditorLines.TUndoList.EndUpdate();
begin
  if (FUpdateCount > 0) then
  begin
    Dec(FUpdateCount);

    if (FUpdateCount = 0) then
    begin
      FChanges := 0;
      FCurrentBlockNumber := 0;
    end;
  end;
end;

function TBCEditorLines.TUndoList.GetItemCount(): Integer;
begin
  Result := FCount;
end;

function TBCEditorLines.TUndoList.GetItems(const AIndex: Integer): TItem;
begin
  Result := TItem(FItems[AIndex]);
end;

function TBCEditorLines.TUndoList.GetUpdated(): Boolean;
begin
  Result := (FUpdateCount > 0) and (FChanges > 0);
end;

procedure TBCEditorLines.TUndoList.GroupBreak();
begin
  FGroupBreak := True;
end;

procedure TBCEditorLines.TUndoList.Grow();
begin
  if (Length(FItems) > 64) then
    SetLength(FItems, Length(FItems) + Length(FItems) div 4)
  else
    SetLength(FItems, Length(FItems) + 16);
end;

function TBCEditorLines.TUndoList.PeekItem(out Item: PItem): Boolean;
begin
  Result := FCount > 0;
  if (Result) then
    Item := @FItems[FCount - 1];
end;

function TBCEditorLines.TUndoList.PopItem(out Item: PItem): Boolean;
begin
  Result := FCount > 0;
  if (Result) then
  begin
    Item := @FItems[FCount - 1];
    Dec(FCount);
  end;
end;

procedure TBCEditorLines.TUndoList.PushItem(const AUndoType: TUndoType; const ACaretPosition: TBCEditorTextPosition;
  const ASelBeginPosition, ASelEndPosition: TBCEditorTextPosition; const ASelMode: TBCEditorSelectionMode;
  const ABeginPosition, AEndPosition: TBCEditorTextPosition; const AText: string = '';
  const ABlockNumber: Integer = 0);
var
  LHandled: Boolean;
begin
  if (not (lsLoading in Lines.State)) then
  begin
    LHandled := False;
    if ((Lines.State * [lsUndo, lsRedo] = [])
      and (loUndoGrouped in Lines.Options)
      and not FGroupBreak
      and (Count > 0) and (FItems[Count - 1].UndoType = AUndoType)) then
      case (AUndoType) of
        utSelection: LHandled := True; // Ignore
        utInsert:
          if (FItems[Count - 1].EndPosition = ABeginPosition) then
          begin
            FItems[Count - 1].EndPosition := AEndPosition;
            LHandled := True;
          end;
        utReplace:
          if (FItems[Count - 1].EndPosition = ABeginPosition) then
          begin
            FItems[Count - 1].EndPosition := AEndPosition;
            FItems[Count - 1].Text := FItems[Count - 1].Text + AText;
            LHandled := True;
          end;
        utBackspace:
          if (FItems[Count - 1].BeginPosition = AEndPosition) then
          begin
            FItems[Count - 1].BeginPosition := ABeginPosition;
            FItems[Count - 1].Text := AText + FItems[Count - 1].Text;
            LHandled := True;
          end;
        utDelete:
          if (FItems[Count - 1].EndPosition = ABeginPosition) then
          begin
            FItems[Count - 1].EndPosition := AEndPosition;
            FItems[Count - 1].Text := FItems[Count - 1].Text + AText;
            LHandled := True;
          end;
      end;

    if (not LHandled) then
    begin
      if (Count = Length(FItems)) then
        Grow();

      with FItems[FCount] do
      begin
        if (ABlockNumber > 0) then
          BlockNumber := ABlockNumber
        else if (FCurrentBlockNumber > 0) then
          BlockNumber := FCurrentBlockNumber
        else
        begin
          Inc(FBlockNumber);
          BlockNumber := FBlockNumber;
        end;
        BeginPosition := ABeginPosition;
        CaretPosition := ACaretPosition;
        EndPosition := AEndPosition;
        SelBeginPosition := ASelBeginPosition;
        SelEndPosition := ASelEndPosition;
        SelMode := ASelMode;
        Text := AText;
        UndoType := AUndoType;
      end;
      Inc(FCount);
    end;

    if (UpdateCount > 0) then
      Inc(FChanges);
    FGroupBreak := False;
  end;
end;

procedure TBCEditorLines.TUndoList.SetItems(const AIndex: Integer; const AValue: TItem);
begin
  FItems[AIndex] := AValue;
end;

{ TBCEditorLines **************************************************************}

function CompareLines(ALines: TBCEditorLines; AIndex1, AIndex2: Integer): Integer;
begin
  Result := ALines.CompareStrings(ALines.Lines[AIndex1].Text, ALines.Lines[AIndex2].Text);
  if (ALines.SortOrder = soDesc) then
    Result := - Result;
end;

function TBCEditorLines.Add(const AText: string): Integer;
begin
  Result := FCount;
  Insert(Result, AText);
end;

procedure TBCEditorLines.Backspace(ABeginPosition, AEndPosition: TBCEditorTextPosition);
var
  LCaretPosition: TBCEditorTextPosition;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
  LText: string;
begin
  Assert((BOFTextPosition <= ABeginPosition) and (ABeginPosition < AEndPosition) and (AEndPosition <= EOFTextPosition));

  LCaretPosition := CaretPosition;
  LSelBeginPosition := SelBeginPosition;
  LSelEndPosition := SelEndPosition;
  LText := GetTextBetween(ABeginPosition, AEndPosition);

  UndoList.BeginUpdate();
  try
    DoDeleteText(ABeginPosition, AEndPosition);

    UndoList.PushItem(utBackspace, LCaretPosition,
      LSelBeginPosition, LSelEndPosition, SelMode,
      ABeginPosition, AEndPosition, LText);
  finally
    UndoList.EndUpdate();
  end;

  CaretPosition := ABeginPosition;
end;

function TBCEditorLines.CalcExpandString(ALine: Integer): string;
var
  LHasTabs: Boolean;
begin
  with Lines[ALine] do
    if (Text = '') then
      Result := ''
    else
    begin
      Result := ConvertTabs(Text, FTabWidth, LHasTabs, loColumns in Options);

      if LHasTabs then
      begin
        Include(Flags, sfHasTabs);
        Exclude(Flags, sfHasNoTabs);
      end
      else
      begin
        Exclude(Flags, sfHasTabs);
        Include(Flags, sfHasNoTabs);
      end;
      ExpandedLength := Length(Result);
    end;
end;

function TBCEditorLines.CharIndexToPosition(const ACharIndex: Integer): TBCEditorTextPosition;
begin
  Result := CharIndexToPosition(ACharIndex, BOFTextPosition);
end;

function TBCEditorLines.CharIndexToPosition(const ACharIndex: Integer;
  const ARelativePosition: TBCEditorTextPosition): TBCEditorTextPosition;
var
  LCharIndex: Integer;
  LLength: Integer;
  LLineBreakLength: Integer;
begin
  Assert((BOFTextPosition <= ARelativePosition) and (ARelativePosition <= EOFTextPosition) or (ACharIndex = 0) and (Count = 0));
  LCharIndex := Max(0, ACharIndex);

  Result := ARelativePosition;

  if ((Result.Char - 1) + LCharIndex <= Length(Lines[Result.Line].Text)) then
    Inc(Result.Char, LCharIndex)
  else
  begin
    LLineBreakLength := Length(LineBreak);

    LLength := LCharIndex - (Length(Lines[Result.Line].Text) - (Result.Char - 1)) - LLineBreakLength;
    Inc(Result.Line);

    while ((Result.Line < Count) and (LLength >= Length(Lines[Result.Line].Text) + LLineBreakLength)) do
    begin
      Dec(LLength, Length(Lines[Result.Line].Text) + LLineBreakLength);
      Inc(Result.Line);
    end;

    if (LLength > Length(Lines[Result.Line].Text)) then
      raise ERangeError.CreateFmt('Character index out of bounds (%d / %d, %d / %d)', [LCharIndex, Length(Text), LLength, Length(Lines[Result.Line].Text)]);

    Result.Char := 1 + LLength;
  end;

  Assert(Result <= EOFTextPosition, 'ACharIndex: ' + IntToStr(ACharIndex) + ', RelPos: ' + ARelativePosition.ToString() + ', Result: ' + Result.ToString());
end;

procedure TBCEditorLines.Clear();
begin
  InternalClear(True);
end;

procedure TBCEditorLines.ClearUndo();
begin
  UndoList.Clear();
  RedoList.Clear();
end;

function TBCEditorLines.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);

  if SortOrder = soDesc then
    Result := -1 * Result;
end;

constructor TBCEditorLines.Create(const AEditor: TCustomControl);
begin
  inherited Create();

  FEditor := AEditor;

  FCaretPosition := BOFTextPosition;
  FCaseSensitive := False;
  FCount := 0;
  FMaxLengthLine := -1;
  FModified := False;
  FOnAfterLoad := nil;
  FOnBeforeLoad := nil;
  FOnCaretMoved := nil;
  FOnCleared := nil;
  FOnDeleted := nil;
  FOnInserted := nil;
  FOnSelChange := nil;
  FOnUpdated := nil;
  FOptions := DefaultOptions;
  FRedoList := TUndoList.Create(Self);
  FReadOnly := False;
  FSelBeginPosition := BOFTextPosition;
  FSelEndPosition := BOFTextPosition;
  FSelMode := smNormal;
  FState := [];
  FUndoList := TUndoList.Create(Self);
  TabWidth := 4;
end;

procedure TBCEditorLines.CustomSort(const ABeginLine, AEndLine: Integer;
  ACompare: TCompare);
var
  LBeginPosition: TBCEditorTextPosition;
  LEndPosition: TBCEditorTextPosition;
  LText: string;
begin
  BeginUpdate();
  UndoList.BeginUpdate();

  try
    LBeginPosition := BOLTextPosition[ABeginLine];
    if (AEndLine < Count - 1) then
      LEndPosition := BOLTextPosition[ABeginLine + 1]
    else
      LEndPosition := TextPosition(Length(Lines[AEndLine].Text), AEndLine);

    LText := GetTextBetween(LBeginPosition, LEndPosition);
    UndoList.PushItem(utDelete, CaretPosition,
      SelBeginPosition, SelEndPosition, SelMode,
      LBeginPosition, InvalidTextPosition, LText);

    QuickSort(ABeginLine, AEndLine, ACompare);

    UndoList.PushItem(utInsert, InvalidTextPosition,
      InvalidTextPosition, InvalidTextPosition, smNormal,
      LBeginPosition, LEndPosition);
  finally
    UndoList.EndUpdate();
    EndUpdate();
    RedoList.Clear();
  end;
end;

procedure TBCEditorLines.Delete(ALine: Integer);
var
  LBeginPosition: TBCEditorTextPosition;
  LCaretPosition: TBCEditorTextPosition;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
  LText: string;
  LUndoType: TUndoList.TUndoType;
begin
  Assert((0 <= ALine) and (ALine < Count));

  LCaretPosition := CaretPosition;
  LSelBeginPosition := SelBeginPosition;
  LSelEndPosition := SelEndPosition;
  if (Count = 1) then
  begin
    LBeginPosition := BOFTextPosition;
    LText := Get(ALine);
    LUndoType := utClear;
  end
  else if (ALine < Count - 1) then
  begin
    LBeginPosition := BOLTextPosition[ALine];
    LText := Get(ALine) + LineBreak;
    LUndoType := utDelete;
  end
  else
  begin
    LBeginPosition := EOLTextPosition[ALine - 1];
    LText := LineBreak + Get(ALine);
    LUndoType := utDelete;
  end;

  UndoList.BeginUpdate();
  try
    DoDelete(ALine);

    UndoList.PushItem(LUndoType, LCaretPosition,
      LSelBeginPosition, LSelEndPosition, SelMode,
      LBeginPosition, InvalidTextPosition, LText);
  finally
    UndoList.EndUpdate();
  end;
end;

procedure TBCEditorLines.DeleteIndent(ABeginPosition, AEndPosition: TBCEditorTextPosition;
  const AIndentText: string; const ASelMode: TBCEditorSelectionMode);
var
  LBeginPosition: TBCEditorTextPosition;
  LCaretPosition: TBCEditorTextPosition;
  LEndPosition: TBCEditorTextPosition;
  LLine: Integer;
  LIndentFound: Boolean;
  LIndentTextLength: Integer;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
begin
  LBeginPosition := Min(ABeginPosition, AEndPosition);
  LEndPosition := Max(ABeginPosition, AEndPosition);

  Assert((BOFTextPosition <= LBeginPosition) and (LBeginPosition <= LEndPosition) and (LEndPosition <= EOFTextPosition));

  LIndentTextLength := Length(AIndentText);
  LIndentFound := LBeginPosition.Line <> LEndPosition.Line;
  for LLine := LBeginPosition.Line to LEndPosition.Line do
    if (Copy(Lines[LLine].Text, LBeginPosition.Char, LIndentTextLength) <> AIndentText) then
    begin
      LIndentFound := False;
      break;
    end;

  if (LIndentFound) then
  begin
    LCaretPosition := CaretPosition;
    LSelBeginPosition := SelBeginPosition;
    LSelEndPosition := SelEndPosition;

    DoDeleteIndent(LBeginPosition, LEndPosition, AIndentText, ASelMode);

    UndoList.PushItem(utDeleteIndent, LCaretPosition,
      LSelBeginPosition, LSelEndPosition, SelMode,
      LBeginPosition, LEndPosition, AIndentText);

    RedoList.Clear();
  end
  else
  begin
    UndoList.BeginUpdate();

    try
      for LLine := LBeginPosition.Line to LEndPosition.Line do
        if (LeftStr(Lines[LLine].Text, LIndentTextLength) = AIndentText) then
          DeleteText(TextPosition(1, LLine), TextPosition(1 + Length(AIndentText), LLine));
    finally
      UndoList.EndUpdate();
    end;
  end;

  if (CaretPosition.Char - 1 > Length(Lines[CaretPosition.Line].Text)) then
    FCaretPosition.Char := 1 + Length(Lines[CaretPosition.Line].Text);
  if (SelBeginPosition.Char - 1 > Length(Lines[SelBeginPosition.Line].Text)) then
    FSelBeginPosition.Char := 1 + Length(Lines[SelBeginPosition.Line].Text);
  if (SelEndPosition.Char > Length(Lines[SelEndPosition.Line].Text)) then
    FSelEndPosition.Char := 1 + Length(Lines[SelEndPosition.Line].Text);
end;

procedure TBCEditorLines.DeleteText(ABeginPosition, AEndPosition: TBCEditorTextPosition;
  const ASelMode: TBCEditorSelectionMode = smNormal);
var
  LCaretPosition: TBCEditorTextPosition;
  LBeginText: TBCEditorTextPosition;
  LEndText: TBCEditorTextPosition;
  LInsertBeginPosition: TBCEditorTextPosition;
  LInsertEndPosition: TBCEditorTextPosition;
  LLine: Integer;
  LLineLength: Integer;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
  LSpaces: string;
  LText: string;
begin
  UndoList.BeginUpdate();
  try
    if (ABeginPosition = AEndPosition) then
      // Do nothing
    else if (ASelMode = smNormal) then
    begin
      LCaretPosition := CaretPosition;
      LSelBeginPosition := SelBeginPosition;
      LSelEndPosition := SelEndPosition;

      if (ABeginPosition.Char - 1 > Length(Lines[ABeginPosition.Line].Text)) then
      begin
        LInsertBeginPosition := EOLTextPosition[ABeginPosition.Line];

        LInsertEndPosition := DoInsertText(LInsertBeginPosition, StringOfChar(BCEDITOR_SPACE_CHAR, ABeginPosition.Char - LInsertBeginPosition.Char));

        UndoList.PushItem(utInsert, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          LInsertBeginPosition, LInsertEndPosition);

        Assert(LInsertEndPosition = ABeginPosition);
      end;

      LText := GetTextBetween(ABeginPosition, AEndPosition);

      DoDeleteText(ABeginPosition, AEndPosition);

      UndoList.PushItem(utDelete, LCaretPosition,
        LSelBeginPosition, LSelEndPosition, SelMode,
        ABeginPosition, InvalidTextPosition, LText);
    end
    else
    begin
      LCaretPosition := CaretPosition;
      LSelBeginPosition := SelBeginPosition;
      LSelEndPosition := SelEndPosition;

      UndoList.PushItem(utSelection, LCaretPosition,
        LSelBeginPosition, LSelEndPosition, SelMode,
        InvalidTextPosition, InvalidTextPosition);

      for LLine := ABeginPosition.Line to AEndPosition.Line do
      begin
        LBeginText := TextPosition(ABeginPosition.Char, LLine);
        if (AEndPosition.Char - 1 < Length(Lines[LLine].Text)) then
          LEndText := TextPosition(1 + Length(Lines[LLine].Text), LLine)
        else
          LEndText := TextPosition(AEndPosition.Char, LLine);

        LText := GetTextBetween(LBeginText, LEndText);

        DoDeleteText(LBeginText, LEndText);

        UndoList.PushItem(utDelete, InvalidTextPosition,
          InvalidTextPosition, InvalidTextPosition, SelMode,
          LBeginText, InvalidTextPosition, LText);

        LLineLength := Length(Lines[LLine].Text);
        if (LLineLength > ABeginPosition.Char - 1) then
        begin
          LSpaces := StringOfChar(BCEDITOR_SPACE_CHAR, ABeginPosition.Char - 1 - LLineLength);

          DoInsertText(LEndText, LSpaces);

          UndoList.PushItem(utInsert, InvalidTextPosition,
            InvalidTextPosition, InvalidTextPosition, SelMode,
            TextPosition(ABeginPosition.Char, LLine), TextPosition(AEndPosition.Char, LLine));
        end;
      end;
    end;

    if (SelMode = smNormal) then
    begin
      CaretPosition := ABeginPosition;
      SelBeginPosition := CaretPosition;
      SelEndPosition := CaretPosition;
    end;
  finally
    UndoList.EndUpdate();
  end;

  RedoList.Clear();
end;

destructor TBCEditorLines.Destroy;
begin
  FRedoList.Free();
  FUndoList.Free();

  inherited;
end;

procedure TBCEditorLines.DoDelete(ALine: Integer);
begin
  Assert((0 <= ALine) and (ALine < Count));

  if (FMaxLengthLine >= 0) then
    if (FMaxLengthLine = ALine) then
      FMaxLengthLine := -1
    else if (FMaxLengthLine > ALine) then
      Dec(FMaxLengthLine);

  Dec(FCount);
  if (ALine < FCount) then
  begin
    Finalize(Lines[ALine]);
    System.Move(Lines[ALine + 1], Lines[ALine], (FCount - ALine) * SizeOf(TLine));
    FillChar(Lines[FCount], SizeOf(Lines[FCount]), 0);
  end;

  if (SelMode = smNormal) then
    if (Count = 0) then
      CaretPosition := BOFTextPosition
    else if (ALine < Count) then
      CaretPosition := BOLTextPosition[ALine]
    else
      CaretPosition := EOLTextPosition[ALine - 1]
  else
  begin
    if (SelBeginPosition.Line > ALine) then
      SelBeginPosition := TextPosition(SelBeginPosition.Char, SelBeginPosition.Line - 1);
    if (SelEndPosition.Line >= ALine) then
      SelEndPosition := TextPosition(SelEndPosition.Char, SelEndPosition.Line - 1);
  end;

  if (UpdateCount > 0) then
    Include(FState, lsTextChanged);

  if ((Count = 0) and Assigned(OnCleared)) then
    OnCleared(Self)
  else if (Assigned(OnDeleted)) then
    OnDeleted(Self, ALine);
end;

procedure TBCEditorLines.DoDeleteIndent(ABeginPosition, AEndPosition: TBCEditorTextPosition;
  const AIndentText: string; const ASelMode: TBCEditorSelectionMode);
var
  LLine: Integer;
  LTextBeginPosition: TBCEditorTextPosition;
  LTextEndPosition: TBCEditorTextPosition;
begin
  Assert((BOFTextPosition <= ABeginPosition) and (AEndPosition <= EOFTextPosition));
  Assert(ABeginPosition <= AEndPosition);

  LTextBeginPosition := BOLTextPosition[ABeginPosition.Line];
  if (Count = 0) then
    LTextEndPosition := InvalidTextPosition
  else if (ABeginPosition = AEndPosition) then
    LTextEndPosition := TextPosition(1 + Length(Lines[AEndPosition.Line].Text), AEndPosition.Line)
  else if ((AEndPosition.Char = 1) and (AEndPosition.Line > ABeginPosition.Line)) then
    LTextEndPosition := TextPosition(1 + Length(Lines[AEndPosition.Line - 1].Text), AEndPosition.Line - 1)
  else
    LTextEndPosition := AEndPosition;

  BeginUpdate();

  try
    for LLine := LTextBeginPosition.Line to LTextEndPosition.Line do
      if (ASelMode = smNormal) then
      begin
        if (LeftStr(Lines[LLine].Text, Length(AIndentText)) = AIndentText) then
          DoPut(LLine, Copy(Lines[LLine].Text, 1 + Length(AIndentText), MaxInt));
      end
      else if (Copy(Lines[LLine].Text, ABeginPosition.Char, Length(AIndentText)) = AIndentText) then
        DoPut(LLine,
          LeftStr(Lines[LLine].Text, ABeginPosition.Char - 1)
            + Copy(Lines[LLine].Text, ABeginPosition.Char + Length(AIndentText), MaxInt));
  finally
    EndUpdate();
  end;
end;

procedure TBCEditorLines.DoDeleteText(ABeginPosition, AEndPosition: TBCEditorTextPosition);
var
  Line: Integer;
begin
  Assert((BOFTextPosition <= ABeginPosition) and (AEndPosition <= EOFTextPosition));
  Assert(ABeginPosition <= AEndPosition);

  if (ABeginPosition = AEndPosition) then
    // Nothing to do...
  else if (ABeginPosition.Line = AEndPosition.Line) then
    DoPut(ABeginPosition.Line, LeftStr(Lines[ABeginPosition.Line].Text, ABeginPosition.Char - 1)
      + Copy(Lines[AEndPosition.Line].Text, AEndPosition.Char, MaxInt))
  else
  begin
    BeginUpdate();

    try
      DoPut(ABeginPosition.Line, LeftStr(Lines[ABeginPosition.Line].Text, ABeginPosition.Char - 1)
        + Copy(Lines[AEndPosition.Line].Text, AEndPosition.Char, MaxInt));

      for Line := AEndPosition.Line downto ABeginPosition.Line + 1 do
        DoDelete(Line);
    finally
      EndUpdate();
    end;
  end;
end;

procedure TBCEditorLines.DoInsertIndent(ABeginPosition, AEndPosition: TBCEditorTextPosition;
  const AIndentText: string; const ASelMode: TBCEditorSelectionMode);
var
  LEndLine: Integer;
  LLine: Integer;
begin
  Assert((BOFTextPosition <= ABeginPosition) and (AEndPosition <= EOFTextPosition));
  Assert(ABeginPosition <= AEndPosition);

  if (Count = 0) then
    LEndLine := -1
  else if ((AEndPosition.Char = 1) and (AEndPosition.Line > ABeginPosition.Line)) then
    LEndLine := AEndPosition.Line - 1
  else
    LEndLine := AEndPosition.Line;

  BeginUpdate();

  try
    for LLine := ABeginPosition.Line to LEndLine do
      if (ASelMode = smNormal) then
        DoPut(LLine, AIndentText + Lines[LLine].Text)
      else if (Length(Lines[LLine].Text) >= ABeginPosition.Char) then
        DoPut(LLine, Copy(Lines[LLine].Text, 1, ABeginPosition.Char - 1)
          + AIndentText
          + Copy(Lines[LLine].Text, ABeginPosition.Char, MaxInt));
  finally
    EndUpdate();
  end;
end;

procedure TBCEditorLines.DoInsert(ALine: Integer; const AText: string);
begin
  Assert((0 <= ALine) and (ALine <= Count));

  if (FCount = FCapacity) then
    Grow();

  if (ALine < FCount) then
    System.Move(Lines[ALine], Lines[ALine + 1], (FCount - ALine) * SizeOf(TLine));
  Inc(FCount);

  Lines[ALine].Attribute.Foreground := clNone;
  Lines[ALine].Attribute.Background := clNone;
  Lines[ALine].Attribute.LineState := lsModified;
  Lines[ALine].ExpandedLength := -1;
  Lines[ALine].Flags := [sfHasTabs, sfHasNoTabs];
  Lines[ALine].Range := nil;
  Pointer(Lines[ALine].Text) := nil;
  DoPut(ALine, AText);

  if (SelMode = smNormal) then
  begin
    if (ALine < Count - 1) then
      CaretPosition := BOLTextPosition[ALine + 1]
    else
      CaretPosition := EOLTextPosition[ALine];
    SelBeginPosition := CaretPosition;
    SelEndPosition := CaretPosition;
  end
  else
  begin
    if (SelBeginPosition.Line < ALine) then
      SelBeginPosition := TextPosition(SelBeginPosition.Char, SelBeginPosition.Line + 1);
    if (SelEndPosition.Line <= ALine) then
      if (ALine < Count) then
        SelEndPosition := EOLTextPosition[ALine]
      else
        SelEndPosition := TextPosition(SelEndPosition.Char, SelEndPosition.Line + 1);
  end;

  if (Assigned(OnInserted)) then
    OnInserted(Self, ALine);
end;

function TBCEditorLines.DoInsertText(APosition: TBCEditorTextPosition;
  const AText: string): TBCEditorTextPosition;
var
  LEndPos: PChar;
  LEOL: Boolean;
  LLine: Integer;
  LLineBeginPos: PChar;
  LLineBreak: array [0..2] of Char;
  LLineEnd: string;
  LPos: PChar;
begin
  Assert(BOFTextPosition <= APosition);
  Assert((APosition.Line = 0) and (Count = 0) or (APosition.Line < Count) and (APosition.Char - 1 <= Length(Lines[APosition.Line].Text)));

  if (AText = '') then
    Result := APosition
  else if (not HasLineBreak(AText)) then
  begin
    if (Count = 0) then
      DoPut(0, AText)
    else
      DoPut(APosition.Line, LeftStr(Lines[APosition.Line].Text, APosition.Char - 1)
        + AText
        + Copy(Lines[APosition.Line].Text, APosition.Char, MaxInt));
    Result := TextPosition(APosition.Char + Length(AText), APosition.Line);
  end
  else
  begin
    LLineBreak[0] := #0; LLineBreak[1] := #0; LLineBreak[2] := #0;

    BeginUpdate();

    try
      LLine := APosition.Line;

      LPos := @AText[1];
      LEndPos := @AText[Length(AText)];

      LLineBeginPos := LPos;
      while ((LPos <= LEndPos) and not CharInSet(LPos^, [BCEDITOR_LINEFEED, BCEDITOR_CARRIAGE_RETURN])) do
        Inc(LPos);

      if (LLine < Count) then
      begin
        if (APosition.Char - 1 = 0) then
        begin
          LLineEnd := Lines[LLine].Text;
          if (LLineBeginPos < LPos) then
            DoPut(LLine, LeftStr(AText, LPos - LLineBeginPos))
          else if (Lines[LLine].Text <> '') then
            DoPut(LLine, '');
        end
        else
        begin
          LLineEnd := Copy(Lines[LLine].Text, APosition.Char, MaxInt);
          if (LLineBeginPos < LPos) then
            DoPut(LLine, LeftStr(Lines[LLine].Text, APosition.Char - 1) + LeftStr(AText, LPos - LLineBeginPos))
          else if (Length(Lines[LLine].Text) > APosition.Char - 1) then
            DoPut(LLine, LeftStr(Lines[LLine].Text, APosition.Char - 1));
        end;
      end
      else
        DoInsert(LLine, LeftStr(AText, LPos - LLineBeginPos));
      Inc(LLine);

      if (LPos <= LEndPos) then
      begin
        LLineBreak[0] := LPos^;
        if ((LLineBreak[0] = BCEDITOR_CARRIAGE_RETURN) and (LPos < LEndPos) and (LPos[1] = BCEDITOR_LINEFEED)) then
          LLineBreak[1] := LPos[1];
      end;

      LEOL := (LPos <= LEndPos) and (LPos[0] = LLineBreak[0]) and ((LLineBreak[1] = #0) or (LPos < LEndPos) and (LPos[1] = LLineBreak[1]));
      while (LEOL) do
      begin
        if (LLineBreak[1] = #0) then
          Inc(LPos)
        else
          Inc(LPos, 2);
        LLineBeginPos := LPos;
        repeat
          LEOL := (LPos <= LEndPos) and (LPos[0] = LLineBreak[0]) and ((LLineBreak[1] = #0) or (LPos < LEndPos) and (LPos[1] = LLineBreak[1]));
          if (not LEOL) then
            Inc(LPos);
        until ((LPos > LEndPos) or LEOL);
        if (LEOL) then
        begin
          DoInsert(LLine, Copy(AText, 1 + LLineBeginPos - @AText[1], LPos - LLineBeginPos));
          Inc(LLine);
        end;
      end;

      if (LPos <= LEndPos) then
      begin
        if (LLine < Count) then
          DoPut(LLine, Copy(AText, LPos - @AText[1], LEndPos + 1 - LPos) + LLineEnd)
        else
          DoInsert(LLine, Copy(AText, LPos - @AText[1], LEndPos + 1 - LPos) + LLineEnd);
        Result := TextPosition(LEndPos + 1 - LLineBeginPos, LLine);
      end
      else
      begin
        if (LLine < Count) then
          DoPut(LLine, RightStr(AText, LEndPos + 1 - LLineBeginPos) + LLineEnd)
        else
          DoInsert(LLine, RightStr(AText, LEndPos + 1 - LLineBeginPos) + LLineEnd);
        Result := TextPosition(1 + LEndPos + 1 - LLineBeginPos, LLine);
      end;

    finally
      EndUpdate();

      if ((lsLoading in State) and (LLineBreak[0] <> #0)) then
        LineBreak := StrPas(PChar(@LLineBreak[0]));
    end;
  end;
end;

procedure TBCEditorLines.DoPut(ALine: Integer; const AText: string);
var
  LModified: Boolean;
  LLength: Integer;
begin
  if (not (loTrimTrailingSpaces in Options) or (lsLoading in State)) then
    LLength := 0 // ... to avoid compiler warning only
  else
  begin
    LLength := Length(AText);
    while ((LLength > 0) and (AText[LLength] = BCEDITOR_SPACE_CHAR)) do
      Dec(LLength);
  end;

  if ((ALine = 0) and (Count = 0)) then
  begin
    if ((loTrimTrailingSpaces in Options) and not (lsLoading in State) and (LLength < Length(AText))) then
      DoInsert(0, Copy(AText, 1, LLength))
    else
      DoInsert(0, AText);
    LModified := True;
  end
  else
  begin
    Assert((0 <= ALine) and (ALine < Count));

    Lines[ALine].Flags := Lines[ALine].Flags - [sfHasTabs, sfHasNoTabs];
    LModified := False;
    if ((loTrimTrailingSpaces in Options) and not (lsLoading in State) and (LLength < Length(AText))) then
    begin
      if (Copy(AText, 1, LLength) <> Lines[ALine].Text) then
      begin
        Lines[ALine].Text := Copy(AText, 1, LLength);
        Lines[ALine].Attribute.LineState := lsModified;
        LModified := True;
      end;
    end
    else
    begin
      if (AText <> Lines[ALine].Text) then
      begin
        Lines[ALine].Text := AText;
        Lines[ALine].Attribute.LineState := lsModified;
        LModified := True;
      end
    end;

    if (LModified and (FMaxLengthLine >= 0)) then
      if (ExpandedStringLengths[ALine] >= Lines[FMaxLengthLine].ExpandedLength) then
        FMaxLengthLine := ALine
      else if (ALine = FMaxLengthLine) then
        FMaxLengthLine := -1;
  end;

  if (LModified) then
  begin
    if (UpdateCount > 0) then
      Include(FState, lsTextChanged);
    if (Assigned(OnUpdated)) then
      OnUpdated(Self, ALine);
  end;
end;

procedure TBCEditorLines.ExchangeItems(ALine1, ALine2: Integer);
var
  LLine: TLine;
begin
  LLine := Lines[ALine1];
  Lines[ALine1] := Lines[ALine2];
  Lines[ALine2] := LLine;
end;

procedure TBCEditorLines.ExecuteUndoRedo(const List: TUndoList);
var
  LPreviousBlockNumber: Integer;
  LCaretPosition: TBCEditorTextPosition;
  LDestinationList: TUndoList;
  LEndPosition: TBCEditorTextPosition;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
  LSelMode: TBCEditorSelectionMode;
  LText: string;
  LUndoItem: TUndoList.PItem;
begin
  if (not ReadOnly and List.PopItem(LUndoItem)) then
  begin
    if (List = UndoList) then
    begin
      Include(FState, lsUndo);
      LDestinationList := RedoList;
    end
    else
    begin
      Include(FState, lsRedo);
      LDestinationList := UndoList;
    end;

    BeginUpdate();

    LCaretPosition := CaretPosition;
    LSelBeginPosition := SelBeginPosition;
    LSelEndPosition := SelEndPosition;
    LSelMode := SelMode;

    repeat
      case (LUndoItem^.UndoType) of
        utSelection:
          begin
            LDestinationList.PushItem(LUndoItem^.UndoType, LCaretPosition,
              LSelBeginPosition, LSelEndPosition, LSelMode,
              LUndoItem^.BeginPosition, LUndoItem^.EndPosition, LUndoItem^.Text, LUndoItem^.BlockNumber);
          end;
        utInsert,
        utReplace,
        utBackspace,
        utDelete:
          begin
            if ((LUndoItem^.BeginPosition <> LUndoItem^.EndPosition)
             and ((LUndoItem^.UndoType in [utReplace])
               or ((LUndoItem^.UndoType in [utBackspace, utDelete]) xor (List = UndoList)))) then
            begin
              LText := GetTextBetween(LUndoItem^.BeginPosition, LUndoItem^.EndPosition);
              DoDeleteText(LUndoItem^.BeginPosition, LUndoItem^.EndPosition);
              if (not (LUndoItem^.UndoType in [utReplace])) then
                LDestinationList.PushItem(LUndoItem^.UndoType, LCaretPosition,
                  LSelBeginPosition, LSelEndPosition, LSelMode,
                  LUndoItem^.BeginPosition, LUndoItem^.EndPosition, LText, LUndoItem^.BlockNumber);
            end
            else
              LText := '';
            if ((LUndoItem^.UndoType in [utReplace])
                or ((LUndoItem^.UndoType in [utBackspace, utDelete]) xor (List <> UndoList))) then
            begin
              if (LUndoItem^.Text = '') then
                LEndPosition := LUndoItem^.BeginPosition
              else
                LEndPosition := DoInsertText(LUndoItem^.BeginPosition, LUndoItem^.Text);
              LDestinationList.PushItem(LUndoItem^.UndoType, LCaretPosition,
                LSelBeginPosition, LSelEndPosition, LSelMode,
                LUndoItem^.BeginPosition, LEndPosition, LText, LUndoItem^.BlockNumber);
            end;
          end;
        utClear:
          if (List = RedoList) then
          begin
            LText := Text;
            FCount := 0;
            LDestinationList.PushItem(LUndoItem^.UndoType, LCaretPosition,
              LSelBeginPosition, LSelEndPosition, LSelMode,
              BOFTextPosition, InvalidTextPosition, LText, LUndoItem^.BlockNumber);
          end
          else
          begin
            LEndPosition := DoInsertText(LUndoItem^.BeginPosition, LUndoItem^.Text);
            LDestinationList.PushItem(LUndoItem^.UndoType, LCaretPosition,
              LSelBeginPosition, LSelEndPosition, LSelMode,
              LUndoItem^.BeginPosition, LEndPosition, '', LUndoItem^.BlockNumber);
          end;
        utInsertIndent,
        utDeleteIndent:
          begin
            if ((LUndoItem^.UndoType <> utInsertIndent) xor (List = UndoList)) then
              DoDeleteIndent(LUndoItem^.BeginPosition, LUndoItem^.EndPosition,
                LUndoItem^.Text, LUndoItem^.SelMode)
            else
              DoInsertIndent(LUndoItem^.BeginPosition, LUndoItem^.EndPosition,
                LUndoItem^.Text, LUndoItem^.SelMode);
            LDestinationList.PushItem(LUndoItem^.UndoType, LCaretPosition,
              LSelBeginPosition, LSelEndPosition, LSelMode,
              LUndoItem^.BeginPosition, LUndoItem^.EndPosition, LUndoItem^.Text, LUndoItem^.BlockNumber);
          end;
        else raise ERangeError.Create('UndoType: ' + IntToStr(Ord(LUndoItem^.UndoType)));
      end;

      LCaretPosition := LUndoItem^.CaretPosition;
      LSelBeginPosition := LUndoItem^.SelBeginPosition;
      LSelEndPosition := LUndoItem^.SelEndPosition;
      LSelMode := LUndoItem^.SelMode;

      LPreviousBlockNumber := LUndoItem^.BlockNumber;
    until (not List.PeekItem(LUndoItem)
      or (LUndoItem^.BlockNumber <> LPreviousBlockNumber)
      or not List.PopItem(LUndoItem));

    CaretPosition := LCaretPosition;
    SelBeginPosition := LSelBeginPosition;
    SelEndPosition := LSelEndPosition;
    SelMode := LSelMode;

    EndUpdate();

    if (List = UndoList) then
      Exclude(FState, lsUndo)
    else
      Exclude(FState, lsRedo);
  end;
end;

function TBCEditorLines.Get(ALine: Integer): string;
begin
 if ((ALine = 0) and (Count = 0)) then
    Result := ''
  else
  begin
    Assert((0 <= ALine) and (ALine < Count), 'Line: ' + IntToStr(ALine) + ', Count: ' + IntToStr(Count));

    Result := Lines[ALine].Text;
  end;
end;

function TBCEditorLines.GetAttributes(ALine: Integer): PLineAttribute;
begin
  if ((ALine = 0) and (Count = 0)) then
    Result := nil
  else
  begin
    Assert((0 <= ALine) and (ALine < Count));

    Result := @Lines[ALine].Attribute;
  end;
end;

function TBCEditorLines.GetBOLTextPosition(ALine: Integer): TBCEditorTextPosition;
begin
  Assert((0 <= ALine) and (ALine < Count) or (ALine = 0) and (Count = 0));

  Result := TextPosition(1, ALine);
end;

function TBCEditorLines.GetCanRedo(): Boolean;
begin
  Result := RedoList.Count > 0;
end;

function TBCEditorLines.GetCanUndo(): Boolean;
begin
  Result := UndoList.Count > 0;
end;

function TBCEditorLines.GetCapacity(): Integer;
begin
  Result := FCapacity;
end;

function TBCEditorLines.GetCount(): Integer;
begin
  Result := FCount;
end;

function TBCEditorLines.GetEOFPosition(): TBCEditorTextPosition;
begin
  if (Count = 0) then
    Result := BOFTextPosition
  else
    Result := EOLTextPosition[Count - 1];
end;

function TBCEditorLines.GetEOLTextPosition(ALine: Integer): TBCEditorTextPosition;
begin
  Assert((0 <= ALine) and (ALine < Count) or (ALine = 0) and (Count = 0));

  if (Count = 0) then
    Result := BOFTextPosition
  else
    Result := TextPosition(1 + Length(Lines[ALine].Text), ALine);
end;

function TBCEditorLines.GetExpandedString(ALine: Integer): string;
begin
  if ((ALine = 0) and (Count = 0)) then
    Result := ''
  else if (sfHasNoTabs in Lines[ALine].Flags) then
    Result := Lines[ALine].Text
  else
    Result := CalcExpandString(ALine);
end;

function TBCEditorLines.GetExpandedStringLength(ALine: Integer): Integer;
begin
  if ((ALine = 0) and (Count = 0)) then
    Result := 0
  else
  begin
    if (Lines[ALine].ExpandedLength >= 0) then
      Lines[ALine].ExpandedLength := Length(ExpandedStrings[ALine]);
    Result := Lines[ALine].ExpandedLength;
  end;
end;

function TBCEditorLines.GetMaxLength(): Integer;
var
  I: Integer;
  LMaxLength: Integer;
  Line: ^TLine;
begin
  if (FMaxLengthLine < 0) then
  begin
    LMaxLength := 0;
    if (FCount > 0) then
    begin
      Line := @Lines[0];
      for I := 0 to Count - 1 do
      begin
        if (Line^.ExpandedLength < 0) then
          CalcExpandString(I);
        if (Line^.ExpandedLength > LMaxLength) then
        begin
          LMaxLength := Line^.ExpandedLength;
          FMaxLengthLine := I;
        end;
        Inc(Line);
      end;
    end;
  end;

  if (FMaxLengthLine < 0) then
    Result := 0
  else
    Result := Lines[FMaxLengthLine].ExpandedLength;
end;

function TBCEditorLines.GetRange(ALine: Integer): TRange;
begin
  if ((ALine = 0) and (Count = 0)) then
    Result := nil
  else
  begin
    Assert((0 <= ALine) and (ALine < Count));

    Result := Lines[ALine].Range;
  end;
end;

function TBCEditorLines.GetTextBetween(const ABeginPosition, AEndPosition: TBCEditorTextPosition): string;
var
  LLine: Integer;
  StringBuilder: TStringBuilder;
begin
  Assert((BOFTextPosition <= ABeginPosition) and (AEndPosition <= EOFTextPosition));
  Assert((AEndPosition = BOFTextPosition) or (AEndPosition.Char - 1 <= Length(Lines[AEndPosition.Line].Text)));
  Assert(ABeginPosition <= AEndPosition);

  if (ABeginPosition = AEndPosition) then
    Result := ''
  else if (ABeginPosition.Line = AEndPosition.Line) then
    Result := Copy(Lines[ABeginPosition.Line].Text, ABeginPosition.Char, AEndPosition.Char - ABeginPosition.Char)
  else
  begin
    StringBuilder := TStringBuilder.Create();

    StringBuilder.Append(Lines[ABeginPosition.Line].Text, ABeginPosition.Char - 1, Length(Lines[ABeginPosition.Line].Text) - (ABeginPosition.Char - 1));
    for LLine := ABeginPosition.Line + 1 to AEndPosition.Line - 1 do
    begin
      StringBuilder.Append(LineBreak);
      StringBuilder.Append(Lines[LLine].Text);
    end;
    StringBuilder.Append(LineBreak);
    StringBuilder.Append(Lines[AEndPosition.Line].Text, 0, AEndPosition.Char - 1);

    Result := StringBuilder.ToString();

    StringBuilder.Free();
  end;
end;

function TBCEditorLines.GetTextBetweenColumn(const ABeginPosition, AEndPosition: TBCEditorTextPosition): string;
var
  StringBuilder: TStringBuilder;
  LBeginPosition: TBCEditorTextPosition;
  LEndPosition: TBCEditorTextPosition;
  LLine: Integer;
begin
  Assert(ABeginPosition <= LEndPosition);
  Assert(ABeginPosition.Char - 1 <= Length(Lines[ABeginPosition.Line].Text));

  LBeginPosition := Min(ABeginPosition, LEndPosition);
  LEndPosition := Max(ABeginPosition, LEndPosition);

  if (LBeginPosition = LEndPosition) then
    Result := ''
  else if (LBeginPosition.Line = LEndPosition.Line) then
    Result := Copy(Lines[LBeginPosition.Line].Text, LBeginPosition.Char, LEndPosition.Char - LBeginPosition.Char)
  else
  begin
    StringBuilder := TStringBuilder.Create();

    for LLine := LBeginPosition.Line to LEndPosition.Line do
    begin
      if (Length(Lines[LBeginPosition.Line].Text) < LBeginPosition.Char - 1) then
        // Do nothing
      else if (Length(Lines[LBeginPosition.Line].Text) < LEndPosition.Char) then
        StringBuilder.Append(Copy(Lines[LBeginPosition.Line].Text, LBeginPosition.Char - 1, Length(Lines[LBeginPosition.Line].Text) - (LBeginPosition.Char - 1)))
      else
        StringBuilder.Append(Copy(Lines[LBeginPosition.Line].Text, LBeginPosition.Char - 1, LEndPosition.Char - LBeginPosition.Char + 1));
      if (LLine < LEndPosition.Line) then
        StringBuilder.Append(LineBreak);
    end;

    Result := StringBuilder.ToString();

    StringBuilder.Free();
  end;
end;

function TBCEditorLines.GetTextLength(): Integer;
var
  i: Integer;
  LLineBreakLength: Integer;
begin
  Result := 0;
  LLineBreakLength := Length(LineBreak);
  for i := 0 to FCount - 1 do
  begin
    if i = FCount - 1 then
      LLineBreakLength := 0;
    Inc(Result, Length(Lines[i].Text) + LLineBreakLength)
  end;
end;

function TBCEditorLines.GetTextStr: string;
begin
  Result := GetTextBetween(BOFTextPosition, EOFTextPosition);
end;

procedure TBCEditorLines.Grow();
begin
  if Capacity > 64 then
    Capacity := Capacity + FCapacity div 4
  else
    Capacity := Capacity + 16;
end;

procedure TBCEditorLines.Insert(ALine: Integer; const AText: string);
var
  LCaretPosition: TBCEditorTextPosition;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
begin
  LCaretPosition := CaretPosition;
  LSelBeginPosition := SelBeginPosition;
  LSelEndPosition := SelEndPosition;

  DoInsert(ALine, AText);

  if (not (lsLoading in State)) then
  begin
    UndoList.PushItem(utInsert, LCaretPosition,
      LSelBeginPosition, LSelEndPosition, SelMode,
      BOLTextPosition[ALine], TextPosition(1 + Length(AText), ALine));

    RedoList.Clear();
  end;
end;

procedure TBCEditorLines.InsertIndent(ABeginPosition, AEndPosition: TBCEditorTextPosition;
  const AIndentText: string; const ASelMode: TBCEditorSelectionMode);
var
  LBeginPosition: TBCEditorTextPosition;
  LCaretPosition: TBCEditorTextPosition;
  LEndPosition: TBCEditorTextPosition;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
begin
  LBeginPosition := Min(ABeginPosition, AEndPosition);
  LEndPosition := Max(ABeginPosition, AEndPosition);

  LCaretPosition := CaretPosition;
  LSelBeginPosition := SelBeginPosition;
  LSelEndPosition := SelBeginPosition;

  DoInsertIndent(LBeginPosition, LEndPosition, AIndentText, ASelMode);

  UndoList.PushItem(utInsertIndent, LCaretPosition,
    LSelBeginPosition, LSelEndPosition, SelMode,
    LBeginPosition, LEndPosition, AIndentText);

  RedoList.Clear();
end;

procedure TBCEditorLines.InsertText(ABeginPosition, AEndPosition: TBCEditorTextPosition;
  const AText: string);
var
  LCaretPosition: TBCEditorTextPosition;
  LDeleteText: string;
  LEndPos: PChar;
  LInsertBeginPosition: TBCEditorTextPosition;
  LInsertEndPosition: TBCEditorTextPosition;
  LInsertText: string;
  LLine: Integer;
  LLineBeginPos: PChar;
  LLineLength: Integer;
  LPos: PChar;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
begin
  Assert(ABeginPosition.Char < AEndPosition.Char);
  Assert(ABeginPosition.Line <= AEndPosition.Line);

  LCaretPosition := CaretPosition;
  LSelBeginPosition := SelBeginPosition;
  LSelEndPosition := SelEndPosition;

  BeginUpdate();

  try
    LPos := PChar(AText);
    LEndPos := @LPos[Length(AText)];
    LLine := ABeginPosition.Line;

    while ((LPos <= LEndPos) or (LLine <= AEndPosition.Line)) do
    begin
      LLineBeginPos := LPos;
      while ((LPos <= LEndPos) and not CharInSet(LPos^, [BCEDITOR_LINEFEED, BCEDITOR_CARRIAGE_RETURN])) do
        Inc(LPos);

      LLineLength := Length(Lines[LLine].Text);
      SetString(LInsertText, LLineBeginPos, LPos - LLineBeginPos);
      if (LLineLength < ABeginPosition.Char - 1) then
      begin
        LInsertText := StringOfChar(BCEDITOR_SPACE_CHAR, ABeginPosition.Char - 1 - LLineLength) + LInsertText;

        LInsertBeginPosition := TextPosition(1 + LLineLength, LLine);
        LInsertEndPosition := InsertText(LInsertBeginPosition, LInsertText);

        UndoList.PushItem(utInsert, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          LInsertBeginPosition, LInsertEndPosition);
      end
      else if (LLineLength < AEndPosition.Char - 1) then
      begin
        LInsertBeginPosition := TextPosition(ABeginPosition.Char, LLine);

        LDeleteText := GetTextBetween(LInsertBeginPosition, TextPosition(1 + LLineLength, LLine));
        DeleteText(LInsertBeginPosition, LInsertEndPosition);

        UndoList.PushItem(utDelete, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          LInsertBeginPosition, InvalidTextPosition, LDeleteText);

        if (LPos > LLineBeginPos) then
        begin
          LInsertEndPosition := InsertText(LInsertBeginPosition, LInsertText);

          UndoList.PushItem(utInsert, InvalidTextPosition,
            InvalidTextPosition, InvalidTextPosition, SelMode,
            LInsertBeginPosition, LInsertEndPosition);
        end;
      end
      else
      begin
        LInsertBeginPosition := TextPosition(ABeginPosition.Char, LLine);
        LInsertEndPosition := TextPosition(AEndPosition.Char, LLine);

        LDeleteText := GetTextBetween(LInsertBeginPosition, LInsertEndPosition);
        DeleteText(LInsertBeginPosition, LInsertEndPosition);

        UndoList.PushItem(utDelete, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          LInsertBeginPosition, InvalidTextPosition, LDeleteText);

        if (LPos > LLineBeginPos) then
        begin
          LInsertEndPosition := InsertText(LInsertBeginPosition, LeftStr(LInsertText, AEndPosition.Char - ABeginPosition.Char));

          UndoList.PushItem(utInsert, LCaretPosition,
            InvalidTextPosition, InvalidTextPosition, SelMode,
            LInsertBeginPosition, LInsertEndPosition);
        end;
      end;

      if ((LPos <= LEndPos) and (LPos^ = BCEDITOR_LINEFEED)) then
        Inc(LPos)
      else if ((LPos <= LEndPos) and (LPos^ = BCEDITOR_CARRIAGE_RETURN)) then
      begin
        Inc(LPos);
        if ((LPos <= LEndPos) and (LPos^ = BCEDITOR_LINEFEED)) then
          Inc(LPos);
      end;

      Inc(LLine);
    end;

  finally
    RedoList.Clear();
    EndUpdate();
  end;
end;

function TBCEditorLines.InsertText(APosition: TBCEditorTextPosition;
  const AText: string): TBCEditorTextPosition;
var
  LCaretPosition: TBCEditorTextPosition;
  LPosition: TBCEditorTextPosition;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
begin
  UndoList.BeginUpdate();
  try
    if (AText = '') then
      Result := APosition
    else
    begin
      LCaretPosition := CaretPosition;
      LSelBeginPosition := SelBeginPosition;
      LSelEndPosition := SelEndPosition;
      if ((APosition.Line = Count) and (APosition.Char - 1 > 0)
        or (APosition.Line < Count) and (APosition.Char - 1 > Length(Lines[APosition.Line].Text))) then
      begin
        LPosition := EOLTextPosition[APosition.Line];

        Result := DoInsertText(LPosition, StringOfChar(BCEDITOR_SPACE_CHAR, APosition.Char - LPosition.Char) + AText);

        UndoList.PushItem(utInsert, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          LPosition, Result);
      end
      else
      begin
        Result := DoInsertText(APosition, AText);

        UndoList.PushItem(utInsert, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          APosition, Result);
      end;
    end;

    if (SelMode = smNormal) then
      CaretPosition := Result;
  finally
    UndoList.EndUpdate();
    RedoList.Clear();
  end;
end;

procedure TBCEditorLines.InternalClear(const AClearUndo: Boolean);
begin
  if (AClearUndo) then
    ClearUndo();

  FMaxLengthLine := -1;
  LineBreak := BCEDITOR_CARRIAGE_RETURN + BCEDITOR_LINEFEED;
  if (Capacity > 0) then
  begin
    FCaretPosition := BOFTextPosition;
    FSelBeginPosition := BOFTextPosition;
    FSelEndPosition := BOFTextPosition;
    Capacity := 0;
    if (Assigned(OnCleared)) then
      OnCleared(Self);
  end;
end;

function TBCEditorLines.IsPositionInSelection(const APosition: TBCEditorTextPosition): Boolean;
begin
  if (SelMode = smNormal) then
    Result := (SelBeginPosition <= APosition) and (APosition <= SelEndPosition)
  else
    Result := (SelBeginPosition.Char <= APosition.Char) and (APosition.Char <= SelEndPosition.Char)
      and (SelBeginPosition.Line <= APosition.Line) and (APosition.Line <= SelEndPosition.Line);
end;

procedure TBCEditorLines.Put(ALine: Integer; const AText: string);
begin
  if ((FCount = 0) and (ALine = 0)) then
    Add(AText)
  else if (AText <> Lines[ALine].Text) then
    ReplaceText(BOLTextPosition[ALine], EOLTextPosition[ALine], AText);
end;

procedure TBCEditorLines.PutAttributes(ALine: Integer; const AValue: PLineAttribute);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Lines[ALine].Attribute := AValue^;
end;

procedure TBCEditorLines.PutRange(ALine: Integer; ARange: TRange);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Lines[ALine].Range := ARange;
end;

procedure TBCEditorLines.QuickSort(ALeft, ARight: Integer; ACompare: TCompare);
var
  LLeft: Integer;
  LMiddle: Integer;
  LRight: Integer;
begin
  repeat
    LLeft := ALeft;
    LRight := ARight;
    LMiddle := (ALeft + ARight) shr 1;
    repeat
      while ACompare(Self, LLeft, LMiddle) < 0 do
        Inc(LLeft);
      while ACompare(Self, LRight, LMiddle) > 0 do
        Dec(LRight);
      if LLeft <= LRight then
      begin
        if LLeft <> LRight then
          ExchangeItems(LLeft, LRight);
        if LMiddle = LLeft then
          LMiddle := LRight
        else
        if LMiddle = LRight then
          LMiddle := LLeft;
        Inc(LLeft);
        Dec(LRight);
      end;
    until LLeft > LRight;
    if ALeft < LRight then
      QuickSort(ALeft, LRight, ACompare);
    ALeft := LLeft;
  until LLeft >= ARight;
end;

procedure TBCEditorLines.Redo();
begin
  ExecuteUndoRedo(RedoList);
end;

function TBCEditorLines.ReplaceText(ABeginPosition, AEndPosition: TBCEditorTextPosition;
  const AText: string): TBCEditorTextPosition;
var
  LCaretPosition: TBCEditorTextPosition;
  LText: string;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
begin
  UndoList.BeginUpdate();
  try
    LCaretPosition := CaretPosition;
    LSelBeginPosition := SelBeginPosition;
    LSelEndPosition := SelEndPosition;

    LText := TextBetween[ABeginPosition, AEndPosition];

    DoDeleteText(ABeginPosition, AEndPosition);
    Result := DoInsertText(ABeginPosition, AText);

    UndoList.PushItem(utReplace, LCaretPosition,
      LSelBeginPosition, LSelEndPosition, SelMode,
      ABeginPosition, Result, LText);

    CaretPosition := Result;
  finally
    UndoList.EndUpdate();
  end;
end;

procedure TBCEditorLines.SaveToStream(AStream: TStream; AEncoding: TEncoding);
begin
  inherited;

  if (not (loUndoAfterSave in Options)) then
  begin
    UndoList.Clear();
    RedoList.Clear();
  end;
end;

procedure TBCEditorLines.SetCapacity(AValue: Integer);
begin
  Assert(AValue >= 0);

  if (AValue <> FCapacity) then
  begin
    SetLength(FLines, AValue);
    FCapacity := AValue;
    FCount := Min(FCount, FCapacity);
  end;
end;

procedure TBCEditorLines.SetCaretPosition(const AValue: TBCEditorTextPosition);
begin
  Assert((BOFTextPosition <= AValue) and ((AValue.Line < Count) or (AValue.Line = 0) and (Count = 0)));

  if (AValue <> FCaretPosition) then
  begin
    BeginUpdate();

    FCaretPosition := AValue;

    SelBeginPosition := AValue;

    Include(FState, lsCaretMoved);
    EndUpdate();
  end
  else
    SelBeginPosition := AValue;
end;

procedure TBCEditorLines.SetModified(const AValue: Boolean);
var
  LLine: Integer;
begin
  if (FModified <> AValue) then
  begin
    FModified := AValue;

    if (not FModified) then
    begin
      UndoList.GroupBreak();

      BeginUpdate();
      for LLine := 0 to Count - 1 do
        if (Lines[LLine].Attribute.LineState = lsModified) then
          Lines[LLine].Attribute.LineState := lsSaved;
      EndUpdate();
      Editor.Invalidate();
    end;
  end;
end;

procedure TBCEditorLines.SetOptions(const AValue: TOptions);
var
  LLine: Integer;
begin
  if (not (loTrimTrailingSpaces in Options) and (loTrimTrailingSpaces in Options)) then
    for LLine := 0 to Count - 1 do
      if ((Lines[LLine].Text <> '') and (Lines[LLine].Text[Length(Lines[LLine].Text)] = BCEDITOR_SPACE_CHAR)) then
        DoPut(LLine, Lines[LLine].Text);

  FOptions := AValue;
end;

procedure TBCEditorLines.SetSelBeginPosition(const AValue: TBCEditorTextPosition);
begin
  Assert((BOFTextPosition <= AValue) and ((AValue.Line < Count) or (AValue.Line = 0) and (Count = 0)));

  if (AValue <> FSelBeginPosition) then
  begin
    BeginUpdate();

    FSelBeginPosition := AValue;
    if (SelMode = smNormal) then
      if (Count = 0) then
        FSelBeginPosition := BOFTextPosition
      else if (FSelBeginPosition.Char - 1 > Length(Lines[FSelBeginPosition.Line].Text)) then
        FSelBeginPosition.Char := 1 + Length(Lines[FSelBeginPosition.Line].Text);

    SelEndPosition := AValue;

    Include(FState, lsSelChanged);
    EndUpdate();
  end
  else
    SelEndPosition := AValue;
end;

procedure TBCEditorLines.SetSelEndPosition(const AValue: TBCEditorTextPosition);
begin
  Assert((BOFTextPosition <= AValue) and ((AValue.Line < Count) or (AValue.Line = 0) and (Count = 0)));

  if (AValue <> FSelEndPosition) then
  begin
    BeginUpdate();

    FSelEndPosition := AValue;
    if (SelMode = smNormal) then
      if (Count = 0) then
        FSelEndPosition := BOFTextPosition
      else if (FSelEndPosition.Char - 1 > Length(Lines[FSelEndPosition.Line].Text)) then
        FSelEndPosition.Char := 1 + Length(Lines[FSelEndPosition.Line].Text);

    Include(FState, lsSelChanged);
    EndUpdate();
  end;
end;

procedure TBCEditorLines.SetTabWidth(const AValue: Integer);
var
  LIndex: Integer;
begin
  if FTabWidth <> AValue then
  begin
    FTabWidth := AValue;
    FMaxLengthLine := -1;
    for LIndex := 0 to FCount - 1 do
      with Lines[LIndex] do
      begin
        ExpandedLength := -1;
        Exclude(Flags, sfHasNoTabs);
      end;
  end;
end;

procedure TBCEditorLines.SetTextStr(const AValue: string);
var
  LEndPosition: TBCEditorTextPosition;
  LLine: Integer;
begin
  Include(FState, lsLoading);

  if (not (csReading in Editor.ComponentState) and Assigned(OnBeforeLoad)) then
    OnBeforeLoad(Self);

  BeginUpdate();

  if (loUndoAfterLoad in Options) then
    DeleteText(BOFTextPosition, EOFTextPosition);

  InternalClear(not (loUndoAfterLoad in Options));

  LEndPosition := InsertText(BOFTextPosition, AValue);
  for LLine := 0 to Count - 1 do
    Attributes[LLine].LineState := lsLoaded;

  if (loUndoAfterLoad in Options) then
  begin
    UndoList.PushItem(utInsert, BOFTextPosition,
      InvalidTextPosition, InvalidTextPosition, SelMode,
      BOFTextPosition, LEndPosition);

    RedoList.Clear();
  end;

  CaretPosition := BOFTextPosition;

  EndUpdate();

  if (not (csReading in Editor.ComponentState) and Assigned(OnAfterLoad)) then
    OnAfterLoad(Self);

  Exclude(FState, lsLoading);
end;

procedure TBCEditorLines.SetUpdateState(AUpdating: Boolean);
begin
  if (AUpdating) then
  begin
    UndoList.BeginUpdate();
    FState := FState - [lsCaretMoved, lsSelChanged, lsTextChanged];
    FOldUndoListCount := UndoList.Count;
    FOldCaretPosition := CaretPosition;
    FOldSelBeginPosition := SelBeginPosition;
    FOldSelEndPosition := SelEndPosition;
  end
  else
  begin
    if (not (lsRedo in State) and ((lsCaretMoved in State) or (lsSelChanged in State)) and not UndoList.Updated) then
    begin
      if (not (lsUndo in State)) then
      begin
        if ((UndoList.Count = FOldUndoListCount)
          and (CaretPosition <> FOldCaretPosition)
            or (SelBeginPosition <> FOldSelBeginPosition)
            or (SelEndPosition <> FOldSelBeginPosition)) then
          UndoList.PushItem(utSelection, FOldCaretPosition,
            FOldSelBeginPosition, FOldSelEndPosition, SelMode,
            InvalidTextPosition, InvalidTextPosition);
        RedoList.Clear();
      end;
    end;

    UndoList.EndUpdate();

    if (Assigned(OnCaretMoved) and (lsCaretMoved in FState)) then
      OnCaretMoved(Self);
    if (Assigned(OnSelChange) and (lsSelChanged in FState)) then
      OnSelChange(Self);

    FState := FState - [lsCaretMoved, lsSelChanged, lsTextChanged];
  end;
end;

procedure TBCEditorLines.Sort(const ABeginLine, AEndLine: Integer);
begin
  CustomSort(ABeginLine, AEndLine, CompareLines);
end;

function TBCEditorLines.PositionToCharIndex(const APosition: TBCEditorTextPosition): Integer;
var
  LLine: Integer;
  LLineBreakLength: Integer;
begin
  LLineBreakLength := Length(LineBreak);
  Result := 0;
  for LLine := 0 to APosition.Line - 1 do
  begin
    Inc(Result, Length(Lines[LLine].Text));
    Inc(Result, LLineBreakLength);
  end;
  Inc(Result, APosition.Char - 1);
end;

procedure TBCEditorLines.Undo();
begin
  ExecuteUndoRedo(UndoList);
end;

procedure TBCEditorLines.UndoGroupBreak();
begin
  if ((loUndoGrouped in Options) and CanUndo) then
    UndoList.GroupBreak();
end;

end.

