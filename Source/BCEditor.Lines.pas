unit BCEditor.Lines;

interface {********************************************************************}

uses
  SysUtils, Classes, Generics.Collections,
  Graphics, Controls,
  BCEditor.Utils, BCEditor.Consts, BCEditor.Types;

type
  TBCEditorLines = class(TStrings)
  protected type
    TChangeEvent = procedure(Sender: TObject; const Line: Integer) of object;
    TCompare = function(Lines: TBCEditorLines; Line1, Line2: Integer): Integer;

    TOption = (loColumnTabs, loTrimTrailingSpaces, loTrimTrailingLines,
      loUndoGrouped, loUndoAfterLoad, loUndoAfterSave);
    TOptions = set of TOption;

    TLine = packed record
    type
      TState = (lsLoaded, lsModified, lsSaved);
    public
      Background: TColor;
      ExpandedLength: Integer;
      Flags: set of (sfHasTabs, sfHasNoTabs);
      FirstRow: Integer;
      Foreground: TColor;
      Range: Pointer;
      State: TState;
      Text: string;
    end;
    TLines = TList<TLine>;

    TState = set of (lsLoading, lsSaving, lsDontTrim, lsUndo, lsRedo, lsCaretMoved,
      lsSelChanged, lsTextChanged, lsInsert);

    TUndoItem = packed record
    type
      TType = (utSelection, utInsert, utReplace, utBackspace, utDelete,
        utClear, utInsertIndent, utDeleteIndent);
    public
      BlockNumber: Integer;
      UndoType: TType;
      CaretPosition: TBCEditorTextPosition;
      SelBeginPosition: TBCEditorTextPosition;
      SelEndPosition: TBCEditorTextPosition;
      SelMode: TBCEditorSelectionMode;
      BeginPosition: TBCEditorTextPosition;
      EndPosition: TBCEditorTextPosition;
      Text: string;
    end;

    TUndoList = class(TList<TUndoItem>)
    strict private
      FBlockNumber: Integer;
      FChanges: Integer;
      FCurrentBlockNumber: Integer;
      FGroupBreak: Boolean;
      FLines: TBCEditorLines;
      FUpdateCount: Integer;
      function GetUpdated(): Boolean;
    protected
      procedure BeginUpdate();
      procedure Clear();
      constructor Create(const ALines: TBCEditorLines);
      procedure EndUpdate();
      procedure GroupBreak();
      function Peek(): TUndoItem;
      function Pop(): TUndoItem;
      procedure Push(const AUndoType: TUndoItem.TType; const ACaretPosition: TBCEditorTextPosition;
        const ASelBeginPosition, ASelEndPosition: TBCEditorTextPosition; const ASelMode: TBCEditorSelectionMode;
        const ABeginPosition, AEndPosition: TBCEditorTextPosition; const AText: string = '';
        const ABlockNumber: Integer = 0); overload;
      property Changes: Integer read FChanges;
      property Lines: TBCEditorLines read FLines;
      property Updated: Boolean read GetUpdated;
    public
      property UpdateCount: Integer read FUpdateCount;
    end;

  const
    BOFPosition: TBCEditorTextPosition = (Char: 0; Line: 0);
    InvalidPosition: TBCEditorTextPosition = (Char: -1; Line: -1);
  strict private const
    DefaultOptions = [loUndoGrouped];
  strict private
    FCaretPosition: TBCEditorTextPosition;
    FCaseSensitive: Boolean;
    FEditor: TCustomControl;
    FLines: TLines;
    FMaxLengthLine: Integer;
    FModified: Boolean;
    FOldCaretPosition: TBCEditorTextPosition;
    FOldSelBeginPosition: TBCEditorTextPosition;
    FOldSelEndPosition: TBCEditorTextPosition;
    FOldUndoListCount: Integer;
    FOnAfterUpdate: TNotifyEvent;
    FOnBeforeUpdate: TNotifyEvent;
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
    procedure ExecuteUndoRedo(const List: TUndoList);
    function GetBackground(ALine: Integer): TColor; inline;
    function GetBOLPosition(ALine: Integer): TBCEditorTextPosition; inline;
    function GetCanRedo(): Boolean;
    function GetCanUndo(): Boolean;
    function GetEOFPosition(): TBCEditorTextPosition;
    function GetEOLPosition(ALine: Integer): TBCEditorTextPosition; inline;
    function GetExpandedString(ALine: Integer): string;
    function GetExpandedStringLength(ALine: Integer): Integer;
    function GetFirstRow(ALine: Integer): Integer; inline;
    function GetForeground(ALine: Integer): TColor; inline;
    function GetLineState(ALine: Integer): TLine.TState; inline;
    function GetMaxLength(): Integer;
    function GetRange(ALine: Integer): Pointer;
    function GetTextBetween(const ABeginPosition, AEndPosition: TBCEditorTextPosition): string; overload;
    function GetTextBetweenColumn(const ABeginPosition, AEndPosition: TBCEditorTextPosition): string; overload;
    procedure InternalClear(const AClearUndo: Boolean); overload;
    procedure PutBackground(ALine: Integer; AValue: TColor); inline;
    procedure PutForeground(ALine: Integer; AValue: TColor); inline;
    procedure PutRange(ALine: Integer; ARange: Pointer); inline;
    procedure PutFirstRow(ALine: Integer; const ARow: Integer); inline;
    procedure SetCaretPosition(const AValue: TBCEditorTextPosition);
    procedure SetModified(const AValue: Boolean);
    procedure SetSelBeginPosition(const AValue: TBCEditorTextPosition);
    procedure SetSelEndPosition(const AValue: TBCEditorTextPosition);
    procedure QuickSort(ALeft, ARight: Integer; ACompare: TCompare);
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
    function GetCount(): Integer; override;
    function GetTextLength(): Integer;
    function GetTextStr(): string; override;
    procedure InsertIndent(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const AIndentText: string; const ASelMode: TBCEditorSelectionMode);
    procedure InsertText(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const AText: string); overload;
    function InsertText(APosition: TBCEditorTextPosition;
      const AText: string): TBCEditorTextPosition; overload;
    function IsPositionInSelection(const APosition: TBCEditorTextPosition): Boolean;
    function PositionToCharIndex(const APosition: TBCEditorTextPosition): Integer;
    procedure Put(ALine: Integer; const AText: string); override;
    procedure Redo(); inline;
    function ReplaceText(ABeginPosition, AEndPosition: TBCEditorTextPosition;
      const AText: string): TBCEditorTextPosition;
    procedure SetTabWidth(const AValue: Integer);
    procedure SetTextStr(const AValue: string); override;
    procedure SetUpdateState(AUpdating: Boolean); override;
    procedure Sort(const ABeginLine, AEndLine: Integer); virtual;
    procedure Undo(); inline;
    procedure UndoGroupBreak();
    property Background[Line: Integer]: TColor read GetBackground write PutBackground;
    property BOLPosition[Line: Integer]: TBCEditorTextPosition read GetBOLPosition;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretPosition: TBCEditorTextPosition read FCaretPosition write SetCaretPosition;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property Editor: TCustomControl read FEditor write FEditor;
    property EOFPosition: TBCEditorTextPosition read GetEOFPosition;
    property EOLPosition[ALine: Integer]: TBCEditorTextPosition read GetEOLPosition;
    property ExpandedStringLengths[ALine: Integer]: Integer read GetExpandedStringLength;
    property ExpandedStrings[Line: Integer]: string read GetExpandedString;
    property FirstRow[Line: Integer]: Integer read GetFirstRow write PutFirstRow;
    property Foreground[Line: Integer]: TColor read GetForeground write PutForeground;
    property Lines: TLines read FLines;
    property LineState[Line: Integer]: TLine.TState read GetLineState;
    property MaxLength: Integer read GetMaxLength;
    property Modified: Boolean read FModified write SetModified;
    property OnAfterUpdate: TNotifyEvent read FOnAfterUpdate write FOnAfterUpdate;
    property OnBeforeUpdate: TNotifyEvent read FOnBeforeUpdate write FOnBeforeUpdate;
    property OnCaretMoved: TNotifyEvent read FOnCaretMoved write FOnCaretMoved;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
    property OnDeleted: TChangeEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TChangeEvent read FOnInserted write FOnInserted;
    property OnSelChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    property OnUpdated: TChangeEvent read FOnUpdated write FOnUpdated;
    property Options: TOptions read FOptions write FOptions;
    property Ranges[Line: Integer]: Pointer read GetRange write PutRange;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property RedoList: TUndoList read FRedoList;
    property SelBeginPosition: TBCEditorTextPosition read FSelBeginPosition write SetSelBeginPosition;
    property SelEndPosition: TBCEditorTextPosition read FSelEndPosition write SetSelEndPosition;
    property SelMode: TBCEditorSelectionMode read FSelMode write FSelMode;
    property SortOrder: TBCEditorSortOrder read FSortOrder write FSortOrder;
    property State: TState read FState;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property TextBetween[const BeginPosition, EndPosition: TBCEditorTextPosition]: string read GetTextBetween;
    property TextBetweenColumn[const BeginPosition, EndPosition: TBCEditorTextPosition]: string read GetTextBetweenColumn;
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

implementation {***************************************************************}

uses
  Math, StrUtils, SysConst;

resourcestring
  SBCEditorCharIndexInLineBreak = 'Character index is inside line break';
  SBCEditorCharIndexIsNegative = 'Character index is negative';

function HasLineBreak(const AText: string): Boolean;
var
  LEndPos: PChar;
  LPos: PChar;
begin
  LPos := PChar(AText); LEndPos := PChar(@AText[Length(AText)]);
  while (LPos <= LEndPos) do
    if (CharInSet(LPos^, [BCEDITOR_LINEFEED, BCEDITOR_CARRIAGE_RETURN])) then
      Exit(True)
    else
      Inc(LPos);
  Result := False;
end;

{ TBCEditorLines.TUndoList ****************************************************}

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
  inherited;

  FBlockNumber := 0;
  FGroupBreak := False;
end;

constructor TBCEditorLines.TUndoList.Create(const ALines: TBCEditorLines);
begin
  inherited Create();

  FLines := ALines;

  FBlockNumber := 0;
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

function TBCEditorLines.TUndoList.GetUpdated(): Boolean;
begin
  Result := (FUpdateCount > 0) and (FChanges > 0);
end;

procedure TBCEditorLines.TUndoList.GroupBreak();
begin
  FGroupBreak := True;
end;

function TBCEditorLines.TUndoList.Peek(): TUndoItem;
begin
  Assert(Count > 0);

  Result := List[Count - 1];
end;

function TBCEditorLines.TUndoList.Pop(): TUndoItem;
begin
  Result := Peek();
  Delete(Count - 1);
end;

procedure TBCEditorLines.TUndoList.Push(const AUndoType: TUndoItem.TType; const ACaretPosition: TBCEditorTextPosition;
  const ASelBeginPosition, ASelEndPosition: TBCEditorTextPosition; const ASelMode: TBCEditorSelectionMode;
  const ABeginPosition, AEndPosition: TBCEditorTextPosition; const AText: string = '';
  const ABlockNumber: Integer = 0);
var
  LHandled: Boolean;
  LItem: TUndoItem;
begin
  if (not (lsLoading in Lines.State)) then
  begin
    LHandled := False;
    if ((Lines.State * [lsUndo, lsRedo] = [])
      and (loUndoGrouped in Lines.Options)
      and not FGroupBreak
      and (Count > 0) and (List[Count - 1].UndoType = AUndoType)) then
      case (AUndoType) of
        utSelection: LHandled := True; // Ignore
        utInsert:
          if (List[Count - 1].EndPosition = ABeginPosition) then
          begin
            List[Count - 1].EndPosition := AEndPosition;
            LHandled := True;
          end;
        utReplace:
          if (List[Count - 1].EndPosition = ABeginPosition) then
          begin
            List[Count - 1].EndPosition := AEndPosition;
            List[Count - 1].Text := List[Count - 1].Text + AText;
            LHandled := True;
          end;
        utBackspace:
          if (List[Count - 1].BeginPosition = AEndPosition) then
          begin
            List[Count - 1].BeginPosition := ABeginPosition;
            List[Count - 1].Text := AText + List[Count - 1].Text;
            LHandled := True;
          end;
        utDelete:
          if (List[Count - 1].EndPosition = ABeginPosition) then
          begin
            List[Count - 1].EndPosition := AEndPosition;
            List[Count - 1].Text := List[Count - 1].Text + AText;
            LHandled := True;
          end;
      end;

    if (not LHandled) then
    begin
      if (ABlockNumber > 0) then
        LItem.BlockNumber := ABlockNumber
      else if (FCurrentBlockNumber > 0) then
        LItem.BlockNumber := FCurrentBlockNumber
      else
      begin
        Inc(FBlockNumber);
        LItem.BlockNumber := FBlockNumber;
      end;
      LItem.BeginPosition := ABeginPosition;
      LItem.CaretPosition := ACaretPosition;
      LItem.EndPosition := AEndPosition;
      LItem.SelBeginPosition := ASelBeginPosition;
      LItem.SelEndPosition := ASelEndPosition;
      LItem.SelMode := ASelMode;
      LItem.Text := AText;
      LItem.UndoType := AUndoType;
      Add(LItem);
    end;

    if (UpdateCount > 0) then
      Inc(FChanges);
    FGroupBreak := False;
  end;
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
  Result := Count;
  Insert(Count, AText);
end;

procedure TBCEditorLines.Backspace(ABeginPosition, AEndPosition: TBCEditorTextPosition);
var
  LCaretPosition: TBCEditorTextPosition;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
  LText: string;
begin
  Assert((BOFPosition <= ABeginPosition) and (ABeginPosition < AEndPosition) and (AEndPosition <= EOFPosition));

  LCaretPosition := CaretPosition;
  LSelBeginPosition := SelBeginPosition;
  LSelEndPosition := SelEndPosition;

  LText := TextBetween[ABeginPosition, AEndPosition];

  UndoList.BeginUpdate();
  try
    DoDeleteText(ABeginPosition, AEndPosition);

    UndoList.Push(utBackspace, LCaretPosition,
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
  if (Lines.List[ALine].Text = '') then
    Result := ''
  else
  begin
    Result := ConvertTabs(Lines.List[ALine].Text, FTabWidth, LHasTabs, loColumnTabs in Options);

    if (LHasTabs) then
    begin
      Include(Lines.List[ALine].Flags, sfHasTabs);
      Exclude(Lines.List[ALine].Flags, sfHasNoTabs);
    end
    else
    begin
      Exclude(Lines.List[ALine].Flags, sfHasTabs);
      Include(Lines.List[ALine].Flags, sfHasNoTabs);
    end;
    Lines.List[ALine].ExpandedLength := Length(Result);
  end;
end;

function TBCEditorLines.CharIndexToPosition(const ACharIndex: Integer): TBCEditorTextPosition;
begin
  Result := CharIndexToPosition(ACharIndex, BOFPosition);
end;

function TBCEditorLines.CharIndexToPosition(const ACharIndex: Integer;
  const ARelativePosition: TBCEditorTextPosition): TBCEditorTextPosition;
var
  LLength: Integer;
  LLineBreakLength: Integer;
begin
  Assert((BOFPosition <= ARelativePosition) and (ARelativePosition <= EOFPosition));

  LLength := ACharIndex;

  if (LLength < 0) then
    raise ERangeError.Create(SBCEditorCharIndexIsNegative);

  Result := ARelativePosition;

  if (LLength <= Length(Lines[Result.Line].Text) - Result.Char) then
    Inc(Result.Char, LLength)
  else
  begin
    LLineBreakLength := Length(LineBreak);

    Dec(LLength, (Length(Lines[Result.Line].Text) - Result.Char) + LLineBreakLength);
    Inc(Result.Line);

    if (LLength < 0) then
      raise ERangeError.Create(SBCEditorCharIndexInLineBreak);

    while ((Result.Line < Count) and (LLength >= Length(Lines[Result.Line].Text) + LLineBreakLength)) do
    begin
      Dec(LLength, Length(Lines[Result.Line].Text) + LLineBreakLength);
      Inc(Result.Line);
    end;

    if (LLength > Length(Lines[Result.Line].Text)) then
      if (Result.Line < Count) then
        raise ERangeError.CreateFmt(SBCEditorCharIndexInLineBreak + ' (%d / %d, %d / %d, %d / %d)', [ACharIndex, Length(Text), LLength, Length(Lines[Result.Line].Text), Result.Line, Count])
      else
        raise ERangeError.CreateFmt(SCharIndexOutOfBounds + ' (%d, %d / %d, %d / %d)', [ACharIndex, Length(Text), LLength, Length(Lines[Result.Line].Text), Result.Line, Count]);

    Result.Char := LLength;
  end;

  Assert(Result <= EOFPosition, 'ACharIndex: ' + IntToStr(ACharIndex) + ', RelPos: ' + ARelativePosition.ToString() + ', Result: ' + Result.ToString());
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

  FCaretPosition := BOFPosition;
  FCaseSensitive := False;
  FLines := TLines.Create();
  FMaxLengthLine := -1;
  FModified := False;
  FOnAfterUpdate := nil;
  FOnBeforeUpdate := nil;
  FOnCaretMoved := nil;
  FOnCleared := nil;
  FOnDeleted := nil;
  FOnInserted := nil;
  FOnSelChange := nil;
  FOnUpdated := nil;
  FOptions := DefaultOptions;
  FRedoList := TUndoList.Create(Self);
  FReadOnly := False;
  FSelBeginPosition := BOFPosition;
  FSelEndPosition := BOFPosition;
  FSelMode := smNormal;
  FState := [];
  FUndoList := TUndoList.Create(Self);
  TabWidth := 4;

  DoInsert(0, '');
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
    LBeginPosition := BOLPosition[ABeginLine];
    if (AEndLine < Count - 1) then
      LEndPosition := BOLPosition[ABeginLine + 1]
    else
      LEndPosition := TextPosition(Length(Lines[AEndLine].Text), AEndLine);

    LText := TextBetween[LBeginPosition, LEndPosition];
    UndoList.Push(utDelete, CaretPosition,
      SelBeginPosition, SelEndPosition, SelMode,
      LBeginPosition, InvalidPosition, LText);

    QuickSort(ABeginLine, AEndLine, ACompare);

    UndoList.Push(utInsert, InvalidPosition,
      InvalidPosition, InvalidPosition, smNormal,
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
  LUndoType: TUndoItem.TType;
begin
  Assert((0 <= ALine) and (ALine < Count));

  LCaretPosition := CaretPosition;
  LSelBeginPosition := SelBeginPosition;
  LSelEndPosition := SelEndPosition;
  if (Count = 1) then
  begin
    LBeginPosition := BOLPosition[ALine];
    LText := Get(ALine);
    LUndoType := utClear;
  end
  else if (ALine < Count - 1) then
  begin
    LBeginPosition := BOLPosition[ALine];
    LText := Get(ALine) + LineBreak;
    LUndoType := utDelete;
  end
  else
  begin
    LBeginPosition := EOLPosition[ALine - 1];
    LText := LineBreak + Get(ALine);
    LUndoType := utDelete;
  end;

  UndoList.BeginUpdate();
  try
    DoDelete(ALine);

    UndoList.Push(LUndoType, LCaretPosition,
      LSelBeginPosition, LSelEndPosition, SelMode,
      LBeginPosition, InvalidPosition, LText);
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

  Assert((BOFPosition <= LBeginPosition) and (LBeginPosition <= LEndPosition) and (LEndPosition <= EOFPosition));

  LIndentTextLength := Length(AIndentText);
  LIndentFound := LBeginPosition.Line <> LEndPosition.Line;
  for LLine := LBeginPosition.Line to LEndPosition.Line do
    if (Copy(Lines[LLine].Text, 1 + LBeginPosition.Char, LIndentTextLength) <> AIndentText) then
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

    UndoList.Push(utDeleteIndent, LCaretPosition,
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
          DeleteText(BOLPosition[LLine], TextPosition(Length(AIndentText), LLine));
    finally
      UndoList.EndUpdate();
    end;
  end;

  if ((ABeginPosition <= CaretPosition) and (CaretPosition <= AEndPosition)
    and (CaretPosition.Char > Length(Lines[CaretPosition.Line].Text))) then
    FCaretPosition.Char := Length(Lines[CaretPosition.Line].Text);
  if ((ABeginPosition <= FSelBeginPosition) and (FSelBeginPosition <= AEndPosition)
    and (CaretPosition.Char > Length(Lines[FSelBeginPosition.Line].Text))) then
    FSelBeginPosition.Char := Length(Lines[SelBeginPosition.Line].Text);
  if ((ABeginPosition <= FSelEndPosition) and (FSelEndPosition <= AEndPosition)
    and (FSelEndPosition.Char > Length(Lines[FSelEndPosition.Line].Text))) then
    FSelEndPosition.Char := Length(Lines[SelEndPosition.Line].Text);
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

      if (ABeginPosition.Char > Length(Lines[ABeginPosition.Line].Text)) then
      begin
        LInsertBeginPosition := EOLPosition[ABeginPosition.Line];

        LInsertEndPosition := DoInsertText(LInsertBeginPosition, StringOfChar(BCEDITOR_SPACE_CHAR, ABeginPosition.Char - LInsertBeginPosition.Char));

        UndoList.Push(utInsert, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          LInsertBeginPosition, LInsertEndPosition);

        Assert(LInsertEndPosition = ABeginPosition);
      end;

      LText := TextBetween[ABeginPosition, AEndPosition];

      DoDeleteText(ABeginPosition, AEndPosition);

      UndoList.Push(utDelete, LCaretPosition,
        LSelBeginPosition, LSelEndPosition, SelMode,
        ABeginPosition, InvalidPosition, LText);
    end
    else
    begin
      LCaretPosition := CaretPosition;
      LSelBeginPosition := SelBeginPosition;
      LSelEndPosition := SelEndPosition;

      UndoList.Push(utSelection, LCaretPosition,
        LSelBeginPosition, LSelEndPosition, SelMode,
        InvalidPosition, InvalidPosition);

      for LLine := ABeginPosition.Line to AEndPosition.Line do
      begin
        LBeginText := TextPosition(ABeginPosition.Char, LLine);
        if (AEndPosition.Char < Length(Lines[LLine].Text)) then
          LEndText := EOLPosition[LLine]
        else
          LEndText := TextPosition(AEndPosition.Char, LLine);

        LText := TextBetween[LBeginText, LEndText];

        DoDeleteText(LBeginText, LEndText);

        UndoList.Push(utDelete, InvalidPosition,
          InvalidPosition, InvalidPosition, SelMode,
          LBeginText, InvalidPosition, LText);

        LLineLength := Length(Lines[LLine].Text);
        if (LLineLength > ABeginPosition.Char) then
        begin
          LSpaces := StringOfChar(BCEDITOR_SPACE_CHAR, ABeginPosition.Char - LLineLength);

          DoInsertText(LEndText, LSpaces);

          UndoList.Push(utInsert, InvalidPosition,
            InvalidPosition, InvalidPosition, SelMode,
            TextPosition(ABeginPosition.Char, LLine), TextPosition(AEndPosition.Char, LLine));
        end;
      end;
    end;

    if (SelMode = smNormal) then
      CaretPosition := ABeginPosition;
  finally
    UndoList.EndUpdate();
  end;

  RedoList.Clear();
end;

destructor TBCEditorLines.Destroy;
begin
  FLines.Free();
  FRedoList.Free();
  FUndoList.Free();

  inherited;
end;

procedure TBCEditorLines.DoDelete(ALine: Integer);
var
  LClear: Boolean;
begin
  Assert((0 <= ALine) and (ALine < Count));

  if (FMaxLengthLine >= 0) then
    if (FMaxLengthLine = ALine) then
      FMaxLengthLine := -1
    else if (FMaxLengthLine > ALine) then
      Dec(FMaxLengthLine);

  LClear := Count = 1;
  if (LClear) then
  begin
    Lines.List[0].Background := clNone;
    Lines.List[0].ExpandedLength := -1;
    Lines.List[0].FirstRow := -1;
    Lines.List[0].Foreground := clNone;
    Lines.List[0].Flags := [sfHasTabs, sfHasNoTabs];
    Lines.List[0].Range := nil;
    Lines.List[0].State := lsLoaded;
    Lines.List[0].Text := '';
  end
  else
    Lines.Delete(ALine);

  if (SelMode = smNormal) then
    if (LClear) then
      CaretPosition := BOFPosition
    else if (ALine < Count) then
      CaretPosition := BOLPosition[ALine]
    else
      CaretPosition := EOLPosition[ALine - 1]
  else
  begin
    if (SelBeginPosition.Line > ALine) then
      SelBeginPosition := TextPosition(SelBeginPosition.Char, SelBeginPosition.Line - 1);
    if (SelEndPosition.Line >= ALine) then
      SelEndPosition := TextPosition(SelEndPosition.Char, SelEndPosition.Line - 1);
  end;

  if (UpdateCount > 0) then
    Include(FState, lsTextChanged);

  if (LClear and Assigned(OnCleared)) then
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
  Assert((BOFPosition <= ABeginPosition) and (AEndPosition <= EOFPosition));
  Assert(ABeginPosition <= AEndPosition);

  if (Count > 0) then
  begin
    LTextBeginPosition := BOLPosition[ABeginPosition.Line];
    if (ABeginPosition = AEndPosition) then
      LTextEndPosition := EOLPosition[AEndPosition.Line]
    else if ((AEndPosition.Char = 0) and (AEndPosition.Line > ABeginPosition.Line)) then
      LTextEndPosition := EOLPosition[AEndPosition.Line - 1]
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
            LeftStr(Lines[LLine].Text, ABeginPosition.Char)
              + Copy(Lines[LLine].Text, 1 + ABeginPosition.Char + Length(AIndentText), MaxInt));
    finally
      EndUpdate();
    end;
  end;
end;

procedure TBCEditorLines.DoDeleteText(ABeginPosition, AEndPosition: TBCEditorTextPosition);
var
  Line: Integer;
begin
  Assert((BOFPosition <= ABeginPosition) and (AEndPosition <= EOFPosition));
  Assert(ABeginPosition <= AEndPosition);

  if (ABeginPosition = AEndPosition) then
    // Nothing to do...
  else if (ABeginPosition.Line = AEndPosition.Line) then
    DoPut(ABeginPosition.Line, LeftStr(Lines[ABeginPosition.Line].Text, ABeginPosition.Char)
      + Copy(Lines[AEndPosition.Line].Text, 1 + AEndPosition.Char, MaxInt))
  else
  begin
    BeginUpdate();

    try
      DoPut(ABeginPosition.Line, LeftStr(Lines[ABeginPosition.Line].Text, ABeginPosition.Char)
        + Copy(Lines[AEndPosition.Line].Text, 1 + AEndPosition.Char, MaxInt));

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
  Assert((BOFPosition <= ABeginPosition) and (AEndPosition <= EOFPosition));
  Assert(ABeginPosition <= AEndPosition);

  if (Count > 0) then
  begin
    if ((AEndPosition.Char = 0) and (AEndPosition.Line > ABeginPosition.Line)) then
      LEndLine := AEndPosition.Line - 1
    else
      LEndLine := AEndPosition.Line;

    BeginUpdate();
    try
      for LLine := ABeginPosition.Line to LEndLine do
        if (ASelMode = smNormal) then
          DoPut(LLine, AIndentText + Lines[LLine].Text)
        else if (Length(Lines[LLine].Text) > ABeginPosition.Char) then
          DoPut(LLine, Copy(Lines[LLine].Text, 1, ABeginPosition.Char)
            + AIndentText
            + Copy(Lines[LLine].Text, 1 + ABeginPosition.Char, MaxInt));
    finally
      EndUpdate();
    end;
  end;
end;

procedure TBCEditorLines.DoInsert(ALine: Integer; const AText: string);
var
  LLine: TLine;
begin
  Assert((0 <= ALine) and (ALine <= Count));

  LLine.Background := clNone;
  LLine.ExpandedLength := -1;
  LLine.FirstRow := -1;
  LLine.Foreground := clNone;
  LLine.Flags := [sfHasTabs, sfHasNoTabs];
  LLine.Range := nil;
  LLine.State := lsModified;
  LLine.Text := '';
  Lines.Insert(ALine, LLine);

  Include(FState, lsInsert);
  try
    DoPut(ALine, AText);
  finally
    Exclude(FState, lsInsert);
  end;

  if (SelMode = smNormal) then
  begin
    if (ALine < Count - 1) then
      CaretPosition := BOLPosition[ALine + 1]
    else
      CaretPosition := EOLPosition[ALine];
    SelBeginPosition := CaretPosition;
    SelEndPosition := CaretPosition;
  end
  else
  begin
    if (SelBeginPosition.Line < ALine) then
      SelBeginPosition := TextPosition(SelBeginPosition.Char, SelBeginPosition.Line + 1);
    if (SelEndPosition.Line <= ALine) then
      if (ALine < Count) then
        SelEndPosition := EOLPosition[ALine]
      else
        SelEndPosition := TextPosition(SelEndPosition.Char, SelEndPosition.Line + 1);
  end;

  if (UpdateCount > 0) then
    Include(FState, lsTextChanged);
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
  Assert(BOFPosition <= APosition);
  Assert((APosition.Line < Count) and (APosition.Char <= Length(Lines[APosition.Line].Text)));

  if (AText = '') then
    Result := APosition
  else if (not HasLineBreak(AText)) then
  begin
    DoPut(APosition.Line, LeftStr(Lines[APosition.Line].Text, APosition.Char)
      + AText
      + Copy(Lines[APosition.Line].Text, 1 + APosition.Char, MaxInt));
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
        if (APosition.Char = 0) then
        begin
          LLineEnd := Lines[LLine].Text;
          if (LLineBeginPos < LPos) then
            DoPut(LLine, LeftStr(AText, LPos - LLineBeginPos))
          else if (Lines[LLine].Text <> '') then
            DoPut(LLine, '');
        end
        else
        begin
          LLineEnd := Copy(Lines[LLine].Text, 1 + APosition.Char, MaxInt);
          if (LLineBeginPos < LPos) then
            DoPut(LLine, LeftStr(Lines[LLine].Text, APosition.Char) + LeftStr(AText, LPos - LLineBeginPos))
          else if (Length(Lines[LLine].Text) > APosition.Char) then
            DoPut(LLine, LeftStr(Lines[LLine].Text, APosition.Char));
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
        DoInsert(LLine, Copy(AText, LPos - @AText[1], LEndPos + 1 - LPos) + LLineEnd);
        Result := TextPosition(LEndPos + 1 - (LLineBeginPos + 1), LLine);
      end
      else
      begin
        DoInsert(LLine, RightStr(AText, LEndPos + 1 - LLineBeginPos) + LLineEnd);
        Result := TextPosition(1 + LEndPos + 1 - (LLineBeginPos + 1), LLine);
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
begin
  Assert((0 <= ALine) and (ALine < Count));

  LModified := AText <> Lines[ALine].Text;
  if (LModified) then
  begin
    Lines.List[ALine].Flags := Lines.List[ALine].Flags - [sfHasTabs, sfHasNoTabs];
    Lines.List[ALine].State := lsModified;
    Lines.List[ALine].Text := AText;
  end;

  if (LModified and (FMaxLengthLine >= 0)) then
    if (ExpandedStringLengths[ALine] >= Lines[FMaxLengthLine].ExpandedLength) then
      FMaxLengthLine := ALine
    else if (ALine = FMaxLengthLine) then
      FMaxLengthLine := -1;

  CaretPosition := EOLPosition[ALine];

  if (LModified and not (lsInsert in State)) then
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
  LUndoItem: TUndoItem;
begin
  if (not ReadOnly and (List.Count > 0)) then
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
      LUndoItem := List.Pop();

      case (LUndoItem.UndoType) of
        utSelection:
          begin
            LDestinationList.Push(LUndoItem.UndoType, LCaretPosition,
              LSelBeginPosition, LSelEndPosition, LSelMode,
              LUndoItem.BeginPosition, LUndoItem.EndPosition, LUndoItem.Text, LUndoItem.BlockNumber);
          end;
        utInsert,
        utReplace,
        utBackspace,
        utDelete:
          begin
            if ((LUndoItem.BeginPosition <> LUndoItem.EndPosition)
             and ((LUndoItem.UndoType in [utReplace])
               or ((LUndoItem.UndoType in [utBackspace, utDelete]) xor (List = UndoList)))) then
            begin
              LText := TextBetween[LUndoItem.BeginPosition, LUndoItem.EndPosition];
              DoDeleteText(LUndoItem.BeginPosition, LUndoItem.EndPosition);
              if (not (LUndoItem.UndoType in [utReplace])) then
                LDestinationList.Push(LUndoItem.UndoType, LCaretPosition,
                  LSelBeginPosition, LSelEndPosition, LSelMode,
                  LUndoItem.BeginPosition, LUndoItem.EndPosition, LText, LUndoItem.BlockNumber);
            end
            else
              LText := '';
            if ((LUndoItem.UndoType in [utReplace])
                or ((LUndoItem.UndoType in [utBackspace, utDelete]) xor (List <> UndoList))) then
            begin
              if (LUndoItem.Text = '') then
                LEndPosition := LUndoItem.BeginPosition
              else
                LEndPosition := DoInsertText(LUndoItem.BeginPosition, LUndoItem.Text);
              LDestinationList.Push(LUndoItem.UndoType, LCaretPosition,
                LSelBeginPosition, LSelEndPosition, LSelMode,
                LUndoItem.BeginPosition, LEndPosition, LText, LUndoItem.BlockNumber);
            end;
          end;
        utClear:
          if (List = RedoList) then
          begin
            LText := TextBetween[BOFPosition, EOFPosition];
            InternalClear(False);
            LDestinationList.Push(LUndoItem.UndoType, LCaretPosition,
              LSelBeginPosition, LSelEndPosition, LSelMode,
              BOFPosition, InvalidPosition, LText, LUndoItem.BlockNumber);
          end
          else
          begin
            LEndPosition := DoInsertText(LUndoItem.BeginPosition, LUndoItem.Text);
            LDestinationList.Push(LUndoItem.UndoType, LCaretPosition,
              LSelBeginPosition, LSelEndPosition, LSelMode,
              LUndoItem.BeginPosition, LEndPosition, '', LUndoItem.BlockNumber);
          end;
        utInsertIndent,
        utDeleteIndent:
          begin
            if ((LUndoItem.UndoType <> utInsertIndent) xor (List = UndoList)) then
              DoDeleteIndent(LUndoItem.BeginPosition, LUndoItem.EndPosition,
                LUndoItem.Text, LUndoItem.SelMode)
            else
              DoInsertIndent(LUndoItem.BeginPosition, LUndoItem.EndPosition,
                LUndoItem.Text, LUndoItem.SelMode);
            LDestinationList.Push(LUndoItem.UndoType, LCaretPosition,
              LSelBeginPosition, LSelEndPosition, LSelMode,
              LUndoItem.BeginPosition, LUndoItem.EndPosition, LUndoItem.Text, LUndoItem.BlockNumber);
          end;
        else raise ERangeError.Create('UndoType: ' + IntToStr(Ord(LUndoItem.UndoType)));
      end;

      LCaretPosition := LUndoItem.CaretPosition;
      LSelBeginPosition := LUndoItem.SelBeginPosition;
      LSelEndPosition := LUndoItem.SelEndPosition;
      LSelMode := LUndoItem.SelMode;

      LPreviousBlockNumber := LUndoItem.BlockNumber;
      if (List.Count > 0) then
        LUndoItem := List.Peek();
    until ((List.Count = 0)
      or (LUndoItem.BlockNumber <> LPreviousBlockNumber));

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
  Assert((0 <= ALine) and (ALine < Count));

  Result := Lines[ALine].Text;
end;

function TBCEditorLines.GetBackground(ALine: Integer): TColor;
begin
  Assert((0 <= ALine) and (ALine < Count));

  Result := Lines[ALine].Background;
end;

function TBCEditorLines.GetBOLPosition(ALine: Integer): TBCEditorTextPosition;
begin
  Result := TextPosition(0, ALine);
end;

function TBCEditorLines.GetCanRedo(): Boolean;
begin
  Result := RedoList.Count > 0;
end;

function TBCEditorLines.GetCanUndo(): Boolean;
begin
  Result := UndoList.Count > 0;
end;

function TBCEditorLines.GetCount(): Integer;
begin
  Result := Lines.Count;
end;

function TBCEditorLines.GetEOFPosition(): TBCEditorTextPosition;
begin
  Result := EOLPosition[Count - 1];
end;

function TBCEditorLines.GetEOLPosition(ALine: Integer): TBCEditorTextPosition;
begin
  Assert((0 <= ALine) and (ALine < Count));

  Result := TextPosition(Length(Lines[ALine].Text), ALine)
end;

function TBCEditorLines.GetExpandedString(ALine: Integer): string;
begin
  Assert((0 <= ALine) and (ALine < Count));

  if (sfHasNoTabs in Lines[ALine].Flags) then
    Result := Lines[ALine].Text
  else
    Result := CalcExpandString(ALine);
end;

function TBCEditorLines.GetExpandedStringLength(ALine: Integer): Integer;
begin
  Assert((0 <= ALine) and (ALine < Count));

  if (Lines[ALine].ExpandedLength >= 0) then
    Lines.List[ALine].ExpandedLength := Length(ExpandedStrings[ALine]);
  Result := Lines[ALine].ExpandedLength;
end;

function TBCEditorLines.GetForeground(ALine: Integer): TColor;
begin
  Assert((0 <= ALine) and (ALine < Count));

  Result := Lines[ALine].Foreground;
end;

function TBCEditorLines.GetLineState(ALine: Integer): TLine.TState;
begin
  Assert((0 <= ALine) and (ALine < Count));

  Result := Lines[ALine].State;
end;

function TBCEditorLines.GetMaxLength(): Integer;
var
  LMaxLength: Integer;
  LLine: Integer;
begin
  if (FMaxLengthLine < 0) then
  begin
    LMaxLength := 0;
    for LLine := 0 to Count - 1 do
    begin
      if (Lines[LLine].ExpandedLength < 0) then
        CalcExpandString(LLine);
      if (Lines[LLine].ExpandedLength > LMaxLength) then
      begin
        LMaxLength := Lines[LLine].ExpandedLength;
        FMaxLengthLine := LLine;
      end;
    end;
  end;

  if (FMaxLengthLine < 0) then
    Result := 0
  else
    Result := Lines[FMaxLengthLine].ExpandedLength;
end;

function TBCEditorLines.GetFirstRow(ALine: Integer): Integer;
begin
  Assert((0 <= ALine) and (ALine < Count));

  Result := Lines[ALine].FirstRow;
end;

function TBCEditorLines.GetRange(ALine: Integer): Pointer;
begin
  Assert((0 <= ALine) and (ALine < Count));

  Result := Lines[ALine].Range;
end;

function TBCEditorLines.GetTextBetween(const ABeginPosition, AEndPosition: TBCEditorTextPosition): string;
var
  LEndChar: Integer;
  LEndLine: Integer;
  LLine: Integer;
  StringBuilder: TStringBuilder;
begin
  Assert((BOFPosition <= ABeginPosition) and (AEndPosition <= EOFPosition));
  Assert(ABeginPosition <= AEndPosition);
  Assert(ABeginPosition.Char <= Length(Lines[ABeginPosition.Line].Text));
  Assert(AEndPosition.Char <= Length(Lines[AEndPosition.Line].Text));

  LEndLine := AEndPosition.Line;
  if ((loTrimTrailingLines in Options) and (lsSaving in State)) then
    while ((LEndLine > 0)
      and (Trim(Lines[LEndLine].Text) = '')
      and (Trim(Lines[LEndLine - 1].Text) = '')) do
      Dec(LEndLine);

  if (ABeginPosition = AEndPosition) then
    Result := ''
  else if (ABeginPosition.Line = LEndLine) then
  begin
    if (LEndLine = AEndPosition.Line) then
      LEndChar := AEndPosition.Char
    else
      LEndChar := Length(Lines[LEndLine].Text);
    if ((loTrimTrailingSpaces in Options) and (lsSaving in State)) then
      while ((LEndChar > 0) and (Lines[LEndLine].Text[1 + LEndChar - 1] = BCEDITOR_SPACE_CHAR)) do
        Dec(LEndChar);
    Result := Copy(Lines[ABeginPosition.Line].Text, 1 + ABeginPosition.Char, LEndChar - ABeginPosition.Char)
  end
  else
  begin
    StringBuilder := TStringBuilder.Create();

    LEndChar := Length(Lines[ABeginPosition.Line].Text);
    if ((loTrimTrailingSpaces in Options) and (lsSaving in State)) then
      while ((LEndChar > ABeginPosition.Char) and (Lines[ABeginPosition.Line].Text[1 + LEndChar - 1] = BCEDITOR_SPACE_CHAR)) do
        Dec(LEndChar);
    StringBuilder.Append(Lines[ABeginPosition.Line].Text, ABeginPosition.Char, LEndChar - ABeginPosition.Char);
    for LLine := ABeginPosition.Line + 1 to LEndLine - 1 do
    begin
      StringBuilder.Append(LineBreak);
      LEndChar := Length(Lines[LLine].Text);
      if ((loTrimTrailingSpaces in Options) and (lsSaving in State)) then
        while ((LEndChar > 0) and (Lines[LLine].Text[1 + LEndChar - 1] = BCEDITOR_SPACE_CHAR)) do
          Dec(LEndChar);
      StringBuilder.Append(Lines[LLine].Text, 0, LEndChar);
    end;
    if (LEndLine = AEndPosition.Line) then
      LEndChar := AEndPosition.Char
    else
      LEndChar := Length(Lines[LEndLine].Text);
    if ((loTrimTrailingSpaces in Options) and (lsSaving in State) and (LEndChar = Length(Lines[LEndLine].Text))) then
      while ((LEndChar > 0) and (Lines[LEndLine].Text[1 + LEndChar - 1] = BCEDITOR_SPACE_CHAR)) do
        Dec(LEndChar);
    if ((LEndChar > 0)
      or not (loTrimTrailingSpaces in Options) or not (lsSaving in State) or (LEndChar <> Length(Lines[LEndLine].Text))) then
    begin
      StringBuilder.Append(LineBreak);
      StringBuilder.Append(Lines[LEndLine].Text, 0, LEndChar);
    end;

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
  Assert(ABeginPosition.Char <= Length(Lines[ABeginPosition.Line].Text));

  LBeginPosition := Min(ABeginPosition, LEndPosition);
  LEndPosition := Max(ABeginPosition, LEndPosition);

  if (LBeginPosition = LEndPosition) then
    Result := ''
  else if (LBeginPosition.Line = LEndPosition.Line) then
    Result := Copy(Lines[LBeginPosition.Line].Text, 1 + LBeginPosition.Char, LEndPosition.Char - LBeginPosition.Char)
  else
  begin
    StringBuilder := TStringBuilder.Create();

    for LLine := LBeginPosition.Line to LEndPosition.Line do
    begin
      if (Length(Lines[LBeginPosition.Line].Text) < LBeginPosition.Char) then
        // Do nothing
      else if (Length(Lines[LBeginPosition.Line].Text) < LEndPosition.Char) then
        StringBuilder.Append(Copy(Lines[LBeginPosition.Line].Text, LBeginPosition.Char, Length(Lines[LBeginPosition.Line].Text) - LBeginPosition.Char))
      else
        StringBuilder.Append(Copy(Lines[LBeginPosition.Line].Text, LBeginPosition.Char, LEndPosition.Char - LBeginPosition.Char + 1));
      if (LLine < LEndPosition.Line) then
        StringBuilder.Append(LineBreak);
    end;

    Result := StringBuilder.ToString();

    StringBuilder.Free();
  end;
end;

function TBCEditorLines.GetTextLength(): Integer;
var
  LLine: Integer;
  LLineBreakLength: Integer;
begin
  Result := 0;
  LLineBreakLength := Length(LineBreak);
  for LLine := 0 to Count - 2 do
  begin
    Inc(Result, Length(Lines[LLine].Text));
    Inc(Result, LLineBreakLength);
  end;
  if (Count > 0) then
    Inc(Result, Length(Lines[Count - 1].Text))
end;

function TBCEditorLines.GetTextStr: string;
begin
  Include(FState, lsSaving);
  try
    Result := TextBetween[BOFPosition, EOFPosition];
  finally
    Exclude(FState, lsSaving);
  end;
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
    UndoList.Push(utInsert, LCaretPosition,
      LSelBeginPosition, LSelEndPosition, SelMode,
      BOLPosition[ALine], TextPosition(Length(AText), ALine));

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

  UndoList.Push(utInsertIndent, LCaretPosition,
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
      if (LLineLength < ABeginPosition.Char) then
      begin
        LInsertText := StringOfChar(BCEDITOR_SPACE_CHAR, ABeginPosition.Char - LLineLength) + LInsertText;

        LInsertBeginPosition := TextPosition(LLineLength, LLine);
        LInsertEndPosition := InsertText(LInsertBeginPosition, LInsertText);

        UndoList.Push(utInsert, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          LInsertBeginPosition, LInsertEndPosition);
      end
      else if (LLineLength < AEndPosition.Char) then
      begin
        LInsertBeginPosition := TextPosition(ABeginPosition.Char, LLine);

        LDeleteText := TextBetween[LInsertBeginPosition, TextPosition(LLineLength, LLine)];
        DeleteText(LInsertBeginPosition, LInsertEndPosition);

        UndoList.Push(utDelete, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          LInsertBeginPosition, InvalidPosition, LDeleteText);

        if (LPos > LLineBeginPos) then
        begin
          LInsertEndPosition := InsertText(LInsertBeginPosition, LInsertText);

          UndoList.Push(utInsert, InvalidPosition,
            InvalidPosition, InvalidPosition, SelMode,
            LInsertBeginPosition, LInsertEndPosition);
        end;
      end
      else
      begin
        LInsertBeginPosition := TextPosition(ABeginPosition.Char, LLine);
        LInsertEndPosition := TextPosition(AEndPosition.Char, LLine);

        LDeleteText := TextBetween[LInsertBeginPosition, LInsertEndPosition];
        DeleteText(LInsertBeginPosition, LInsertEndPosition);

        UndoList.Push(utDelete, LCaretPosition,
          LSelBeginPosition, LSelEndPosition, SelMode,
          LInsertBeginPosition, InvalidPosition, LDeleteText);

        if (LPos > LLineBeginPos) then
        begin
          LInsertEndPosition := InsertText(LInsertBeginPosition, LeftStr(LInsertText, AEndPosition.Char - ABeginPosition.Char));

          UndoList.Push(utInsert, LCaretPosition,
            InvalidPosition, InvalidPosition, SelMode,
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
  LIndex: Integer;
  LPosition: TBCEditorTextPosition;
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
  LText: string;
begin
  BeginUpdate();
  try
    if (AText = '') then
      Result := APosition
    else
    begin
      LCaretPosition := CaretPosition;
      LSelBeginPosition := SelBeginPosition;
      LSelEndPosition := SelEndPosition;
      if ((APosition.Line < Count) and (APosition.Char <= Length(Lines[APosition.Line].Text))) then
      begin
        LPosition := APosition;
        Result := DoInsertText(LPosition, AText);
      end
      else if (APosition.Line < Count) then
      begin
        LPosition := EOLPosition[APosition.Line];
        Result := DoInsertText(LPosition, StringOfChar(BCEDITOR_SPACE_CHAR, APosition.Char - LPosition.Char) + AText);
      end
      else
      begin
        LPosition := EOLPosition[Count - 1];
        LText := '';
        for LIndex := Count to APosition.Line do
          LText := LText + LineBreak;
        LText := LText + StringOfChar(BCEDITOR_SPACE_CHAR, APosition.Char);
        Result := DoInsertText(LPosition, LText + AText);
      end;

      UndoList.Push(utInsert, LCaretPosition,
        LSelBeginPosition, LSelEndPosition, SelMode,
        LPosition, Result);
    end;

    if (SelMode = smNormal) then
      CaretPosition := Result;
  finally
    EndUpdate();
    RedoList.Clear();
  end;
end;

procedure TBCEditorLines.InternalClear(const AClearUndo: Boolean);
begin
  if (AClearUndo) then
    ClearUndo();

  FMaxLengthLine := -1;
  LineBreak := BCEDITOR_CARRIAGE_RETURN + BCEDITOR_LINEFEED;
  FCaretPosition := BOFPosition;
  FSelBeginPosition := BOFPosition;
  FSelEndPosition := BOFPosition;
  Lines.Clear();
  DoInsert(0, '');
  Lines.List[0].State := lsLoaded;
  if (Assigned(OnCleared)) then
    OnCleared(Self);
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
  Assert((0 <= ALine) and (ALine < Count));

  ReplaceText(BOLPosition[ALine], EOLPosition[ALine], AText);
end;

procedure TBCEditorLines.PutFirstRow(ALine: Integer; const ARow: Integer);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Lines.List[ALine].FirstRow := ARow;
end;

procedure TBCEditorLines.PutBackground(ALine: Integer; AValue: TColor);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Lines.List[ALine].Background := AValue;
end;

procedure TBCEditorLines.PutForeground(ALine: Integer; AValue: TColor);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Lines.List[ALine].Foreground := AValue;
end;

procedure TBCEditorLines.PutRange(ALine: Integer; ARange: Pointer);
begin
  Assert((0 <= ALine) and (ALine < Count));

  Lines.List[ALine].Range := ARange;
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
  LSelBeginPosition: TBCEditorTextPosition;
  LSelEndPosition: TBCEditorTextPosition;
  LText: string;
begin
  if (ABeginPosition = AEndPosition) then
    InsertText(ABeginPosition, AText)
  else
  begin
    UndoList.BeginUpdate();
    try
      LCaretPosition := CaretPosition;
      LSelBeginPosition := SelBeginPosition;
      LSelEndPosition := SelEndPosition;

      LText := TextBetween[ABeginPosition, AEndPosition];

      DoDeleteText(ABeginPosition, AEndPosition);
      Result := DoInsertText(ABeginPosition, AText);

      UndoList.Push(utReplace, LCaretPosition,
        LSelBeginPosition, LSelEndPosition, SelMode,
        ABeginPosition, Result, LText);

      CaretPosition := Result;
    finally
      UndoList.EndUpdate();
    end;
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

procedure TBCEditorLines.SetCaretPosition(const AValue: TBCEditorTextPosition);
begin
  Assert(BOFPosition <= AValue);

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
        if (Lines[LLine].State = lsModified) then
          Lines.List[LLine].State := lsSaved;
      EndUpdate();
      Editor.Invalidate();
    end;
  end;
end;

procedure TBCEditorLines.SetSelBeginPosition(const AValue: TBCEditorTextPosition);
begin
  Assert(BOFPosition <= AValue);

  if (AValue <> FSelBeginPosition) then
  begin
    BeginUpdate();

    FSelBeginPosition := AValue;
    if (SelMode = smNormal) then
      if (FSelBeginPosition.Line < Count) then
        FSelBeginPosition.Char := Min(FSelBeginPosition.Char, Length(Lines[FSelBeginPosition.Line].Text))
      else
        FSelBeginPosition := EOFPosition;

    SelEndPosition := AValue;

    Include(FState, lsSelChanged);
    EndUpdate();
  end
  else
    SelEndPosition := AValue;
end;

procedure TBCEditorLines.SetSelEndPosition(const AValue: TBCEditorTextPosition);
begin
  Assert(BOFPosition <= AValue);

  if (AValue <> FSelEndPosition) then
  begin
    BeginUpdate();

    FSelEndPosition := AValue;
    if (SelMode = smNormal) then
      if (FSelEndPosition.Line < Count) then
        FSelEndPosition.Char := Min(FSelEndPosition.Char, Length(Lines[FSelEndPosition.Line].Text))
      else
        FSelEndPosition := EOFPosition;

    Include(FState, lsSelChanged);
    EndUpdate();
  end;
end;

procedure TBCEditorLines.SetTabWidth(const AValue: Integer);
var
  LLine: Integer;
begin
  if FTabWidth <> AValue then
  begin
    FTabWidth := AValue;
    FMaxLengthLine := -1;
    for LLine := 0 to Count - 1 do
    begin
      Lines.List[LLine].ExpandedLength := -1;
      Exclude(Lines.List[LLine].Flags, sfHasNoTabs);
    end;
  end;
end;

procedure TBCEditorLines.SetTextStr(const AValue: string);
var
  LEndPosition: TBCEditorTextPosition;
  LLine: Integer;
begin
  Include(FState, lsLoading);

  BeginUpdate();

  if (loUndoAfterLoad in Options) then
    DeleteText(BOFPosition, EOFPosition);

  InternalClear(not (loUndoAfterLoad in Options));

  LEndPosition := InsertText(BOFPosition, AValue);
  for LLine := 0 to Count - 1 do
    Lines.List[LLine].State := lsLoaded;

  if (loUndoAfterLoad in Options) then
  begin
    UndoList.Push(utInsert, BOFPosition,
      InvalidPosition, InvalidPosition, SelMode,
      BOFPosition, LEndPosition);

    RedoList.Clear();
  end;

  CaretPosition := BOFPosition;

  EndUpdate();

  Exclude(FState, lsLoading);
end;

procedure TBCEditorLines.SetUpdateState(AUpdating: Boolean);
begin
  if (AUpdating) then
  begin
    if (not (csReading in Editor.ComponentState) and Assigned(OnBeforeUpdate)) then
      OnBeforeUpdate(Self);

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
          UndoList.Push(utSelection, FOldCaretPosition,
            FOldSelBeginPosition, FOldSelEndPosition, SelMode,
            InvalidPosition, InvalidPosition);
        RedoList.Clear();
      end;
    end;

    UndoList.EndUpdate();

    if (Assigned(OnCaretMoved) and (lsCaretMoved in FState)) then
      OnCaretMoved(Self);
    if (Assigned(OnSelChange) and (lsSelChanged in FState)) then
      OnSelChange(Self);
    if (Assigned(OnAfterUpdate)) then
      OnAfterUpdate(Self);

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
  Inc(Result, APosition.Char);
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

