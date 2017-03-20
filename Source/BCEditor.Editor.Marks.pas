unit BCEditor.Editor.Marks;

interface

uses
  Classes, Contnrs,
  Controls, Graphics,
  BCEditor.Consts;

type
  TBCEditorMark = class
  protected
    FBackground: TColor;
    FChar: Integer;
    FData: Pointer;
    FEditor: TCustomControl;
    FImageIndex: Integer;
    FIndex: Integer;
    FLine: Integer;
    FVisible: Boolean;
  public
    constructor Create(AOwner: TCustomControl);
    property Background: TColor read FBackground write FBackground default clNone;
    property Char: Integer read FChar write FChar;
    property Data: Pointer read FData write FData;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Index: Integer read FIndex write FIndex;
    property Line: Integer read FLine write FLine;
    property Visible: Boolean read FVisible write FVisible;
  end;

  TBCEditorMarkEvent = procedure(ASender: TObject; var AMark: TBCEditorMark) of object;
  TBCEditorMarks = array of TBCEditorMark;

  TBCEditorMarkList = class(TObjectList)
  protected
    FEditor: TCustomControl;
    FOnChange: TNotifyEvent;
    function GetItem(AIndex: Integer): TBCEditorMark;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure SetItem(AIndex: Integer; AItem: TBCEditorMark);
    property OwnsObjects;
  public
    constructor Create(AOwner: TCustomControl);
    procedure ClearLine(ALine: Integer);
    function Extract(AItem: TBCEditorMark): TBCEditorMark;
    function Find(const AIndex: Integer): TBCEditorMark;
    function First: TBCEditorMark;
    procedure GetMarksForLine(ALine: Integer; var AMarks: TBCEditorMarks);
    function Last: TBCEditorMark;
    procedure Place(AMark: TBCEditorMark);
    property Items[AIndex: Integer]: TBCEditorMark read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function CompareLines(Item1, Item2: Pointer): Integer;

implementation

uses
  Types,
  BCEditor.Editor;

type
  TCustomBCEditor = class(BCEditor.Editor.TCustomBCEditor);

function CompareLines(Item1, Item2: Pointer): Integer;
begin
  Result := TBCEditorMark(Item1).Line - TBCEditorMark(Item2).Line;
end;

constructor TBCEditorMark.Create(AOwner: TCustomControl);
begin
  inherited Create;

  FBackground := clNone;
  FIndex := -1;
  FEditor := AOwner;
end;

constructor TBCEditorMarkList.Create(AOwner: TCustomControl);
begin
  inherited Create;
  FEditor := AOwner;
end;

procedure TBCEditorMarkList.ClearLine(ALine: Integer);
var
  LIndex: Integer;
begin
  for LIndex := Count - 1 downto 0 do
  if Items[LIndex].Line = ALine then
    Delete(LIndex);
end;

function TBCEditorMarkList.Extract(AItem: TBCEditorMark): TBCEditorMark;
begin
  Result := TBCEditorMark(inherited Extract(AItem));
end;

function TBCEditorMarkList.Find(const AIndex: Integer): TBCEditorMark;
var
  LIndex: Integer;
  LMark: TBCEditorMark;
begin
  Result := nil;
  for LIndex := Count - 1 downto 0 do
  begin
    LMark := Items[LIndex];
    if LMark.Index = AIndex then
      Exit(LMark);
  end;
end;

function TBCEditorMarkList.First: TBCEditorMark;
begin
  Result := TBCEditorMark(inherited First);
end;

function TBCEditorMarkList.GetItem(AIndex: Integer): TBCEditorMark;
begin
  Result := TBCEditorMark(inherited GetItem(AIndex));
end;

procedure TBCEditorMarkList.GetMarksForLine(ALine: Integer; var AMarks: TBCEditorMarks);
var
  LIndex: Integer;
  LIndex2: Integer;
  LMark: TBCEditorMark;
begin
  SetLength(AMarks, Count);
  LIndex2 := 0;
  for LIndex := 0 to Count - 1 do
  begin
    LMark := Items[LIndex];
    if LMark.Line = ALine then
    begin
      AMarks[LIndex2] := LMark;
      Inc(LIndex2);
    end;
  end;
  SetLength(AMarks, LIndex2);
end;

function TBCEditorMarkList.Last: TBCEditorMark;
begin
  Result := TBCEditorMark(inherited Last);
end;

{ TBCEditorBookmarkList }

procedure TBCEditorMarkList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorMarkList.Place(AMark: TBCEditorMark);
var
  LEditor: TCustomBCEditor;
begin
  LEditor := nil;
  if Assigned(FEditor) and (FEditor is TCustomBCEditor) then
    LEditor := FEditor as TCustomBCEditor;
  if Assigned(LEditor) then
    if Assigned(LEditor.OnBeforeMarkPlaced) then
      LEditor.OnBeforeMarkPlaced(FEditor, AMark);
  if Assigned(AMark) then
    Add(AMark);
  if Assigned(LEditor) then
    if Assigned(LEditor.OnAfterMarkPlaced) then
      LEditor.OnAfterMarkPlaced(FEditor);
end;

procedure TBCEditorMarkList.SetItem(AIndex: Integer; AItem: TBCEditorMark);
begin
  inherited SetItem(AIndex, AItem);
end;

end.
