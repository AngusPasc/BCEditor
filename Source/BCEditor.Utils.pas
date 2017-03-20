unit BCEditor.Utils;

interface {********************************************************************}

uses
  Classes,
  Graphics;

function CaseNone(const AChar: Char): Char;
function CaseStringNone(const AString: string): string;
function CaseUpper(const AChar: Char): Char;
procedure ClearList(var AList: TList);
function ConvertTabs(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean; const AColumns: Boolean): string;
procedure FreeList(var AList: TList);
function MinMax(const AValue, AMinValue, AMaxValue: Integer): Integer;
function TextHeight(ACanvas: TCanvas; const AText: string): Integer;
function TextWidth(ACanvas: TCanvas; const AText: string): Integer;

implementation {***************************************************************}

uses
  Windows,
  Math, SysUtils, Character,
  BCEditor.Consts, BCEditor.Types;

function CaseNone(const AChar: Char): Char;
begin
  Result := AChar;
end;

function CaseStringNone(const AString: string): string;
begin
  Result := AString;
end;

function CaseUpper(const AChar: Char): Char;
begin
  Result := AChar;
  case AChar of
    'a'..'z':
      Result := Char(Word(AChar) and $FFDF);
  end;
end;

procedure ClearList(var AList: TList);
var
  LIndex: Integer;
begin
  if not Assigned(AList) then
    Exit;
  for LIndex := 0 to AList.Count - 1 do
    if Assigned(AList[LIndex]) then
    begin
      TObject(AList[LIndex]).Free;
      AList[LIndex] := nil;
    end;
  AList.Clear;
end;

function ConvertTabs(const ALine: string; ATabWidth: Integer; var AHasTabs: Boolean; const AColumns: Boolean): string;
var
  LCount: Integer;
  LPosition: Integer;
begin
  AHasTabs := False;
  Result := ALine;
  LPosition := 1;
  while True do
  begin
    LPosition := Pos(BCEDITOR_TAB_CHAR, Result, LPosition);
    if LPosition = 0 then
      Break;

    AHasTabs := True;

    Delete(Result, LPosition, Length(BCEDITOR_TAB_CHAR));

    if AColumns then
      LCount := ATabWidth - (LPosition - ATabWidth - 1) mod ATabWidth
    else
      LCount := ATabWidth;

    Insert(StringOfChar(BCEDITOR_SPACE_CHAR, LCount), Result, LPosition);
    Inc(LPosition, LCount);
  end;
end;

procedure FreeList(var AList: TList);
begin
  ClearList(AList);
  if Assigned(AList) then
  begin
    AList.Free;
    AList := nil;
  end;
end;

function MinMax(const AValue, AMinValue, AMaxValue: Integer): Integer;
begin
  Result := Max(Min(AValue, AMaxValue), AMinValue);
end;

function TextHeight(ACanvas: TCanvas; const AText: string): Integer;
var
  LSize: TSize;
begin
  GetTextExtentPoint32(ACanvas.Handle, PChar(AText), Length(AText), LSize);
  Result := LSize.cy;
end;

function TextWidth(ACanvas: TCanvas; const AText: string): Integer;
var
  LSize: TSize;
begin
  GetTextExtentPoint32(ACanvas.Handle, PChar(AText), Length(AText), LSize);
  Result := LSize.cx;
end;

end.
