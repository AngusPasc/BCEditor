unit BCEditor.Search;

interface {********************************************************************}

uses
  Classes, RegularExpressions,
  BCEditor.Lines;

type
  TBCEditorCustomSearch = class
  strict private
    FCaseSensitive: Boolean;
    FStatus: string;
    FWholeWordsOnly: Boolean;
    procedure SetCaseSensitive(const AValue: Boolean);
  protected
    procedure CaseSensitiveChanged; virtual; abstract;
    function GetLength(const AIndex: Integer): Integer; virtual; abstract;
    function GetPattern: string; virtual; abstract;
    function GetResult(const AIndex: Integer): Integer; virtual; abstract;
    function GetResultCount: Integer; virtual; abstract;
    procedure SetPattern(const AValue: string); virtual; abstract;
  public
    constructor Create;
    procedure Clear; virtual; abstract;
    function SearchAll(const ALines: TBCEditorLines): Integer; virtual; abstract;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive default False;
    property Lengths[const AIndex: Integer]: Integer read GetLength;
    property Pattern: string read GetPattern write SetPattern;
    property ResultCount: Integer read GetResultCount;
    property Results[const AIndex: Integer]: Integer read GetResult;
    property Status: string read FStatus write FStatus;
    property WholeWordsOnly: Boolean read FWholeWordsOnly write FWholeWordsOnly default False;
  end;

  TBCEditorNormalSearch = class(TBCEditorCustomSearch)
  strict private
    FCount: Integer;
    FLookAt: Integer;
    FOrigin: PChar;
    FPattern, FCasedPattern: string;
    FPatternLength, FPatternLengthSuccessor: Integer;
    FResults: TList;
    FRun: PChar;
    FShift: array [AnsiChar] of Integer;
    FShiftInitialized: Boolean;
    FTextLength: Integer;
    FTextToSearch: string;
    FTheEnd: PChar;
    function GetFinished: Boolean;
    procedure InitShiftTable;
  protected
    procedure CaseSensitiveChanged; override;
    function GetLength(const AIndex: Integer): Integer; override;
    function GetPattern: string; override;
    function GetResult(const AIndex: Integer): Integer; override;
    function GetResultCount: Integer; override;
    procedure SetPattern(const AValue: string); override;
    function TestWholeWord: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    function FindFirst(const AText: string): Integer;
    function Next: Integer;
    function SearchAll(const ALines: TBCEditorLines): Integer; override;
    property Count: Integer read FCount write FCount;
    property Finished: Boolean read GetFinished;
    property Pattern read FCasedPattern;
  end;

  TBCEditorRegexSearch = class(TBCEditorCustomSearch)
  strict private
    FLengths: TList;
    FOptions: TRegexOptions;
    FPattern: string;
    FPositions: TList;
  protected
    procedure CaseSensitiveChanged; override;
    function GetLength(const AIndex: Integer): Integer; override;
    function GetPattern: string; override;
    function GetResult(const AIndex: Integer): Integer; override;
    function GetResultCount: Integer; override;
    procedure SetPattern(const AValue: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    function SearchAll(const ALines: TBCEditorLines): Integer; override;
  end;

type
  TBCEditorWildcardSearch = class(TBCEditorRegexSearch)
  strict private
    FPattern: string;
  protected
    function GetPattern: string; override;
    procedure SetPattern(const AValue: string); override;
    function WildCardToRegExpr(const AWildCard: string): string;
  public
    constructor Create;
  end;

implementation {***************************************************************}

uses
  Windows,
  Character, SysUtils,
  BCEditor.Language, BCEditor.Consts;

{ TBCEditorSearchBase *********************************************************}

constructor TBCEditorCustomSearch.Create;
begin
  inherited;

  FCaseSensitive := False;
  FWholeWordsOnly := False;
end;

procedure TBCEditorCustomSearch.SetCaseSensitive(const AValue: Boolean);
begin
  FCaseSensitive := AValue;
  CaseSensitiveChanged;
end;

{ TBCEditorNormalSearch *******************************************************}

constructor TBCEditorNormalSearch.Create;
begin
  inherited;
  FResults := TList.Create;
end;

destructor TBCEditorNormalSearch.Destroy;
begin
  FResults.Free;
  inherited Destroy;
end;

procedure TBCEditorNormalSearch.CaseSensitiveChanged;
begin
  if CaseSensitive then
    FPattern := FCasedPattern
  else
    FPattern := LowerCase(FCasedPattern);
  FShiftInitialized := False;
end;

procedure TBCEditorNormalSearch.Clear;
begin
  FResults.Count := 0;
end;

function TBCEditorNormalSearch.FindFirst(const AText: string): Integer;
begin
  if not FShiftInitialized then
    InitShiftTable;
  Result := -1;
  FTextLength := Length(AText);
  if FTextLength >= FPatternLength then
  begin
    FTextToSearch := AText;
    if not CaseSensitive then
      CharLowerBuff(PChar(FTextToSearch), FTextLength);
    FOrigin := PChar(FTextToSearch);
    FTheEnd := FOrigin + FTextLength;
    FRun := FOrigin - 1;
    Result := Next;
  end;
end;

function TBCEditorNormalSearch.GetFinished: Boolean;
begin
  Result := (FRun >= FTheEnd) or (FPatternLength >= FTextLength);
end;

function TBCEditorNormalSearch.GetLength(const AIndex: Integer): Integer;
begin
  Result := FPatternLength;
end;

function TBCEditorNormalSearch.GetPattern: string;
begin
  Result := FCasedPattern;
end;

function TBCEditorNormalSearch.GetResult(const AIndex: Integer): Integer;
begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < FResults.Count) then
    Result := Integer(FResults[AIndex]);
end;

function TBCEditorNormalSearch.GetResultCount: Integer;
begin
  Result := FResults.Count;
end;

procedure TBCEditorNormalSearch.InitShiftTable;
var
  LAnsiChar: AnsiChar;
  LIndex: Integer;
begin
  FPatternLength := Length(FPattern);
  if FPatternLength = 0 then
    Status := SBCEditorPatternIsEmpty;
  FPatternLengthSuccessor := FPatternLength + 1;
  FLookAt := 1;
  for LAnsiChar := Low(AnsiChar) to High(AnsiChar) do
    FShift[LAnsiChar] := FPatternLengthSuccessor;
  for LIndex := 1 to FPatternLength do
    FShift[AnsiChar(FPattern[LIndex])] := FPatternLengthSuccessor - LIndex;
  while FLookAt < FPatternLength do
  begin
    if FPattern[FPatternLength] = FPattern[FPatternLength - FLookAt] then
      Break;
    Inc(FLookAt);
  end;
  FShiftInitialized := True;
end;

function TBCEditorNormalSearch.Next: Integer;
var
  LIndex: Integer;
  LPValue: PChar;
begin
  Result := -1;
  Inc(FRun, FPatternLength);
  while FRun < FTheEnd do
  begin
    if FPattern[FPatternLength] <> FRun^ then
      Inc(FRun, FShift[AnsiChar((FRun + 1)^)])
    else
    begin
      LPValue := FRun - FPatternLength + 1;
      LIndex := 1;
      while FPattern[LIndex] = LPValue^ do
      begin
        if LIndex = FPatternLength then
        begin
          if WholeWordsOnly then
            if not TestWholeWord then
              Break;
          Inc(FCount);
          Result := FRun - FOrigin - FPatternLength + 1;
          Exit;
        end;
        Inc(LIndex);
        Inc(LPValue);
      end;
      Inc(FRun, FLookAt);
      if FRun >= FTheEnd then
        Break;
      Inc(FRun, FShift[AnsiChar(FRun^)] - 1);
    end;
  end;
end;

function TBCEditorNormalSearch.SearchAll(const ALines: TBCEditorLines): Integer;
var
  Found: Integer;
begin
  Status := '';
  Clear;
  Found := FindFirst(ALines.Text);
  while Found >= 0 do
  begin
    FResults.Add(Pointer(Found));
    Found := Next;
  end;
  Result := FResults.Count;
  SetLength(FTextToSearch, 0);
end;

procedure TBCEditorNormalSearch.SetPattern(const AValue: string);
var
  LValue: string;
begin
  LValue := AValue;

  LValue := StringReplace(LValue, '\n', sLineBreak, [rfReplaceAll]);

  if FPattern <> LValue then
  begin
    FCasedPattern := LValue;
    if CaseSensitive then
      FPattern := FCasedPattern
    else
      FPattern := LowerCase(FCasedPattern);
    FShiftInitialized := False;
  end;
  FCount := 0;
end;

function TBCEditorNormalSearch.TestWholeWord: Boolean;
var
  LPTest: PChar;

  function IsWordBreakChar(AChar: Char): Boolean;
  begin
    if (AChar < BCEDITOR_EXCLAMATION_MARK) or AChar.IsWhiteSpace then
      Result := True
    else
    if AChar = BCEDITOR_LOW_LINE then
      Result := False
    else
      Result := not AChar.IsLetterOrDigit;
  end;

begin
  LPTest := FRun - FPatternLength;

  Result := ((LPTest < FOrigin) or IsWordBreakChar(LPTest[0])) and ((FRun >= FTheEnd) or IsWordBreakChar(FRun[1]));
end;

{ TBCEditorWildcardSearch *****************************************************}

constructor TBCEditorWildcardSearch.Create;
begin
  inherited Create;
  FPattern := '';
end;

function TBCEditorWildcardSearch.GetPattern: string;
begin
  Result := FPattern;
end;

procedure TBCEditorWildcardSearch.SetPattern(const AValue: string);
begin
  FPattern := AValue;

  inherited SetPattern(WildCardToRegExpr(AValue));
end;

function TBCEditorWildcardSearch.WildCardToRegExpr(const AWildCard: string): string;
var
  LIndex: Integer;
begin
  Result := '';

  for LIndex := 1 to Length(AWildCard) do
    case AWildCard[LIndex] of
      '*':
        Result := Result + '.*';
      '?':
        Result := Result + '.?';
    else
      Result := Result + AWildCard[LIndex];
    end;
end;

{ TBCEditorRegexSearch ********************************************************}

constructor TBCEditorRegexSearch.Create;
begin
  inherited Create;

  FOptions := [roMultiLine];
  {$if CompilerVersion > 26}
  Include(FOptions, roNotEmpty);
  {$endif}
  FPositions := TList.Create;
  FLengths := TList.Create;
end;

destructor TBCEditorRegexSearch.Destroy;
begin
  inherited;
  FPositions.Free;
  FLengths.Free;
end;

procedure TBCEditorRegexSearch.CaseSensitiveChanged;
begin
  if CaseSensitive then
    Exclude(FOptions, roIgnoreCase)
  else
    Include(FOptions, roIgnoreCase);
end;

procedure TBCEditorRegexSearch.Clear;
begin
  FPositions.Clear;
  FLengths.Clear;
end;

function TBCEditorRegexSearch.GetLength(const AIndex: Integer): Integer;
begin
  Result := Integer(FLengths[AIndex]);
end;

function TBCEditorRegexSearch.GetPattern: string;
begin
  Result := FPattern;
end;

function TBCEditorRegexSearch.GetResult(const AIndex: Integer): Integer;
begin
  Result := Integer(FPositions[AIndex]);
end;

function TBCEditorRegexSearch.GetResultCount: Integer;
begin
  Result := FPositions.Count;
end;

function TBCEditorRegexSearch.SearchAll(const ALines: TBCEditorLines): Integer;

  procedure AddResult(const APos, ALength: Integer);
  begin
    FPositions.Add(Pointer(APos));
    FLengths.Add(Pointer(ALength));
  end;

var
  LMatch: TMatch;
  LRegex: TRegEx;
begin
  Result := 0;
  Clear;
  Status := '';
  try
    LRegex := TRegEx.Create(FPattern, FOptions);
    LMatch := LRegex.Match(ALines.Text);
    while LMatch.Success do
    begin
      AddResult(LMatch.Index, LMatch.Length);
      LMatch := LMatch.NextMatch;
      Inc(Result);
    end;
  except
    on E: Exception do
      Status := E.Message;
  end;
end;

procedure TBCEditorRegexSearch.SetPattern(const AValue: string);
begin
  FPattern := AValue;
end;

end.

