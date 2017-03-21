unit BCEditor.Highlighter;

interface {********************************************************************}

uses
  Classes, SysUtils,
  Controls, Graphics,
  JsonDataObjects,
  BCEditor.Consts, BCEditor.Editor.CodeFolding, BCEditor.Types;

type
  TBCEditorHighlighter = class(TObject)
  type
    TAttribute = class;
    TToken = class;
    TRange = class;
    TDefaultParser = class;
    TDelimitersParser = class;
    TTokenNodeList = class;
    TBaseParser = class;
    TAbstractParserArray = array [AnsiChar] of TBaseParser;

    TMatchingPairMatch = record
      OpenToken: string;
      CloseToken: string;
      OpenTokenPos: TBCEditorTextPosition;
      CloseTokenPos: TBCEditorTextPosition;
      TokenAttribute: TAttribute;
    end;

    PMatchingPairToken = ^TMatchingPairToken;
    TMatchingPairToken = record
      OpenToken: string;
      CloseToken: string;
    end;

    PElement = ^TElement;
    TElement = record
      Background: TColor;
      Foreground: TColor;
      Name: string;
      FontStyles: TFontStyles;
    end;

    TInfo = class
      Author: record
        Comments: string;
        Email: string;
        Name: string;
      end;
      General: record
        Date: string;
        Sample: string;
        Version: string;
      end;
      procedure Clear();
    end;

    TAttribute = class(TPersistent)
    strict private
      procedure Changed; virtual;
    strict private
      FBackground: TColor;
      FBackgroundDefault: TColor;
      FElement: string;
      FEscapeChar: Char;
      FFontStyles: TFontStyles;
      FFontStylesDefault: TFontStyles;
      FForeground: TColor;
      FForegroundDefault: TColor;
      FName: string;
      FOnChange: TNotifyEvent;
      FParentBackground: Boolean;
      FParentForeground: Boolean;
      function GetBackgroundColorStored: Boolean;
      function GetFontStylesStored: Boolean;
      function GetForegroundColorStored: Boolean;
      procedure SetBackground(const AValue: TColor);
      procedure SetFontStyles(const AValue: TFontStyles);
      procedure SetForeground(const AValue: TColor);
    public
      procedure Assign(ASource: TPersistent); override;
      procedure AssignColorAndStyle(ASource: TAttribute);
      constructor Create(const AttributeName: string);
      procedure InternalSaveDefaultValues;
      property Name: string read FName write FName;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    published
      property Background: TColor read FBackground write SetBackground stored GetBackgroundColorStored;
      property Element: string read FElement write FElement;
      property EscapeChar: Char read FEscapeChar write FEscapeChar default BCEDITOR_NONE_CHAR;
      property FontStyles: TFontStyles read FFontStyles write SetFontStyles stored GetFontStylesStored;
      property Foreground: TColor read FForeground write SetForeground stored GetForegroundColorStored;
      property ParentBackground: Boolean read FParentBackground write FParentBackground;
      property ParentForeground: Boolean read FParentForeground write FParentForeground;
    end;

    TColors = class(TObject)
    strict private
      FElements: TList;
      FFileName: string;
      FInfo: TInfo;
      FName: string;
      FHighlighter: TBCEditorHighlighter;
    protected
      property Highlighter: TBCEditorHighlighter read FHighlighter;
    public
      procedure Clear;
      constructor Create(const AHighlighter: TBCEditorHighlighter);
      destructor Destroy; override;
      function GetElement(const Name: string): PElement;
      procedure LoadFromFile(const AFileName: string);
      procedure LoadFromResource(const ResName: string; const ResType: PChar);
      procedure LoadFromStream(AStream: TStream);
      property FileName: string read FFileName write FFileName;
      property Info: TInfo read FInfo write FInfo;
      property Name: string read FName write FName;
      property Styles: TList read FElements write FElements;
    end;

    TAbstractRule = class(TObject)
    strict private
      FTokenType: TBCEditorRangeType;
    public
      property TokenType: TBCEditorRangeType read FTokenType write FTokenType;
    end;

    TAbstractToken = class(TObject)
    strict private
      FAttribute: TAttribute;
      FBreakType: TBCEditorBreakType;
      FOpenRule: TAbstractRule;
    public
      procedure Clear;
      constructor Create; reintroduce; overload;
      constructor Create(const AToken: TAbstractToken); reintroduce; overload;
      constructor Create(const AHighlighterAttribute: TAttribute); reintroduce; overload;
      property Attribute: TAttribute read FAttribute write FAttribute;
      property BreakType: TBCEditorBreakType read FBreakType write FBreakType;
      property OpenRule: TAbstractRule read FOpenRule write FOpenRule;
    end;

    TMultiToken = class(TAbstractToken)
    strict private
      FSymbols: TStringList;
      function GetSymbol(const AIndex: Integer): string;
      procedure SetSymbol(const AIndex: Integer; const ASymbol: string);
    public
      function AddSymbol(const ASymbol: string): Integer;
      procedure Clear;
      constructor Create; reintroduce; overload;
      constructor Create(const AHighlighterAttribute: TAttribute); reintroduce; overload;
      constructor Create(const AMultiToken: TMultiToken); reintroduce; overload;
      procedure DeleteSymbol(const AIndex: Integer);
      destructor Destroy; override;
      function SymbolCount: Integer;
      property Symbols[const AIndex: Integer]: string read GetSymbol write SetSymbol;
    end;

    TToken = class(TAbstractToken)
    strict private
      FClosingToken: TToken;
      FSymbol: string;
      FTemporary: Boolean;
    public
      procedure Clear;
      constructor Create; overload;
      constructor Create(const AHighlighterAttribute: TAttribute); overload;
      constructor Create(const AMultiToken: TMultiToken; const AIndex: Integer); overload;
      constructor Create(const AToken: TToken); overload;
      property ClosingToken: TToken read FClosingToken write FClosingToken;
      property Symbol: string read FSymbol write FSymbol;
      property Temporary: Boolean read FTemporary write FTemporary;
    end;

    TTokenNode = class(TObject)
    strict private
      FBreakType: TBCEditorBreakType;
      FChar: Char;
      FNextNodes: TTokenNodeList;
      FToken: TToken;
    public
      constructor Create(const AChar: Char); overload;
      constructor Create(const AChar: Char; const AToken: TToken; const ABreakType: TBCEditorBreakType); overload;
      destructor Destroy; override;
      property BreakType: TBCEditorBreakType read FBreakType write FBreakType;
      property Char: Char read FChar write FChar;
      property NextNodes: TTokenNodeList read FNextNodes write FNextNodes;
      property Token: TToken read FToken write FToken;
    end;

    TTokenNodeList = class(TObject)
    strict private
      FNodeList: TList;
    public
      procedure AddNode(const ANode: TTokenNode);
      constructor Create;
      destructor Destroy; override;
      function FindNode(const AChar: Char): TTokenNode;
      function GetCount: Integer;
      function GetNode(const AIndex: Integer): TTokenNode;
      procedure SetNode(const AIndex: Integer; const AValue: TTokenNode);
      property Count: Integer read GetCount;
      property Nodes[const Aindex: Integer]: TTokenNode read GetNode write SetNode;
    end;

    TRule = class(TAbstractRule)
    private
      FAttribute: TAttribute;
      FStyle: string;
    protected
      FParent: TRange;
    public
      constructor Create;
      destructor Destroy; override;
      property Attribute: TAttribute read FAttribute;
      property Parent: TRange read FParent write FParent;
      property Style: string read FStyle;
    end;

    TKeyList = class(TRule)
    strict private
      FKeyList: TStringList;
    public
      constructor Create;
      destructor Destroy; override;
      property KeyList: TStringList read FKeyList write FKeyList;
    end;

    TSet = class(TRule)
    strict private
      FCharSet: TBCEditorCharSet;
    public
      constructor Create(ACharSet: TBCEditorCharSet = []);
      property CharSet: TBCEditorCharSet read FCharSet write FCharSet;
    end;

    TCaseFunction = function(const AChar: Char): Char;
    TStringCaseFunction = function(const AString: string): string;

    TRange = class(TRule)
    strict private
      FAlternativeCloseArray: TBCEditorArrayOfString;
      FAlternativeCloseArrayCount: Integer;
      FCaseFunct: TCaseFunction;
      FCaseSensitive: Boolean;
      FCloseOnEndOfLine: Boolean;
      FCloseOnTerm: Boolean;
      FCloseParent: Boolean;
      FCloseToken: TMultiToken;
      FClosingToken: TToken;
      FDefaultSymbols: TDefaultParser;
      FDefaultTermSymbol: TDelimitersParser;
      FDefaultToken: TToken;
      FDelimiters: TBCEditorCharSet;
      FKeyList: TList;
      FOpenBeginningOfLine: Boolean;
      FOpenToken: TMultiToken;
      FPrepared: Boolean;
      FRanges: TList;
      FSets: TList;
      FSkipWhitespace: Boolean;
      FStringCaseFunct: TStringCaseFunction;
      FSymbolList: TAbstractParserArray;
      FTokens: TList;
      FUseDelimitersForText: Boolean;
      function GetKeyList(const AIndex: Integer): TKeyList;
      function GetKeyListCount: Integer;
      function GetRange(const AIndex: Integer): TRange;
      function GetRangeCount: Integer;
      function GetSet(const AIndex: Integer): TSet;
      function GetSetCount: Integer;
      function GetToken(const AIndex: Integer): TToken;
      procedure SetAlternativeCloseArrayCount(const AValue: Integer);
      procedure SetCaseSensitive(const AValue: Boolean);
    public
      procedure AddKeyList(NewKeyList: TKeyList);
      procedure AddRange(NewRange: TRange);
      procedure AddSet(NewSet: TSet);
      procedure AddToken(const AToken: TToken);
      procedure AddTokenRange(const AOpenToken: string; AOpenTokenBreakType: TBCEditorBreakType; const ACloseToken: string;
        ACloseTokenBreakType: TBCEditorBreakType);
      procedure Clear;
      constructor Create(const AOpenToken: string = ''; const ACloseToken: string = ''); virtual;
      destructor Destroy; override;
      function FindToken(const AString: string): TToken;
      procedure Prepare(AParent: TRange);
      procedure Reset;
      procedure SetDelimiters(const ADelimiters: TBCEditorCharSet);
      property AlternativeCloseArray: TBCEditorArrayOfString read FAlternativeCloseArray write FAlternativeCloseArray;
      property AlternativeCloseArrayCount: Integer read FAlternativeCloseArrayCount write SetAlternativeCloseArrayCount;
      property CaseFunct: TCaseFunction read FCaseFunct;
      property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
      property CloseOnEndOfLine: Boolean read FCloseOnEndOfLine write FCloseOnEndOfLine;
      property CloseOnTerm: Boolean read FCloseOnTerm write FCloseOnTerm;
      property CloseParent: Boolean read FCloseParent write FCloseParent;
      property CloseToken: TMultiToken read FCloseToken write FCloseToken;
      property ClosingToken: TToken read FClosingToken write FClosingToken;
      property DefaultToken: TToken read FDefaultToken;
      property Delimiters: TBCEditorCharSet read FDelimiters write FDelimiters;
      property KeyList[const AIndex: Integer]: TKeyList read GetKeyList;
      property KeyListCount: Integer read GetKeyListCount;
      property OpenBeginningOfLine: Boolean read FOpenBeginningOfLine write FOpenBeginningOfLine;
      property OpenToken: TMultiToken read FOpenToken write FOpenToken;
      property Prepared: Boolean read FPrepared;
      property RangeCount: Integer read GetRangeCount;
      property Ranges[const AIndex: Integer]: TRange read GetRange;
      property SetCount: Integer read GetSetCount;
      property Sets[const AIndex: Integer]: TSet read GetSet;
      property SkipWhitespace: Boolean read FSkipWhitespace write FSkipWhitespace;
      property StringCaseFunct: TStringCaseFunction read FStringCaseFunct;
      property SymbolList: TAbstractParserArray read FSymbolList;
      property Tokens[const AIndex: Integer]: TToken read GetToken;
      property UseDelimitersForText: Boolean read FUseDelimitersForText write FUseDelimitersForText;
    end;

    TComments = class(TObject)
    strict private
      FChars: TBCEditorCharSet;
      FBlockComments: TBCEditorArrayOfString;
      FLineComments: TBCEditorArrayOfString;
      procedure AddChars(const AToken: string);
    public
      procedure AddBlockComment(const AOpenToken: string; const ACloseToken: string);
      procedure AddLineComment(const AToken: string);
      procedure Clear;
      destructor Destroy; override;
      property Chars: TBCEditorCharSet read FChars write FChars;
      property BlockComments: TBCEditorArrayOfString read FBlockComments;
      property LineComments: TBCEditorArrayOfString read FLineComments;
    end;

    TBaseParser = class abstract
    public
      function GetToken(ACurrentRule: TRange; const ALine: string; var AIndex: Integer; var AToken: TToken): Boolean;
        virtual; abstract;
    end;

    TParser = class(TBaseParser)
    strict private
      FHeadNode: TTokenNode;
      FSets: TList;
    public
      procedure AddSet(ASet: TSet);
      procedure AddTokenNode(const AString: string; AToken: TToken; ABreakType: TBCEditorBreakType);
      constructor Create(AChar: Char; AToken: TToken; ABreakType: TBCEditorBreakType); reintroduce; overload; virtual;
      constructor Create(ASet: TSet); reintroduce; overload; virtual;
      destructor Destroy; override;
      function GetToken(ACurrentRange: TRange; const ALine: string; var AIndex: Integer; var AToken: TToken): Boolean; override;
      property HeadNode: TTokenNode read FHeadNode;
      property Sets: TList read FSets;
    end;

    TDefaultParser = class(TBaseParser)
    strict private
      FToken: TToken;
    public
      constructor Create(AToken: TToken); reintroduce; virtual;
      destructor Destroy; override;
      function GetToken(ACurrentRange: TRange; const ALine: string; var AIndex: Integer; var AToken: TToken): Boolean; override;
      property Token: TToken read FToken;
    end;

    TDelimitersParser = class(TBaseParser)
    strict private
      FToken: TToken;
    public
      constructor Create(AToken: TToken); virtual;
      destructor Destroy; override;
      function GetToken(ACurrentRange: TRange; const ALine: string; var AIndex: Integer; var AToken: TToken): Boolean; override;
      property Token: TToken read FToken;
    end;

    TImportJSON = class(TObject)
    type
      EException = class(Exception);
    private
      FHighlighter: TBCEditorHighlighter;
      procedure ImportAttributes(AHighlighterAttribute: TAttribute; AAttributesObject: TJsonObject;
        const AElementPrefix: string);
      procedure ImportCodeFolding(ACodeFoldingObject: TJsonObject);
      procedure ImportCodeFoldingFoldRegion(ACodeFoldingRegion: TBCEditorCodeFolding.TRegion; ACodeFoldingObject: TJsonObject);
      procedure ImportCodeFoldingOptions(ACodeFoldingRegion: TBCEditorCodeFolding.TRegion; ACodeFoldingObject: TJsonObject);
      procedure ImportCodeFoldingSkipRegion(ACodeFoldingRegion: TBCEditorCodeFolding.TRegion; ACodeFoldingObject: TJsonObject);
      procedure ImportColors(AJSONObject: TJsonObject);
      procedure ImportColorsEditorProperties(AEditorObject: TJsonObject);
      procedure ImportColorsInfo(AInfoObject: TJsonObject);
      procedure ImportCompletionProposal(ACompletionProposalObject: TJsonObject);
      procedure ImportEditorProperties(AEditorObject: TJsonObject);
      procedure ImportElements(AColorsObject: TJsonObject);
      procedure ImportHighlighter(AJSONObject: TJsonObject);
      procedure ImportInfo(AInfoObject: TJsonObject);
      procedure ImportKeyList(AKeyList: TKeyList; KeyListObject: TJsonObject; const AElementPrefix: string);
      procedure ImportMatchingPair(AMatchingPairObject: TJsonObject);
      procedure ImportRange(ARange: TRange; RangeObject: TJsonObject; AParentRange: TRange = nil;
        ASkipBeforeSubRules: Boolean = False; const AElementPrefix: string = '');
      procedure ImportSet(ASet: TSet; SetObject: TJsonObject; const AElementPrefix: string);
    protected
      property Highlighter: TBCEditorHighlighter read FHighlighter;
    public
      constructor Create(AHighlighter: TBCEditorHighlighter); overload;
      procedure ImportColorsFromStream(AStream: TStream);
      procedure ImportFromStream(AStream: TStream);
    end;

  strict private
    FAllDelimiters: TBCEditorCharSet;
    FAttributes: TStringList;
    FBeginningOfLine: Boolean;
    FCodeFoldingRangeCount: Integer;
    FCodeFoldingRegions: TBCEditorCodeFolding.TRegions;
    FColors: TColors;
    FComments: TComments;
    FCompletionProposalSkipRegions: TBCEditorCodeFolding.TSkipRegions;
    FCurrentLine: string;
    FCurrentRange: TRange;
    FCurrentToken: TToken;
    FEditor: TCustomControl;
    FEndOfLine: Boolean;
    FFileName: string;
    FFoldCloseKeyChars: TBCEditorCharSet;
    FFoldOpenKeyChars: TBCEditorCharSet;
    FInfo: TInfo;
    FLoading: Boolean;
    FMainRules: TRange;
    FMatchingPairHighlight: Boolean;
    FMatchingPairs: TList;
    FMultiHighlighter: Boolean;
    FName: string;
    FPreviousEndOfLine: Boolean;
    FCurrentLineIndex: LongInt;
    FSkipCloseKeyChars: TBCEditorCharSet;
    FSkipOpenKeyChars: TBCEditorCharSet;
    FTemporaryCurrentTokens: TList;
    FTokenPosition: Integer;
    FWordBreakChars: TBCEditorCharSet;
    procedure AddAllAttributes(ARange: TRange);
    procedure UpdateAttributes(ARange: TRange; AParentRange: TRange);
  protected
    function GetAttribute(AIndex: Integer): TAttribute;
    procedure AddAttribute(AHighlighterAttribute: TAttribute);
    procedure Prepare;
    procedure Reset;
    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure SetCodeFoldingRangeCount(AValue: Integer);
    procedure SetWordBreakChars(AChars: TBCEditorCharSet);
    property Editor: TCustomControl read FEditor;
  public
    constructor Create(AEditor: TCustomControl);
    destructor Destroy; override;

    function GetCurrentRange: TRange;
    function GetCurrentRangeAttribute: TAttribute;
    function GetEndOfLine: Boolean;
    function GetTokenAttribute: TAttribute;
    function GetTokenKind: TBCEditorRangeType;
    function GetTokenLength: Integer;
    function GetTokenPosition: Integer;
    procedure AddKeyChar(AKeyCharType: TBCEditorKeyCharType; AChar: Char);
    procedure AddKeywords(var AStringList: TStringList);
    procedure Clear;
    procedure GetToken(var AResult: string);
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromResource(const ResName: string; const ResType: PChar);
    procedure LoadFromStream(AStream: TStream);
    procedure Next;
    procedure NextToEndOfLine;
    procedure ResetCurrentRange;
    procedure SetCurrentLine(const ANewValue: string);
    procedure SetCurrentRange(AValue: Pointer);
    procedure UpdateColors;
    property Attribute[AIndex: Integer]: TAttribute read GetAttribute;
    property Attributes: TStringList read FAttributes;
    property CodeFoldingRangeCount: Integer read FCodeFoldingRangeCount write SetCodeFoldingRangeCount;
    property CodeFoldingRegions: TBCEditorCodeFolding.TRegions read FCodeFoldingRegions write FCodeFoldingRegions;
    property Colors: TColors read FColors write FColors;
    property Comments: TComments read FComments write FComments;
    property CompletionProposalSkipRegions: TBCEditorCodeFolding.TSkipRegions read FCompletionProposalSkipRegions write FCompletionProposalSkipRegions;
    property FileName: string read FFileName write FFileName;
    property FoldCloseKeyChars: TBCEditorCharSet read FFoldCloseKeyChars write FFoldCloseKeyChars;
    property FoldOpenKeyChars: TBCEditorCharSet read FFoldOpenKeyChars write FFoldOpenKeyChars;
    property Info: TInfo read FInfo write FInfo;
    property xLoading: Boolean read FLoading write FLoading;
    property MainRules: TRange read FMainRules;
    property MatchingPairHighlight: Boolean read FMatchingPairHighlight write FMatchingPairHighlight default True;
    property MatchingPairs: TList read FMatchingPairs write FMatchingPairs;
    property MultiHighlighter: Boolean read FMultiHighlighter write FMultiHighlighter;
    property Name: string read FName write FName;
    property SkipCloseKeyChars: TBCEditorCharSet read FSkipCloseKeyChars write FSkipCloseKeyChars;
    property SkipOpenKeyChars: TBCEditorCharSet read FSkipOpenKeyChars write FSkipOpenKeyChars;
    property WordBreakChars: TBCEditorCharSet read FWordBreakChars write SetWordBreakChars;
  end;

implementation {***************************************************************}

uses
  Types, IOUtils, TypInfo,
  GraphUtil,
  BCEditor.Editor, BCEditor.Language, BCEditor.Utils,
  BCEditor.Editor.CompletionProposal;

resourcestring
  SBCEditorErrorInHighlighterParse = 'JSON parse error on line %d column %d: %s';
  SBCEditorErrorInHighlighterImport = 'Error in highlighter import: %s';

type
  TCustomBCEditor = class(BCEditor.Editor.TCustomBCEditor);

{ TBCEditorHighlighter.TInfo **************************************************}

procedure TBCEditorHighlighter.TInfo.Clear();
begin
  Author.Comments := '';
  Author.Email := '';
  Author.Name := '';
  General.Date := '';
  General.Sample := '';
  General.Version := '';
end;

{ TBCEditorHighlighter.TAttribute *********************************************}

constructor TBCEditorHighlighter.TAttribute.Create(const AttributeName: string);
begin
  inherited Create;

  FBackground := clNone;
  FForeground := clNone;
  FName := AttributeName;
  FEscapeChar := BCEDITOR_NONE_CHAR;
end;

procedure TBCEditorHighlighter.TAttribute.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TAttribute) then
    with ASource as TAttribute do
    begin
      Self.FName := FName;
      Self.AssignColorAndStyle(ASource as TAttribute);
    end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorHighlighter.TAttribute.AssignColorAndStyle(ASource: TAttribute);
var
  IsChanged: Boolean;
begin
  IsChanged := False;
  if FBackground <> ASource.FBackground then
  begin
    FBackground := ASource.FBackground;
    IsChanged := True;
  end;
  if FForeground <> ASource.FForeground then
  begin
    FForeground := ASource.FForeground;
    IsChanged := True;
  end;
  if FFontStyles <> ASource.FFontStyles then
  begin
    FFontStyles := ASource.FFontStyles;
    IsChanged := True;
  end;
  FParentForeground := ASource.ParentForeground;
  FParentBackground := ASource.ParentBackground;
  if IsChanged then
    Changed;
end;

procedure TBCEditorHighlighter.TAttribute.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBCEditorHighlighter.TAttribute.GetBackgroundColorStored: Boolean;
begin
  Result := FBackground <> FBackgroundDefault;
end;

function TBCEditorHighlighter.TAttribute.GetFontStylesStored: Boolean;
begin
  Result := FFontStyles <> FFontStylesDefault;
end;

function TBCEditorHighlighter.TAttribute.GetForegroundColorStored: Boolean;
begin
  Result := FForeground <> FForegroundDefault;
end;

procedure TBCEditorHighlighter.TAttribute.InternalSaveDefaultValues;
begin
  FForegroundDefault := FForeground;
  FBackgroundDefault := FBackground;
  FFontStylesDefault := FFontStyles;
end;

procedure TBCEditorHighlighter.TAttribute.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    Changed;
  end;
end;

procedure TBCEditorHighlighter.TAttribute.SetFontStyles(const AValue: TFontStyles);
begin
  if FFontStyles <> AValue then
  begin
    FFontStyles := AValue;
    Changed;
  end;
end;

procedure TBCEditorHighlighter.TAttribute.SetForeground(const AValue: TColor);
begin
  if FForeground <> AValue then
  begin
    FForeground := AValue;
    Changed;
  end;
end;

{ TBCEditorHighlighter.TColors ************************************************}

constructor TBCEditorHighlighter.TColors.Create(const AHighlighter: TBCEditorHighlighter);
begin
  inherited Create;

  FHighlighter := AHighlighter;

  FElements := TList.Create;
  FInfo := TInfo.Create;
end;

destructor TBCEditorHighlighter.TColors.Destroy;
begin
  Clear;
  FElements.Free;
  FInfo.Free;

  inherited;
end;

procedure TBCEditorHighlighter.TColors.Clear;
var
  LIndex: Integer;
begin
  for LIndex := FElements.Count - 1 downto 0 do
    Dispose(PElement(FElements.Items[LIndex]));
  FElements.Clear;
end;

function TBCEditorHighlighter.TColors.GetElement(const Name: string): PElement;
var
  LElement: PElement;
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to FElements.Count - 1 do
  begin
    LElement := PElement(FElements.Items[LIndex]);
    if LElement^.Name = Name then
      Exit(LElement);
  end;
end;

procedure TBCEditorHighlighter.TColors.LoadFromFile(const AFileName: string);
var
  LStream: TStream;
begin
  FFileName := AFileName;
  FName := TPath.GetFileNameWithoutExtension(AFileName);

  LStream := TCustomBCEditor(Highlighter.Editor).CreateFileStream(TCustomBCEditor(Highlighter.Editor).GetColorsFileName(AFileName));
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TBCEditorHighlighter.TColors.LoadFromResource(const ResName: string; const ResType: PChar);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, PChar(ResName), ResType);
  LoadFromStream(Stream);
  Stream.Free();
end;

procedure TBCEditorHighlighter.TColors.LoadFromStream(AStream: TStream);
begin
  TCustomBCEditor(Highlighter.Editor).BeginUpdate();
  with TImportJSON.Create(Highlighter) do
    try
      ImportColorsFromStream(AStream);
    finally
      Free;
    end;
  Highlighter.UpdateColors();
  TCustomBCEditor(Highlighter.Editor).EndUpdate();
end;

{ TBCEditorHighlighter.TAbstractToken *****************************************}

constructor TBCEditorHighlighter.TAbstractToken.Create;
begin
  inherited;

  FAttribute := nil;
  FOpenRule := nil;
  FBreakType := btUnspecified;
end;

constructor TBCEditorHighlighter.TAbstractToken.Create(const AToken: TAbstractToken);
begin
  inherited Create;
  FAttribute := AToken.Attribute;
  FBreakType := AToken.BreakType;
end;

constructor TBCEditorHighlighter.TAbstractToken.Create(const AHighlighterAttribute: TAttribute);
begin
  Create;
  FAttribute := AHighlighterAttribute;
end;

procedure TBCEditorHighlighter.TAbstractToken.Clear;
begin
  FBreakType := btUnspecified;
end;

{ TBCEditorHighlighter.TMultiToken ********************************************}

constructor TBCEditorHighlighter.TMultiToken.Create;
begin
  inherited;

  FSymbols := TStringList.Create;
  BreakType := btUnspecified;
end;

constructor TBCEditorHighlighter.TMultiToken.Create(const AHighlighterAttribute: TAttribute);
begin
  inherited;

  Create;
end;

constructor TBCEditorHighlighter.TMultiToken.Create(const AMultiToken: TMultiToken);
begin
  inherited Create(AMultiToken as TAbstractToken);

  Create;
end;

destructor TBCEditorHighlighter.TMultiToken.Destroy;
begin
  FSymbols.Free;
  FSymbols := nil;
  inherited;
end;

function TBCEditorHighlighter.TMultiToken.AddSymbol(const ASymbol: string): Integer;
begin
  Result := FSymbols.Add(ASymbol);
end;

procedure TBCEditorHighlighter.TMultiToken.Clear;
begin
  FSymbols.Clear;
end;

procedure TBCEditorHighlighter.TMultiToken.DeleteSymbol(const AIndex: Integer);
begin
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    FSymbols.Delete(AIndex)
end;

function TBCEditorHighlighter.TMultiToken.GetSymbol(const AIndex: Integer): string;
begin
  Result := '';
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    Result := FSymbols[AIndex]
end;

procedure TBCEditorHighlighter.TMultiToken.SetSymbol(const AIndex: Integer; const ASymbol: string);
begin
  if (AIndex > -1) and (AIndex < FSymbols.Count) then
    FSymbols[AIndex] := ASymbol
end;

function TBCEditorHighlighter.TMultiToken.SymbolCount: Integer;
begin
  Result := FSymbols.Count;
end;

{ TBCEditorHighlighter.TToken *************************************************}

constructor TBCEditorHighlighter.TToken.Create;
begin
  inherited Create;

  Symbol := '';
  FTemporary := False;
end;

constructor TBCEditorHighlighter.TToken.Create(const AHighlighterAttribute: TAttribute);
begin
  inherited Create(AHighlighterAttribute);
  Symbol := '';
end;

constructor TBCEditorHighlighter.TToken.Create(const AMultiToken: TMultiToken; const AIndex: Integer);
begin
  inherited Create(AMultiToken as TAbstractToken);

  Symbol := AMultiToken.Symbols[AIndex];
end;

constructor TBCEditorHighlighter.TToken.Create(const AToken: TToken);
begin
  inherited Create(AToken as TAbstractToken);

  Symbol := AToken.Symbol;
end;

procedure TBCEditorHighlighter.TToken.Clear;
begin
  Symbol := '';
end;

{ TBCEditorHighlighter.TTokenNode *********************************************}

constructor TBCEditorHighlighter.TTokenNode.Create(const AChar: Char);
begin
  inherited Create;

  FChar := AChar;
  FNextNodes := TBCEditorHighlighter.TTokenNodeList.Create;
  FToken := nil;
end;

constructor TBCEditorHighlighter.TTokenNode.Create(const AChar: Char; const AToken: TToken; const ABreakType: TBCEditorBreakType);
begin
  Create(AChar);
  FBreakType := ABreakType;
  FToken := AToken;
end;

destructor TBCEditorHighlighter.TTokenNode.Destroy;
begin
  FNextNodes.Free;
  FNextNodes := nil;
  inherited;
end;

{ TBCEditorHighlighter.TTokenNodeList *****************************************}

constructor TBCEditorHighlighter.TTokenNodeList.Create;
begin
  inherited;

  FNodeList := TList.Create;
end;

destructor TBCEditorHighlighter.TTokenNodeList.Destroy;
begin
  FreeList(FNodeList);
  inherited;
end;

procedure TBCEditorHighlighter.TTokenNodeList.AddNode(const ANode: TTokenNode);
begin
  FNodeList.Add(ANode);
end;

function TBCEditorHighlighter.TTokenNodeList.FindNode(const AChar: Char): TTokenNode;
var
  LIndex: Integer;
  LTokenNode: TBCEditorHighlighter.TTokenNode;
begin
  Result := nil;
  for LIndex := FNodeList.Count - 1 downto 0 do
  begin
    LTokenNode := TTokenNode(FNodeList.List[LIndex]);
    if LTokenNode.Char = AChar then
      Exit(LTokenNode);
  end;
end;

function TBCEditorHighlighter.TTokenNodeList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

function TBCEditorHighlighter.TTokenNodeList.GetNode(const AIndex: Integer): TTokenNode;
begin
  Result := TBCEditorHighlighter.TTokenNode(FNodeList[AIndex]);
end;

procedure TBCEditorHighlighter.TTokenNodeList.SetNode(const AIndex: Integer; const AValue: TTokenNode);
begin
  if AIndex < FNodeList.Count then
    TBCEditorHighlighter.TTokenNode(FNodeList[AIndex]).Free;
  FNodeList[AIndex] := AValue;
end;

{ TBCEditorHighlighter.TRule **************************************************}

constructor TBCEditorHighlighter.TRule.Create;
begin
  inherited;

  FAttribute := TAttribute.Create('');
end;

destructor TBCEditorHighlighter.TRule.Destroy;
begin
  FAttribute.Free;
  FAttribute := nil;

  inherited;
end;

{ TBCEditorHighlighter.TKeyList ***********************************************}

constructor TBCEditorHighlighter.TKeyList.Create;
begin
  inherited;

  FKeyList := TStringList.Create;
  FKeyList.Sorted := True;
  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

destructor TBCEditorHighlighter.TKeyList.Destroy;
begin
  FKeyList.Free;
  FKeyList := nil;

  inherited;
end;

{ TBCEditorHighlighter.TSet ***************************************************}

constructor TBCEditorHighlighter.TSet.Create(ACharSet: TBCEditorCharSet = []);
begin
  inherited Create;

  FCharSet := ACharSet;
  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

{ TBCEditorHighlighter.TRange *************************************************}

constructor TBCEditorHighlighter.TRange.Create(const AOpenToken: string; const ACloseToken: string);
begin
  inherited Create;

  FOpenToken := TMultiToken.Create;
  FCloseToken := TMultiToken.Create;
  AddTokenRange(AOpenToken, btUnspecified, ACloseToken, btUnspecified);

  SetCaseSensitive(False);

  FAlternativeCloseArrayCount := 0;

  FPrepared := False;

  FRanges := TList.Create;
  FKeyList := TList.Create;
  FSets := TList.Create;
  FTokens := TList.Create;

  FDelimiters := BCEDITOR_DEFAULT_DELIMITERS;

  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

destructor TBCEditorHighlighter.TRange.Destroy;
begin
  Clear;
  Reset;

  FOpenToken.Free;
  FOpenToken := nil;
  FCloseToken.Free;
  FCloseToken := nil;
  FAttribute.Free;
  FAttribute := nil;
  FKeyList.Free;
  FSets.Free;
  FTokens.Free;
  FTokens := nil;
  FRanges.Free;
  FRanges := nil;

  inherited;
end;

procedure TBCEditorHighlighter.TRange.AddKeyList(NewKeyList: TKeyList);
begin
  FKeyList.Add(NewKeyList);
  NewKeyList.Parent := Self;
end;

procedure TBCEditorHighlighter.TRange.AddRange(NewRange: TRange);
begin
  FRanges.Add(NewRange);
  NewRange.Parent := Self;
end;

procedure TBCEditorHighlighter.TRange.AddSet(NewSet: TSet);
begin
  FSets.Add(NewSet);
  NewSet.Parent := Self;
end;

procedure TBCEditorHighlighter.TRange.AddToken(const AToken: TToken);
var
  LCompare: Integer;
  LHigh: Integer;
  LLow: Integer;
  LMiddle: Integer;
  LToken: TToken;
begin
  LLow := 0;
  LHigh := FTokens.Count - 1;

  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) shr 1;
    LToken := TToken(FTokens.Items[LMiddle]);
    LCompare := CompareStr(LToken.Symbol, AToken.Symbol);

    if LCompare < 0 then
      LLow := LMiddle + 1
    else
    if LCompare > 0 then
      LHigh := LMiddle - 1
    else
      Exit;
  end;

  FTokens.Insert(LLow, AToken);
end;

procedure TBCEditorHighlighter.TRange.AddTokenRange(const AOpenToken: string; AOpenTokenBreakType: TBCEditorBreakType; const ACloseToken: string;
  ACloseTokenBreakType: TBCEditorBreakType);
begin
  FOpenToken.AddSymbol(AOpenToken);
  FOpenToken.BreakType := AOpenTokenBreakType;
  FCloseToken.AddSymbol(ACloseToken);
  FCloseToken.BreakType := ACloseTokenBreakType;
end;

procedure TBCEditorHighlighter.TRange.Clear;
var
  LIndex: Integer;
begin
  OpenToken.Clear;
  CloseToken.Clear;
  CloseOnTerm := False;
  CloseOnEndOfLine := False;
  CloseParent := False;
  Reset;

  if Assigned(FRanges) then
  for LIndex := 0 to FRanges.Count - 1 do
    TRange(FRanges[LIndex]).Clear;

  ClearList(FRanges);
  ClearList(FTokens);
  ClearList(FKeyList);
  ClearList(FSets);
end;

function TBCEditorHighlighter.TRange.FindToken(const AString: string): TToken;
var
  LCompare: Integer;
  LHigh: Integer;
  LLow: Integer;
  LMiddle: Integer;
  LToken: TToken;
begin
  Result := nil;

  LLow := 0;
  LHigh := FTokens.Count - 1;

  while LLow <= LHigh do
  begin
    LMiddle := LLow + (LHigh - LLow) shr 1;

    LToken := TToken(FTokens.Items[LMiddle]);
    LCompare := CompareStr(LToken.Symbol, AString);

    if LCompare = 0 then
      Exit(LToken)
    else
    if LCompare < 0 then
      LLow := LMiddle + 1
    else
      LHigh := LMiddle - 1;
  end;
end;

function TBCEditorHighlighter.TRange.GetKeyList(const AIndex: Integer): TKeyList;
begin
  Result := TKeyList(FKeyList[AIndex]);
end;

function TBCEditorHighlighter.TRange.GetKeyListCount: Integer;
begin
  Result := FKeyList.Count;
end;

function TBCEditorHighlighter.TRange.GetRange(const AIndex: Integer): TRange;
begin
  Result := TRange(FRanges[AIndex]);
end;

function TBCEditorHighlighter.TRange.GetRangeCount: Integer;
begin
  Result := FRanges.Count;
end;

function TBCEditorHighlighter.TRange.GetSet(const AIndex: Integer): TSet;
begin
  Result := TSet(FSets.List[AIndex]);
end;

function TBCEditorHighlighter.TRange.GetSetCount: Integer;
begin
  Result := FSets.Count;
end;

function TBCEditorHighlighter.TRange.GetToken(const AIndex: Integer): TToken;
begin
  Result := TToken(FTokens[AIndex]);
end;

procedure TBCEditorHighlighter.TRange.Prepare(AParent: TRange);
var
  LBreakType: TBCEditorBreakType;
  LFirstChar: Char;
  LIndex: Integer;
  LIndex2: Integer;
  LLength: Integer;
  LSymbol: string;

  function InsertTokenDefault(AToken: TToken; ARules: TRange; AAttribute: TAttribute): TToken;
  begin
    Result := ARules.FindToken(AToken.Symbol);
    if not Assigned(Result) then
      Result := AToken
    else
      AToken.Free;
    ARules.AddToken(Result);
    if not Assigned(Result.Attribute) then
      Result.Attribute := AAttribute;
  end;

  procedure InsertToken(AToken: TToken; ARules: TRange);
  var
    LToken: TToken;
  begin
    LToken := ARules.FindToken(AToken.Symbol);
    if not Assigned(LToken) then
      ARules.AddToken(AToken)
    else
    begin
      LToken.Attribute := AToken.Attribute;
      AToken.Free;
    end;
  end;

var
  LAnsiChar: AnsiChar;
  LChar: Char;
  LKeyList: TKeyList;
  LRange: TRange;
  LSet: TSet;
  LTempToken: TToken;
  LToken: TToken;
begin
  Reset;
  FDefaultToken := TToken.Create(Attribute);
  if Assigned(FDefaultTermSymbol) then
  begin
    FDefaultTermSymbol.Free;
    FDefaultTermSymbol := nil;
  end;
  FDefaultTermSymbol := TDelimitersParser.Create(TToken.Create(Attribute));
  FDefaultSymbols := TDefaultParser.Create(TToken.Create(Attribute));

  FDelimiters := FDelimiters + BCEDITOR_ABSOLUTE_DELIMITERS;

  if Assigned(FRanges) then
  for LIndex := 0 to FRanges.Count - 1 do
  begin
    LRange := TRange(FRanges[LIndex]);

    for LIndex2 := 0 to LRange.FOpenToken.SymbolCount - 1 do
    begin
      LTempToken := TToken.Create(LRange.OpenToken, LIndex2);
      LToken := InsertTokenDefault(LTempToken, Self, LRange.Attribute);
      LToken.OpenRule := LRange;

      LTempToken := TToken.Create(LRange.CloseToken, LIndex2);
      LToken.ClosingToken := InsertTokenDefault(LTempToken, LRange, LRange.Attribute);
    end;
    LRange.Prepare(Self);
  end;

  if Assigned(FKeyList) then
  for LIndex := 0 to FKeyList.Count - 1 do
  begin
    LKeyList := TKeyList(FKeyList[LIndex]);

    for LIndex2 := 0 to LKeyList.KeyList.Count - 1 do
    begin
      LTempToken := TToken.Create(LKeyList.Attribute);
      LTempToken.Symbol := LKeyList.KeyList[LIndex2];
      InsertToken(LTempToken, Self);
    end;
  end;

  if Assigned(FTokens) then
  for LIndex := 0 to FTokens.Count - 1 do
  begin
    LTempToken := TToken(FTokens[LIndex]);
    LLength := Length(LTempToken.Symbol);
    if LLength < 1 then
      Continue;

    LSymbol := LTempToken.Symbol;
    LFirstChar := LSymbol[1];

    if CharInSet(LFirstChar, FDelimiters) then
      LBreakType := btAny
    else
    if LTempToken.BreakType <> btUnspecified then
      LBreakType := LTempToken.BreakType
    else
      LBreakType := btTerm;

    LChar := CaseFunct(LFirstChar);
    if Ord(LChar) < 256 then
    begin
      LAnsiChar := AnsiChar(LChar);
      if not Assigned(SymbolList[LAnsiChar]) then
      begin
        if LLength = 1 then
          FSymbolList[LAnsiChar] := TParser.Create(LFirstChar, LTempToken, LBreakType)
        else
          FSymbolList[LAnsiChar] := TParser.Create(LFirstChar, FDefaultToken, LBreakType);
      end;
      if CharInSet(LSymbol[LLength], FDelimiters) then
        LBreakType := btAny;
      if LLength <> 1 then
        TParser(SymbolList[LAnsiChar]).AddTokenNode(StringCaseFunct(Copy(LSymbol, 2, LLength - 1)), LTempToken,
          LBreakType);
    end;
  end;

  if Assigned(FSets) then
    if FSets.Count > 0 then
    for LIndex := 0 to 255 do
    begin
      LAnsiChar := AnsiChar(CaseFunct(Char(LIndex)));
      for LIndex2 := 0 to FSets.Count - 1 do
      begin
        LSet := TSet(FSets.List[LIndex2]);
        if CharInSet(LAnsiChar, LSet.CharSet) then
          if not Assigned(SymbolList[LAnsiChar]) then
            FSymbolList[LAnsiChar] := TParser.Create(LSet)
          else
            TParser(SymbolList[LAnsiChar]).AddSet(LSet);
      end;
    end;

  for LIndex := 0 to 255 do
  begin
    LAnsiChar := AnsiChar(LIndex);
    if not Assigned(SymbolList[LAnsiChar]) then
    begin
      if CharInSet(LAnsiChar, FDelimiters) then
        FSymbolList[LAnsiChar] := FDefaultTermSymbol
      else
        FSymbolList[LAnsiChar] := FDefaultSymbols;
    end;
  end;

  FPrepared := True;
end;

procedure TBCEditorHighlighter.TRange.Reset;
var
  LAnsiChar: AnsiChar;
  LIndex: Integer;
begin
  if not FPrepared then
    Exit;

  for LIndex := 0 to 255 do
  begin
    LAnsiChar := AnsiChar(LIndex);
    if Assigned(SymbolList[LAnsiChar]) and (SymbolList[LAnsiChar] <> FDefaultTermSymbol) and (SymbolList[LAnsiChar] <> FDefaultSymbols) then
      FSymbolList[LAnsiChar].Free;
    FSymbolList[LAnsiChar] := nil;
  end;

  FDefaultToken.Free;
  FDefaultToken := nil;
  FDefaultTermSymbol.Free;
  FDefaultTermSymbol := nil;
  FDefaultSymbols.Free;
  FDefaultSymbols := nil;

  if Assigned(FRanges) then
  for LIndex := 0 to FRanges.Count - 1 do
    TRange(FRanges[LIndex]).Reset;

  ClearList(FTokens);
  FPrepared := False;
end;

procedure TBCEditorHighlighter.TRange.SetAlternativeCloseArrayCount(const AValue: Integer);
begin
  FAlternativeCloseArrayCount := AValue;
  SetLength(FAlternativeCloseArray, AValue);
end;

procedure TBCEditorHighlighter.TRange.SetCaseSensitive(const AValue: Boolean);
begin
  FCaseSensitive := AValue;
  if not AValue then
  begin
    FCaseFunct := CaseUpper;
    FStringCaseFunct := AnsiUpperCase;
  end
  else
  begin
    FCaseFunct := CaseNone;
    FStringCaseFunct := CaseStringNone;
  end;
end;

procedure TBCEditorHighlighter.TRange.SetDelimiters(const ADelimiters: TBCEditorCharSet);
var
  LIndex: Integer;
begin
  Delimiters := ADelimiters;
  for LIndex := 0 to RangeCount - 1 do
    Ranges[LIndex].SetDelimiters(ADelimiters);
end;

{ TBCEditorHighlighter.TComments **********************************************}

destructor TBCEditorHighlighter.TComments.Destroy;
begin
  Clear;

  inherited Destroy;
end;

procedure TBCEditorHighlighter.TComments.AddChars(const AToken: string);
var
  LIndex: Integer;
begin
  for LIndex := 1 to Length(AToken) do
    FChars := FChars + [AToken[LIndex]];
end;

procedure TBCEditorHighlighter.TComments.AddBlockComment(const AOpenToken: string; const ACloseToken: string);
var
  LIndex: Integer;
  LLength: Integer;
begin
  LLength := Length(FBlockComments);

  for LIndex := 0 to LLength - 1 do
  begin
    if (FBlockComments[LIndex] = AOpenToken) and (FBlockComments[LIndex + 1] = ACloseToken) then
      Exit;
  end;

  SetLength(FBlockComments, LLength + 2);
  FBlockComments[LLength] := AOpenToken;
  FBlockComments[LLength + 1] := ACloseToken;

  AddChars(AOpenToken);
  AddChars(ACloseToken);
end;

procedure TBCEditorHighlighter.TComments.AddLineComment(const AToken: string);
var
  LIndex: Integer;
  LLength: Integer;
begin
  LLength := Length(FLineComments);

  for LIndex := 0 to LLength - 1 do
    if FLineComments[LIndex] = AToken then
      Exit;

  SetLength(FLineComments, LLength + 1);
  FLineComments[LLength] := AToken;

  AddChars(AToken);
end;

procedure TBCEditorHighlighter.TComments.Clear;
begin
  SetLength(FBlockComments, 0);
  SetLength(FLineComments, 0);
  FChars := [];
end;

{ TBCEditorHighlighter.TParser ************************************************}

constructor TBCEditorHighlighter.TParser.Create(AChar: Char; AToken: TToken; ABreakType: TBCEditorBreakType);
begin
  inherited Create;

  FHeadNode := TTokenNode.Create(AChar, AToken, ABreakType);
  FSets := TList.Create;
end;

constructor TBCEditorHighlighter.TParser.Create(ASet: TSet);
begin
  inherited Create;

  FSets := TList.Create;
  AddSet(ASet);
end;

destructor TBCEditorHighlighter.TParser.Destroy;
begin
  if Assigned(FHeadNode) then
  begin
    FHeadNode.Free;
    FHeadNode := nil;
  end;
  FSets.Clear;
  FSets.Free;
  FSets := nil;
  inherited;
end;

procedure TBCEditorHighlighter.TParser.AddSet(ASet: TSet);
begin
  Sets.Add(ASet);
end;

procedure TBCEditorHighlighter.TParser.AddTokenNode(const AString: string; AToken: TToken; ABreakType: TBCEditorBreakType);
var
  LChar: Char;
  LIndex: Integer;
  LLength: Integer;
  LTokenNode: TTokenNode;
  LTokenNodeList: TTokenNodeList;
begin
  LTokenNodeList := HeadNode.NextNodes;
  LTokenNode := nil;
  LLength := Length(AString);
  for LIndex := 1 to LLength do
  begin
    LChar := AString[LIndex];
    LTokenNode := LTokenNodeList.FindNode(LChar);
    if not Assigned(LTokenNode) then
    begin
      LTokenNode := TTokenNode.Create(LChar);
      LTokenNodeList.AddNode(LTokenNode);
    end;
    LTokenNodeList := LTokenNode.NextNodes;
  end;
  LTokenNode.BreakType := ABreakType;
  LTokenNode.Token := AToken;
end;

function TBCEditorHighlighter.TParser.GetToken(ACurrentRange: TRange;
  const ALine: string; var AIndex: Integer; var AToken: TToken): Boolean;
var
  LAllowedDelimiters: TBCEditorCharSet;
  LCurrentTokenNode: TTokenNode;
  LFindTokenNode: TTokenNode;
  LIndex: Integer;
  LLinePos: PChar;
  LLineEndPos: PChar;
  LNextPosition: Integer;
  LPreviousPosition: Integer;
  LSet: TSet;
  LStartPosition: Integer;
  LStartTokenNode: TTokenNode;
begin
  Result := False;

  LStartPosition := AIndex;
  if Assigned(HeadNode) then
  begin
    LCurrentTokenNode := HeadNode;
    LNextPosition := LStartPosition;
    LStartTokenNode := nil;
    repeat
      if Assigned(LStartTokenNode) then
      begin
        LCurrentTokenNode := LStartTokenNode;
        AIndex := LNextPosition;
        LStartTokenNode := nil;
      end;
      if Assigned(LCurrentTokenNode.Token) then
        LFindTokenNode := LCurrentTokenNode
      else
        LFindTokenNode := nil;
      LPreviousPosition := AIndex;
      while ((LCurrentTokenNode.NextNodes.Count > 0) and (AIndex < Length(ALine))) do
      begin
        Inc(AIndex);
        LCurrentTokenNode := LCurrentTokenNode.NextNodes.FindNode(ACurrentRange.CaseFunct(ALine[1 + AIndex]));
        if not Assigned(LCurrentTokenNode) then
        begin
          Dec(AIndex);
          Break;
        end;

        if Assigned(LCurrentTokenNode.Token) then
        begin
          LFindTokenNode := LCurrentTokenNode;
          LPreviousPosition := AIndex;
        end;

        if not Assigned(LStartTokenNode) then
          if CharInSet(LCurrentTokenNode.Char, ACurrentRange.Delimiters) then
          begin
            LStartTokenNode := LCurrentTokenNode;
            lNextPosition := AIndex;
          end;
      end;

      AIndex := LPreviousPosition;

      if (not Assigned(LFindTokenNode) or not Assigned(LFindTokenNode.Token)
        or ((LFindTokenNode.Token.Attribute.EscapeChar <> BCEDITOR_NONE_CHAR)
          and (LStartPosition > 0)
          and (ALine[1 + LStartPosition - 1] = LFindTokenNode.Token.Attribute.EscapeChar))) then
        Continue;

      if (AIndex <= Length(ALine)) then
        Inc(AIndex);

      if ((LFindTokenNode.BreakType = btAny) or (AIndex = Length(ALine)) or CharInSet(ALine[1 + AIndex], ACurrentRange.Delimiters)) then
      begin
        AToken := LFindTokenNode.Token;
        Exit(True);
      end;
    until not Assigned(LStartTokenNode);
  end;

  LAllowedDelimiters := ACurrentRange.Delimiters;
  for LIndex := 0 to Sets.Count - 1 do
    LAllowedDelimiters := LAllowedDelimiters - TSet(Sets.List[LIndex]).CharSet;

  if (AIndex < Length(ALine)) then
    for LIndex := 0 to Sets.Count - 1 do
    begin
      AIndex := LStartPosition;
      LSet := TSet(Sets.List[LIndex]);
      LLinePos := @ALine[1 + AIndex];
      LLineEndPos := @ALine[Length(ALine)];
      repeat
        Inc(AIndex);
        Inc(LLinePos);
      until (not CharInSet(LLinePos^, LSet.CharSet) or (LLinePos = LLineEndPos));

      if CharInSet(LLinePos^, LAllowedDelimiters) then
      begin
        AToken := TToken.Create(LSet.Attribute);
        AToken.Temporary := True;
        Exit(True);
      end;
    end;
  AIndex := LStartPosition + 1;
end;

{ TBCEditorHighlighter.TDefaultParser *****************************************}

constructor TBCEditorHighlighter.TDefaultParser.Create(AToken: TToken);
begin
  FToken := AToken;
end;

destructor TBCEditorHighlighter.TDefaultParser.Destroy;
begin
  FToken.Free;
  FToken := nil;
  inherited;
end;

function TBCEditorHighlighter.TDefaultParser.GetToken(ACurrentRange: TRange; const ALine: string; var AIndex: Integer;
  var AToken: TToken): Boolean;
begin
  Inc(AIndex);
  Result := False;
end;

{ TBCEditorHighlighter.TBCEditorHighlighter.TDelimitersParser **************************************}

constructor TBCEditorHighlighter.TDelimitersParser.Create(AToken: TToken);
begin
  inherited Create;
  FToken := AToken;
end;

destructor TBCEditorHighlighter.TDelimitersParser.Destroy;
begin
  FToken.Free;
  FToken := nil;
  inherited;
end;

function TBCEditorHighlighter.TDelimitersParser.GetToken(ACurrentRange: TRange; const ALine: string; var AIndex: Integer; var AToken: TToken): Boolean;
begin
  if (AIndex < Length(ALine)) then
    Inc(AIndex);
  AToken := Self.Token;
  Result := True;
end;

{ TBCEditorHighlighter.TImportJSON ********************************************}

function StringToColorDef(const AString: string; const DefaultColor: TColor): Integer;
begin
  if Trim(AString) = '' then
    Result := DefaultColor
  else
  if Pos('clWeb', AString) = 1 then
    Result := WebColorNameToColor(AString)
  else
    Result := StringToColor(AString);
end;

function StrToSet(const AString: string): TBCEditorCharSet;
var
  LIndex: Integer;
begin
  Result := [];
  for LIndex := 1 to Length(AString) do
    Result := Result + [AString[LIndex]];
end;

function StrToStrDef(const AString: string; const AStringDef: string): string;
begin
  if Trim(AString) = '' then
    Result := AStringDef
  else
    Result := AString
end;

function StrToFontStyle(const AString: string): TFontStyles;
begin
  Result := [];
  if Pos('Bold', AString) > 0 then
    Include(Result, fsBold);
  if Pos('Italic', AString) > 0 then
    Include(Result, fsItalic);
  if Pos('Underline', AString) > 0 then
    Include(Result, fsUnderline);
  if Pos('StrikeOut', AString) > 0 then
    Include(Result, fsStrikeOut);
end;

function StrToBreakType(const AString: string): TBCEditorBreakType;
begin
  if AString = 'Any' then
    Result := btAny
  else
  if (AString = 'Term') or (AString = '') then
    Result := btTerm
  else
    Result := btUnspecified;
end;

function StrToRegionType(const AString: string): TBCEditorCodeFolding.TSkipRegions.TItem.TItemType;
begin
  if AString = 'SingleLine' then
    Result := ritSingleLineComment
  else
  if AString = 'MultiLine' then
    Result := ritMultiLineComment
  else
  if AString = 'SingleLineString' then
    Result := ritSingleLineString
  else
    Result := ritMultiLineString
end;

function StrToRangeType(const AString: string): TBCEditorRangeType;
var
  LIndex: Integer;
begin
  LIndex := GetEnumValue(TypeInfo(TBCEditorRangeType), 'tt' + AString);
  if LIndex = -1 then
    Result := ttUnspecified
  else
    Result := TBCEditorRangeType(LIndex);
end;

constructor TBCEditorHighlighter.TImportJSON.Create(AHighlighter: TBCEditorHighlighter);
begin
  inherited Create;
  FHighlighter := AHighlighter;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportAttributes(AHighlighterAttribute: TAttribute;
  AAttributesObject: TJsonObject; const AElementPrefix: string);
begin
  if Assigned(AAttributesObject) then
  begin
    AHighlighterAttribute.Element := AElementPrefix + AAttributesObject['Element'].Value;
    AHighlighterAttribute.ParentForeground := StrToBoolDef(AAttributesObject['ParentForeground'].Value, False);
    AHighlighterAttribute.ParentBackground := StrToBoolDef(AAttributesObject['ParentBackground'].Value, True);
    if AAttributesObject.Contains('EscapeChar') then
      AHighlighterAttribute.EscapeChar := AAttributesObject['EscapeChar'].Value[1];
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportCodeFolding(ACodeFoldingObject: TJsonObject);
var
  i: Integer;
  LArray: TJsonArray;
  LCodeFoldingObject: TJsonObject;
  LCount: Integer;
begin
  if not Assigned(ACodeFoldingObject) then
    Exit;
  LArray := ACodeFoldingObject['Ranges'].ArrayValue;
  LCount := LArray.Count;
  if LCount > 0 then
  begin
    FHighlighter.CodeFoldingRangeCount := LCount;
    for i := 0 to LCount - 1 do
    begin
      FHighlighter.CodeFoldingRegions[i] := TBCEditorCodeFolding.TRegion.Create(TBCEditorCodeFolding.TRegion.TItem);
      LCodeFoldingObject := LArray.Items[i].ObjectValue;

      ImportCodeFoldingOptions(FHighlighter.CodeFoldingRegions[i], LCodeFoldingObject);
      ImportCodeFoldingSkipRegion(FHighlighter.CodeFoldingRegions[i], LCodeFoldingObject);
      ImportCodeFoldingFoldRegion(FHighlighter.CodeFoldingRegions[i], LCodeFoldingObject);
    end;
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportCodeFoldingFoldRegion(ACodeFoldingRegion: TBCEditorCodeFolding.TRegion;
  ACodeFoldingObject: TJsonObject);
var
  LCloseToken: string;
  LFileName: string;
  LFileStream: TStream;
  LFoldRegionArray: TJsonArray;
  LIndex: Integer;
  LIndex2: Integer;
  LJsonDataValue: PJsonDataValue;
  LJSONObject: TJsonObject;
  LMemberObject: TJsonObject;
  LOpenToken: string;
  LRegionItem: TBCEditorCodeFolding.TRegion.TItem;
  LSkipIfFoundAfterOpenTokenArray: TJsonArray;
begin
  if ACodeFoldingObject.Contains('FoldRegion') then
  begin
    LFoldRegionArray := ACodeFoldingObject['FoldRegion'].ArrayValue;
    for LIndex := 0 to LFoldRegionArray.Count - 1 do
    begin
      LJsonDataValue := LFoldRegionArray.Items[LIndex];
      LOpenToken := LJsonDataValue.ObjectValue['OpenToken'].Value;
      LCloseToken := LJsonDataValue.ObjectValue['CloseToken'].Value;

      if FHighlighter.MultiHighlighter then
      begin
        { Multi highlighter code folding fold region include }
        LFileName := LJsonDataValue.ObjectValue['File'].Value;
        if LFileName <> '' then
        begin
          LFileStream := TCustomBCEditor(Highlighter.Editor).CreateFileStream(TCustomBCEditor(Highlighter.Editor).GetHighlighterFileName(LFileName));
          LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
          if Assigned(LJSONObject) then
          try
            if LJSONObject.Contains('CodeFolding') then
              ImportCodeFoldingFoldRegion(ACodeFoldingRegion, LJSONObject['CodeFolding']['Ranges'].ArrayValue.Items[0].ObjectValue);
          finally
            LJSONObject.Free;
            LFileStream.Free;
          end;
        end;
        { Skip duplicates }
        if ACodeFoldingRegion.Contains(LOpenToken, LCloseToken) then
          Continue;
      end;

      LRegionItem := ACodeFoldingRegion.Add(LOpenToken, LCloseToken);

      LMemberObject := LJsonDataValue.ObjectValue['Properties'].ObjectValue;
      if Assigned(LMemberObject) then
      begin
        { Options }
        LRegionItem.OpenTokenBeginningOfLine := LMemberObject.B['OpenTokenBeginningOfLine'];
        LRegionItem.CloseTokenBeginningOfLine := LMemberObject.B['CloseTokenBeginningOfLine'];
        LRegionItem.SharedClose := LMemberObject.B['SharedClose'];
        LRegionItem.OpenIsClose := LMemberObject.B['OpenIsClose'];
        LRegionItem.OpenTokenCanBeFollowedBy := LMemberObject['OpenTokenCanBeFollowedBy'].Value;
        LRegionItem.TokenEndIsPreviousLine := LMemberObject.B['TokenEndIsPreviousLine'];
        LRegionItem.NoSubs := LMemberObject.B['NoSubs'];
        LRegionItem.BeginWithBreakChar := LMemberObject.B['BeginWithBreakChar'];

        LSkipIfFoundAfterOpenTokenArray := LMemberObject['SkipIfFoundAfterOpenToken'].ArrayValue;
        if LSkipIfFoundAfterOpenTokenArray.Count > 0 then
        begin
          LRegionItem.SkipIfFoundAfterOpenTokenArrayCount := LSkipIfFoundAfterOpenTokenArray.Count;
          for LIndex2 := 0 to LRegionItem.SkipIfFoundAfterOpenTokenArrayCount - 1 do
            LRegionItem.SkipIfFoundAfterOpenTokenArray[LIndex2] := LSkipIfFoundAfterOpenTokenArray.Items[LIndex2].Value;
        end;

        if LMemberObject.Contains('BreakCharFollows') then
          LRegionItem.BreakCharFollows := LMemberObject.B['BreakCharFollows'];
        LRegionItem.BreakIfNotFoundBeforeNextRegion := LMemberObject['BreakIfNotFoundBeforeNextRegion'].Value;
        LRegionItem.OpenTokenEnd := LMemberObject['OpenTokenEnd'].Value;
        LRegionItem.ShowGuideLine := StrToBoolDef(LMemberObject['ShowGuideLine'].Value, True);
        LRegionItem.OpenTokenBreaksLine := LMemberObject.B['OpenTokenBreaksLine'];
      end;
      if LOpenToken <> '' then
        FHighlighter.AddKeyChar(ctFoldOpen, LOpenToken[1]);
      if LRegionItem.BreakIfNotFoundBeforeNextRegion <> '' then
        FHighlighter.AddKeyChar(ctFoldOpen, LRegionItem.BreakIfNotFoundBeforeNextRegion[1]);
      if LCloseToken <> '' then
        FHighlighter.AddKeyChar(ctFoldClose, LCloseToken[1]);
    end;
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportCodeFoldingOptions(ACodeFoldingRegion: TBCEditorCodeFolding.TRegion;
  ACodeFoldingObject: TJsonObject);
var
  LCodeFoldingObject: TJsonObject;
begin
  FHighlighter.MatchingPairHighlight := True;

  if ACodeFoldingObject.Contains('Options') then
  begin
    LCodeFoldingObject := ACodeFoldingObject['Options'].ObjectValue;

    if LCodeFoldingObject.Contains('OpenToken') then
      ACodeFoldingRegion.OpenToken := LCodeFoldingObject['OpenToken'].Value;

    if LCodeFoldingObject.Contains('CloseToken') then
      ACodeFoldingRegion.CloseToken := LCodeFoldingObject['CloseToken'].Value;

    if LCodeFoldingObject.Contains('EscapeChar') then
      ACodeFoldingRegion.EscapeChar := LCodeFoldingObject['EscapeChar'].Value[1];

    if LCodeFoldingObject.Contains('FoldTags') then
      ACodeFoldingRegion.FoldTags := LCodeFoldingObject.B['FoldTags'];

    if LCodeFoldingObject.Contains('StringEscapeChar') then
      ACodeFoldingRegion.StringEscapeChar := LCodeFoldingObject['StringEscapeChar'].Value[1];

    if LCodeFoldingObject.Contains('MatchingPairHighlight') then
      FHighlighter.MatchingPairHighlight := LCodeFoldingObject.B['MatchingPairHighlight'];
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportCodeFoldingSkipRegion(ACodeFoldingRegion: TBCEditorCodeFolding.TRegion;
  ACodeFoldingObject: TJsonObject);
var
  LCloseToken: string;
  LFileName: string;
  LFileStream: TStream;
  LIndex: Integer;
  LJsonDataValue: PJsonDataValue;
  LJSONObject: TJsonObject;
  LOpenToken: string;
  LRegionItem: TBCEditorCodeFolding.TRegion.TItem;
  LSkipRegionArray: TJsonArray;
  LSkipRegionItem: TBCEditorCodeFolding.TSkipRegions.TItem;
  LSkipRegionType: TBCEditorCodeFolding.TSkipRegions.TItem.TItemType;
begin
  if ACodeFoldingObject.Contains('SkipRegion') then
  begin
    LSkipRegionArray := ACodeFoldingObject['SkipRegion'].ArrayValue;
    for LIndex := 0 to LSkipRegionArray.Count - 1 do
    begin
      LJsonDataValue := LSkipRegionArray.Items[LIndex];
      LOpenToken := LJsonDataValue.ObjectValue['OpenToken'].Value;
      LCloseToken := LJsonDataValue.ObjectValue['CloseToken'].Value;

      if FHighlighter.MultiHighlighter then
      begin
        { Multi highlighter code folding skip region include }
        LFileName := LJsonDataValue.ObjectValue['File'].Value;
        if LFileName <> '' then
        begin
          LFileStream := TCustomBCEditor(Highlighter.Editor).CreateFileStream(TCustomBCEditor(Highlighter.Editor).GetHighlighterFileName(LFileName));
          LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
          if Assigned(LJSONObject) then
          try
            if LJSONObject.Contains('CodeFolding') then
              ImportCodeFoldingSkipRegion(ACodeFoldingRegion, LJSONObject['CodeFolding']['Ranges'].ArrayValue.Items[0].ObjectValue);
          finally
            LJSONObject.Free;
            LFileStream.Free;
          end;
        end;
        { Skip duplicates }
        if ACodeFoldingRegion.SkipRegions.Contains(LOpenToken, LCloseToken) then
          Continue;
      end;

      LSkipRegionType := StrToRegionType(LJsonDataValue.ObjectValue['RegionType'].Value);
      if (LSkipRegionType = ritMultiLineComment) and (cfoFoldMultilineComments in TCustomBCEditor(FHighlighter.Editor).CodeFolding.Options) then
      begin
        LRegionItem := ACodeFoldingRegion.Add(LOpenToken, LCloseToken);
        LRegionItem.NoSubs := True;
        FHighlighter.AddKeyChar(ctFoldOpen, LOpenToken[1]);
        if LCloseToken <> '' then
          FHighlighter.AddKeyChar(ctFoldClose, LCloseToken[1]);
      end
      else
      begin
        LSkipRegionItem := ACodeFoldingRegion.SkipRegions.Add(LOpenToken, LCloseToken);
        LSkipRegionItem.RegionType := LSkipRegionType;
        LSkipRegionItem.SkipEmptyChars := LJsonDataValue.ObjectValue.B['SkipEmptyChars'];
        LSkipRegionItem.SkipIfNextCharIsNot := BCEDITOR_NONE_CHAR;
        if LJsonDataValue.ObjectValue.Contains('NextCharIsNot') then
          LSkipRegionItem.SkipIfNextCharIsNot := LJsonDataValue.ObjectValue['NextCharIsNot'].Value[1];
        if LOpenToken <> '' then
          FHighlighter.AddKeyChar(ctSkipOpen, LOpenToken[1]);
        if LCloseToken <> '' then
          FHighlighter.AddKeyChar(ctSkipClose, LCloseToken[1]);
      end;
    end;
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportColors(AJSONObject: TJsonObject);
var
  LColorsObject: TJsonObject;
begin
  FHighlighter.Colors.Clear;

  LColorsObject := AJSONObject['Colors'];
  ImportColorsInfo(LColorsObject['Info'].ObjectValue);
  ImportColorsEditorProperties(LColorsObject['Editor'].ObjectValue);
  ImportElements(AJSONObject['Colors'].ObjectValue);
end;

procedure TBCEditorHighlighter.TImportJSON.ImportColorsEditorProperties(AEditorObject: TJsonObject);
var
  LColorsObject: TJsonObject;
  LFontSizesObject: TJsonObject;
  LFontsObject: TJsonObject;
  LIndex: Integer;
begin
  if Assigned(AEditorObject) then
    with TCustomBCEditor(Highlighter.Editor) do
    begin
      LColorsObject := AEditorObject['Colors'].ObjectValue;
      if Assigned(LColorsObject) then
      begin
        BackgroundColor := StringToColorDef(LColorsObject['Background'].Value, BackgroundColor);
        ActiveLine.Color := StringToColorDef(LColorsObject['ActiveLineBackground'].Value, ActiveLine.Color);
        CodeFolding.Colors.ActiveLineBackground := StringToColorDef(LColorsObject['CodeFoldingActiveLineBackground'].Value, CodeFolding.Colors.ActiveLineBackground);
        CodeFolding.Colors.Background := StringToColorDef(LColorsObject['CodeFoldingBackground'].Value, CodeFolding.Colors.Background);
        CodeFolding.Colors.CollapsedLine := StringToColorDef(LColorsObject['CodeFoldingCollapsedLine'].Value, CodeFolding.Colors.CollapsedLine);
        CodeFolding.Colors.FoldingLine := StringToColorDef(LColorsObject['CodeFoldingFoldingLine'].Value, CodeFolding.Colors.FoldingLine);
        CodeFolding.Colors.FoldingLineHighlight := StringToColorDef(LColorsObject['CodeFoldingFoldingLineHighlight'].Value, CodeFolding.Colors.FoldingLineHighlight);
        CodeFolding.Colors.Indent := StringToColorDef(LColorsObject['CodeFoldingIndent'].Value, CodeFolding.Colors.Indent);
        CodeFolding.Colors.IndentHighlight := StringToColorDef(LColorsObject['CodeFoldingIndentHighlight'].Value, CodeFolding.Colors.IndentHighlight);
        CodeFolding.Hint.Colors.Background := StringToColorDef(LColorsObject['CodeFoldingHintBackground'].Value, CodeFolding.Hint.Colors.Background);
        CodeFolding.Hint.Colors.Border := StringToColorDef(LColorsObject['CodeFoldingHintBorder'].Value, CodeFolding.Hint.Colors.Border);
        CodeFolding.Hint.Font.Color := StringToColorDef(LColorsObject['CodeFoldingHintText'].Value, CodeFolding.Hint.Font.Color);
        CodeFolding.Hint.Indicator.Colors.Background := StringToColorDef(LColorsObject['CodeFoldingHintIndicatorBackground'].Value, CodeFolding.Hint.Indicator.Colors.Background);
        CodeFolding.Hint.Indicator.Colors.Border := StringToColorDef(LColorsObject['CodeFoldingHintIndicatorBorder'].Value, CodeFolding.Hint.Indicator.Colors.Border);
        CodeFolding.Hint.Indicator.Colors.Mark := StringToColorDef(LColorsObject['CodeFoldingHintIndicatorMark'].Value, CodeFolding.Hint.Indicator.Colors.Mark);
        CompletionProposal.Colors.Background := StringToColorDef(LColorsObject['CompletionProposalBackground'].Value, CompletionProposal.Colors.Background);
        CompletionProposal.Colors.Foreground := StringToColorDef(LColorsObject['CompletionProposalForeground'].Value, CompletionProposal.Colors.Foreground);
        CompletionProposal.Colors.SelectedBackground := StringToColorDef(LColorsObject['CompletionProposalSelectedBackground'].Value, CompletionProposal.Colors.SelectedBackground);
        CompletionProposal.Colors.SelectedText := StringToColorDef(LColorsObject['CompletionProposalSelectedText'].Value, CompletionProposal.Colors.SelectedText);
        LeftMargin.Colors.Background := StringToColorDef(LColorsObject['LeftMarginBackground'].Value, LeftMargin.Colors.Background);
        LeftMargin.Colors.Border := StringToColorDef(LColorsObject['LeftMarginBorder'].Value, LeftMargin.Colors.Border);
        LeftMargin.Font.Color := StringToColorDef(LColorsObject['LeftMarginLineNumbers'].Value, LeftMargin.Font.Color);
        LeftMargin.Colors.BookmarkPanelBackground := StringToColorDef(LColorsObject['LeftMarginBookmarkPanel'].Value, LeftMargin.Colors.BookmarkPanelBackground);
        LeftMargin.Colors.LineStateModified := StringToColorDef(LColorsObject['LeftMarginLineStateModified'].Value, LeftMargin.Colors.LineStateModified);
        LeftMargin.Colors.LineStateNormal := StringToColorDef(LColorsObject['LeftMarginLineStateNormal'].Value, LeftMargin.Colors.LineStateNormal);
        Minimap.Colors.Background := StringToColorDef(LColorsObject['MinimapBackground'].Value, Minimap.Colors.Background);
        Minimap.Colors.Bookmark := StringToColorDef(LColorsObject['MinimapBookmark'].Value, Minimap.Colors.Bookmark);
        Minimap.Colors.VisibleLines := StringToColorDef(LColorsObject['MinimapVisibleLines'].Value, Minimap.Colors.VisibleLines);
        MatchingPair.Colors.Matched := StringToColorDef(LColorsObject['MatchingPairMatched'].Value, MatchingPair.Colors.Matched);
        MatchingPair.Colors.Underline := StringToColorDef(LColorsObject['MatchingPairUnderline'].Value, MatchingPair.Colors.Underline);
        MatchingPair.Colors.Unmatched := StringToColorDef(LColorsObject['MatchingPairUnmatched'].Value, MatchingPair.Colors.Unmatched);
        RightMargin.Colors.Edge := StringToColorDef(LColorsObject['RightEdge'].Value, RightMargin.Colors.Edge);
        RightMargin.Colors.MovingEdge := StringToColorDef(LColorsObject['RightMovingEdge'].Value, RightMargin.Colors.MovingEdge);
        Search.Highlighter.Colors.Background := StringToColorDef(LColorsObject['SearchHighlighterBackground'].Value, Search.Highlighter.Colors.Background);
        Search.Highlighter.Colors.Border := StringToColorDef(LColorsObject['SearchHighlighterBorder'].Value, Search.Highlighter.Colors.Border);
        Search.Highlighter.Colors.Foreground := StringToColorDef(LColorsObject['SearchHighlighterForeground'].Value, Search.Highlighter.Colors.Foreground);
        Search.InSelection.Background := StringToColorDef(LColorsObject['SearchInSelectionBackground'].Value, Search.InSelection.Background);
        Search.Map.Colors.ActiveLine := StringToColorDef(LColorsObject['SearchMapActiveLine'].Value, Search.Map.Colors.ActiveLine);
        Search.Map.Colors.Background := StringToColorDef(LColorsObject['SearchMapBackground'].Value, Search.Map.Colors.Background);
        Search.Map.Colors.Foreground := StringToColorDef(LColorsObject['SearchMapForeground'].Value, Search.Map.Colors.Foreground);
        Selection.Colors.Background := StringToColorDef(LColorsObject['SelectionBackground'].Value, Selection.Colors.Background);
        Selection.Colors.Foreground := StringToColorDef(LColorsObject['SelectionForeground'].Value, Selection.Colors.Foreground);
        SyncEdit.Colors.Background := StringToColorDef(LColorsObject['SyncEditBackground'].Value, SyncEdit.Colors.Background);
        SyncEdit.Colors.EditBorder := StringToColorDef(LColorsObject['SyncEditEditBorder'].Value, SyncEdit.Colors.EditBorder);
        SyncEdit.Colors.WordBorder := StringToColorDef(LColorsObject['SyncEditWordBorder'].Value, SyncEdit.Colors.WordBorder);
        WordWrap.Colors.Arrow := StringToColorDef(LColorsObject['WordWrapIndicatorArrow'].Value, WordWrap.Colors.Arrow);
        WordWrap.Colors.Lines := StringToColorDef(LColorsObject['WordWrapIndicatorLines'].Value, WordWrap.Colors.Lines);
      end;
      LFontsObject := AEditorObject['Fonts'].ObjectValue;
      if Assigned(LFontsObject) then
      begin
        LeftMargin.Font.Name := StrToStrDef(LFontsObject['LineNumbers'].Value, LeftMargin.Font.Name);
        Font.Name := StrToStrDef(LFontsObject['Text'].Value, Font.Name);
        Minimap.Font.Name := StrToStrDef(LFontsObject['Minimap'].Value, Minimap.Font.Name);
        CodeFolding.Hint.Font.Name := StrToStrDef(LFontsObject['CodeFoldingHint'].Value, CodeFolding.Hint.Font.Name);
        if cpoUseHighlighterColumnFont in CompletionProposal.Options then
          for LIndex := 0 to CompletionProposal.Columns.Count - 1 do
            CompletionProposal.Columns[LIndex].Font.Name := StrToStrDef(LFontsObject['CompletionProposal'].Value, CompletionProposal.Columns[0].Font.Name);
      end;
      LFontSizesObject := AEditorObject['FontSizes'].ObjectValue;
      if Assigned(LFontSizesObject) then
      begin
        LeftMargin.Font.Size := StrToIntDef(LFontSizesObject['LineNumbers'].Value, LeftMargin.Font.Size);
        Font.Size := StrToIntDef(LFontSizesObject['Text'].Value, Font.Size);
        Minimap.Font.Size := StrToIntDef(LFontSizesObject['Minimap'].Value, Minimap.Font.Size);
        CodeFolding.Hint.Font.Size := StrToIntDef(LFontSizesObject['CodeFoldingHint'].Value, CodeFolding.Hint.Font.Size);
        if cpoUseHighlighterColumnFont in CompletionProposal.Options then
          for LIndex := 0 to CompletionProposal.Columns.Count - 1 do
            CompletionProposal.Columns[LIndex].Font.Size := StrToIntDef(LFontSizesObject['CompletionProposal'].Value, CompletionProposal.Columns[0].Font.Size);
      end;
    end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportColorsFromStream(AStream: TStream);
var
  JSONObject: TJsonObject;
begin
  try
    JSONObject := TJsonObject.ParseFromStream(AStream) as TJsonObject;
    if Assigned(JSONObject) then
    try
      ImportColors(JSONObject);
    finally
      JSONObject.Free;
    end;
  except
    on E: EJsonParserException do
      raise EException.Create(Format(SBCEditorErrorInHighlighterParse, [E.LineNum, E.Column, E.Message]));
    on E: Exception do
      raise EException.Create(Format(SBCEditorErrorInHighlighterImport, [E.Message]));
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportColorsInfo(AInfoObject: TJsonObject);
var
  LHighlighterInfo: TInfo;
  LObject: TJsonObject;
begin
  if Assigned(AInfoObject) then
  begin
    LHighlighterInfo := FHighlighter.Colors.Info;
    { General }
    LObject := AInfoObject['General'];
    LHighlighterInfo.General.Version := LObject['Version'].Value;
    LHighlighterInfo.General.Date := LObject['Date'].Value;
    { Author }
    LObject := AInfoObject['Author'];
    LHighlighterInfo.Author.Name := LObject['Name'].Value;
    LHighlighterInfo.Author.Email := LObject['Email'].Value;
    LHighlighterInfo.Author.Comments := LObject['Comments'].Value;
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportCompletionProposal(ACompletionProposalObject: TJsonObject);
var
  LEditor: TCustomBCEditor;
  LFileName: string;
  LFileStream: TStream;
  LIndex: Integer;
  LJsonDataValue: PJsonDataValue;
  LJSONObject: TJsonObject;
  LSkipRegionArray: TJsonArray;
  LSkipRegionItem: TBCEditorCodeFolding.TSkipRegions.TItem;
begin
  if not Assigned(ACompletionProposalObject) then
    Exit;
  { Skip regions }
  LSkipRegionArray := ACompletionProposalObject['SkipRegion'].ArrayValue;
  for LIndex := 0 to LSkipRegionArray.Count - 1 do
  begin
    LJsonDataValue := LSkipRegionArray.Items[LIndex];

    if FHighlighter.MultiHighlighter then
    begin
      { Multi highlighter code folding skip region include }
      LFileName := LJsonDataValue.ObjectValue['File'].Value;
      if LFileName <> '' then
      begin
        LEditor := FHighlighter.Editor as TCustomBCEditor;
        LFileStream := LEditor.CreateFileStream(LEditor.GetHighlighterFileName(LFileName));
        LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
        if Assigned(LJSONObject) then
        try
          if LJSONObject.Contains('CompletionProposal') then
            ImportCompletionProposal(LJSONObject['CompletionProposal'].ObjectValue);
        finally
          LJSONObject.Free;
          LFileStream.Free;
        end;
      end;
      { Skip duplicates }
      if FHighlighter.CompletionProposalSkipRegions.Contains(LJsonDataValue.ObjectValue['OpenToken'].Value, LJsonDataValue.ObjectValue['CloseToken'].Value) then
        Continue;
    end;

    LSkipRegionItem := FHighlighter.CompletionProposalSkipRegions.Add(LJsonDataValue.ObjectValue['OpenToken'].Value,
      LJsonDataValue.ObjectValue['CloseToken'].Value);
    LSkipRegionItem.RegionType := StrToRegionType(LJsonDataValue.ObjectValue['RegionType'].Value);
    LSkipRegionItem.SkipEmptyChars := LJsonDataValue.ObjectValue.B['SkipEmptyChars'];
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportEditorProperties(AEditorObject: TJsonObject);
begin
  if Assigned(AEditorObject) then
    TCustomBCEditor(FHighlighter.Editor).URIOpener := StrToBoolDef(AEditorObject['URIOpener'].Value, False);
end;

procedure TBCEditorHighlighter.TImportJSON.ImportElements(AColorsObject: TJsonObject);
var
  LElement: PElement;
  LElementsArray: TJsonArray;
  LIndex: Integer;
  LJsonDataValue: PJsonDataValue;
begin
  if not Assigned(AColorsObject) then
    Exit;

  LElementsArray :=  AColorsObject['Elements'].ArrayValue;
  for LIndex := 0 to LElementsArray.Count - 1 do
  begin
    LJsonDataValue := LElementsArray.Items[LIndex];
    New(LElement);
    LElement.Background := StringToColorDef(LJsonDataValue.ObjectValue['Background'].Value, clWindow);
    LElement.Foreground := StringToColorDef(LJsonDataValue.ObjectValue['Foreground'].Value, clWindowText);
    LElement.Name := LJsonDataValue.ObjectValue['Name'].Value;
    LElement.FontStyles := StrToFontStyle(LJsonDataValue.ObjectValue['Style'].Value);
    FHighlighter.Colors.Styles.Add(LElement);

    if LElement.Name = 'Editor' then
      TCustomBCEditor(Highlighter.Editor).ForegroundColor := LElement.Foreground;
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportFromStream(AStream: TStream);
var
  JSONObject: TJsonObject;
begin
  try
    JSONObject := TJsonObject.ParseFromStream(AStream) as TJsonObject;
    if Assigned(JSONObject) then
    try
      ImportHighlighter(JSONObject);
    finally
      JSONObject.Free;
    end;
  except
    on E: EJsonParserException do
      raise EException.Create(Format(SBCEditorErrorInHighlighterParse, [E.LineNum, E.Column, E.Message]));
    on E: Exception do
      raise EException.Create(Format(SBCEditorErrorInHighlighterImport, [E.Message]));
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportHighlighter(AJSONObject: TJsonObject);
var
  LHighlighterObject: TJsonObject;
begin
  FHighlighter.Clear;

  LHighlighterObject := AJSONObject['Highlighter'];
  ImportInfo(LHighlighterObject['Info'].ObjectValue);
  ImportEditorProperties(LHighlighterObject['Editor'].ObjectValue);
  ImportRange(FHighlighter.MainRules, LHighlighterObject['MainRules'].ObjectValue);
  ImportCodeFolding(AJSONObject['CodeFolding'].ObjectValue);
  ImportMatchingPair(AJSONObject['MatchingPair'].ObjectValue);
  ImportCompletionProposal(AJSONObject['CompletionProposal'].ObjectValue);
end;

procedure TBCEditorHighlighter.TImportJSON.ImportInfo(AInfoObject: TJsonObject);
var
  LHighlighterInfo: TInfo;
  LIndex: Integer;
  LObject: TJsonObject;
  LSampleArray: TJsonArray;
begin
  if Assigned(AInfoObject) then
  begin
    LHighlighterInfo := FHighlighter.Info;
    { General }
    LObject := AInfoObject['General'];
    FHighlighter.MultiHighlighter := LObject.B['MultiHighlighter'];
    LHighlighterInfo.General.Version := LObject['Version'].Value;
    LHighlighterInfo.General.Date := LObject['Date'].Value;
    LSampleArray := LObject.A['Sample'];
    for LIndex := 0 to LSampleArray.Count - 1 do
      LHighlighterInfo.General.Sample := LHighlighterInfo.General.Sample + LSampleArray.S[LIndex];
    { Author }
    LObject := AInfoObject['Author'];
    LHighlighterInfo.Author.Name := LObject['Name'].Value;
    LHighlighterInfo.Author.Email := LObject['Email'].Value;
    LHighlighterInfo.Author.Comments := LObject['Comments'].Value;
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportKeyList(AKeyList: TKeyList; KeyListObject: TJsonObject;
  const AElementPrefix: string);
var
  LIndex: Integer;
  LWordArray: TJsonArray;
begin
  if Assigned(KeyListObject) then
  begin
    AKeyList.TokenType := StrToRangeType(KeyListObject['Type'].Value);
    LWordArray := KeyListObject.A['Words'];
    for LIndex := 0 to LWordArray.Count - 1 do
      AKeyList.KeyList.Add(LWordArray.S[LIndex]);
    ImportAttributes(AKeyList.Attribute, KeyListObject['Attributes'].ObjectValue, AElementPrefix);
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportMatchingPair(AMatchingPairObject: TJsonObject);
var
  LArray: TJsonArray;
  LEditor: TCustomBCEditor;
  LFileName: string;
  LFileStream: TStream;
  LIndex: Integer;
  LJsonDataValue: PJsonDataValue;
  LJSONObject: TJsonObject;
  LTokenMatch: PMatchingPairToken;
begin
  if not Assigned(AMatchingPairObject) then
    Exit;
  { Matching token pairs }
  LArray := AMatchingPairObject['Pairs'].ArrayValue;
  for LIndex := 0 to LArray.Count - 1 do
  begin
    LJsonDataValue := LArray.Items[LIndex];

    if FHighlighter.MultiHighlighter then
    begin
      { Multi highlighter code folding fold region include }
      LFileName := LJsonDataValue.ObjectValue['File'].Value;
      if LFileName <> '' then
      begin
        LEditor := FHighlighter.Editor as TCustomBCEditor;
        LFileStream := LEditor.CreateFileStream(LEditor.GetHighlighterFileName(LFileName));
        LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
        if Assigned(LJSONObject) then
        try
          if LJSONObject.Contains('MatchingPair') then
            ImportMatchingPair(LJSONObject['MatchingPair'].ObjectValue);
        finally
          LJSONObject.Free;
          LFileStream.Free;
        end;
      end;
    end;

    New(LTokenMatch);
    LTokenMatch.OpenToken := LJsonDataValue.ObjectValue['OpenToken'].Value;
    LTokenMatch.CloseToken := LJsonDataValue.ObjectValue['CloseToken'].Value;
    FHighlighter.MatchingPairs.Add(LTokenMatch)
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportRange(ARange: TRange; RangeObject: TJsonObject;
  AParentRange: TRange = nil; ASkipBeforeSubRules: Boolean = False;
  const AElementPrefix: string = ''); { Recursive method }
var
  LAlternativeCloseArray: TJsonArray;
  LCloseToken: string;
  LEditor: TCustomBCEditor;
  LElementPrefix: string;
  LFileName: string;
  LFileStream: TStream;
  LIndex: Integer;
  LIndex2: Integer;
  LJSONObject: TJsonObject;
  LJSONSubRulesObject: TJsonObject;
  LNewKeyList: TKeyList;
  LNewRange: TRange;
  LNewSet: TSet;
  LOpenToken: string;
  LPropertiesObject: TJsonObject;
  LSubRulesObject: TJsonObject;
  LTokenRangeObject: TJsonObject;
begin
  if Assigned(RangeObject) then
  begin
    LFileName := RangeObject['File'].Value;
    if FHighlighter.MultiHighlighter and (LFileName <> '') then
    begin
      LElementPrefix := RangeObject['ElementPrefix'].Value;
      LEditor := FHighlighter.Editor as TCustomBCEditor;
      LFileStream := LEditor.CreateFileStream(LEditor.GetHighlighterFileName(LFileName));
      LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
      if Assigned(LJSONObject) then
      try
        LTokenRangeObject := LJSONObject['Highlighter']['MainRules'].ObjectValue;
        { You can include MainRules... }
        if LTokenRangeObject['Name'].Value = RangeObject['IncludeRange'].Value then
          ImportRange(AParentRange, LTokenRangeObject, nil, True, LElementPrefix)
        else
        { or SubRules... }
        begin
          LSubRulesObject := LTokenRangeObject['SubRules'].ObjectValue;
          if Assigned(LSubRulesObject) then
          for LIndex := 0 to LSubRulesObject.Count - 1 do
          begin
            if LSubRulesObject.Names[LIndex] = 'Range' then
            for LIndex2 := 0 to LSubRulesObject.Items[LIndex].ArrayValue.Count - 1 do
            begin
              LJSONSubRulesObject := LSubRulesObject.Items[LIndex].ArrayValue.O[LIndex2];
              if LJSONSubRulesObject.S['Name'] = RangeObject['IncludeRange'].Value then
              begin
                ImportRange(ARange, LJSONSubRulesObject, nil, False, LElementPrefix);
                Break;
              end;
            end;
          end;
        end;
      finally
        LJSONObject.Free;
        LFileStream.Free;
      end;
    end
    else
    begin
      if not ASkipBeforeSubRules then
      begin
        ARange.Clear;
        ARange.CaseSensitive := RangeObject.B['CaseSensitive'];
        ImportAttributes(ARange.Attribute, RangeObject['Attributes'].ObjectValue, AElementPrefix);
        if RangeObject['Delimiters'].Value <> '' then
          ARange.Delimiters := StrToSet(RangeObject['Delimiters'].Value);
        ARange.TokenType := StrToRangeType(RangeObject['Type'].Value);

        LPropertiesObject := RangeObject['Properties'].ObjectValue;
        if Assigned(LPropertiesObject) then
        begin
          ARange.CloseOnEndOfLine := LPropertiesObject.B['CloseOnEndOfLine'];
          ARange.CloseOnTerm := LPropertiesObject.B['CloseOnTerm'];
          ARange.SkipWhitespace := LPropertiesObject.B['SkipWhitespace'];
          ARange.CloseParent := LPropertiesObject.B['CloseParent'];
          ARange.UseDelimitersForText := LPropertiesObject.B['UseDelimitersForText'];

          LAlternativeCloseArray := LPropertiesObject['AlternativeClose'].ArrayValue;
          if LAlternativeCloseArray.Count > 0 then
          begin
            ARange.AlternativeCloseArrayCount := LAlternativeCloseArray.Count;
            for LIndex := 0 to ARange.AlternativeCloseArrayCount - 1 do
              ARange.AlternativeCloseArray[LIndex] := LAlternativeCloseArray.Items[LIndex].Value;
          end;
          ARange.OpenBeginningOfLine := LPropertiesObject.B['OpenBeginningOfLine'];
        end;

        ARange.OpenToken.Clear;
        ARange.OpenToken.BreakType := btUnspecified;
        ARange.CloseToken.Clear;
        ARange.CloseToken.BreakType := btUnspecified;

        LTokenRangeObject := RangeObject['TokenRange'].ObjectValue;
        if Assigned(LTokenRangeObject) then
        begin
          LOpenToken := LTokenRangeObject['Open'].Value;
          LCloseToken := LTokenRangeObject['Close'].Value;

          ARange.AddTokenRange(LOpenToken, StrToBreakType(LTokenRangeObject['OpenBreakType'].Value), LCloseToken,
            StrToBreakType(LTokenRangeObject['CloseBreakType'].Value));

          case ARange.TokenType of
            ttLineComment: FHighlighter.Comments.AddLineComment(LOpenToken);
            ttBlockComment: FHighlighter.Comments.AddBlockComment(LOpenToken, LCloseToken);
          end;
        end;
      end;
      { Sub rules }
      LSubRulesObject := RangeObject['SubRules'].ObjectValue;

      if Assigned(LSubRulesObject) then
      begin
        for LIndex := 0 to LSubRulesObject.Count - 1 do
        begin
          if LSubRulesObject.Names[LIndex] = 'Range' then
          for LIndex2 := 0 to LSubRulesObject.Items[LIndex].ArrayValue.Count - 1 do
          begin
            LNewRange := TRange.Create;
            ImportRange(LNewRange, LSubRulesObject.Items[LIndex].ArrayValue.O[LIndex2], ARange); { ARange is for the MainRules include }
            ARange.AddRange(LNewRange);
          end
          else
          if LSubRulesObject.Names[LIndex] = 'KeyList' then
          for LIndex2 := 0 to LSubRulesObject.Items[LIndex].ArrayValue.Count - 1 do
          begin
            LNewKeyList := TKeyList.Create;
            ImportKeyList(LNewKeyList, LSubRulesObject.Items[LIndex].ArrayValue.O[LIndex2], AElementPrefix);
            ARange.AddKeyList(LNewKeyList);
          end
          else
          if LSubRulesObject.Names[LIndex] = 'Set' then
          for LIndex2 := 0 to LSubRulesObject.Items[LIndex].ArrayValue.Count - 1 do
          begin
            LNewSet := TSet.Create;
            ImportSet(LNewSet, LSubRulesObject.Items[LIndex].ArrayValue.O[LIndex2], AElementPrefix);
            ARange.AddSet(LNewSet);
          end
        end;
      end;
    end;
  end;
end;

procedure TBCEditorHighlighter.TImportJSON.ImportSet(ASet: TSet; SetObject: TJsonObject;
  const AElementPrefix: string);
begin
  if Assigned(SetObject) then
  begin
    ASet.CharSet := StrToSet(SetObject['Symbols'].Value);
    ImportAttributes(ASet.Attribute, SetObject['Attributes'].ObjectValue, AElementPrefix);
  end;
end;

{ TBCEditorHighlighter ********************************************************}

procedure TBCEditorHighlighter.AddKeyChar(AKeyCharType: TBCEditorKeyCharType; AChar: Char);
begin
  case AKeyCharType of
    ctFoldOpen: FFoldOpenKeyChars := FFoldOpenKeyChars + [AChar];
    ctFoldClose: FFoldCloseKeyChars := FFoldCloseKeyChars + [AChar];
    ctSkipOpen: FSkipOpenKeyChars := FSkipOpenKeyChars + [AChar];
    ctSkipClose: FSkipCloseKeyChars := FSkipCloseKeyChars + [AChar];
  end;
end;

constructor TBCEditorHighlighter.Create(AEditor: TCustomControl);
begin
  inherited Create;

  FEditor := AEditor;
  FWordBreakChars := BCEDITOR_WORD_BREAK_CHARACTERS;

  FAttributes := TStringList.Create;
  FAttributes.Duplicates := dupIgnore;
  FAttributes.Sorted := False;

  FCodeFoldingRangeCount := 0;

  FComments := TComments.Create;

  FCompletionProposalSkipRegions := TBCEditorCodeFolding.TSkipRegions.Create(TBCEditorCodeFolding.TSkipRegions.TItem);

  Info := TInfo.Create;
  FMainRules := TRange.Create;
  FMainRules.Parent := FMainRules;

  FEndOfLine := False;
  FBeginningOfLine := True;
  FPreviousEndOfLine := False;
  FCurrentRange := MainRules;

  FColors := TColors.Create(Self);
  FMatchingPairs := TList.Create;
  FMatchingPairHighlight := True;

  FTemporaryCurrentTokens := TList.Create;

  FAllDelimiters := BCEDITOR_DEFAULT_DELIMITERS + BCEDITOR_ABSOLUTE_DELIMITERS;

  FLoading := False;
end;

destructor TBCEditorHighlighter.Destroy;
begin
  Clear;

  FComments.Free;
  FComments := nil;
  FMainRules.Free;
  FMainRules := nil;
  FInfo.Free;
  FInfo := nil;
  FAttributes.Free;
  FAttributes := nil;
  FCompletionProposalSkipRegions.Free;
  FCompletionProposalSkipRegions := nil;
  FMatchingPairs.Free;
  FMatchingPairs := nil;
  FColors.Free;
  FColors := nil;
  FTemporaryCurrentTokens.Free;

  inherited;
end;

procedure TBCEditorHighlighter.AddAllAttributes(ARange: TRange);
var
  LIndex: Integer;
begin
  AddAttribute(ARange.Attribute);
  for LIndex := 0 to ARange.KeyListCount - 1 do
    AddAttribute(ARange.KeyList[LIndex].Attribute);
  for LIndex := 0 to ARange.SetCount - 1 do
    AddAttribute(ARange.Sets[LIndex].Attribute);
  for LIndex := 0 to ARange.RangeCount - 1 do
    AddAllAttributes(ARange.Ranges[LIndex]);
end;

procedure TBCEditorHighlighter.SetCurrentLine(const ANewValue: string);
begin
  if Assigned(FCurrentRange) then
    if not FCurrentRange.Prepared then
      Prepare;

  FCurrentLine := ANewValue;
  FCurrentLineIndex := 0;
  FTokenPosition := 0;
  FEndOfLine := False;
  FBeginningOfLine := True;
  FPreviousEndOfLine := False;
  FCurrentToken := nil;
  Next;
end;

procedure TBCEditorHighlighter.Next;
var
  LCloseParent: Boolean;
  LDelimiters: TBCEditorCharSet;
  LIndex: Integer;
  LKeywordText: string;
  LParser: TBaseParser;
begin
  while FTemporaryCurrentTokens.Count > 0 do
  begin
    FCurrentToken := TToken(FTemporaryCurrentTokens[0]);
    FCurrentToken.Free;
    FCurrentToken := nil;
    FTemporaryCurrentTokens.Delete(0);
  end;

  if FPreviousEndOfLine then
  begin
    if Assigned(FCurrentRange) then
      if FCurrentRange.CloseOnEndOfLine or FCurrentRange.CloseOnTerm then
        FCurrentRange := FCurrentRange.Parent;
    FEndOfLine := True;
    Exit;
  end;

  if (Assigned(FCurrentRange) and (FCurrentRange.AlternativeCloseArrayCount > 0)) then
    for LIndex := 0 to FCurrentRange.AlternativeCloseArrayCount - 1 do
    begin
      LKeywordText := FCurrentRange.AlternativeCloseArray[LIndex];
      if ((Length(FCurrentLine) - FCurrentLineIndex >= Length(LKeywordText))
        and (StrLComp(@FCurrentLine[1 + FCurrentLineIndex], PChar(LKeywordText), Length(LKeywordText)) = 0)) then
      begin
        FCurrentRange := FCurrentRange.Parent;
        Break;
      end;
    end;

  FTokenPosition := FCurrentLineIndex;

  if Assigned(FCurrentRange) then
  begin
    LCloseParent := FCurrentRange.CloseParent;
    if (FCurrentRange.CloseOnTerm and CharInSet(FCurrentLine[1 + FCurrentLineIndex], FCurrentRange.Delimiters) and
      not (FCurrentRange.SkipWhitespace and CharInSet(FCurrentLine[1 + FCurrentLineIndex], BCEDITOR_ABSOLUTE_DELIMITERS))) then
    begin
      FCurrentRange := FCurrentRange.Parent;
      if Assigned(FCurrentRange) then
        if LCloseParent then
          FCurrentRange := FCurrentRange.Parent;
    end;

    if ((FCurrentLine = '') or (FCurrentLineIndex = Length(FCurrentLine))) then
      LParser := nil
    else if (Ord(FCurrentLine[1 + FCurrentLineIndex]) < 256) then
      LParser := FCurrentRange.SymbolList[AnsiChar(FCurrentRange.CaseFunct(FCurrentLine[1 + FCurrentLineIndex]))]
    else
      LParser := FCurrentRange.SymbolList['a'];

    if (not Assigned(LParser)) then
      Inc(FCurrentLineIndex)
    else if (not LParser.GetToken(FCurrentRange, FCurrentLine, FCurrentLineIndex, FCurrentToken)) then
    begin
      FCurrentToken := FCurrentRange.DefaultToken;

      if (FCurrentRange.UseDelimitersForText) then
        LDelimiters := FCurrentRange.Delimiters
      else
        LDelimiters := FAllDelimiters;

      if (Ord(FCurrentLine[1 + FCurrentLineIndex - 1]) < 256) then
        while ((FCurrentLineIndex < Length(FCurrentLine))
          and (Ord(FCurrentLine[1 + FCurrentLineIndex]) < 256)
          and not CharInSet(FCurrentLine[1 + FCurrentLineIndex], LDelimiters)) do
          Inc(FCurrentLineIndex)
      else
        while ((FCurrentLineIndex < Length(FCurrentLine))
          and (Ord(FCurrentLine[1 + FCurrentLineIndex]) >= 256)
          and not CharInSet(FCurrentLine[1 + FCurrentLineIndex], LDelimiters)) do
          Inc(FCurrentLineIndex)
    end
    else if (FCurrentRange.ClosingToken = FCurrentToken) then
      FCurrentRange := FCurrentRange.Parent
    else if (Assigned(FCurrentToken) and Assigned(FCurrentToken.OpenRule) and (FCurrentToken.OpenRule is TRange)) then
    begin
      FCurrentRange := TRange(FCurrentToken.OpenRule);
      FCurrentRange.ClosingToken := FCurrentToken.ClosingToken;
      if (FCurrentRange.OpenBeginningOfLine and not FBeginningOfLine) then
      begin
        FCurrentRange := FCurrentRange.Parent;
        FCurrentToken := FCurrentRange.DefaultToken;
      end;
    end;

    if (Assigned(FCurrentToken) and FCurrentToken.Temporary) then
      FTemporaryCurrentTokens.Add(FCurrentToken);
  end;

  if (FBeginningOfLine
    and (FCurrentLineIndex > 0)
    and ((FCurrentLineIndex > Length(FCurrentLine)) or not CharInset(FCurrentLine[1 + FCurrentLineIndex - 1], BCEDITOR_ABSOLUTE_DELIMITERS))) then
    FBeginningOfLine := False;

  if (FCurrentLineIndex > Length(FCurrentLine)) then
    FPreviousEndOfLine := True;
end;

function TBCEditorHighlighter.GetCurrentRangeAttribute: TAttribute;
begin
  Result := nil;
  if Assigned(FCurrentRange) then
    Result := FCurrentRange.Attribute;
end;

function TBCEditorHighlighter.GetEndOfLine: Boolean;
begin
  Result := FEndOfLine;
end;

function TBCEditorHighlighter.GetCurrentRange: TRange;
begin
  Result := FCurrentRange;
end;

function TBCEditorHighlighter.GetTokenAttribute: TAttribute;
begin
  if Assigned(FCurrentToken) then
    Result := FCurrentToken.Attribute
  else
    Result := nil;
end;

function TBCEditorHighlighter.GetTokenPosition: Integer;
begin
  Result := FTokenPosition;
end;

procedure TBCEditorHighlighter.ResetCurrentRange;
begin
  FCurrentRange := MainRules;
end;

procedure TBCEditorHighlighter.SetCodeFoldingRangeCount(AValue: Integer);
begin
  if FCodeFoldingRangeCount <> AValue then
  begin
    SetLength(FCodeFoldingRegions, AValue);
    FCodeFoldingRangeCount := AValue;
  end;
end;

procedure TBCEditorHighlighter.SetCurrentRange(AValue: Pointer);
begin
  FCurrentRange := TRange(AValue);
end;

procedure TBCEditorHighlighter.AddKeywords(var AStringList: TStringList);
var
  LIndex: Integer;
  LIndex2: Integer;
begin
  if not Assigned(AStringList) then
    Exit;
  for LIndex := 0 to FMainRules.KeyListCount - 1 do
    for LIndex2 := 0 to FMainRules.KeyList[LIndex].KeyList.Count - 1 do
      AStringList.Add(FMainRules.KeyList[LIndex].KeyList[LIndex2]);
end;

procedure TBCEditorHighlighter.GetToken(var AResult: string);
begin
  AResult := Copy(FCurrentLine, 1 + FTokenPosition, FCurrentLineIndex - FTokenPosition);
end;

procedure TBCEditorHighlighter.Reset;
begin
  MainRules.Reset;
end;

function TBCEditorHighlighter.GetTokenKind: TBCEditorRangeType;
var
  LCurrentRangeKeyList: TKeyList;
  LIndex: Integer;
  LToken: string;
  LTokenType: TBCEditorRangeType;
begin
  LTokenType := FCurrentRange.TokenType;
  if LTokenType <> ttUnspecified then
    Result := LTokenType
  else
  { keyword token type }
  begin
    GetToken(LToken);
    for LIndex := 0 to FCurrentRange.KeyListCount - 1 do
    begin
      LCurrentRangeKeyList := FCurrentRange.KeyList[LIndex];
      if LCurrentRangeKeyList.KeyList.IndexOf(LToken) <> -1 then
        Exit(LCurrentRangeKeyList.TokenType);
    end;
    Result := ttUnspecified
  end;
end;

procedure TBCEditorHighlighter.Clear;
var
  LIndex: Integer;
begin
  FFoldOpenKeyChars := [];
  FFoldCloseKeyChars := [];
  FSkipOpenKeyChars := [];
  FSkipCloseKeyChars := [];
  FAttributes.Clear;
  FMainRules.Clear;
  FInfo.Clear;
  FComments.Clear;
  FCompletionProposalSkipRegions.Clear;
  for LIndex := FMatchingPairs.Count - 1 downto 0 do
    Dispose(PMatchingPairToken(FMatchingPairs.Items[LIndex]));
  FMatchingPairs.Clear;
  for LIndex := 0 to FCodeFoldingRangeCount - 1 do
  begin
    FCodeFoldingRegions[LIndex].Free;
    FCodeFoldingRegions[LIndex] := nil;
  end;
  CodeFoldingRangeCount := 0;
  TCustomBCEditor(Editor).ClearMatchingPair;
end;

procedure TBCEditorHighlighter.Prepare;
begin
  FAttributes.Clear;
  AddAllAttributes(MainRules);
  FMainRules.Prepare(FMainRules);
end;

procedure TBCEditorHighlighter.UpdateAttributes(ARange: TRange; AParentRange: TRange);
var
  LIndex: Integer;

  procedure SetAttributes(AAttribute: TAttribute; AParentRange: TRange);
  var
    LElement: PElement;
  begin
    LElement := FColors.GetElement(AAttribute.Element);

    if AAttribute.ParentBackground and Assigned(AParentRange) then
      AAttribute.Background := AParentRange.Attribute.Background
    else
    if Assigned(LElement) then
      AAttribute.Background := LElement.Background;
    if AAttribute.ParentForeground and Assigned(AParentRange) then
      AAttribute.Foreground := AParentRange.Attribute.Foreground
    else
    if Assigned(LElement) then
      AAttribute.Foreground := LElement.Foreground;
    if Assigned(LElement) then
      AAttribute.FontStyles := LElement.FontStyles;
  end;

begin
  SetAttributes(ARange.Attribute, AParentRange);

  for LIndex := 0 to ARange.KeyListCount - 1 do
    SetAttributes(ARange.KeyList[LIndex].Attribute, ARange);
  for LIndex := 0 to ARange.SetCount - 1 do
    SetAttributes(ARange.Sets[LIndex].Attribute, ARange);

  if ARange.RangeCount > 0 then
  for LIndex := 0 to ARange.RangeCount - 1 do
    UpdateAttributes(ARange.Ranges[LIndex], ARange);
end;

procedure TBCEditorHighlighter.UpdateColors;
var
  LFontDummy: TFont;
begin
  UpdateAttributes(MainRules, nil);
  LFontDummy := TFont.Create;
  try
    LFontDummy.Name := TCustomBCEditor(Editor).Font.Name;
    LFontDummy.Size := TCustomBCEditor(Editor).Font.Size;
    TCustomBCEditor(Editor).Font.Assign(LFontDummy);
  finally
    LFontDummy.Free;
  end;
end;

procedure TBCEditorHighlighter.LoadFromFile(const AFileName: string);
var
  LStream: TStream;
begin
  FFileName := AFileName;
  FName := TPath.GetFileNameWithoutExtension(AFileName);
  LStream := TCustomBCEditor(Editor).CreateFileStream(TCustomBCEditor(Editor).GetHighlighterFileName(AFileName));
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TBCEditorHighlighter.LoadFromResource(const ResName: string; const ResType: PChar);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, PChar(ResName), ResType);
  LoadFromStream(Stream);
  Stream.Free();
end;

procedure TBCEditorHighlighter.LoadFromStream(AStream: TStream);
begin
  Clear;
  FLoading := True;
  with TImportJSON.Create(Self) do
    try
      ImportFromStream(AStream);
    finally
      Free;
    end;
  UpdateColors;
  FLoading := False;
end;

function TBCEditorHighlighter.GetAttribute(AIndex: Integer): TAttribute;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FAttributes.Count) then
    Result := TAttribute(FAttributes.Objects[AIndex]);
end;

procedure TBCEditorHighlighter.AddAttribute(AHighlighterAttribute: TAttribute);
begin
  FAttributes.AddObject(AHighlighterAttribute.Name, AHighlighterAttribute);
end;

procedure TBCEditorHighlighter.SetWordBreakChars(AChars: TBCEditorCharSet);
begin
  FWordBreakChars := AChars;
end;

procedure TBCEditorHighlighter.NextToEndOfLine;
begin
  while not GetEndOfLine do
    Next;
end;

procedure TBCEditorHighlighter.SetAttributesOnChange(AEvent: TNotifyEvent);
var
  LHighlighterAttribute: TAttribute;
  LIndex: Integer;
begin
  for LIndex := FAttributes.Count - 1 downto 0 do
  begin
    LHighlighterAttribute := TAttribute(FAttributes.Objects[LIndex]);
    if Assigned(LHighlighterAttribute) then
    begin
      LHighlighterAttribute.OnChange := AEvent;
      LHighlighterAttribute.InternalSaveDefaultValues;
    end;
  end;
end;

function TBCEditorHighlighter.GetTokenLength: Integer;
begin
  Result := FCurrentLineIndex - FTokenPosition;
end;

end.

