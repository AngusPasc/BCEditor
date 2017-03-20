unit BCEditor.Print;

interface {********************************************************************}

uses
  Windows,
  SysUtils, Classes,
  Graphics, Printers,
  BCEditor.Editor, BCEditor.Types, BCEditor.Utils, BCEditor.Highlighter,
  BCEditor.Editor.Selection, BCEditor.PaintHelper;

type
  TBCEditorPrinterInfo = class
  strict private
    FBottomMargin: Integer;
    FIsUpdated: Boolean;
    FLeftMargin: Integer;
    FPhysicalHeight: Integer;
    FPhysicalWidth: Integer;
    FPrintableHeight: Integer;
    FPrintableWidth: Integer;
    FRightMargin: Integer;
    FTopMargin: Integer;
    FXPixPerInch: Integer;
    FXPixPermm: Single;
    FYPixPerInch: Integer;
    FYPixPermm: Single;
    procedure FillDefault;
    function GetBottomMargin: Integer;
    function GetLeftMargin: Integer;
    function GetPhysicalHeight: Integer;
    function GetPhysicalWidth: Integer;
    function GetPrintableHeight: Integer;
    function GetPrintableWidth: Integer;
    function GetRightMargin: Integer;
    function GetTopMargin: Integer;
    function GetXPixPerInch: Integer;
    function GetXPixPermm: Single;
    function GetYPixPerInch: Integer;
    function GetYPixPermm: Single;
  public
    function PixFromBottom(const AValue: Double): Integer;
    function PixFromLeft(const AValue: Double): Integer;
    function PixFromRight(const AValue: Double): Integer;
    function PixFromTop(const AValue: Double): Integer;
    procedure UpdatePrinter;
    property BottomMargin: Integer read GetBottomMargin;
    property LeftMargin: Integer read GetLeftMargin;
    property PhysicalHeight: Integer read GetPhysicalHeight;
    property PhysicalWidth: Integer read GetPhysicalWidth;
    property PrintableHeight: Integer read GetPrintableHeight;
    property PrintableWidth: Integer read GetPrintableWidth;
    property RightMargin: Integer read GetRightMargin;
    property TopMargin: Integer read GetTopMargin;
    property XPixPerInch: Integer read GetXPixPerInch;
    property XPixPermm: Single read GetXPixPermm;
    property YPixPerInch: Integer read GetYPixPerInch;
    property YPixPermm: Single read GetYPixPermm;
  end;

  TBCEditorPageLine = class
  private
    FFirstLine: Integer;
  public
    property FirstLine: Integer read FFirstLine write FFirstLine;
  end;

  TBCEditorPrint = class(TComponent)
  type
    TFrameType = (ftLine, ftBox, ftShaded);
    TFrameTypes = set of TFrameType;
    TUnitSystem = (usMM, usCm, usInch, muThousandthsOfInches);
    TStatus = (psBegin, psNewPage, psEnd);
    TStatusEvent = procedure(ASender: TObject; const AStatus: TStatus; const APageNumber: Integer;
      var AAbort: Boolean) of object;
    TLineEvent = procedure(ASender: TObject; const ALineNumber: Integer; const APageNumber: Integer) of object;

    TMargins = class(TPersistent)
    strict private
      function ConvertFrom(AValue: Double): Double;
      function ConvertTo(AValue: Double): Double;
    strict private
      FBottom: Double;
      FFooter: Double;
      FHeader: Double;
      FInternalMargin: Double;
      FLeft: Double;
      FLeftTextIndent: Double;
      FMargin: Double;
      FMirrorMargins: Boolean;
      FPixelBottom: Integer;
      FPixelFooter: Integer;
      FPixelHeader: Integer;
      FPixelInternalMargin: Integer;
      FPixelLeft: Integer;
      FPixelLeftTextIndent: Integer;
      FPixelMargin: Integer;
      FPixelRight: Integer;
      FPixelRightTextIndent: Integer;
      FPixelTop: Integer;
      FRight: Double;
      FRightTextIndent: Double;
      FTop: Double;
      FUnitSystem: TUnitSystem;
      function GetBottom: Double;
      function GetFooter: Double;
      function GetHeader: Double;
      function GetInternalMargin: Double;
      function GetLeft: Double;
      function GetLeftTextIndent: Double;
      function GetMargin: Double;
      function GetRight: Double;
      function GetRightTextIndent: Double;
      function GetTop: Double;
      procedure SetBottom(const AValue: Double);
      procedure SetFooter(const AValue: Double);
      procedure SetHeader(const AValue: Double);
      procedure SetInternalMargin(const AValue: Double);
      procedure SetLeft(const AValue: Double);
      procedure SetLeftTextIndent(const AValue: Double);
      procedure SetMargin(const AValue: Double);
      procedure SetRight(const AValue: Double);
      procedure SetRightTextIndent(const AValue: Double);
      procedure SetTop(const AValue: Double);
    public
      procedure Assign(ASource: TPersistent); override;
      constructor Create;
      procedure InitPage(ACanvas: TCanvas; APageNumber: Integer; APrinterInfo: TBCEditorPrinterInfo;
        ALineNumbers, ALineNumbersInMargin: Boolean; AMaxLineNumber: Integer);
      procedure LoadFromStream(AStream: TStream);
      procedure SaveToStream(AStream: TStream);
      property PixelBottom: Integer read FPixelBottom write FPixelBottom;
      property PixelFooter: Integer read FPixelFooter write FPixelFooter;
      property PixelHeader: Integer read FPixelHeader write FPixelHeader;
      property PixelInternalMargin: Integer read FPixelInternalMargin write FPixelInternalMargin;
      property PixelLeft: Integer read FPixelLeft write FPixelLeft;
      property PixelLeftTextIndent: Integer read FPixelLeftTextIndent write FPixelLeftTextIndent;
      property PixelMargin: Integer read FPixelMargin write FPixelMargin;
      property PixelRight: Integer read FPixelRight write FPixelRight;
      property PixelRightTextIndent: Integer read FPixelRightTextIndent write FPixelRightTextIndent;
      property PixelTop: Integer read FPixelTop write FPixelTop;
    published
      property Bottom: Double read GetBottom write SetBottom;
      property Footer: Double read GetFooter write SetFooter;
      property Header: Double read GetHeader write SetHeader;
      property InternalMargin: Double read GetInternalMargin write SetInternalMargin;
      property Left: Double read GetLeft write SetLeft;
      property LeftTextIndent: Double read GetLeftTextIndent write SetLeftTextIndent;
      property Margin: Double read GetMargin write SetMargin;
      property MirrorMargins: Boolean read FMirrorMargins write FMirrorMargins;
      property Right: Double read GetRight write SetRight;
      property RightTextIndent: Double read GetRightTextIndent write SetRightTextIndent;
      property Top: Double read GetTop write SetTop;
      property UnitSystem: TUnitSystem read FUnitSystem write FUnitSystem default usMM;
    end;

    TLineInfo = class
    public
      LineHeight: Integer;
      MaxBaseDistance: Integer;
    end;

    TSectionType = (stHeader, stFooter);

    TSectionItem = class
    strict private
      FAlignment: TAlignment;
      FFont: TFont;
      FIndex: Integer;
      FLineNumber: Integer;
      FText: string;
      procedure SetFont(const AValue: TFont);
    public
      constructor Create;
      destructor Destroy; override;
      function GetText(ANumberOfPages, APageNumber: Integer; ARoman: Boolean; const ATitle, ATime, ADate: string): string;
      procedure LoadFromStream(AStream: TStream);
      procedure SaveToStream(AStream: TStream);
      property Alignment: TAlignment read FAlignment write FAlignment;
      property Font: TFont read FFont write SetFont;
      property Index: Integer read FIndex write FIndex;
      property LineNumber: Integer read FLineNumber write FLineNumber;
      property Text: string read FText write FText;
    end;

    TSection = class(TPersistent)
    strict private
      FDate: string;
      FDefaultFont: TFont;
      FFrameHeight: Integer;
      FFrameTypes: TFrameTypes;
      FItems: TList;
      FLineColor: TColor;
      FLineCount: Integer;
      FLineInfo: TList;
      FMargins: TMargins;
      FMirrorPosition: Boolean;
      FNumberOfPages: Integer;
      FOldBrush: TBrush;
      FOldFont: TFont;
      FOldPen: TPen;
      FRomanNumbers: Boolean;
      FSectionType: TSectionType;
      FShadedColor: TColor;
      FTime: string;
      FTitle: string;
      procedure CalculateHeight(ACanvas: TCanvas);
      procedure DrawFrame(ACanvas: TCanvas);
      procedure RestoreFontPenBrush(ACanvas: TCanvas);
      procedure SaveFontPenBrush(ACanvas: TCanvas);
      procedure SetDefaultFont(const AValue: TFont);
    public
      constructor Create;
      destructor Destroy; override;
      function Add(const AText: string; const AFont: TFont; const AAlignment: TAlignment; const ALineNumber: Integer): Integer;
      procedure Assign(ASource: TPersistent); override;
      procedure Clear;
      function Count: Integer;
      procedure Delete(AIndex: Integer);
      procedure FixLines;
      function Get(AIndex: Integer): TBCEditorPrint.TSectionItem;
      procedure InitPrint(ACanvas: TCanvas; NumberOfPages: Integer; const Title: string; Margins: TMargins);
      procedure LoadFromStream(AStream: TStream);
      procedure Print(ACanvas: TCanvas; PageNum: Integer);
      procedure SaveToStream(AStream: TStream);
      procedure SetPixelsPerInch(AValue: Integer);
      property NumberOfPages: Integer read FNumberOfPages write FNumberOfPages;
      property SectionType: TSectionType read FSectionType write FSectionType;
    published
      property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
      property FrameTypes: TFrameTypes read FFrameTypes write FFrameTypes default [ftLine];
      property LineColor: TColor read FLineColor write FLineColor default clBlack;
      property MirrorPosition: Boolean read FMirrorPosition write FMirrorPosition default False;
      property RomanNumbers: Boolean read FRomanNumbers write FRomanNumbers default False;
      property ShadedColor: TColor read FShadedColor write FShadedColor default clSilver;
    end;

    THeader = class(TSection)
    public
      constructor Create;
    end;

    TFooter = class(TSection)
    public
      constructor Create;
    end;

  strict private type
    TCustomBCEditor = class(BCEditor.Editor.TCustomBCEditor);
  strict private
    FAbort: Boolean;
    FBlockBeginPosition: TBCEditorTextPosition;
    FBlockEndPosition: TBCEditorTextPosition;
    FCanvas: TCanvas;
    FCharWidth: Integer;
    FColors: Boolean;
    FColumns: Boolean;
    FCopies: Integer;
    FDefaultBackground: TColor;
    FDocumentTitle: string;
    FEditor: TCustomBCEditor;
    FFont: TFont;
    FFontDummy: TFont;
    FFontColor: TColor;
    FFooter: TFooter;
    FHeader: THeader;
    FHighlight: Boolean;
    FHighlighter: TBCEditorHighlighter;
    FHighlighterRangesSet: Boolean;
    FLineHeight: Integer;
    FLineNumber: Integer;
    FLineNumbers: Boolean;
    FLineNumbersInMargin: Boolean;
    FLineOffset: Integer;
    FLines: TStrings;
    FMargins: TMargins;
    FMaxColumn: Integer;
    FMaxLeftChar: Integer;
    FMaxWidth: Integer;
    FOldFont: TFont;
    FOnPrintLine: TLineEvent;
    FOnPrintStatus: TStatusEvent;
    FPaintHelper: TBCEditorPaintHelper;
    FPageCount: Integer;
    FPageOffset: Integer;
    FPages: TList;
    FPagesCounted: Boolean;
    FPrinterInfo: TBCEditorPrinterInfo;
    FPrinting: Boolean;
    FSelectionAvailable: Boolean;
    FSelectedOnly: Boolean;
    FSelectionMode: TBCEditorSelectionMode;
    FTabWidth: Integer;
    FTitle: string;
    FWrap: Boolean;
    FYPos: Integer;
    function ClipLineToRect(var ALine: string): string;
    function GetPageCount: Integer;
    procedure CalculatePages;
    procedure HandleWrap(const AText: string);
    procedure InitHighlighterRanges;
    procedure InitPrint;
    procedure PrintPage(APageNumber: Integer);
    procedure RestoreCurrentFont;
    procedure SaveCurrentFont;
    procedure SetCharWidth(const AValue: Integer);
    procedure SetEditor(const AValue: TCustomBCEditor);
    procedure SetFont(const AValue: TFont);
    procedure SetFooter(const AValue: TFooter);
    procedure SetHeader(const AValue: THeader);
    procedure SetHighlighter(const AValue: TBCEditorHighlighter);
    procedure SetLines(const AValue: TStrings);
    procedure SetMargins(const AValue: TMargins);
    procedure SetMaxLeftChar(const aValue: Integer);
    procedure SetPixelsPerInch;
    procedure SetWrap(const AValue: Boolean);
    procedure TextOut(const AText: string; AList: TList);
    procedure WriteLine(const AText: string);
    procedure WriteLineNumber;
  protected
    procedure PrintStatus(const AStatus: TStatus; const APageNumber: Integer; var AAbort: Boolean); virtual;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    property MaxLeftChar: Integer read FMaxLeftChar write SetMaxLeftChar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream);
    procedure Print(const AStartPage: Integer = 1; const AEndPage: Integer = -1);
    procedure PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
    procedure SaveToStream(AStream: TStream);
    procedure UpdatePages(ACanvas: TCanvas);
    property Editor: TCustomBCEditor read FEditor write SetEditor;
    property PageCount: Integer read GetPageCount;
    property PrinterInfo: TBCEditorPrinterInfo read FPrinterInfo;
  published
    property Color: TColor read FDefaultBackground write FDefaultBackground;
    property Colors: Boolean read FColors write FColors default False;
    property Copies: Integer read FCopies write FCopies;
    property DocumentTitle: string read FDocumentTitle write FDocumentTitle;
    property Font: TFont read FFont write SetFont;
    property Footer: TFooter read FFooter write SetFooter;
    property Header: THeader read FHeader write SetHeader;
    property Highlight: Boolean read FHighlight write FHighlight default True;
    property Highlighter: TBCEditorHighlighter read FHighlighter write SetHighlighter;
    property LineNumbers: Boolean read FLineNumbers write FLineNumbers default False;
    property LineNumbersInMargin: Boolean read FLineNumbersInMargin write FLineNumbersInMargin default False;
    property LineOffset: Integer read FLineOffset write FLineOffset default 0;
    property Margins: TMargins read FMargins write SetMargins;
    property OnPrintLine: TLineEvent read FOnPrintLine write FOnPrintLine;
    property OnPrintStatus: TStatusEvent read FOnPrintStatus write FOnPrintStatus;
    property PageOffset: Integer read FPageOffset write FPageOffset default 0;
    property SelectedOnly: Boolean read FSelectedOnly write FSelectedOnly default False;
    property Title: string read FTitle write FTitle;
    property Wrap: Boolean read FWrap write SetWrap default True;
  end;

  TBCEditorWrapPosition = class
  public
    Index: Integer;
  end;

function IntToRoman(AValue: Integer): string;
function WrapTextEx(const ALine: string; ABreakChars: TSysCharSet; AMaxColumn: Integer; AList: TList): Boolean;

implementation {***************************************************************}

uses
  UITypes, Types, Math,
  BCEditor.Consts;

const
  mmPerInch = 25.4;
  mmPerCm = 10;

  DEFAULT_LEFT_MARGIN_MM = 20;
  DEFAULT_RIGHT_MARGIN_MM = 15;
  DEFAULT_TOP_MARGIN_MM = 18;
  DEFAULT_BOTTOM_MM = 18;
  DEFAULT_HEADER_MM = 15;
  DEFAULT_FOOTER_MM = 15;
  DEFAULT_LEFT_TEXT_INDENT_MM = 2;
  DEFAULT_RIGHT_TEXT_INDENT_MM = 2;
  DEFAULT_INTERNAL_MARGIN_MM = 0.5;
  DEFAULT_MARGIN_MM = 0;

function WrapTextEx(const ALine: string; ABreakChars: TSysCharSet; AMaxColumn: Integer; AList: TList): Boolean;
var
  LFound: Boolean;
  LPosition: Integer;
  LPreviousPosition: Integer;
  LWrapPosition: TBCEditorWrapPosition;
begin
  if Length(ALine) <= AMaxColumn then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
  LPosition := 1;
  LPreviousPosition := 0;
  LWrapPosition := TBCEditorWrapPosition.Create;
  while LPosition <= Length(ALine) do
  begin
    LFound := (LPosition - LPreviousPosition > AMaxColumn) and (LWrapPosition.Index <> 0);
    if not LFound and (ALine[LPosition] <= High(Char)) and CharInSet(Char(ALine[LPosition]), ABreakChars) then
      LWrapPosition.Index := LPosition;

    if LFound then
    begin
      Result := True;
      AList.Add(LWrapPosition);
      LPreviousPosition := LWrapPosition.Index;

      if ((Length(ALine) - LPreviousPosition) > AMaxColumn) and (LPosition < Length(ALine)) then
        LWrapPosition := TBCEditorWrapPosition.Create
      else
        Break;
    end;
    Inc(LPosition);
  end;

  if (AList.Count = 0) or (AList.Last <> LWrapPosition) then
    LWrapPosition.Free;
end;

function IntToRoman(AValue: Integer): string;
begin
  Result := '';
  while AValue >= 1000 do
  begin
    Result := Result + 'M';
    AValue := AValue - 1000;
  end;

  if AValue >= 900 then
  begin
    Result := Result + 'CM';
    AValue := AValue - 900;
  end;

  while AValue >= 500 do
  begin
    Result := Result + 'D';
    AValue := AValue - 500;
  end;

  if AValue >= 400 then
  begin
    Result := Result + 'CD';
    AValue := AValue - 400;
  end;

  while AValue >= 100 do
  begin
    Result := Result + 'C';
    AValue := AValue - 100;
  end;

  if AValue >= 90 then
  begin
    Result := Result + 'XC';
    AValue := AValue - 90;
  end;

  while AValue >= 50 do
  begin
    Result := Result + 'L';
    AValue := AValue - 50;
  end;

  if AValue >= 40 then
  begin
    Result := Result + 'XL';
    AValue := AValue - 40;
  end;

  while AValue >= 10 do
  begin
    Result := Result + 'X';
    AValue := AValue - 10;
  end;

  if AValue >= 9 then
  begin
    Result := Result + 'IX';
    AValue := AValue - 9;
  end;

  while AValue >= 5 do
  begin
    Result := Result + 'V';
    AValue := AValue - 5;
  end;

  if AValue >= 4 then
  begin
    Result := Result + 'IV';
    AValue := AValue - 4;
  end;

  while AValue > 0 do
  begin
    Result := Result + 'I';
    Dec(AValue);
  end;
end;

procedure TBCEditorPrinterInfo.FillDefault;
begin
  FPhysicalWidth := 2481;
  FPhysicalHeight := 3507;
  FPrintableWidth := 2358;
  FPrintableHeight := 3407;
  FLeftMargin := 65;
  FRightMargin := 58;
  FTopMargin := 50;
  FBottomMargin := 50;
  FXPixPerInch := 300;
  FYPixPerInch := 300;
  FXPixPermm := FXPixPerInch / 25.4;
  FYPixPermm := FYPixPerInch / 25.4;
end;

function TBCEditorPrinterInfo.GetBottomMargin: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FBottomMargin;
end;

function TBCEditorPrinterInfo.GetLeftMargin: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FLeftMargin;
end;

function TBCEditorPrinterInfo.GetPhysicalHeight: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPhysicalHeight;
end;

function TBCEditorPrinterInfo.GetPhysicalWidth: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPhysicalWidth;
end;

function TBCEditorPrinterInfo.GetPrintableHeight: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPrintableHeight;
end;

function TBCEditorPrinterInfo.GetPrintableWidth: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FPrintableWidth;
end;

function TBCEditorPrinterInfo.GetRightMargin: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FRightMargin;
end;

function TBCEditorPrinterInfo.GetTopMargin: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FTopMargin;
end;

function TBCEditorPrinterInfo.GetXPixPerInch: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FXPixPerInch;
end;

function TBCEditorPrinterInfo.GetXPixPermm: Single;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FXPixPermm;
end;

function TBCEditorPrinterInfo.GetYPixPerInch: Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FYPixPerInch;
end;

function TBCEditorPrinterInfo.GetYPixPermm: Single;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := FYPixPermm;
end;

{ TBCEditorPrinterInfo ********************************************************}

function TBCEditorPrinterInfo.PixFromBottom(const AValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(AValue * FYPixPermm - FBottomMargin);
end;

function TBCEditorPrinterInfo.PixFromLeft(const AValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(AValue * FXPixPermm - FLeftMargin);
end;

function TBCEditorPrinterInfo.PixFromRight(const AValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(AValue * FXPixPermm - FRightMargin);
end;

function TBCEditorPrinterInfo.PixFromTop(const AValue: Double): Integer;
begin
  if not FIsUpdated then
    UpdatePrinter;
  Result := Round(AValue * FYPixPermm - FTopMargin);
end;

procedure TBCEditorPrinterInfo.UpdatePrinter;
begin
  FIsUpdated := True;
  Printer.Refresh;
  if Printer.Printers.Count <= 0 then
  begin
    FillDefault;
    Exit;
  end;
  FPhysicalWidth := GetDeviceCaps(Printer.Handle, PhysicalWidth);
  FPhysicalHeight := GetDeviceCaps(Printer.Handle, PhysicalHeight);
  FPrintableWidth := Printer.PageWidth;
  FPrintableHeight := Printer.PageHeight;
  FLeftMargin := GetDeviceCaps(Printer.Handle, PhysicalOffsetX);
  FTopMargin := GetDeviceCaps(Printer.Handle, PhysicalOffsetY);
  FRightMargin := FPhysicalWidth - FPrintableWidth - FLeftMargin;
  FBottomMargin := FPhysicalHeight - FPrintableHeight - FTopMargin;
  FXPixPerInch := GetDeviceCaps(Printer.Handle, LogPixelsX);
  FYPixPerInch := GetDeviceCaps(Printer.Handle, LogPixelsY);
  FXPixPermm := FXPixPerInch / 25.4;
  FYPixPermm := FYPixPerInch / 25.4;
end;

{ TBCEditorPrint.TMargins *****************************************************}

constructor TBCEditorPrint.TMargins.Create;
begin
  inherited;
  FUnitSystem := usMM;
  FLeft := DEFAULT_LEFT_MARGIN_MM;
  FRight := DEFAULT_RIGHT_MARGIN_MM;
  FTop := DEFAULT_TOP_MARGIN_MM;
  FBottom := DEFAULT_BOTTOM_MM;
  FHeader := DEFAULT_HEADER_MM;
  FFooter := DEFAULT_FOOTER_MM;
  FLeftTextIndent := DEFAULT_LEFT_TEXT_INDENT_MM;
  FRightTextIndent := DEFAULT_RIGHT_TEXT_INDENT_MM;
  FInternalMargin := DEFAULT_INTERNAL_MARGIN_MM;
  FMargin := DEFAULT_MARGIN_MM;
  FMirrorMargins := False;
end;

procedure TBCEditorPrint.TMargins.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TMargins) then
  with ASource as TMargins do
  begin
    Self.FLeft := FLeft;
    Self.FRight := FRight;
    Self.FTop := FTop;
    Self.FBottom := FBottom;
    Self.FHeader := FHeader;
    Self.FFooter := FFooter;
    Self.FLeftTextIndent := FLeftTextIndent;
    Self.FRightTextIndent := FRightTextIndent;
    Self.FInternalMargin := FInternalMargin;
    Self.FMargin := FMargin;
    Self.FMirrorMargins := FMirrorMargins;
    Self.FUnitSystem := FUnitSystem;
  end
  else
    inherited Assign(ASource);
end;

function TBCEditorPrint.TMargins.ConvertFrom(AValue: Double): Double;
begin
  case FUnitSystem of
    usCm:
      Result := AValue / mmPerCm;
    usInch:
      Result := AValue / mmPerInch;
    muThousandthsOfInches:
      Result := 1000 * AValue / mmPerInch;
  else
    Result := AValue;
  end;
end;

function TBCEditorPrint.TMargins.ConvertTo(AValue: Double): Double;
begin
  case FUnitSystem of
    usCm:
      Result := AValue * mmPerCm;
    usInch:
      Result := AValue * mmPerInch;
    muThousandthsOfInches:
      Result := mmPerInch * AValue / 1000;
  else
    Result := AValue;
  end;
end;

function TBCEditorPrint.TMargins.GetBottom: Double;
begin
  Result := ConvertFrom(FBottom);
end;

function TBCEditorPrint.TMargins.GetFooter: Double;
begin
  Result := ConvertFrom(FFooter);
end;

function TBCEditorPrint.TMargins.GetHeader: Double;
begin
  Result := ConvertFrom(FHeader);
end;

function TBCEditorPrint.TMargins.GetInternalMargin: Double;
begin
  Result := ConvertFrom(FInternalMargin);
end;

function TBCEditorPrint.TMargins.GetLeft: Double;
begin
  Result := ConvertFrom(FLeft);
end;

function TBCEditorPrint.TMargins.GetLeftTextIndent: Double;
begin
  Result := ConvertFrom(FLeftTextIndent);
end;

function TBCEditorPrint.TMargins.GetMargin: Double;
begin
  Result := ConvertFrom(FMargin);
end;

function TBCEditorPrint.TMargins.GetRight: Double;
begin
  Result := ConvertFrom(FRight);
end;

function TBCEditorPrint.TMargins.GetRightTextIndent: Double;
begin
  Result := ConvertFrom(FRightTextIndent);
end;

function TBCEditorPrint.TMargins.GetTop: Double;
begin
  Result := ConvertFrom(FTop);
end;

procedure TBCEditorPrint.TMargins.InitPage(ACanvas: TCanvas; APageNumber: Integer; APrinterInfo: TBCEditorPrinterInfo;
  ALineNumbers, ALineNumbersInMargin: Boolean; AMaxLineNumber: Integer);
begin
  if FMirrorMargins and ((APageNumber mod 2) = 0) then
  begin
    PixelLeft := APrinterInfo.PixFromLeft(FRight);
    PixelRight := APrinterInfo.PrintableWidth - APrinterInfo.PixFromRight(FLeft + FMargin);
  end
  else
  begin
    PixelLeft := APrinterInfo.PixFromLeft(FLeft + FMargin);
    PixelRight := APrinterInfo.PrintableWidth - APrinterInfo.PixFromRight(FRight);
  end;
  if ALineNumbers and (not ALineNumbersInMargin) then
    PixelLeft := PixelLeft + TextWidth(ACanvas, IntToStr(AMaxLineNumber) + ': ');
  PixelTop := APrinterInfo.PixFromTop(FTop);
  PixelBottom := APrinterInfo.PrintableHeight - APrinterInfo.PixFromBottom(FBottom);
  PixelHeader := APrinterInfo.PixFromTop(FHeader);
  PixelFooter := APrinterInfo.PrintableHeight - APrinterInfo.PixFromBottom(FFooter);
  PixelInternalMargin := Round(APrinterInfo.YPixPermm * FInternalMargin);
  PixelMargin := Round(APrinterInfo.XPixPermm * FMargin);
  PixelRightTextIndent := PixelRight - Round(APrinterInfo.XPixPermm * FRightTextIndent);
  PixelLeftTextIndent := PixelLeft + Round(APrinterInfo.XPixPermm * FLeftTextIndent);
end;

procedure TBCEditorPrint.TMargins.LoadFromStream(AStream: TStream);
begin
  with AStream do
  begin
    Read(FUnitSystem, SizeOf(FUnitSystem));
    Read(FLeft, SizeOf(FLeft));
    Read(FRight, SizeOf(FRight));
    Read(FTop, SizeOf(FTop));
    Read(FBottom, SizeOf(FBottom));
    Read(FHeader, SizeOf(FHeader));
    Read(FFooter, SizeOf(FFooter));
    Read(FLeftTextIndent, SizeOf(FLeftTextIndent));
    Read(FRightTextIndent, SizeOf(FRightTextIndent));
    Read(FInternalMargin, SizeOf(FInternalMargin));
    Read(FMargin, SizeOf(FMargin));
    Read(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

procedure TBCEditorPrint.TMargins.SaveToStream(AStream: TStream);
begin
  with AStream do
  begin
    Write(FUnitSystem, SizeOf(FUnitSystem));
    Write(FLeft, SizeOf(FLeft));
    Write(FRight, SizeOf(FRight));
    Write(FTop, SizeOf(FTop));
    Write(FBottom, SizeOf(FBottom));
    Write(FHeader, SizeOf(FHeader));
    Write(FFooter, SizeOf(FFooter));
    Write(FLeftTextIndent, SizeOf(FLeftTextIndent));
    Write(FRightTextIndent, SizeOf(FRightTextIndent));
    Write(FInternalMargin, SizeOf(FInternalMargin));
    Write(FMargin, SizeOf(FMargin));
    Write(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

procedure TBCEditorPrint.TMargins.SetBottom(const AValue: Double);
begin
  FBottom := ConvertTo(AValue);
end;

procedure TBCEditorPrint.TMargins.SetFooter(const AValue: Double);
begin
  FFooter := ConvertTo(AValue);
end;

procedure TBCEditorPrint.TMargins.SetHeader(const AValue: Double);
begin
  FHeader := ConvertTo(AValue);
end;

procedure TBCEditorPrint.TMargins.SetInternalMargin(const AValue: Double);
begin
  FInternalMargin := ConvertTo(AValue);
end;

procedure TBCEditorPrint.TMargins.SetLeft(const AValue: Double);
begin
  FLeft := ConvertTo(AValue);
end;

procedure TBCEditorPrint.TMargins.SetLeftTextIndent(const AValue: Double);
begin
  FLeftTextIndent := ConvertTo(AValue);
end;

procedure TBCEditorPrint.TMargins.SetMargin(const AValue: Double);
begin
  FMargin := ConvertTo(AValue);
end;

procedure TBCEditorPrint.TMargins.SetRight(const AValue: Double);
begin
  FRight := ConvertTo(AValue);
end;

procedure TBCEditorPrint.TMargins.SetRightTextIndent(const AValue: Double);
begin
  FRightTextIndent := ConvertTo(AValue);
end;

procedure TBCEditorPrint.TMargins.SetTop(const AValue: Double);
begin
  FTop := ConvertTo(AValue);
end;

{ TBCEditorPrint.TSectionItem *************************************************}

function CompareItems(Item1, Item2: Pointer): Integer;
begin
  Result := TBCEditorPrint.TSectionItem(Item1).LineNumber - TBCEditorPrint.TSectionItem(Item2).LineNumber;
  if Result = 0 then
    Result := Integer(Item1) - Integer(Item2);
end;

constructor TBCEditorPrint.TSectionItem.Create;
begin
  inherited;
  FFont := TFont.Create;
end;

destructor TBCEditorPrint.TSectionItem.Destroy;
begin
  inherited;
  FFont.Free;
end;

function TBCEditorPrint.TSectionItem.GetText(ANumberOfPages, APageNumber: Integer; ARoman: Boolean; const ATitle, ATime, ADate: string): string;
var
  LLength: Integer;
  LString: string;
  Run: Integer;
  Start: Integer;

  procedure DoAppend(AText: string);
  begin
    Result := Result + AText;
  end;
  procedure TryAppend(var First: Integer; After: Integer);
  begin
    if After > First then
    begin
      DoAppend(Copy(LString, First, After - First));
      First := After;
    end;
  end;
  function TryExecuteMacro: Boolean;
  var
    Macro: string;
  begin
    Result := True;
    Macro := AnsiUpperCase(Copy(FText, Start, Run - Start + 1));
    if Macro = '$PAGENUM$' then
    begin
      if ARoman then
        DoAppend(IntToRoman(APageNumber))
      else
        DoAppend(IntToStr(APageNumber));
      Exit;
    end;
    if Macro = '$PAGECOUNT$' then
    begin
      if ARoman then
        DoAppend(IntToRoman(ANumberOfPages))
      else
        DoAppend(IntToStr(ANumberOfPages));
      Exit;
    end;
    if Macro = '$TITLE$' then
    begin
      DoAppend(ATitle);
      Exit;
    end;
    if Macro = '$DATE$' then
    begin
      DoAppend(ADate);
      Exit;
    end;
    if Macro = '$TIME$' then
    begin
      DoAppend(ATime);
      Exit;
    end;
    if Macro = '$DATETIME$' then
    begin
      DoAppend(ADate + ' ' + ATime);
      Exit;
    end;
    if Macro = '$TIMEDATE$' then
    begin
      DoAppend(ATime + ' ' + ADate);
      Exit;
    end;
    Result := False;
  end;

begin
  Result := '';
  LString := FText;
  if Trim(LString) = '' then
    Exit;
  LLength := Length(LString);
  if LLength > 0 then
  begin
    Start := 1;
    Run := 1;
    while Run <= LLength do
    begin
      if LString[Run] = '$' then
      begin
        TryAppend(Start, Run);
        Inc(Run);
        while Run <= LLength do
        begin
          if LString[Run] = '$' then
          begin
            if TryExecuteMacro then
            begin
              Inc(Run);
              Start := Run;
              break;
            end
            else
            begin
              TryAppend(Start, Run);
              Inc(Run);
            end;
          end
          else
            Inc(Run);
        end;
      end
      else
        Inc(Run);
    end;
    TryAppend(Start, Run);
  end;
end;

procedure TBCEditorPrint.TSectionItem.LoadFromStream(AStream: TStream);
var
  LAnsiBuffer: AnsiString;
  LBufferSize: Integer;
  LCharset: TFontCharset;
  LColor: TColor;
  LHeight: Integer;
  LLength: Integer;
  LName: TFontName;
  LPitch: TFontPitch;
  LSize: Integer;
  LStyle: TFontStyles;
begin
  with AStream do
  begin
    Read(LLength, SizeOf(LLength));
    LBufferSize := LLength * SizeOf(Char);
    SetLength(LAnsiBuffer, LBufferSize);
    Read(LAnsiBuffer[1], LBufferSize);
    FText := string(LAnsiBuffer);
    Read(FLineNumber, SizeOf(FLineNumber));
    Read(LCharset, SizeOf(lCharset));
    Read(LColor, SizeOf(LColor));
    Read(LHeight, SizeOf(LHeight));
    Read(LBufferSize, SizeOf(LBufferSize));
    SetLength(LAnsiBuffer, LBufferSize);
    Read(LAnsiBuffer[1], LBufferSize);
    LName := string(LAnsiBuffer);
    Read(LPitch, SizeOf(LPitch));
    Read(LSize, SizeOf(LSize));
    Read(LStyle, SizeOf(LStyle));
    FFont.Charset := LCharset;
    FFont.Color := LColor;
    FFont.Height := LHeight;
    FFont.Name := LName;
    FFont.Pitch := LPitch;
    FFont.Size := LSize;
    FFont.Style := LStyle;
    Read(FAlignment, SizeOf(FAlignment));
  end;
end;

procedure TBCEditorPrint.TSectionItem.SaveToStream(AStream: TStream);
var
  LCharset: TFontCharset;
  LColor: TColor;
  LHeight: Integer;
  LLength: Integer;
  LName: TFontName;
  LPitch: TFontPitch;
  LSize: Integer;
  LStyle: TFontStyles;
begin
  with AStream do
  begin
    LLength := Length(FText);
    Write(LLength, SizeOf(LLength));
    Write(PChar(FText)^, LLength * SizeOf(Char));
    Write(FLineNumber, SizeOf(FLineNumber));
    lCharset := FFont.Charset;
    LColor := FFont.Color;
    LHeight := FFont.Height;
    LName := FFont.Name;
    LPitch := FFont.Pitch;
    LSize := FFont.Size;
    LStyle := FFont.Style;
    Write(LCharset, SizeOf(LCharset));
    Write(LColor, SizeOf(LColor));
    Write(LHeight, SizeOf(LHeight));
    LLength := Length(LName);
    Write(LLength, SizeOf(LLength));
    Write(PAnsiChar(AnsiString(LName))^, LLength);
    Write(LPitch, SizeOf(LPitch));
    Write(LSize, SizeOf(LSize));
    Write(LStyle, SizeOf(LStyle));
    Write(FAlignment, SizeOf(FAlignment));
  end;
end;

procedure TBCEditorPrint.TSectionItem.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorPrint.TBCEditorPrint.TSection *********************************************}

constructor TBCEditorPrint.TSection.Create;
begin
  inherited;
  FFrameTypes := [ftLine];
  FShadedColor := clSilver;
  FLineColor := clBlack;
  FItems := TList.Create;
  FDefaultFont := TFont.Create;
  FOldPen := TPen.Create;
  FOldBrush := TBrush.Create;
  FOldFont := TFont.Create;
  FRomanNumbers := False;
  FMirrorPosition := False;
  FLineInfo := TList.Create;
  with FDefaultFont do
  begin
    Name := 'Courier New';
    Size := 9;
    Color := clBlack;
  end;
end;

destructor TBCEditorPrint.TSection.Destroy;
var
  LIndex: Integer;
begin
  Clear;
  FItems.Free;
  FDefaultFont.Free;
  FOldPen.Free;
  FOldBrush.Free;
  FOldFont.Free;
  for LIndex := 0 to FLineInfo.Count - 1 do
    TLineInfo(FLineInfo[LIndex]).Free;
  FLineInfo.Free;
  inherited;
end;

function TBCEditorPrint.TSection.Add(const AText: string; const AFont: TFont; const AAlignment: TAlignment; const ALineNumber: Integer): Integer;
var
  LSectionItem: TSectionItem;
begin
  LSectionItem := TSectionItem.Create;
  if not Assigned(AFont) then
    LSectionItem.Font := FDefaultFont
  else
    LSectionItem.Font := AFont;
  LSectionItem.Alignment := AAlignment;
  LSectionItem.LineNumber := ALineNumber;
  LSectionItem.Index := FItems.Add(LSectionItem);
  LSectionItem.Text := AText;
  Result := LSectionItem.Index;
end;

procedure TBCEditorPrint.TSection.Assign(ASource: TPersistent);
var
  LIndex: Integer;
  LSectionItem: TSectionItem;
begin
  if Assigned(ASource) and (ASource is TBCEditorPrint.TSection) then
  with ASource as TBCEditorPrint.TSection do
  begin
    Clear;
    Self.FSectionType := FSectionType;
    Self.FFrameTypes := FFrameTypes;
    Self.FShadedColor := FShadedColor;
    Self.FLineColor := FLineColor;
    for LIndex := 0 to FItems.Count - 1 do
    begin
      LSectionItem := TSectionItem(FItems[LIndex]);
      Self.Add(LSectionItem.Text, LSectionItem.Font, LSectionItem.Alignment, LSectionItem.LineNumber);
    end;
    Self.FDefaultFont.Assign(FDefaultFont);
    Self.FRomanNumbers := FRomanNumbers;
    Self.FMirrorPosition := FMirrorPosition;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorPrint.TSection.CalculateHeight(ACanvas: TCanvas);
var
  i: Integer;
  LCurrentLine: Integer;
  LOrginalHeight: Integer;
  LSectionItem: TSectionItem;
  LTextMetric: TTextMetric;
begin
  FFrameHeight := -1;
  if FItems.Count <= 0 then
    Exit;

  LCurrentLine := 1;
  FFrameHeight := 0;
  LOrginalHeight := FFrameHeight;
  for i := 0 to FItems.Count - 1 do
  begin
    LSectionItem := TSectionItem(FItems[i]);
    if LSectionItem.LineNumber <> LCurrentLine then
    begin
      LCurrentLine := LSectionItem.LineNumber;
      LOrginalHeight := FFrameHeight;
    end;
    ACanvas.Font.Assign(LSectionItem.Font);
    GetTextMetrics(ACanvas.Handle, LTextMetric);
    with TLineInfo(FLineInfo[LCurrentLine - 1]), LTextMetric do
    begin
      LineHeight := Max(LineHeight, TextHeight(ACanvas, 'W'));
      MaxBaseDistance := Max(MaxBaseDistance, tmHeight - tmDescent);
    end;
    FFrameHeight := Max(FFrameHeight, LOrginalHeight + TextHeight(ACanvas, 'W'));
  end;
  FFrameHeight := FFrameHeight + 2 * FMargins.PixelInternalMargin;
end;

procedure TBCEditorPrint.TSection.Clear;
var
  LIndex: Integer;
begin
  for LIndex := 0 to FItems.Count - 1 do
    TSectionItem(FItems[LIndex]).Free;
  FItems.Clear;
end;

function TBCEditorPrint.TSection.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TBCEditorPrint.TSection.Delete(AIndex: Integer);
var
  LIndex: Integer;
begin
  for LIndex := 0 to FItems.Count - 1 do
  if TSectionItem(FItems[LIndex]).Index = AIndex then
  begin
    FItems.Delete(LIndex);
    Break;
  end;
end;

procedure TBCEditorPrint.TSection.DrawFrame(ACanvas: TCanvas);
begin
  if FrameTypes = [] then
    Exit;
  with ACanvas, FMargins do
  begin
    Pen.Color := LineColor;
    Brush.Color := ShadedColor;
    if ftShaded in FrameTypes then
      Brush.Style := bsSolid
    else
      Brush.Style := bsClear;
    if ftBox in FrameTypes then
      Pen.Style := psSolid
    else
      Pen.Style := psClear;
    if FrameTypes * [ftBox, ftShaded] <> [] then
    begin
      if FSectionType = stHeader then
        Rectangle(PixelLeft, PixelHeader - FFrameHeight, PixelRight, PixelHeader)
      else
        Rectangle(PixelLeft, PixelFooter, PixelRight, PixelFooter + FFrameHeight);
    end;
    if ftLine in FrameTypes then
    begin
      Pen.Style := psSolid;
      if FSectionType = stHeader then
      begin
        MoveTo(PixelLeft, PixelHeader);
        LineTo(PixelRight, PixelHeader);
      end
      else
      begin
        MoveTo(PixelLeft, PixelFooter);
        LineTo(PixelRight, PixelFooter);
      end
    end;
  end;
end;

procedure TBCEditorPrint.TSection.FixLines;
var
  i: Integer;
  LCurrentLine: Integer;
  LLineInfo: TLineInfo;
begin
  for i := 0 to FLineInfo.Count - 1 do
    TLineInfo(FLineInfo[i]).Free;
  FLineInfo.Clear;
  LCurrentLine := 0;
  FLineCount := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    if TSectionItem(FItems[i]).LineNumber <> LCurrentLine then
    begin
      LCurrentLine := TSectionItem(FItems[i]).LineNumber;
      FLineCount := FLineCount + 1;
      LLineInfo := TLineInfo.Create;
      FLineInfo.Add(LLineInfo);
    end;
    TSectionItem(FItems[i]).LineNumber := FLineCount;
  end;
end;

function TBCEditorPrint.TSection.Get(AIndex: Integer): TSectionItem;
begin
  Result := TSectionItem(FItems[AIndex]);
end;

procedure TBCEditorPrint.TSection.InitPrint(ACanvas: TCanvas; NumberOfPages: Integer; const Title: string; Margins: TMargins);
begin
  SaveFontPenBrush(ACanvas);
  FDate := DateToStr(Now);
  FTime := TimeToStr(Now);
  FNumberOfPages := NumberOfPages;
  FMargins := Margins;
  FTitle := Title;
  FItems.Sort(CompareItems);
  FixLines;
  CalculateHeight(ACanvas);
  RestoreFontPenBrush(ACanvas);
end;

procedure TBCEditorPrint.TSection.LoadFromStream(AStream: TStream);
var
  LAnsiBuffer: AnsiString;
  LBufferSize: Integer;
  LCharset: TFontCharset;
  LColor: TColor;
  LCount: Integer;
  LHeight: Integer;
  LIndex: Integer;
  LName: TFontName;
  LPitch: TFontPitch;
  LSize: Integer;
  LStyle: TFontStyles;
begin
  with AStream do
  begin
    Read(FFrameTypes, SizeOf(FFrameTypes));
    Read(FShadedColor, SizeOf(FShadedColor));
    Read(FLineColor, SizeOf(FLineColor));
    Read(FRomanNumbers, SizeOf(FRomanNumbers));
    Read(FMirrorPosition, SizeOf(FMirrorPosition));
    Read(LCharset, SizeOf(LCharset));
    Read(LColor, SizeOf(LColor));
    Read(LHeight, SizeOf(LHeight));
    Read(LBufferSize, SizeOf(LBufferSize));
    SetLength(LAnsiBuffer, LBufferSize);
    Read(LAnsiBuffer[1], LBufferSize);
    LName := string(LAnsiBuffer);
    Read(LPitch, SizeOf(LPitch));
    Read(LSize, SizeOf(LSize));
    Read(LStyle, SizeOf(LStyle));
    FDefaultFont.Charset := LCharset;
    FDefaultFont.Color := LColor;
    FDefaultFont.Height := LHeight;
    FDefaultFont.Name := LName;
    FDefaultFont.Pitch := LPitch;
    FDefaultFont.Size := LSize;
    FDefaultFont.Style := LStyle;
    Read(LCount, SizeOf(LCount));
    while LCount > 0 do
    begin
      LIndex := Add('', nil, taLeftJustify, 1);
      Get(LIndex).LoadFromStream(AStream);
      Dec(LCount);
    end;
  end;
end;

procedure TBCEditorPrint.TSection.Print(ACanvas: TCanvas; PageNum: Integer);
var
  i: Integer;
  LAlignment: TAlignment;
  LCurrentLine: Integer;
  LOldAlign: UINT;
  LSectionItem: TSectionItem;
  S: string;
  X: Integer;
  Y: Integer;
begin
  if FFrameHeight <= 0 then
    Exit;
  SaveFontPenBrush(ACanvas);
  DrawFrame(ACanvas);
  ACanvas.Brush.Style := bsClear;
  if FSectionType = stHeader then
    Y := FMargins.PixelHeader - FFrameHeight
  else
    Y := FMargins.PixelFooter;
  Y := Y + FMargins.PixelInternalMargin;

  LCurrentLine := 1;
  for i := 0 to FItems.Count - 1 do
  begin
    LSectionItem := TSectionItem(FItems[i]);
    ACanvas.Font := LSectionItem.Font;
    if LSectionItem.LineNumber <> LCurrentLine then
    begin
      Y := Y + TLineInfo(FLineInfo[LCurrentLine - 1]).LineHeight;
      LCurrentLine := LSectionItem.LineNumber;
    end;
    S := LSectionItem.GetText(FNumberOfPages, PageNum, FRomanNumbers, FTitle, FTime, FDate);
    LAlignment := LSectionItem.Alignment;
    if MirrorPosition and ((PageNum mod 2) = 0) then
    begin
      case LSectionItem.Alignment of
        taRightJustify:
          LAlignment := taLeftJustify;
        taLeftJustify:
          LAlignment := taRightJustify;
      end;
    end;
    with FMargins do
    begin
      X := PixelLeftTextIndent;
      case LAlignment of
        taRightJustify:
          X := PixelRightTextIndent - TextWidth(ACanvas, S);
        taCenter:
          X := (PixelLeftTextIndent + PixelRightTextIndent - TextWidth(ACanvas, S)) div 2;
      end;
    end;
    LOldAlign := SetTextAlign(ACanvas.Handle, TA_BASELINE);
    ExtTextOut(ACanvas.Handle, X, Y + TLineInfo(FLineInfo[LCurrentLine - 1]).MaxBaseDistance, 0, nil, PChar(S),
      Length(S), nil);
    SetTextAlign(ACanvas.Handle, LOldAlign);
  end;
  RestoreFontPenBrush(ACanvas);
end;

procedure TBCEditorPrint.TSection.RestoreFontPenBrush(ACanvas: TCanvas);
begin
  ACanvas.Font.Assign(FOldFont);
  ACanvas.Brush.Assign(FOldBrush);
  ACanvas.Pen.Assign(FOldPen);
end;

procedure TBCEditorPrint.TSection.SaveFontPenBrush(ACanvas: TCanvas);
begin
  FOldFont.Assign(ACanvas.Font);
  FOldBrush.Assign(ACanvas.Brush);
  FOldPen.Assign(ACanvas.Pen);
end;

procedure TBCEditorPrint.TSection.SaveToStream(AStream: TStream);
var
  LCharset: TFontCharset;
  LColor: TColor;
  LCount: Integer;
  LHeight: Integer;
  LIndex: Integer;
  LLength: Integer;
  LName: TFontName;
  LPitch: TFontPitch;
  LSize: Integer;
  LStyle: TFontStyles;
begin
  with AStream do
  begin
    Write(FFrameTypes, SizeOf(FFrameTypes));
    Write(FShadedColor, SizeOf(FShadedColor));
    Write(FLineColor, SizeOf(FLineColor));
    Write(FRomanNumbers, SizeOf(FRomanNumbers));
    Write(FMirrorPosition, SizeOf(FMirrorPosition));
    LCharset := FDefaultFont.Charset;
    LColor := FDefaultFont.Color;
    LHeight := FDefaultFont.Height;
    LName := FDefaultFont.Name;
    LPitch := FDefaultFont.Pitch;
    LSize := FDefaultFont.Size;
    LStyle := FDefaultFont.Style;
    Write(LCharset, SizeOf(LCharset));
    Write(LColor, SizeOf(LColor));
    Write(LHeight, SizeOf(LHeight));
    LLength := Length(LName);
    Write(LLength, SizeOf(LLength));
    Write(PAnsiChar(AnsiString(LName))^, Length(LName));
    Write(LPitch, SizeOf(LPitch));
    Write(LSize, SizeOf(LSize));
    Write(LStyle, SizeOf(LStyle));
    LCount := Count;
    Write(LCount, SizeOf(LCount));
    for LIndex := 0 to LCount - 1 do
      Get(LIndex).SaveToStream(AStream);
  end;
end;

procedure TBCEditorPrint.TSection.SetDefaultFont(const AValue: TFont);
begin
  FDefaultFont.Assign(AValue);
end;

procedure TBCEditorPrint.TSection.SetPixelsPerInch(AValue: Integer);
var
  i: Integer;
  LFont: TFont;
  LTmpSize: Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    LFont := TSectionItem(FItems[i]).Font;
    LTmpSize := LFont.Size;
    LFont.PixelsPerInch := AValue;
    LFont.Size := LTmpSize;
  end;
end;

{ TBCEditorPrint.THeader ******************************************************}

constructor TBCEditorPrint.THeader.Create;
begin
  inherited;
  SectionType := stHeader;
end;

{ TBCEditorPrint.TFooter ******************************************************}

constructor TBCEditorPrint.TFooter.Create;
begin
  inherited;
  SectionType := stFooter;
end;

{ TBCEditorPrint **************************************************************}

constructor TBCEditorPrint.Create(AOwner: TComponent);
begin
  inherited;
  FCopies := 1;
  FFooter := TFooter.Create;
  FHeader := THeader.Create;
  FLines := TStringList.Create;
  FMargins := TMargins.Create;
  FPrinterInfo := TBCEditorPrinterInfo.Create;
  FFont := TFont.Create;
  FOldFont := TFont.Create;
  MaxLeftChar := 1024;
  FWrap := True;
  FHighlight := True;
  FColors := False;
  FLineNumbers := False;
  FLineOffset := 0;
  FPageOffset := 0;
  FLineNumbersInMargin := False;
  FPages := TList.Create;
  FTabWidth := 8;
  FDefaultBackground := clWhite;
  FFontDummy := TFont.Create;
  with FFontDummy do
  begin
    Name := 'Courier New';
    Size := 10;
  end;
  FPaintHelper := TBCEditorPaintHelper.Create([fsBold], FFontDummy);
end;

destructor TBCEditorPrint.Destroy;
var
  LIndex: Integer;
begin
  FFooter.Free;
  FHeader.Free;
  FLines.Free;
  FMargins.Free;
  FPrinterInfo.Free;
  FFont.Free;
  FOldFont.Free;
  for LIndex := 0 to FPages.Count - 1 do
    TBCEditorPageLine(FPages[LIndex]).Free;
  FPages.Free;
  FPaintHelper.Free;
  FFontDummy.Free;
  inherited;
end;

procedure TBCEditorPrint.SetLines(const AValue: TStrings);
var
  LHasTabs: Boolean;
  LIndex: Integer;
  LLine: string;
  LPosition: Integer;
begin
  with FLines do
  begin
    BeginUpdate;
    try
      Clear;
      for LIndex := 0 to AValue.Count - 1 do
      begin
        LLine := ConvertTabs(AValue[LIndex], FTabWidth, LHasTabs, FColumns);
        LPosition := Pos(BCEDITOR_TAB_CHAR, LLine);
        while LPosition > 0 do
        begin
          LLine[LPosition] := ' ';
          LPosition := Pos(BCEDITOR_TAB_CHAR, LLine);
        end;
        Add(LLine);
      end;
    finally
      EndUpdate;
    end;
  end;
  FHighlighterRangesSet := False;
  FPagesCounted := False;
end;

procedure TBCEditorPrint.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
  FPagesCounted := False;
end;

procedure TBCEditorPrint.SetCharWidth(const AValue: Integer);
begin
  if FCharWidth <> AValue then
    FCharWidth := AValue;
end;

procedure TBCEditorPrint.SetMaxLeftChar(const AValue: Integer);
begin
  if FMaxLeftChar <> AValue then
    FMaxLeftChar := AValue;
end;

procedure TBCEditorPrint.SetHighlighter(const AValue: TBCEditorHighlighter);
begin
  FHighlighter := AValue;
  FHighlighterRangesSet := False;
  FPagesCounted := False;
end;

procedure TBCEditorPrint.SetWrap(const AValue: Boolean);
begin
  if AValue <> FWrap then
  begin
    FWrap := AValue;
    if FPages.Count > 0 then
    begin
      CalculatePages;
      FHeader.NumberOfPages := FPageCount;
      FFooter.NumberOfPages := FPageCount;
   end;
  end;
end;

procedure TBCEditorPrint.InitPrint;
var
  LSize: Integer;
  LTextMetric: TTextMetric;
begin
  FFontColor := FFont.Color;

  FCanvas.Font.Assign(FFont);
  if not FPrinting then
  begin
    SetPixelsPerInch;
    LSize := FCanvas.Font.Size;
    FCanvas.Font.PixelsPerInch := FFont.PixelsPerInch;
    FCanvas.Font.Size := LSize;
  end;
  FCanvas.Font.Style := [fsBold, fsItalic, fsUnderline, fsStrikeOut];

  GetTextMetrics(FCanvas.Handle, LTextMetric);
  CharWidth := LTextMetric.tmAveCharWidth;
  FLineHeight := LTextMetric.tmHeight + LTextMetric.tmExternalLeading;

  FPaintHelper.SetBaseFont(FFont);
  FPaintHelper.SetStyle(FFont.Style);

  FMargins.InitPage(FCanvas, 1, FPrinterInfo, FLineNumbers, FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
  CalculatePages;
  FHeader.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
  FFooter.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
end;

procedure TBCEditorPrint.SetPixelsPerInch;
var
  LSize: Integer;
begin
  FHeader.SetPixelsPerInch(FPrinterInfo.YPixPerInch);
  FFooter.SetPixelsPerInch(FPrinterInfo.YPixPerInch);
  LSize := FFont.Size;
  FFont.PixelsPerInch := FPrinterInfo.YPixPerInch;
  FFont.Size := LSize;
end;

procedure TBCEditorPrint.InitHighlighterRanges;
var
  LIndex: Integer;
begin
  if not FHighlighterRangesSet and Assigned(FHighlighter) and (FLines.Count > 0) then
  begin
    FHighlighter.ResetCurrentRange;
    FLines.Objects[0] := FHighlighter.GetCurrentRange;
    LIndex := 1;
    while LIndex < FLines.Count do
    begin
      FHighlighter.SetCurrentLine(FLines[LIndex - 1]);
      FHighlighter.NextToEndOfLine;
      FLines.Objects[LIndex] := FHighlighter.GetCurrentRange;
      Inc(LIndex);
    end;
    FHighlighterRangesSet := True;
  end;
end;

procedure TBCEditorPrint.CalculatePages;
var
  LEndLine: Integer;
  LIndex: Integer;
  LIndex2: Integer;
  LList: TList;
  LPageLine: TBCEditorPageLine;
  LSelectionLength: Integer;
  LSelectionStart: Integer;
  LStartLine: Integer;
  LText: string;
  LYPos: Integer;

  procedure CountWrapped;
  var
    LIndex: Integer;
  begin
    for LIndex := 0 to LList.Count - 1 do
      LYPos := LYPos + FLineHeight;
  end;

begin
  InitHighlighterRanges;
  for LIndex := 0 to FPages.Count - 1 do
    TBCEditorPageLine(FPages[LIndex]).Free;
  FPages.Clear;
  FMaxWidth := FMargins.PixelRight - FMargins.PixelLeft;
  FMaxColumn := FMaxWidth div TextWidth(FCanvas, 'W') - 1;
  FMaxWidth := TextWidth(FCanvas, StringOfChar('W', FMaxColumn)); // TODO: This does not work with non-fixed width fonts
  FPageCount := 1;
  LPageLine := TBCEditorPageLine.Create;
  LPageLine.FirstLine := 0;
  FPages.Add(LPageLine);
  LYPos := FMargins.PixelTop;
  if SelectedOnly then
  begin
    LStartLine := FBlockBeginPosition.Line - 1;
    LEndLine := FBlockEndPosition.Line - 1;
  end
  else
  begin
    LStartLine := 0;
    LEndLine := FLines.Count - 1;
  end;
  for LIndex := LStartLine to LEndLine do
  begin
    if LYPos + FLineHeight > FMargins.PixelBottom then
    begin
      LYPos := FMargins.PixelTop;
      FPageCount := FPageCount + 1;
      LPageLine := TBCEditorPageLine.Create;
      LPageLine.FirstLine := LIndex;
      FPages.Add(LPageLine);
    end;

    if Wrap then
    begin
      if not FSelectedOnly then
        LText := FLines[LIndex]
      else
      begin
        if (FSelectionMode = smColumn) or (LIndex = FBlockBeginPosition.Line - 1) then
          LSelectionStart := FBlockBeginPosition.Char
        else
          LSelectionStart := 1;
        if (FSelectionMode = smColumn) or (LIndex = FBlockEndPosition.Line - 1) then
          LSelectionLength := FBlockEndPosition.Char - LSelectionStart
        else
          LSelectionLength := MaxInt;
        LText := Copy(FLines[LIndex], LSelectionStart, LSelectionLength);
      end;

      if TextWidth(FCanvas, LText) > FMaxWidth then
      begin
        LList := TList.Create;
        try
          if WrapTextEx(LText, [' ', '-', BCEDITOR_TAB_CHAR, ','], FMaxColumn, LList) then
            CountWrapped
          else
          begin
            if WrapTextEx(LText, [';', ')', '.'], FMaxColumn, LList) then
              CountWrapped
            else
            while Length(LText) > 0 do
            begin
              Delete(LText, 1, FMaxColumn);
              if Length(LText) > 0 then
                LYPos := LYPos + FLineHeight;
            end;
          end;
          for LIndex2 := 0 to LList.Count - 1 do
            TBCEditorWrapPosition(LList[LIndex2]).Free;
        finally
          LList.Free;
        end;
      end;
    end;

    LYPos := LYPos + FLineHeight;
  end;
  FPagesCounted := True;
end;

procedure TBCEditorPrint.WriteLineNumber;
var
  LLineNumber: string;
begin
  SaveCurrentFont;
  LLineNumber := (FLineNumber + FLineOffset).ToString + ': ';
  FCanvas.Brush.Color := FDefaultBackground;
  FCanvas.Font.Style := [];
  FCanvas.Font.Color := clBlack;
  FCanvas.TextOut(FMargins.PixelLeft - FCanvas.TextWidth(LLineNumber), FYPos, LLineNumber);
  RestoreCurrentFont;
end;

procedure TBCEditorPrint.HandleWrap(const AText: string);
var
  LList: TList;
  LListIndex: Integer;

  procedure WrapPrimitive;
  var
    LIndex: Integer;
    LText: string;
    LWrapPosition: TBCEditorWrapPosition;
  begin
    LIndex := 1;
    while LIndex <= Length(AText) do
    begin
      LText := '';
      while (Length(LText) < FMaxColumn) and (LIndex <= Length(AText)) do
      begin
        LText := LText + AText[LIndex];
        Inc(LIndex);
      end;
      LWrapPosition := TBCEditorWrapPosition.Create;
      LWrapPosition.Index := LIndex - 1;
      LList.Add(LWrapPosition);
      if (Length(LText) - LIndex) <= FMaxColumn then
        Break;
    end;
  end;

begin
  LList := TList.Create;
  try
    if WrapTextEx(AText, [' ', '-', BCEDITOR_TAB_CHAR, ','], FMaxColumn, LList) then
      TextOut(AText, LList)
    else
    begin
      if WrapTextEx(AText, [';', ')', '.'], FMaxColumn, LList) then
        TextOut(AText, LList)
      else
      begin
        WrapPrimitive;
        TextOut(AText, LList)
      end;
    end;
    for LListIndex := 0 to LList.Count - 1 do
      TBCEditorWrapPosition(LList[LListIndex]).Free;
  finally
    LList.Free;
  end;
end;

procedure TBCEditorPrint.SaveCurrentFont;
begin
  FOldFont.Assign(FCanvas.Font);
end;

procedure TBCEditorPrint.RestoreCurrentFont;
begin
  FCanvas.Font.Assign(FOldFont);
end;

function TBCEditorPrint.ClipLineToRect(var ALine: string): string;
begin
  while FCanvas.TextWidth(ALine) > FMaxWidth do
    SetLength(ALine, Length(ALine) - 1);

  Result := ALine;
end;

procedure TBCEditorPrint.TextOut(const AText: string; AList: TList);
var
  LClipRect: TRect;
  LColor: TColor;
  LCount: Integer;
  LHandled: Boolean;
  LHighlighterAttribute: TBCEditorHighlighter.TAttribute;
  LIndex: Integer;
  LLines: TStringList;
  LOldWrapPosition: Integer;
  LToken: string;
  LTokenPosition: Integer;
  LTokenStart: Integer;
  LWrapPosition: Integer;

  procedure ClippedTextOut(X, Y: Integer; AText: string);
  begin
    AText := ClipLineToRect(AText);
    if Highlight and Assigned(FHighlighter) and (FLines.Count > 0) then
    begin
      SetBkMode(FCanvas.Handle, TRANSPARENT);
      ExtTextOut(FCanvas.Handle, X, Y, 0, @LClipRect, PChar(AText), Length(AText), nil);
      SetBkMode(FCanvas.Handle, OPAQUE);
    end
    else
      ExtTextOut(FCanvas.Handle, X, Y, 0, nil, PChar(AText), Length(AText), nil);
  end;

  procedure SplitToken;
  var
    LFirstPosition: Integer;
    LLast: Integer;
    LTempText: string;
    LTokenEnd: Integer;
  begin
    LLast := LTokenPosition;
    LFirstPosition := LTokenPosition;
    LTokenEnd := LTokenPosition + Length(LToken);
    while (LCount < AList.Count) and (LTokenEnd > TBCEditorWrapPosition(AList[LCount]).Index) do
    begin
      LTempText := Copy(AText, LLast + 1, TBCEditorWrapPosition(AList[LCount]).Index - LLast);
      LLast := TBCEditorWrapPosition(AList[LCount]).Index;
      ClippedTextOut(FMargins.PixelLeft + LFirstPosition * FPaintHelper.CharWidth, FYPos, LTempText);
      LFirstPosition := 0;
      LCount := LCount + 1;
      FYPos := FYPos + FLineHeight;
    end;
    LTempText := Copy(AText, LLast + 1, LTokenEnd - LLast);
    ClippedTextOut(FMargins.PixelLeft + LFirstPosition * FPaintHelper.CharWidth, FYPos, LTempText);
    LTokenStart := LTokenPosition + Length(LToken) - Length(LTempText);
  end;

var
  LLeft: Integer;
  LTempText: string;
begin
  FPaintHelper.BeginDrawing(FCanvas.Handle);
  with FMargins do
    LClipRect := Rect(PixelLeft, PixelTop, PixelRight, PixelBottom);

  if Highlight and Assigned(FHighlighter) and (FLines.Count > 0) then
  begin
    SaveCurrentFont;
     if FLineNumber = 0 then
      FHighlighter.ResetCurrentRange
    else
      FHighlighter.SetCurrentRange(FLines.Objects[FLineNumber - 1]);
    FHighlighter.SetCurrentLine(AText);
    LToken := '';
    LTokenStart := 0;
    LCount := 0;
    LLeft := FMargins.PixelLeft;
    while not FHighlighter.GetEndOfLine do
    begin
      FHighlighter.GetToken(LToken);
      LTokenPosition := FHighlighter.GetTokenPosition;
      LHighlighterAttribute := FHighlighter.GetTokenAttribute;

      FCanvas.Font.Color := FFontColor;
      FCanvas.Brush.Color := FDefaultBackground;

      if Assigned(LHighlighterAttribute) then
      begin
        FCanvas.Font.Style := LHighlighterAttribute.FontStyles;
        if FColors then
        begin
          LColor := LHighlighterAttribute.Foreground;
          if LColor = clNone then
            LColor := FFont.Color;
          FCanvas.Font.Color := LColor;
          LColor := LHighlighterAttribute.Background;
          if LColor = clNone then
            LColor := FDefaultBackground;
          FCanvas.Brush.Color := LColor;
        end;
      end;

      LHandled := False;
      if Assigned(AList) then
        if LCount < AList.Count then
        begin
          if LTokenPosition >= TBCEditorWrapPosition(AList[LCount]).Index then
          begin
            LLeft := FMargins.PixelLeft;
            LCount := LCount + 1;
            LTokenStart := LTokenPosition;
            FYPos := FYPos + FLineHeight;
          end
          else
          if LTokenPosition + Length(LToken) > TBCEditorWrapPosition(AList[LCount]).Index then
          begin
            LHandled := True;
            SplitToken;
          end;
        end;
      if not LHandled then
      begin
        ClippedTextOut(LLeft, FYPos, LToken);
        Inc(LLeft, TextWidth(FCanvas, LToken));
      end;
      FHighlighter.Next;
    end;
    RestoreCurrentFont;
  end
  else
  begin
    LLines := TStringList.Create;
    try
      LOldWrapPosition := 0;
      if Assigned(AList) then
        for LIndex := 0 to AList.Count - 1 do
        begin
          LWrapPosition := TBCEditorWrapPosition(AList[LIndex]).Index;
          if LIndex = 0 then
            LTempText := Copy(AText, 1, LWrapPosition)
          else
            LTempText := Copy(AText, LOldWrapPosition + 1, LWrapPosition - LOldWrapPosition);
          LLines.Add(LTempText);
          LOldWrapPosition := LWrapPosition;
        end;
      if Length(AText) > 0 then
        LLines.Add(Copy(AText, LOldWrapPosition + 1, MaxInt));

      for LIndex := 0 to LLines.Count - 1 do
      begin
        ClippedTextOut(FMargins.PixelLeft, FYPos, LLines[LIndex]);
        if LIndex < LLines.Count - 1 then
          FYPos := FYPos + FLineHeight;
      end;
    finally
      LLines.Free;
    end
  end;
  FPaintHelper.EndDrawing;
end;

procedure TBCEditorPrint.WriteLine(const AText: string);
begin
  if FLineNumbers then
    WriteLineNumber;
  if Wrap and (FCanvas.TextWidth(AText) > FMaxWidth) then
    HandleWrap(AText)
  else
    TextOut(AText, nil);
  FYPos := FYPos + FLineHeight;
end;

procedure TBCEditorPrint.PrintPage(APageNumber: Integer);
var
  i: Integer;
  LEndLine: Integer;
  LRect: TRect;
  LSelectionLength: Integer;
  LSelectionStart: Integer;
begin
  PrintStatus(psNewPage, APageNumber, FAbort);
  if not FAbort then
  begin
    FCanvas.Brush.Color := Color;
    LRect.Left := FMargins.PixelLeft;
    LRect.Right := FMargins.PixelRight;
    LRect.Top := FMargins.PixelTop;
    LRect.Bottom := FMargins.PixelBottom;
    ExtTextOut(FCanvas.Handle, 0, 0, ETO_OPAQUE, LRect, '', 0, nil);
    FMargins.InitPage(FCanvas, APageNumber, FPrinterInfo, FLineNumbers, FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
    FHeader.Print(FCanvas, APageNumber + FPageOffset);
    if FPages.Count > 0 then
    begin
      FYPos := FMargins.PixelTop;
      if APageNumber = FPageCount then
        LEndLine := FLines.Count - 1
      else
        LEndLine := TBCEditorPageLine(FPages[APageNumber]).FirstLine - 1;
      for i := TBCEditorPageLine(FPages[APageNumber - 1]).FirstLine to LEndLine do
      begin
        FLineNumber := i + 1;
        if (not FSelectedOnly or ((i >= FBlockBeginPosition.Line - 1) and (i <= FBlockEndPosition.Line - 1))) then
        begin
          if not FSelectedOnly then
            WriteLine(FLines[i])
          else
          begin
            if (FSelectionMode = smColumn) or (i = FBlockBeginPosition.Line - 1) then
              LSelectionStart := FBlockBeginPosition.Char
            else
              LSelectionStart := 1;
            if (FSelectionMode = smColumn) or (i = FBlockEndPosition.Line - 1) then
              LSelectionLength := FBlockEndPosition.Char - LSelectionStart
            else
              LSelectionLength := MaxInt;
            WriteLine(Copy(FLines[i], LSelectionStart, LSelectionLength));
          end;
          if Assigned(FOnPrintLine) then
            FOnPrintLine(Self, i + 1, APageNumber);
        end;
      end;
    end;
    FFooter.Print(FCanvas, APageNumber + FPageOffset);
  end;
end;

procedure TBCEditorPrint.UpdatePages(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  FPrinterInfo.UpdatePrinter;
  InitPrint;
end;

procedure TBCEditorPrint.PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
begin
  FAbort := False;
  FPrinting := False;
  FCanvas := ACanvas;
  PrintPage(PageNumber);
end;

procedure TBCEditorPrint.Print(const AStartPage: Integer = 1; const AEndPage: Integer = -1);
var
  LEndPage: Integer;
  LIndex: Integer;
  LPage: Integer;
begin
  if FSelectedOnly and not FSelectionAvailable then
    Exit;

  LEndPage := AEndPage;
  FPrinting := True;
  FAbort := False;
  if FDocumentTitle <> '' then
    Printer.Title := FDocumentTitle
  else
    Printer.Title := FTitle;
  Printer.BeginDoc;
  if Printer.Printing then
  begin
    PrintStatus(psBegin, AStartPage, FAbort);
    UpdatePages(Printer.Canvas);

    for LIndex := 1 to Copies do
    begin
      LPage := AStartPage;
      if LEndPage < 0 then
        LEndPage := FPageCount;
      while (LPage <= LEndPage) and (not FAbort) do
      begin
        PrintPage(LPage);
        if ((LPage < LEndPage) or (LIndex < Copies)) and not FAbort then
          Printer.NewPage;
        Inc(LPage);
      end;
    end;
    if not FAbort then
      PrintStatus(psEnd, LEndPage, FAbort);
    Printer.EndDoc;
  end;
  FPrinting := False;
end;

procedure TBCEditorPrint.PrintStatus(const AStatus: TStatus; const APageNumber: Integer; var AAbort: Boolean);
begin
  AAbort := False;
  if Assigned(FOnPrintStatus) then
    FOnPrintStatus(Self, AStatus, APageNumber, AAbort);
  if AAbort and FPrinting then
    Printer.Abort;
end;

function TBCEditorPrint.GetPageCount: Integer;
var
  LCanvas: TCanvas;
  LHandle: HDC;
begin
  Result := 0;
  if FPagesCounted then
    Result := FPageCount
  else
  begin
    LCanvas := TCanvas.Create;
    LHandle := GetDC(0);
    try
      if LHandle <> 0 then
      begin
        LCanvas.Handle := LHandle;
        UpdatePages(LCanvas);
        LCanvas.Handle := 0;
        Result := FPageCount;
        FPagesCounted := True;
      end;
    finally
      ReleaseDC(0, LHandle);
      LCanvas.Free;
    end;
  end;
end;

procedure TBCEditorPrint.SetEditor(const AValue: TCustomBCEditor);
begin
  FEditor := AValue;
  Highlighter := AValue.Highlighter;
  Font := AValue.Font;
  CharWidth := AValue.CharWidth;
  FColumns := toColumns in AValue.Tabs.Options;
  FTabWidth := AValue.Tabs.Width;
  SetLines(AValue.Lines);
  FSelectionAvailable := AValue.SelectionAvailable;
  FBlockBeginPosition := AValue.SelectionBeginPosition;
  FBlockEndPosition := AValue.SelectionEndPosition;
  FSelectionMode := AValue.SelectionMode;
end;

procedure TBCEditorPrint.LoadFromStream(AStream: TStream);
var
  LBufferSize: Integer;
  LLength: Integer;
begin
  FHeader.LoadFromStream(AStream);
  FFooter.LoadFromStream(AStream);
  FMargins.LoadFromStream(AStream);
  with AStream do
  begin
    Read(LLength, SizeOf(LLength));
    LBufferSize := LLength * SizeOf(Char);
    SetLength(FTitle, LBufferSize div SizeOf(FTitle[1]));
    Read(FTitle[1], LBufferSize);
    Read(LLength, SizeOf(LLength));
    LBufferSize := LLength * SizeOf(Char);
    SetLength(FDocumentTitle, LBufferSize div SizeOf(FDocumentTitle[1]));
    Read(FDocumentTitle[1], LBufferSize);
    Read(FWrap, SizeOf(FWrap));
    Read(FHighlight, SizeOf(FHighlight));
    Read(FColors, SizeOf(FColors));
    Read(FLineNumbers, SizeOf(FLineNumbers));
    Read(FLineOffset, SizeOf(FLineOffset));
    Read(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TBCEditorPrint.SaveToStream(AStream: TStream);
var
  LLength: Integer;
begin
  FHeader.SaveToStream(AStream);
  FFooter.SaveToStream(AStream);
  FMargins.SaveToStream(AStream);
  with AStream do
  begin
    LLength := Length(FTitle);
    Write(LLength, SizeOf(LLength));
    Write(PChar(FTitle)^, LLength * SizeOf(Char));
    LLength := Length(FDocumentTitle);
    Write(LLength, SizeOf(LLength));
    Write(PChar(FDocumentTitle)^, LLength * SizeOf(Char));
    Write(FWrap, SizeOf(FWrap));
    Write(FHighlight, SizeOf(FHighlight));
    Write(FColors, SizeOf(FColors));
    Write(FLineNumbers, SizeOf(FLineNumbers));
    Write(FLineOffset, SizeOf(FLineOffset));
    Write(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TBCEditorPrint.SetFooter(const AValue: TFooter);
begin
  FFooter.Assign(AValue);
end;

procedure TBCEditorPrint.SetHeader(const AValue: THeader);
begin
  FHeader.Assign(AValue);
end;

procedure TBCEditorPrint.SetMargins(const AValue: TMargins);
begin
  FMargins.Assign(AValue);
end;

end.
