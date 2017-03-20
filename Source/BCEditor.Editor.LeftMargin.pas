unit BCEditor.Editor.LeftMargin;

interface {********************************************************************}

uses
  Classes, UITypes,
  Graphics, ImgList,
  BCEditor.Editor.Marks, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorLeftMargin = class(TPersistent)
  type
    TGetTextEvent = procedure(ASender: TObject; ALine: Integer; var AText: string) of object;
    TPaintEvent = procedure(ASender: TObject; ALine: Integer; X, Y: Integer) of object;
    TClickEvent = procedure(ASender: TObject; AButton: TMouseButton; X, Y, ALine: Integer; AMark: TBCEditorMark) of object;

    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FBookmarkBackground: TColor;
      FBookmarkPanelBackground: TColor;
      FBorder: TColor;
      FLineStateModified: TColor;
      FLineStateNormal: TColor;
      FMarkDefaultBackground: TColor;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write FBackground default clLeftMarginBackground;
      property BookmarkBackground: TColor read FBookmarkBackground write FBookmarkBackground default clNone;
      property BookmarkPanelBackground: TColor read FBookmarkPanelBackground write FBookmarkPanelBackground default clLeftMarginBackground;
      property Border: TColor read FBorder write FBorder default clLeftMarginBackground;
      property LineStateModified: TColor read FLineStateModified write FLineStateModified default clYellow;
      property LineStateNormal: TColor read FLineStateNormal write FLineStateNormal default clLime;
      property MarkDefaultBackground: TColor read FMarkDefaultBackground write FMarkDefaultBackground default clNone;
    end;

    TBookMarks = class(TPersistent)
    strict private
      FImages: TCustomImageList;
      FLeftMargin: Integer;
      FOnChange: TNotifyEvent;
      FOwner: TComponent;
      FShortCuts: Boolean;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetImages(const AValue: TCustomImageList);
      procedure SetVisible(AValue: Boolean);
    public
      constructor Create(AOwner: TComponent);
      procedure Assign(ASource: TPersistent); override;
    published
      property Images: TCustomImageList read FImages write SetImages;
      property LeftMargin: Integer read FLeftMargin write FLeftMargin default 2;
      property ShortCuts: Boolean read FShortCuts write FShortCuts default True;
      property Visible: Boolean read FVisible write SetVisible default True;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TMarks = class(TPersistent)
    type
      TPanel = class(TPersistent)
      type
        TOptions = set of TBCEditorLeftMarginBookMarkPanelOption;

      strict private
        FOnChange: TNotifyEvent;
        FOptions: TOptions;
        FVisible: Boolean;
        FWidth: Integer;
        procedure DoChange;
        procedure SetVisible(const AValue: Boolean);
        procedure SetWidth(AValue: Integer);
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Options: TOptions read FOptions write FOptions default [bpoToggleBookmarkByClick];
        property Visible: Boolean read FVisible write SetVisible default True;
        property Width: Integer read FWidth write SetWidth default 20;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
      end;

    strict private
      FDefaultImageIndex: Integer;
      FImages: TCustomImageList;
      FLeftMargin: Integer;
      FOnChange: TNotifyEvent;
      FOverlappingOffset: Integer;
      FOwner: TComponent;
      FShortCuts: Boolean;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetImages(const AValue: TCustomImageList);
      procedure SetVisible(AValue: Boolean);
    public
      constructor Create(AOwner: TComponent);
      procedure Assign(ASource: TPersistent); override;
    published
      property DefaultImageIndex: Integer read FDefaultImageIndex write FDefaultImageIndex default -1;
      property Images: TCustomImageList read FImages write SetImages;
      property LeftMargin: Integer read FLeftMargin write FLeftMargin default 2;
      property OverlappingOffset: Integer read FOverlappingOffset write FOverlappingOffset default 4;
      property Visible: Boolean read FVisible write SetVisible default True;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TLineNumbers = class(TPersistent)
    type
      TOptions = set of TBCEditorLeftMarginLineNumberOption;
    strict private
      FAutosizeDigitCount: Integer;
      FDigitCount: Integer;
      FOnChange: TNotifyEvent;
      FOptions: TOptions;
      FStartFrom: Integer;
      FVisible: Boolean;
      procedure DoChange;
      procedure SetDigitCount(AValue: Integer);
      procedure SetOptions(const AValue: TOptions);
      procedure SetStartFrom(const AValue: Integer);
      procedure SetVisible(const AValue: Boolean);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
      procedure SetOption(const AOption: TBCEditorLeftMarginLineNumberOption; const AEnabled: Boolean);
      property AutosizeDigitCount: Integer read FAutosizeDigitCount write FAutosizeDigitCount;
    published
      property DigitCount: Integer read FDigitCount write SetDigitCount default 4;
      property Options: TOptions read FOptions write SetOptions default [lnoIntens];
      property StartFrom: Integer read FStartFrom write SetStartFrom default 1;
      property Visible: Boolean read FVisible write SetVisible default True;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TLineState = class(TPersistent)
    strict private
      FEnabled: Boolean;
      FOnChange: TNotifyEvent;
      FWidth: Integer;
      procedure DoChange;
      procedure SetEnabled(const AValue: Boolean);
      procedure SetOnChange(AValue: TNotifyEvent);
      procedure SetWidth(const AValue: Integer);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Enabled: Boolean read FEnabled write SetEnabled default True;
      property Width: Integer read FWidth write SetWidth default 2;
      property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    end;

    TBorder = class(TPersistent)
    strict private
      FOnChange: TNotifyEvent;
      FStyle: TBCEditorLeftMarginBorderStyle;
      procedure DoChange;
      procedure SetStyle(const AValue: TBCEditorLeftMarginBorderStyle);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Style: TBCEditorLeftMarginBorderStyle read FStyle write SetStyle default mbsNone;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  strict private
    FAutosize: Boolean;
    FBookMarks: TBCEditorLeftMargin.TBookmarks;
    FBorder: TBCEditorLeftMargin.TBorder;
    FColors: TBCEditorLeftMargin.TColors;
    FCursor: TCursor;
    FFont: TFont;
    FLineNumbers: TBCEditorLeftMargin.TLineNumbers;
    FLineState: TBCEditorLeftMargin.TLineState;
    FMarks: TBCEditorLeftMargin.TMarks;
    FMarksPanel: TMarks.TPanel;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure SetAutosize(const AValue: Boolean);
    procedure SetBookMarks(const AValue: TBCEditorLeftMargin.TBookmarks);
    procedure SetColors(const AValue: TBCEditorLeftMargin.TColors);
    procedure SetFont(AValue: TFont);
    procedure SetMarks(const AValue: TBCEditorLeftMargin.TMarks);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure AutosizeDigitCount(ALinesCount: Integer);
    procedure ChangeScale(M, D: Integer);
    function FormatLineNumber(ALine: Integer): string;
    function GetWidth: Integer;
    function RealLeftMarginWidth(ACharWidth: Integer): Integer;
  published
    property Autosize: Boolean read FAutosize write SetAutosize default True;
    property Bookmarks: TBCEditorLeftMargin.TBookmarks read FBookMarks write SetBookMarks;
    property Border: TBCEditorLeftMargin.TBorder read FBorder write FBorder;
    property Colors: TBCEditorLeftMargin.TColors read FColors write SetColors;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property Font: TFont read FFont write SetFont;
    property LineNumbers: TBCEditorLeftMargin.TLineNumbers read FLineNumbers write FLineNumbers;
    property LineState: TBCEditorLeftMargin.TLineState read FLineState write FLineState;
    property Marks: TBCEditorLeftMargin.TMarks read FMarks write SetMarks;
    property MarksPanel: TMarks.TPanel read FMarksPanel write FMarksPanel;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default 55;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

uses
  SysUtils, Math,
  BCEditor.Utils;

{ TBCEditorLeftMargin.TColors *************************************************}

constructor TBCEditorLeftMargin.TColors.Create;
begin
  inherited;

  FBackground := clLeftMarginBackground;
  FBookmarkBackground := clNone;
  FBookmarkPanelBackground := clLeftMarginBackground;
  FBorder := clLeftMarginBackground;
  FLineStateModified := clYellow;
  FLineStateNormal := clLime;
  FMarkDefaultBackground := clNone;
end;

procedure TBCEditorLeftMargin.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorLeftMargin.TColors then
  with ASource as TBCEditorLeftMargin.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBookmarkPanelBackground := FBookmarkPanelBackground;
    Self.FBorder := FBorder;
    Self.FLineStateModified := FLineStateModified;
    Self.FLineStateNormal := FLineStateNormal;
    Self.FMarkDefaultBackground := FMarkDefaultBackground;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorLeftMargin.TBookmarks **********************************************}

constructor TBCEditorLeftMargin.TBookmarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FLeftMargin := 2;
  FShortCuts := True;
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TBookmarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TBookmarks) then
  with ASource as TBCEditorLeftMargin.TBookmarks do
  begin
    Self.FImages := FImages;
    Self.FLeftMargin := FLeftMargin;
    Self.FShortCuts := FShortCuts;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TBookmarks.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TBookmarks.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
    DoChange;
  end;
end;

procedure TBCEditorLeftMargin.TBookmarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

{ TBCEditorLeftMargin.TMarks.TPanel *******************************************}

constructor TBCEditorLeftMargin.TMarks.TPanel.Create;
begin
  inherited;

  FWidth := 20;
  FOptions := [bpoToggleBookmarkByClick];
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TMarks.TPanel.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TMarks.TPanel) then
  with ASource as TBCEditorLeftMargin.TMarks.TPanel do
  begin
    Self.FVisible := FVisible;
    Self.FWidth := FWidth;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TMarks.TPanel.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TMarks.TPanel.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TMarks.TPanel.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange
  end;
end;

{ TBCEditorLeftMargin.TMarks **************************************************}

constructor TBCEditorLeftMargin.TMarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FDefaultImageIndex := -1;
  FLeftMargin := 2;
  FOverlappingOffset := 4;
  FShortCuts := True;
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TMarks.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TMarks) then
  with ASource as TBCEditorLeftMargin.TMarks do
  begin
    Self.FDefaultImageIndex := FDefaultImageIndex;
    Self.FImages := FImages;
    Self.FLeftMargin := FLeftMargin;
    Self.FOverlappingOffset := FOverlappingOffset;
    Self.FShortCuts := FShortCuts;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TMarks.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TMarks.SetImages(const AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
    DoChange;
  end;
end;

procedure TBCEditorLeftMargin.TMarks.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

{ TBCEditorLeftMargin.TLineNumbers ********************************************}

constructor TBCEditorLeftMargin.TLineNumbers.Create;
begin
  inherited;

  FAutosizeDigitCount := 4;
  FDigitCount := 4;
  FOptions := [lnoIntens];
  FStartFrom := 1;
  FVisible := True;
end;

procedure TBCEditorLeftMargin.TLineNumbers.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TLineNumbers) then
  with ASource as TBCEditorLeftMargin.TLineNumbers do
  begin
    Self.FAutosizeDigitCount := FAutosizeDigitCount;
    Self.FDigitCount := FDigitCount;
    Self.FOptions := FOptions;
    Self.FStartFrom := FStartFrom;
    Self.FVisible := FVisible;

    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TLineNumbers.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetDigitCount(AValue: Integer);
begin
  AValue := MinMax(AValue, 1, 12);
  if FDigitCount <> AValue then
  begin
    FDigitCount := AValue;
    FAutosizeDigitCount := FDigitCount;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetOption(const AOption: TBCEditorLeftMarginLineNumberOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetOptions(const AValue: TOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetStartFrom(const AValue: Integer);
begin
  if FStartFrom <> AValue then
  begin
    FStartFrom := AValue;
    if FStartFrom < 0 then
      FStartFrom := 0;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TLineNumbers.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

{ TBCEditorLeftMargin.TLineState **********************************************}

constructor TBCEditorLeftMargin.TLineState.Create;
begin
  inherited;

  FEnabled := True;
  FWidth := 2;
end;

procedure TBCEditorLeftMargin.TLineState.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TLineState) then
  with ASource as TBCEditorLeftMargin.TLineState do
  begin
    Self.FEnabled := FEnabled;
    Self.FWidth := FWidth;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TLineState.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TLineState.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.TLineState.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

procedure TBCEditorLeftMargin.TLineState.SetWidth(const AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange
  end;
end;

{ TBCEditorLeftMargin.TBorder *************************************************}

constructor TBCEditorLeftMargin.TBorder.Create;
begin
  inherited;

  FStyle := mbsNone;
end;

procedure TBCEditorLeftMargin.TBorder.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorLeftMargin.TBorder) then
  with ASource as TBCEditorLeftMargin.TBorder do
  begin
    Self.FStyle := FStyle;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.TBorder.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMargin.TBorder.SetStyle(const AValue: TBCEditorLeftMarginBorderStyle);
begin
  FStyle := AValue;
  DoChange
end;

{ TBCEditorLeftMargin *********************************************************}

constructor TBCEditorLeftMargin.Create(AOwner: TComponent);
begin
  inherited Create;

  FAutosize := True;
  FColors := TColors.Create;
  FCursor := crDefault;
  FBorder := TBorder.Create;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FFont.Style := [];
  FFont.Color := clLeftMarginFontForeground;
  FWidth := 55;
  FVisible := True;

  FBookmarks := TBookmarks.Create(AOwner);
  FMarks := TMarks.Create(AOwner);
  FLineState := TLineState.Create;
  FLineNumbers := TLineNumbers.Create;
  FMarksPanel := TMarks.TPanel.Create;
end;

destructor TBCEditorLeftMargin.Destroy;
begin
  FBookmarks.Free;
  FMarks.Free;
  FBorder.Free;
  FColors.Free;
  FFont.Free;
  FLineState.Free;
  FLineNumbers.Free;
  FMarksPanel.Free;

  inherited Destroy;
end;

procedure TBCEditorLeftMargin.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorLeftMargin then
  with ASource as TBCEditorLeftMargin do
  begin
    Self.FAutosize := FAutosize;
    Self.FBookmarks.Assign(FBookmarks);
    Self.FMarks.Assign(FMarks);
    Self.FColors.Assign(FColors);
    Self.FBorder.Assign(FBorder);
    Self.FCursor := FCursor;
    Self.FFont.Assign(FFont);
    Self.FLineNumbers.Assign(FLineNumbers);
    Self.FMarksPanel.Assign(FMarksPanel);
    Self.FWidth := FWidth;
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorLeftMargin.AutosizeDigitCount(ALinesCount: Integer);
var
  NumberOfDigits: Integer;
begin
  if FLineNumbers.Visible and FAutosize then
  begin
    if FLineNumbers.StartFrom = 0 then
      Dec(ALinesCount)
    else
    if FLineNumbers.StartFrom > 1 then
      Inc(ALinesCount, FLineNumbers.StartFrom - 1);

    NumberOfDigits := Max(Length(ALinesCount.ToString), FLineNumbers.DigitCount);
    if FLineNumbers.AutosizeDigitCount <> NumberOfDigits then
    begin
      FLineNumbers.AutosizeDigitCount := NumberOfDigits;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  end
  else
    FLineNumbers.AutosizeDigitCount := FLineNumbers.DigitCount;
end;

procedure TBCEditorLeftMargin.ChangeScale(M, D: Integer);
begin
  FWidth := FWidth * M div D;
end;

procedure TBCEditorLeftMargin.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBCEditorLeftMargin.FormatLineNumber(ALine: Integer): string;
var
  LIndex: Integer;
begin
  if FLineNumbers.StartFrom = 0 then
    Dec(ALine)
  else
  if FLineNumbers.StartFrom > 1 then
    Inc(ALine, FLineNumbers.StartFrom - 1);

  Result := Format('%*d', [FLineNumbers.AutosizeDigitCount, ALine]);

  if lnoLeadingZeros in FLineNumbers.Options then
  for LIndex := 1 to FLineNumbers.AutosizeDigitCount - 1 do
  begin
    if Result[LIndex] <> ' ' then
      Break;
    Result[LIndex] := '0';
  end;
end;

function TBCEditorLeftMargin.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

function TBCEditorLeftMargin.RealLeftMarginWidth(ACharWidth: Integer): Integer;
var
  PanelWidth: Integer;
begin
  PanelWidth := FMarksPanel.Width;
  if not FMarksPanel.Visible and not FBookmarks.Visible and not FMarks.Visible then
    PanelWidth := 0;

  if not FVisible then
    Result := 0
  else
  if FLineNumbers.Visible then
    Result := PanelWidth + FLineState.Width + FLineNumbers.AutosizeDigitCount * ACharWidth + 5
  else
    Result := FWidth;
end;

procedure TBCEditorLeftMargin.SetAutosize(const AValue: Boolean);
begin
  if FAutosize <> AValue then
  begin
    FAutosize := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.SetBookMarks(const AValue: TBookmarks);
begin
  FBookmarks.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetMarks(const AValue: TMarks);
begin
  FMarks.Assign(AValue);
end;

procedure TBCEditorLeftMargin.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;

  FBookmarks.OnChange := AValue;
  FBorder.OnChange := AValue;
  FFont.OnChange := AValue;
  FLineState.OnChange := AValue;
  FLineNumbers.OnChange := AValue;
  FMarksPanel.OnChange := AValue;
end;

procedure TBCEditorLeftMargin.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange
  end;
end;

end.
