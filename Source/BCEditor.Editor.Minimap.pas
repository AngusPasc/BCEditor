unit BCEditor.Editor.Minimap;

interface {********************************************************************}

uses
  Classes, UITypes,
  Graphics,
  BCEditor.Types, BCEditor.Consts;

type
  TBCEditorMinimap = class(TPersistent)
  type
    TOptions = set of TBCEditorMinimapOption;

    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FBookmark: TColor;
      FOnChange: TNotifyEvent;
      FVisibleRows: TColor;
      procedure DoChange;
      procedure SetBackground(const AValue: TColor);
      procedure SetBookmark(const AValue: TColor);
      procedure SetVisibleRows(const AValue: TColor);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write SetBackground default clNone;
      property Bookmark: TColor read FBookmark write SetBookmark default clMinimapBookmark;
      property VisibleRows: TColor read FVisibleRows write SetVisibleRows default clMinimapVisibleLines;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TIndicator = class(TPersistent)
    type
      TOptions = set of TBCEditorMinimapIndicatorOption;
    strict private
      FAlphaBlending: Byte;
      FOnChange: TNotifyEvent;
      FOptions: TBCEditorMinimap.TIndicator.TOptions;
      procedure DoChange;
      procedure SetAlphaBlending(const AValue: Byte);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
      procedure SetOption(const AOption: TBCEditorMinimapIndicatorOption; const AEnabled: Boolean);
    published
      property AlphaBlending: Byte read FAlphaBlending write SetAlphaBlending default 96;
      property Options: TBCEditorMinimap.TIndicator.TOptions read FOptions write FOptions default [];
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  strict private
    FAlign: TBCEditorMinimapAlign;
    FCharHeight: Integer;
    FClicked: Boolean;
    FColors: TBCEditorMinimap.TColors;
    FCursor: TCursor;
    FDragging: Boolean;
    FFont: TFont;
    FIndicator: TBCEditorMinimap.TIndicator;
    FOnChange: TNotifyEvent;
    FOptions: TOptions;
    FTopRow: Integer;
    FVisible: Boolean;
    FVisibleRows: Integer;
    FWidth: Integer;
    procedure DoChange;
    procedure SetAlign(const AValue: TBCEditorMinimapAlign);
    procedure SetColors(const AValue: TColors);
    procedure SetFont(AValue: TFont);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    function GetWidth: Integer;
    procedure SetOption(const AOption: TBCEditorMinimapOption; const AEnabled: Boolean);
    property CharHeight: Integer read FCharHeight write FCharHeight;
    property Clicked: Boolean read FClicked write FClicked;
    property Dragging: Boolean read FDragging write FDragging;
    property TopRow: Integer read FTopRow write FTopRow default 1;
    property VisibleRows: Integer read FVisibleRows write FVisibleRows;
  published
    property Align: TBCEditorMinimapAlign read FAlign write SetAlign default maRight;
    property Colors: TBCEditorMinimap.TColors read FColors write SetColors;
    property Cursor: TCursor read FCursor write FCursor default crArrow;
    property Font: TFont read FFont write SetFont;
    property Indicator: TBCEditorMinimap.TIndicator read FIndicator write FIndicator;
    property Options: TOptions read FOptions write FOptions default [];
    property Visible: Boolean read FVisible write SetVisible default False;
    property Width: Integer read FWidth write SetWidth default 140;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

uses
  Math;

{ TBCEditorMiniMap.TColors ****************************************************}

constructor TBCEditorMinimap.TColors.Create;
begin
  inherited;

  FBackground := clNone;
  FBookmark := clMinimapBookmark;
  FVisibleRows := clMinimapVisibleLines;
end;

procedure TBCEditorMinimap.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorMinimap.TColors then
  with ASource as TBCEditorMinimap.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FBookmark := FBookmark;
    Self.FVisibleRows := FVisibleRows;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorMinimap.TColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorMinimap.TColors.SetBackground(const AValue: TColor);
begin
  if FBackground <> AValue then
  begin
    FBackground := AValue;
    DoChange;
  end;
end;

procedure TBCEditorMinimap.TColors.SetBookmark(const AValue: TColor);
begin
  if FBookmark <> AValue then
  begin
    FBookmark := AValue;
    DoChange;
  end;
end;

procedure TBCEditorMinimap.TColors.SetVisibleRows(const AValue: TColor);
begin
  if FVisibleRows <> AValue then
  begin
    FVisibleRows := AValue;
    DoChange;
  end;
end;

{ TBCEditorMiniMap.TIndicator *************************************************}

constructor TBCEditorMinimap.TIndicator.Create;
begin
  inherited;

  FAlphaBlending := 96;
  FOptions := [];
end;

procedure TBCEditorMinimap.TIndicator.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorMinimap.TIndicator) then
  with ASource as TBCEditorMinimap.TIndicator do
  begin
    Self.FAlphaBlending := FAlphaBlending;
    Self.FOptions := FOptions;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorMinimap.TIndicator.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorMinimap.TIndicator.SetAlphaBlending(const AValue: Byte);
begin
  if FAlphaBlending <> AValue then
  begin
    FAlphaBlending := AValue;
    DoChange;
  end;
end;

procedure TBCEditorMinimap.TIndicator.SetOption(const AOption: TBCEditorMinimapIndicatorOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

{ TBCEditorMiniMap ************************************************************}

constructor TBCEditorMinimap.Create;
begin
  inherited;

  FAlign := maRight;

  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 1;
  FFont.Style := [];

  FVisible := False;
  FWidth := 140;
  FDragging := False;
  FOptions := [];
  FCursor := crArrow;

  FClicked := False;

  FTopRow := 1;

  FIndicator := TBCEditorMinimap.TIndicator.Create;
  FColors := TBCEditorMinimap.TColors.Create;
end;

destructor TBCEditorMinimap.Destroy;
begin
  FFont.Free;
  FIndicator.Free;
  FColors.Free;

  inherited Destroy;
end;

procedure TBCEditorMinimap.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorMinimap then
  with ASource as TBCEditorMinimap do
  begin
    Self.FAlign := FAlign;
    Self.FColors.Assign(FColors);
    Self.FFont.Assign(FFont);
    Self.FOptions := FOptions;
    Self.FVisible := FVisible;
    Self.FWidth := FWidth;
    Self.FCursor := FCursor;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorMinimap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBCEditorMinimap.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorMinimap.SetAlign(const AValue: TBCEditorMinimapAlign);
begin
  if FAlign <> AValue then
  begin
    FAlign := AValue;
    DoChange;
  end;
end;

procedure TBCEditorMinimap.SetColors(const AValue: TBCEditorMinimap.TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorMinimap.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TBCEditorMinimap.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FFont.OnChange := AValue;
  FColors.OnChange := AValue;
  FIndicator.OnChange := AValue;
end;

procedure TBCEditorMinimap.SetOption(const AOption: TBCEditorMinimapOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorMinimap.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

procedure TBCEditorMinimap.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange;
  end;
end;

end.
