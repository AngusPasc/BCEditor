unit BCEditor.Editor.WordWrap;

interface {********************************************************************}

uses
  Classes,
  Graphics,
  BCEditor.Editor.Glyph, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorWordWrap = class(TPersistent)
  type

    TColors = class(TPersistent)
    strict private
      FArrow: TColor;
      FLines: TColor;
      FOnChange: TNotifyEvent;
      procedure DoChange;
      procedure SetArrow(const AValue: TColor);
      procedure SetLines(const AValue: TColor);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Arrow: TColor read FArrow write SetArrow default clWordWrapIndicatorArrow;
      property Lines: TColor read FLines write SetLines default clWordWrapIndicatorLines;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  strict private
    FBitmap: Graphics.TBitmap;
    FColors: TBCEditorWordWrap.TColors;
    FEnabled: Boolean;
    FIndicator: TBCEditorGlyph;
    FOnChange: TNotifyEvent;
    FWidth: TBCEditorWordWrapWidth;
    procedure CreateInternalBitmap;
    procedure DoChange;
    procedure OnColorsChange(ASender: TObject);
    procedure SetColors(const AValue: TBCEditorWordWrap.TColors);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetIndicator(const AValue: TBCEditorGlyph);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetWidth(const AValue: TBCEditorWordWrapWidth);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TBCEditorWordWrap.TColors read FColors write SetColors;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Indicator: TBCEditorGlyph read FIndicator write SetIndicator;
    property Width: TBCEditorWordWrapWidth read FWidth write SetWidth default wwwPage;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

{ TBCEditorWordWrap.TColors ***************************************************}

constructor TBCEditorWordWrap.TColors.Create;
begin
  inherited;

  FArrow := clWordWrapIndicatorArrow;
  FLines := clWordWrapIndicatorLines;
end;

procedure TBCEditorWordWrap.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorWordWrap.TColors then
  with ASource as TBCEditorWordWrap.TColors do
  begin
    Self.FArrow := FArrow;
    Self.FLines := FLines;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorWordWrap.TColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorWordWrap.TColors.SetArrow(const AValue: TColor);
begin
  if FArrow <> AValue then
  begin
    FArrow := AValue;
    DoChange;
  end;
end;

procedure TBCEditorWordWrap.TColors.SetLines(const AValue: TColor);
begin
  if FLines <> AValue then
  begin
    FLines := AValue;
    DoChange;
  end;
end;

{ TBCEditorWordWrap ***********************************************************}

constructor TBCEditorWordWrap.Create;
begin
  inherited;

  FColors := TBCEditorWordWrap.TColors.Create;

  FEnabled := False;
  FIndicator := TBCEditorGlyph.Create(HInstance, '', clFuchsia);
  CreateInternalBitmap;
  FWidth := wwwPage;
end;

destructor TBCEditorWordWrap.Destroy;
begin
  FBitmap.Free;
  FIndicator.Free;
  FColors.Free;

  inherited;
end;

procedure TBCEditorWordWrap.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorWordWrap) then
  with ASource as TBCEditorWordWrap do
  begin
    Self.FColors.Assign(FColors);
    Self.FEnabled := FEnabled;
    Self.FWidth := FWidth;
    Self.FIndicator.Assign(FIndicator);
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorWordWrap.CreateInternalBitmap;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
  FBitmap := Graphics.TBitmap.Create;
  with FBitmap do
  begin
    Canvas.Brush.Color := clFuchsia;
    Width := 15;
    Height := 14;
    Canvas.Pen.Color := FColors.Arrow;
    Canvas.MoveTo(6, 4);
    Canvas.LineTo(13, 4);
    Canvas.MoveTo(13, 5);
    Canvas.LineTo(13, 9);
    Canvas.MoveTo(12, 9);
    Canvas.LineTo(7, 9);
    Canvas.MoveTo(10, 7);
    Canvas.LineTo(10, 12);
    Canvas.MoveTo(9, 8);
    Canvas.LineTo(9, 11);
    Canvas.Pen.Color := FColors.Lines;
    Canvas.MoveTo(2, 6);
    Canvas.LineTo(7, 6);
    Canvas.MoveTo(2, 8);
    Canvas.LineTo(5, 8);
    Canvas.MoveTo(2, 10);
    Canvas.LineTo(5, 10);
    Canvas.MoveTo(2, 12);
    Canvas.LineTo(7, 12);
  end;
  FIndicator.MaskColor := clFuchsia;
  FIndicator.Bitmap.Handle := FBitmap.Handle;
end;

procedure TBCEditorWordWrap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorWordWrap.OnColorsChange(ASender: TObject);
begin
  CreateInternalBitmap;
end;

procedure TBCEditorWordWrap.SetColors(const AValue: TBCEditorWordWrap.TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorWordWrap.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    DoChange;
  end;
end;

procedure TBCEditorWordWrap.SetIndicator(const AValue: TBCEditorGlyph);
begin
  FIndicator.Assign(AValue);
end;

procedure TBCEditorWordWrap.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FIndicator.OnChange := AValue;
  FColors.OnChange := OnColorsChange;
end;

procedure TBCEditorWordWrap.SetWidth(const AValue: TBCEditorWordWrapWidth);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    DoChange;
  end;
end;

end.
