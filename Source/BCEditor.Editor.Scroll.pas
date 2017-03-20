unit BCEditor.Editor.Scroll;

interface

uses
  Classes, UITypes,
  Forms,
  BCEditor.Editor.Glyph, BCEditor.Types;

type
  TBCEditorScroll = class(TPersistent)
  type
    TEvent = procedure(ASender: TObject; AScrollBar: TScrollBarKind) of object;
    TOptions = set of TBCEditorScrollOption;

    THint = class(TPersistent)
    strict private
      FFormat: TBCEditorScrollHintFormat;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Format: TBCEditorScrollHintFormat read FFormat write FFormat default shfTopLineOnly;
    end;

  strict private const
    DefaultOptions = [soPastEndOfLine, soShowVerticalScrollHint, soWheelClickMove];
  strict private
    FBars: System.UITypes.TScrollStyle;
    FHint: TBCEditorScroll.THint;
    FIndicator: TBCEditorGlyph;
    FMaxWidth: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TOptions;
    procedure DoChange;
    procedure SetBars(const AValue: System.UITypes.TScrollStyle);
    procedure SetHint(const AValue: TBCEditorScroll.THint);
    procedure SetIndicator(const AValue: TBCEditorGlyph);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetOptions(const AValue: TOptions);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorScrollOption; const AEnabled: Boolean);
  published
    property Bars: System.UITypes.TScrollStyle read FBars write SetBars default System.UITypes.TScrollStyle.ssBoth;
    property Hint: TBCEditorScroll.THint read FHint write SetHint;
    property Indicator: TBCEditorGlyph read FIndicator write SetIndicator;
    property Options: TOptions read FOptions write SetOptions default DefaultOptions;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

uses
  Graphics,
  BCEditor.Utils, BCEditor.Consts;

{ TBCEditorScroll.THint *******************************************************}

constructor TBCEditorScroll.THint.Create;
begin
  inherited;

  FFormat := shfTopLineOnly;
end;

procedure TBCEditorScroll.THint.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorScroll.THint then
  with ASource as TBCEditorScroll.THint do
    Self.FFormat := FFormat
  else
    inherited Assign(ASource);
end;

{ TBCEditorScroll *************************************************************}

constructor TBCEditorScroll.Create;
begin
  inherited;

  FOptions := DefaultOptions;
  FMaxWidth := 1024;
  FBars := System.UITypes.TScrollStyle.ssBoth;
  FHint := TBCEditorScroll.THint.Create;
  FIndicator := TBCEditorGlyph.Create(HInstance, BCEDITOR_MOUSE_MOVE_SCROLL, clFuchsia);
end;

destructor TBCEditorScroll.Destroy;
begin
  FHint.Free;
  FIndicator.Free;

  inherited;
end;

procedure TBCEditorScroll.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorScroll then
  with ASource as TBCEditorScroll do
  begin
    Self.FBars := FBars;
    Self.FHint.Assign(FHint);
    Self.FIndicator.Assign(FIndicator);
    Self.FOptions := FOptions;
    Self.FMaxWidth := FMaxWidth;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorScroll.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorScroll.SetBars(const AValue: System.UITypes.TScrollStyle);
begin
  if FBars <> AValue then
  begin
    FBars := AValue;
    DoChange;
  end;
end;

procedure TBCEditorScroll.SetHint(const AValue: THint);
begin
  FHint.Assign(AValue);
end;

procedure TBCEditorScroll.SetIndicator(const AValue: TBCEditorGlyph);
begin
  FIndicator.Assign(AValue);
end;

procedure TBCEditorScroll.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

procedure TBCEditorScroll.SetOption(const AOption: TBCEditorScrollOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorScroll.SetOptions(const AValue: TOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

end.
