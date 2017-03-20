unit BCEditor.Editor.MatchingPair;

interface {********************************************************************}

uses
  Classes,
  Graphics,
  BCEditor.Types, BCEditor.Consts;

type
  TBCEditorMatchingPair = class(TPersistent)
  type
    TOptions = set of TBCEditorMatchingPairOption;

    TColors = class(TPersistent)
    strict private
      FMatched: TColor;
      FUnderline: TColor;
      FUnmatched: TColor;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Matched: TColor read FMatched write FMatched default clAqua;
      property Underline: TColor read FUnderline write FUnderline default clMatchingPairUnderline;
      property Unmatched: TColor read FUnmatched write FUnmatched default clYellow;
    end;

  strict private const
    DefaultOptions = [mpoUseMatchedColor];
  strict private
    FColors: TColors;
    FEnabled: Boolean;
    FOptions: TOptions;
    procedure SetColors(const AValue: TColors);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorMatchingPairOption; const AEnabled: Boolean);
  published
    property Colors: TColors read FColors write SetColors;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Options: TOptions read FOptions write FOptions default DefaultOptions;
  end;

implementation {***************************************************************}

{ TBCEditorMatchingPair *******************************************************}

constructor TBCEditorMatchingPair.TColors.Create;
begin
  inherited;

  FMatched := clAqua;
  FUnderline := clMatchingPairUnderline;
  FUnmatched := clYellow;
end;

procedure TBCEditorMatchingPair.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorMatchingPair.TColors then
  with ASource as TBCEditorMatchingPair.TColors do
  begin
    Self.FMatched := FMatched;
    Self.FUnmatched := FUnmatched;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorMatchingPair *******************************************************}

constructor TBCEditorMatchingPair.Create;
begin
  inherited;

  FColors := TBCEditorMatchingPair.TColors.Create;
  FEnabled := True;
  FOptions := DefaultOptions;
end;

destructor TBCEditorMatchingPair.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorMatchingPair.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorMatchingPair) then
  with ASource as TBCEditorMatchingPair do
  begin
    Self.FEnabled := FEnabled;
    Self.FColors.Assign(FColors);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorMatchingPair.SetColors(const AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorMatchingPair.SetOption(const AOption: TBCEditorMatchingPairOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

end.
