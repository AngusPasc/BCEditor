unit BCEditor.Editor.TokenInfo;

interface {********************************************************************}

uses
  Classes,
  Controls, Graphics,
  BCEditor.Types;

type
  TBCEditorTokenInfo = class(TPersistent)
  type
    TOptions = set of TBCEditorTokenInfoOption;

    TTitle = class(TPersistent)
    type
      TColors = class(TPersistent)
      strict private
        FBackground: TColor;
        FReference: TColor;
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Background: TColor read FBackground write FBackground default clWindow;
        property Reference: TColor read FReference write FReference default clBlue;
      end;

    strict private
      FColors: TColors;
      FFont: TFont;
      FVisible: Boolean;
      procedure SetFont(const AValue: TFont);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
    published
      property Colors: TColors read FColors write FColors;
      property Font: TFont read FFont write SetFont;
      property Visible: Boolean read FVisible write FVisible default False;
    end;

    TColors = class(TPersistent)
    strict private
      FBackground: TColor;
      FReference: TColor;
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Background: TColor read FBackground write FBackground default clWindow;
      property Reference: TColor read FReference write FReference default clBlue;
    end;

  strict private const
    DefaultOptions = [tioAutoSize];
  strict private
    FColors: TColors;
    FDelayInterval: Cardinal;
    FEnabled: Boolean;
    FFont: TFont;
    FHeight: Integer;
    FOptions: TBCEditorTokenInfo.TOptions;
    FTitle: TTitle;
    FWidth: Integer;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Colors: TColors read FColors write FColors;
    property DelayInterval: Cardinal read FDelayInterval write FDelayInterval default 300;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Font: TFont read FFont write SetFont;
    property Height: Integer read FHeight write FHeight default 0;
    property Options: TBCEditorTokenInfo.TOptions read FOptions write FOptions default DefaultOptions;
    property Title: TTitle read FTitle write FTitle;
    property Width: Integer read FWidth write FWidth default 0;
  end;

implementation {***************************************************************}

{ TBCEditorTokenInfo.TTitle.TColors *******************************************}

constructor TBCEditorTokenInfo.TTitle.TColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FReference := clBlue;
end;

procedure TBCEditorTokenInfo.TTitle.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TColors then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FReference := FReference;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorTokenInfo.TTitle ***************************************************}

constructor TBCEditorTokenInfo.TTitle.Create;
begin
  inherited;

  FColors := TColors.Create;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FVisible := False;
end;

destructor TBCEditorTokenInfo.TTitle.Destroy;
begin
  FColors.Free;
  FFont.Free;

  inherited;
end;

procedure TBCEditorTokenInfo.TTitle.Assign(ASource: TPersistent);
begin
  if ASource is TTitle then
  with ASource as TTitle do
  begin
    Self.FColors.Assign(FColors);
    Self.FFont.Assign(FFont);
    Self.FVisible := FVisible;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTokenInfo.TTitle.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TBCEditorTokenInfo.TColors **************************************************}

constructor TBCEditorTokenInfo.TColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FReference := clBlue;
end;

procedure TBCEditorTokenInfo.TColors.Assign(ASource: TPersistent);
begin
  if ASource is TColors then
  with ASource as TColors do
  begin
    Self.FBackground := FBackground;
    Self.FReference := FReference;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorTokenInfo **********************************************************}

constructor TBCEditorTokenInfo.Create;
begin
  inherited Create;

  FColors := TColors.Create;
  FDelayInterval := 300;
  FEnabled := False;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FHeight := 0;
  FOptions := DefaultOptions;
  FTitle := TTitle.Create;
  FWidth := 0;
end;

destructor TBCEditorTokenInfo.Destroy;
begin
  FColors.Free;
  FFont.Free;
  FTitle.Free;

  inherited;
end;

procedure TBCEditorTokenInfo.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorTokenInfo then
  with ASource as TBCEditorTokenInfo do
  begin
    Self.FColors.Assign(FColors);
    Self.FDelayInterval := FDelayInterval;
    Self.FEnabled := FEnabled;
    Self.FFont.Assign(FFont);
    Self.FHeight := FHeight;
    Self.FOptions := FOptions;
    Self.FTitle.Assign(FTitle);
    Self.FWidth := FWidth;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorTokenInfo.SetFont(const AValue: TFont);
begin
  FFont.Assign(AValue);
end;

end.
