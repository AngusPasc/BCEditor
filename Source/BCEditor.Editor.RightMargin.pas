unit BCEditor.Editor.RightMargin;

interface

uses
  Classes, UITypes,
  Graphics,
  BCEditor.Types;

type
  TBCEditorRightMargin = class(TPersistent)
  type
    TOptions = set of TBCEditorRightMarginOption;

    TColors = class(TPersistent)
    strict private
      FEdge: TColor;
      FMovingEdge: TColor;
      FOnChange: TNotifyEvent;
      procedure DoChange;
      procedure SetEdge(AValue: TColor);
      procedure SetMovingEdge(AValue: TColor);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Edge: TColor read FEdge write SetEdge default clSilver;
      property MovingEdge: TColor read FMovingEdge write SetMovingEdge default clSilver;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  strict private
    FColors: TBCEditorRightMargin.TColors;
    FCursor: TCursor;
    FMouseOver: Boolean;
    FMoving: Boolean;
    FOnChange: TNotifyEvent;
    FOptions: TOptions;
    FPosition: Integer;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColors(const AValue: TColors);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetPosition(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorRightMarginOption; const AEnabled: Boolean);
    property MouseOver: Boolean read FMouseOver write FMouseOver;
    property Moving: Boolean read FMoving write FMoving;
  published
    property Colors: TBCEditorRightMargin.TColors read FColors write SetColors;
    property Cursor: TCursor read FCursor write FCursor default crHSplit;
    property Options: TOptions read FOptions write FOptions default [rmoMouseMove, rmoShowMovingHint];
    property Position: Integer read FPosition write SetPosition default 80;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

{ TBCEditorRightMargin.TColors ************************************************}

constructor TBCEditorRightMargin.TColors.Create;
begin
  inherited;

  FEdge := clSilver;
  FMovingEdge := clSilver;
end;

procedure TBCEditorRightMargin.TColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorRightMargin.TColors) then
  with ASource as TBCEditorRightMargin.TColors do
  begin
    Self.FEdge := FEdge;
    Self.FMovingEdge := FMovingEdge;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorRightMargin.TColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorRightMargin.TColors.SetEdge(AValue: TColor);
begin
  if FEdge <> AValue then
  begin
    FEdge := AValue;
    DoChange;
  end;
end;

procedure TBCEditorRightMargin.TColors.SetMovingEdge(AValue: TColor);
begin
  if FMovingEdge <> AValue then
  begin
    FMovingEdge := AValue;
    DoChange;
  end;
end;

{ TBCEditorRightMargin ********************************************************}

constructor TBCEditorRightMargin.Create;
begin
  inherited;

  FVisible := True;
  FPosition := 80;
  FColors := TBCEditorRightMargin.TColors.Create;
  FOptions := [rmoMouseMove, rmoShowMovingHint];
  FMoving := False;
  FMouseOver := False;
  FCursor := crHSplit;
end;

destructor TBCEditorRightMargin.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorRightMargin.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorRightMargin) then
  with ASource as TBCEditorRightMargin do
  begin
    Self.FVisible := FVisible;
    Self.FPosition := FPosition;
    Self.FColors.Assign(fColors);
    Self.FOptions := FOptions;
    Self.FCursor := FCursor;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorRightMargin.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorRightMargin.SetColors(const AValue: TBCEditorRightMargin.TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorRightMargin.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FColors.OnChange := AValue;
end;

procedure TBCEditorRightMargin.SetOption(const AOption: TBCEditorRightMarginOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorRightMargin.SetPosition(const AValue: Integer);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    DoChange
  end;
end;

procedure TBCEditorRightMargin.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange
  end;
end;

end.
