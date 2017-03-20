unit BCEditor.Editor.Caret;

interface {********************************************************************}

uses
  Classes,
  Graphics,
  BCEditor.Types;

type
  TBCEditorCaret = class(TPersistent)
  type
    TChangedEvent = procedure(ASender: TObject; X, Y: Integer) of object;
    TOptions = set of TBCEditorCaretOption;

    TStyles = class(TPersistent)
    strict private
      FInsert: TBCEditorCaretStyle;
      FOnChange: TNotifyEvent;
      FOverwrite: TBCEditorCaretStyle;
      procedure DoChange;
      procedure SetInsert(const AValue: TBCEditorCaretStyle);
      procedure SetOverwrite(const AValue: TBCEditorCaretStyle);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Insert: TBCEditorCaretStyle read FInsert write SetInsert default csVerticalLine;
      property Overwrite: TBCEditorCaretStyle read FOverwrite write SetOverwrite default csBlock;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TOffsets = class(TPersistent)
    strict private
      FLeft: Integer;
      FOnChange: TNotifyEvent;
      FTop: Integer;
      procedure DoChange(ASender: TObject);
      procedure SetLeft(const AValue: Integer);
      procedure SetTop(const AValue: Integer);
    public
      constructor Create;
      procedure Assign(ASource: TPersistent); override;
    published
      property Left: Integer read FLeft write SetLeft default 0;
      property Top: Integer read FTop write SetTop default 0;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TNonBlinking = class(TPersistent)
    type

      TColors = class(TPersistent)
      strict private
        FBackground: TColor;
        FForeground: TColor;
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Background: TColor read FBackground write FBackground default clBlack;
        property Foreground: TColor read FForeground write FForeground default clWhite;
      end;

    strict private
      FColors: TBCEditorCaret.TNonBlinking.TColors;
      FEnabled: Boolean;
      FOnChange: TNotifyEvent;
      procedure DoChange;
      procedure SetColors(AValue: TBCEditorCaret.TNonBlinking.TColors);
      procedure SetEnabled(AValue: Boolean);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
    published
      property Colors: TBCEditorCaret.TNonBlinking.TColors read FColors write SetColors;
      property Enabled: Boolean read FEnabled write SetEnabled default False;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

    TMultiEdit = class(TPersistent)
    type
      TOptions = set of TBCEditorCaretMultiEditOption;


      TColors = class(TPersistent)
      strict private
        FBackground: TColor;
        FForeground: TColor;
      public
        constructor Create;
        procedure Assign(ASource: TPersistent); override;
      published
        property Background: TColor read FBackground write FBackground default clBlack;
        property Foreground: TColor read FForeground write FForeground default clWhite;
      end;

    strict private const
      DefaultOptions = [meoShowActiveLine, meoShowGhost];
    strict private
      FColors: TBCEditorCaret.TMultiEdit.TColors;
      FEnabled: Boolean;
      FOnChange: TNotifyEvent;
      FOptions: TBCEditorCaret.TMultiEdit.TOptions;
      FStyle: TBCEditorCaretStyle;
      procedure DoChange;
      procedure SetColors(AValue: TBCEditorCaret.TMultiEdit.TColors);
      procedure SetEnabled(AValue: Boolean);
      procedure SetOptions(const AValue: TBCEditorCaret.TMultiEdit.TOptions);
      procedure SetStyle(const AValue: TBCEditorCaretStyle);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TPersistent); override;
    published
      property Colors: TBCEditorCaret.TMultiEdit.TColors read FColors write SetColors;
      property Enabled: Boolean read FEnabled write SetEnabled default True;
      property Options: TBCEditorCaret.TMultiEdit.TOptions read FOptions write SetOptions default DefaultOptions;
      property Style: TBCEditorCaretStyle read FStyle write SetStyle default csVerticalLine;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

  strict private const
    DefaultOptions = [];
  strict private
    FMultiEdit: TBCEditorCaret.TMultiEdit;
    FNonBlinking: TBCEditorCaret.TNonBlinking;
    FOffsets: TBCEditorCaret.TOffsets;
    FOnChange: TNotifyEvent;
    FOptions: TOptions;
    FStyles: TStyles;
    procedure DoChange(ASender: TObject);
    procedure SetMultiEdit(AValue: TBCEditorCaret.TMultiEdit);
    procedure SetNonBlinking(AValue: TBCEditorCaret.TNonBlinking);
    procedure SetOffsets(AValue: TBCEditorCaret.TOffsets);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetOptions(const AValue: TOptions);
    procedure SetStyles(const AValue: TBCEditorCaret.TStyles);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetOption(const AOption: TBCEditorCaretOption; const AEnabled: Boolean);
  published
    property MultiEdit: TBCEditorCaret.TMultiEdit read FMultiEdit write SetMultiEdit;
    property NonBlinking: TBCEditorCaret.TNonBlinking read FNonBlinking write SetNonBlinking;
    property Offsets: TBCEditorCaret.TOffsets read FOffsets write SetOffsets;
    property Options: TOptions read FOptions write SetOptions default DefaultOptions;
    property Styles: TBCEditorCaret.TStyles read FStyles write SetStyles;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation {***************************************************************}

{ TBCEditorCaret.TStyles ******************************************************}

constructor TBCEditorCaret.TStyles.Create;
begin
  inherited;

  FInsert := csVerticalLine;
  FOverwrite := csBlock;
end;

procedure TBCEditorCaret.TStyles.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret.TStyles) then
  with ASource as TBCEditorCaret.TStyles do
  begin
    Self.FOverwrite := FOverwrite;
    Self.FInsert := FInsert;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaret.TStyles.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaret.TStyles.SetInsert(const AValue: TBCEditorCaretStyle);
begin
  if FInsert <> AValue then
  begin
    FInsert := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCaret.TStyles.SetOverwrite(const AValue: TBCEditorCaretStyle);
begin
  if FOverwrite <> AValue then
  begin
    FOverwrite := AValue;
    DoChange;
  end;
end;

{ TBCEditorCaret.TOffsets *****************************************************}

constructor TBCEditorCaret.TOffsets.Create;
begin
  inherited;

  FLeft := 0;
  FTop := 0;
end;

procedure TBCEditorCaret.TOffsets.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret.TOffsets) then
  with ASource as TBCEditorCaret.TOffsets do
  begin
    Self.FLeft := FLeft;
    Self.FTop := FTop;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaret.TOffsets.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorCaret.TOffsets.SetLeft(const AValue: Integer);
begin
  if FLeft <> AValue then
  begin
    FLeft := AValue;
    DoChange(Self);
  end;
end;

procedure TBCEditorCaret.TOffsets.SetTop(const AValue: Integer);
begin
  if FTop <> AValue then
  begin
    FTop := AValue;
    DoChange(Self);
  end;
end;

{ TBCEditorCaret.TNonBlinking.TColors *****************************************}

constructor TBCEditorCaret.TNonBlinking.TColors.Create;
begin
  inherited;

  FBackground := clBlack;
  FForeground := clWhite;
end;

procedure TBCEditorCaret.TNonBlinking.TColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret.TNonBlinking.TColors) then
  with ASource as TBCEditorCaret.TNonBlinking.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCaret.TNonBlinking *************************************************}

constructor TBCEditorCaret.TNonBlinking.Create;
begin
  inherited;

  FColors := TBCEditorCaret.TNonBlinking.TColors.Create;
  FEnabled := False;
end;

destructor TBCEditorCaret.TNonBlinking.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorCaret.TNonBlinking.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TNonBlinking) then
  with ASource as TBCEditorCaret.TNonBlinking do
  begin
    Self.FColors.Assign(FColors);
    Self.FEnabled := FEnabled;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaret.TNonBlinking.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaret.TNonBlinking.SetColors(AValue: TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorCaret.TNonBlinking.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    DoChange;
  end;
end;

{ TBCEditorCaret.TMultiEdit.TColors *******************************************}

constructor TBCEditorCaret.TMultiEdit.TColors.Create;
begin
  inherited;

  FBackground := clBlack;
  FForeground := clWhite;
end;

procedure TBCEditorCaret.TMultiEdit.TColors.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret.TMultiEdit.TColors) then
  with ASource as TBCEditorCaret.TMultiEdit.TColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
  end
  else
    inherited Assign(ASource);
end;

{ TBCEditorCaret.TMultiEdit ***************************************************}

constructor TBCEditorCaret.TMultiEdit.Create;
begin
  inherited;

  FColors := TBCEditorCaret.TMultiEdit.TColors.Create;
  FEnabled := True;
  FStyle := csVerticalLine;
  FOptions := DefaultOptions;
end;

destructor TBCEditorCaret.TMultiEdit.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorCaret.TMultiEdit.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret.TMultiEdit) then
  with ASource as TBCEditorCaret.TMultiEdit do
  begin
    Self.FColors.Assign(FColors);
    Self.FEnabled := FEnabled;
    Self.FOptions := FOptions;
    Self.FStyle := FStyle;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaret.TMultiEdit.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaret.TMultiEdit.SetColors(AValue: TBCEditorCaret.TMultiEdit.TColors);
begin
  FColors.Assign(AValue);
end;

procedure TBCEditorCaret.TMultiEdit.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCaret.TMultiEdit.SetOptions(const AValue: TBCEditorCaret.TMultiEdit.TOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange;
  end;
end;

procedure TBCEditorCaret.TMultiEdit.SetStyle(const AValue: TBCEditorCaretStyle);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    DoChange;
  end;
end;

{ TBCEditorCaret **************************************************************}

constructor TBCEditorCaret.Create;
begin
  inherited;

  FMultiEdit := TBCEditorCaret.TMultiEdit.Create;
  FNonBlinking := TBCEditorCaret.TNonBlinking.Create;
  FOffsets := TBCEditorCaret.TOffsets.Create;
  FOptions := DefaultOptions;
  FStyles := TBCEditorCaret.TStyles.Create;
end;

destructor TBCEditorCaret.Destroy;
begin
  FMultiEdit.Free;
  FNonBlinking.Free;
  FOffsets.Free;
  FStyles.Free;

  inherited;
end;

procedure TBCEditorCaret.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret) then
  with ASource as TBCEditorCaret do
  begin
    Self.FStyles.Assign(FStyles);
    Self.FMultiEdit.Assign(FMultiEdit);
    Self.FNonBlinking.Assign(FNonBlinking);
    Self.FOffsets.Assign(FOffsets);
    Self.FOptions := FOptions;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaret.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorCaret.SetMultiEdit(AValue: TBCEditorCaret.TMultiEdit);
begin
  FMultiEdit.Assign(AValue);
end;

procedure TBCEditorCaret.SetNonBlinking(AValue: TBCEditorCaret.TNonBlinking);
begin
  FNonBlinking.Assign(AValue);
end;

procedure TBCEditorCaret.SetOffsets(AValue: TBCEditorCaret.TOffsets);
begin
  FOffsets.Assign(AValue);
end;

procedure TBCEditorCaret.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FOffsets.OnChange := AValue;
  FStyles.OnChange := AValue;
  FMultiEdit.OnChange := AValue;
  FNonBlinking.OnChange := AValue;
end;

procedure TBCEditorCaret.SetOption(const AOption: TBCEditorCaretOption; const AEnabled: Boolean);
begin
  if AEnabled then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;

procedure TBCEditorCaret.SetOptions(const AValue: TOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange(Self);
  end;
end;

procedure TBCEditorCaret.SetStyles(const AValue: TBCEditorCaret.TStyles);
begin
  FStyles.Assign(AValue);
end;

end.
