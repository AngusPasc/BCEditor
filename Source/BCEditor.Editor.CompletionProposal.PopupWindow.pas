unit BCEditor.Editor.CompletionProposal.PopupWindow;

interface

uses
  Messages,
  Classes, Types,
  Forms, Controls, Graphics,
  BCEditor.Utils, BCEditor.Types, BCEditor.Editor.PopupWindow,
  BCEditor.Editor.CompletionProposal;

type
  TBCEditorCompletionProposalPopupWindow = class(TBCEditorPopupWindow)
  strict private
    FAdjustCompletionStart: Boolean;
    FBitmapBuffer: Graphics.TBitmap;
    FCaseSensitive: Boolean;
    FCompletionProposal: TBCEditorCompletionProposal;
    FCompletionStartChar: Integer;
    FCurrentString: string;
    FFiltered: Boolean;
    FItemHeight: Integer;
    FItemIndexArray: array of Integer;
    FItems: TStrings;
    FMargin: Integer;
    FOnCanceled: TNotifyEvent;
    FOnSelected: TBCEditorCompletionProposal.TSelectedEvent;
    FOnValidate: TBCEditorCompletionProposal.TValidateEvent;
    FSelectedLine: Integer;
    FSendToEditor: Boolean;
    FTitleHeight: Integer;
    FTitleVisible: Boolean;
    FTopLine: Integer;
    FValueSet: Boolean;
    function GetItemHeight: Integer;
    function GetItems: TBCEditorCompletionProposal.TItems;
    function GetTitleHeight: Integer;
    function GetVisibleLines: Integer;
    procedure HandleDblClick(ASender: TObject);
    procedure HandleOnValidate(ASender: TObject; AShift: TShiftState; AEndToken: Char);
    procedure MoveSelectedLine(ALineCount: Integer);
    procedure SetCurrentString(const AValue: string);
    procedure SetTopLine(const AValue: Integer);
    procedure UpdateScrollBar;
    procedure WMVScroll(var AMessage: TWMScroll); message WM_VSCROLL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(const AEditor: TCustomControl);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Execute(const ACurrentString: string; const APoint: TPoint);
    function GetCurrentInput: string;
    procedure MouseWheel(AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint);
    procedure WndProc(var Msg: TMessage); override;
    property CurrentString: string read FCurrentString write SetCurrentString;
    property Items: TBCEditorCompletionProposal.TItems read GetItems;
    property TopLine: Integer read FTopLine write SetTopLine;
    property OnCanceled: TNotifyEvent read FOnCanceled write FOnCanceled;
    property OnSelected: TBCEditorCompletionProposal.TSelectedEvent read FOnSelected write FOnSelected;
  end;

implementation {***************************************************************}

uses
  Windows,
  SysUtils, UITypes, Math,
  Themes, Dialogs,
  BCEditor.Consts, BCEditor.Editor, BCEditor.Editor.KeyCommands, BCEditor.Lines;

type
  TCustomBCEditor = class(BCEditor.Editor.TCustomBCEditor);

{ TBCEditorCompletionProposalPopupWindow **************************************}

constructor TBCEditorCompletionProposalPopupWindow.Create(const AEditor: TCustomControl);
begin
  inherited Create(AEditor);

  FCaseSensitive := False;
  FFiltered := False;
  FItemHeight := 0;
  FMargin := 2;
  FOnCanceled := nil;
  FOnSelected := nil;
  FValueSet := False;
  Visible := False;

  FItems := TStringList.Create;
  FBitmapBuffer := Graphics.TBitmap.Create;

  FOnValidate := HandleOnValidate;
  OnDblClick := HandleDblClick;
end;

destructor TBCEditorCompletionProposalPopupWindow.Destroy;
begin
  if FItemHeight <> 0 then
    FCompletionProposal.VisibleLines := ClientHeight div FItemHeight;
  FCompletionProposal.Width := Width;

  if not FValueSet and Assigned(FOnCanceled) then
    FOnCanceled(FCompletionProposal);

  FBitmapBuffer.Free;
  SetLength(FItemIndexArray, 0);
  FItems.Free;

  inherited Destroy;
end;

procedure TBCEditorCompletionProposalPopupWindow.Assign(ASource: TPersistent);
begin
  if ASource is TBCEditorCompletionProposal then
  begin
    FCompletionProposal := ASource as TBCEditorCompletionProposal;
    with FCompletionProposal do
    begin
      Self.FCaseSensitive := cpoCaseSensitive in Options;
      Self.FFiltered := cpoFiltered in Options;
      Self.Width := Width;
      Self.Constraints.Assign(Constraints);
    end
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCompletionProposalPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;

  if cpoResizeable in FCompletionProposal.Options then
    Params.Style := Params.Style or WS_SIZEBOX;
end;

procedure TBCEditorCompletionProposalPopupWindow.Execute(const ACurrentString: string; const APoint: TPoint);
var
  LPoint: TPoint;

  procedure CalculateFormPlacement;
  begin
    LPoint.X := APoint.X - TextWidth(FBitmapBuffer.Canvas, ACurrentString);
    LPoint.Y := APoint.Y;

    ClientHeight := FItemHeight * FCompletionProposal.VisibleLines + FTitleHeight + 2;

    if LPoint.X + ClientWidth > Screen.DesktopWidth then
    begin
      LPoint.X := Screen.DesktopWidth - ClientWidth - 5;
      if LPoint.X < 0 then
        LPoint.X := 0;
    end;

    if LPoint.Y + ClientHeight > Screen.DesktopHeight then
    begin
      LPoint.Y := LPoint.Y - ClientHeight - TCustomBCEditor(Editor).LineHeight - 2;
      if LPoint.Y < 0 then
        LPoint.Y := 0;
    end;
  end;

  procedure CalculateColumnWidths;
  var
    LAutoWidthCount: Integer;
    LColumnIndex: Integer;
    LIndex: Integer;
    LItems: TBCEditorCompletionProposal.TItems;
    LMaxWidth: Integer;
    LProposalColumn: TBCEditorCompletionProposal.TColumns.TColumn;
    LTempWidth: Integer;
    LVisibleColumnCount: Integer;
    LWidthSum: Integer;
  begin
    LVisibleColumnCount := 0;
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
      if FCompletionProposal.Columns[LColumnIndex].Visible then
        Inc(LVisibleColumnCount);

    if LVisibleColumnCount = 1 then
    begin
      LProposalColumn := nil; // Hide compiler warning only.
      for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
        if FCompletionProposal.Columns[LColumnIndex].Visible then
          LProposalColumn := FCompletionProposal.Columns[LColumnIndex];
      if LProposalColumn.AutoWidth then
        LProposalColumn.Width := Width;
      Exit;
    end;

    LAutoWidthCount := 0;
    LWidthSum := 0;
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
    begin
      LProposalColumn := FCompletionProposal.Columns[LColumnIndex];
      if LProposalColumn.Visible and LProposalColumn.AutoWidth then
      begin
        LItems := LProposalColumn.Items;
        LMaxWidth := 0;
        for LIndex := 0 to LItems.Count - 1 do
        begin
          LTempWidth := TextWidth(FBitmapBuffer.Canvas, LItems[LIndex].Value);
          if LTempWidth > LMaxWidth then
            LMaxWidth := LTempWidth;
        end;
        LProposalColumn.Width := LMaxWidth;
        LWidthSum := LWidthSum + LMaxWidth;
        Inc(LAutoWidthCount);
      end;
    end;

    LMaxWidth := (Width - LWidthSum - GetSystemMetrics(SM_CYHSCROLL)) div LAutoWidthCount;
    if LMaxWidth > 0 then
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
    begin
      LProposalColumn := FCompletionProposal.Columns[LColumnIndex];
      if LProposalColumn.Visible and LProposalColumn.AutoWidth then
        LProposalColumn.Width := LProposalColumn.Width + LMaxWidth;
    end;
  end;

  function GetTitleVisible: Boolean;
  var
    LColumn: TBCEditorCompletionProposal.TColumns.TColumn;
    LColumnIndex: Integer;
  begin
    Result := False;
    for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
    begin
      LColumn := FCompletionProposal.Columns[LColumnIndex];
      if LColumn.Visible and LColumn.Title.Visible then
        Exit(True);
    end;
  end;

  procedure SetAutoConstraints;
  begin
    if cpoAutoConstraints in FCompletionProposal.Options then
    begin
      FCompletionProposal.Constraints.MinHeight := Height;
      FCompletionProposal.Constraints.MinWidth := Width;
      Constraints.Assign(FCompletionProposal.Constraints);
    end;
  end;

var
  LCount: Integer;
  LIndex: Integer;
begin
  LCount := GetItems.Count;
  SetLength(FItemIndexArray, 0);
  SetLength(FItemIndexArray, LCount);
  for LIndex := 0 to LCount - 1 do
    FItemIndexArray[LIndex] := LIndex;

  if Length(FItemIndexArray) > 0 then
  begin
    FTitleVisible := GetTitleVisible;
    FItemHeight := GetItemHeight;
    FTitleHeight := GetTitleHeight;
    CalculateFormPlacement;
    CalculateColumnWidths;
    SetAutoConstraints;
    CurrentString := ACurrentString;
    if Length(FItemIndexArray) > 0 then
    begin
      UpdateScrollBar;
      Show(LPoint);
    end;
  end;
end;

function TBCEditorCompletionProposalPopupWindow.GetCurrentInput: string;
var
  LChar: Integer;
  LLineText: string;
  LTextCaretPosition: TBCEditorTextPosition;
begin
  Result := '';

  LTextCaretPosition := TextPosition(TCustomBCEditor(Editor).CaretPos);

  LLineText := TCustomBCEditor(Editor).Lines[LTextCaretPosition.Line];
  LChar := LTextCaretPosition.Char;
  if LChar < Length(LLineText) then
  begin
    FAdjustCompletionStart := False;
    while (LChar >= 0) and (LLineText[1 + LChar] > BCEDITOR_SPACE_CHAR) and not TCustomBCEditor(Editor).IsWordBreakChar(LLineText[1 + LChar]) do
      Dec(LChar);

    FCompletionStartChar := LChar;
    Result := Copy(LLineText, 1 + FCompletionStartChar, LTextCaretPosition.Char - FCompletionStartChar);
  end
  else
  begin
    FAdjustCompletionStart := True;
    FCompletionStartChar := LTextCaretPosition.Char;
  end;
end;

function TBCEditorCompletionProposalPopupWindow.GetItemHeight: Integer;
var
  LColumn: TBCEditorCompletionProposal.TColumns.TColumn;
  LColumnIndex: Integer;
  LHeight: Integer;
begin
  Result := 0;
  for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
  begin
    LColumn := FCompletionProposal.Columns[LColumnIndex];
    FBitmapBuffer.Canvas.Font.Assign(LColumn.Font);
    LHeight := TextHeight(FBitmapBuffer.Canvas, 'X');
    if LHeight > Result then
      Result := LHeight;
  end;
end;

function TBCEditorCompletionProposalPopupWindow.GetItems: TBCEditorCompletionProposal.TItems;
begin
  Result := nil;
  if FCompletionProposal.CompletionColumnIndex <  FCompletionProposal.Columns.Count then
    Result := FCompletionProposal.Columns[FCompletionProposal.CompletionColumnIndex].Items;
end;

function TBCEditorCompletionProposalPopupWindow.GetTitleHeight: Integer;
var
  LColumn: TBCEditorCompletionProposal.TColumns.TColumn;
  LColumnIndex: Integer;
  LHeight: Integer;
begin
  Result := 0;
  if FTitleVisible then
  for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
  begin
    LColumn := FCompletionProposal.Columns[LColumnIndex];
    FBitmapBuffer.Canvas.Font.Assign(LColumn.Title.Font);
    LHeight := TextHeight(FBitmapBuffer.Canvas, 'X');
    if LHeight > Result then
      Result := LHeight;
  end;
end;

function TBCEditorCompletionProposalPopupWindow.GetVisibleLines: Integer;
begin
  Result := (ClientHeight - FTitleHeight) div FItemHeight;
end;

procedure TBCEditorCompletionProposalPopupWindow.HandleDblClick(ASender: TObject);
begin
  if Assigned(FOnValidate) then
    FOnValidate(Self, [], BCEDITOR_NONE_CHAR);
  Hide;
end;

procedure TBCEditorCompletionProposalPopupWindow.HandleOnValidate(ASender: TObject; AShift: TShiftState; AEndToken: Char);
var
  LLine: string;
  LTextPosition: TBCEditorTextPosition;
  LValue: string;
begin
  with TCustomBCEditor(Editor) do
  begin
    BeginUpdate;
    Lines.BeginUpdate();
    try
      LTextPosition := TextPosition(CaretPos);

      if not SelectionAvailable then
      begin
        SelectionBeginPosition := TextPosition(FCompletionStartChar, LTextPosition.Line);
        if AEndToken = BCEDITOR_NONE_CHAR then
        begin
          LLine := Lines[LTextPosition.Line];
          if (LTextPosition.Char < Length(LLine)) and IsWordBreakChar(LLine[1 + LTextPosition.Char]) then
            SelectionEndPosition := LTextPosition
          else
            SelectionEndPosition := TextPosition(WordEnd.Char, LTextPosition.Line)
        end
        else
          SelectionEndPosition := LTextPosition;
      end;

      if FSelectedLine < Length(FItemIndexArray) then
        LValue := GetItems[FItemIndexArray[FSelectedLine]].Value
      else
        LValue := SelText;

      if Assigned(FOnSelected) then
        FOnSelected(FCompletionProposal, LValue);

      FValueSet := SelText <> LValue;
      if FValueSet then
        SelText := LValue;

      if CanFocus then
        SetFocus;

      EnsureCaretPositionVisible;
      CaretPos := Point(SelectionEndPosition);
      SelectionBeginPosition := TextPosition(CaretPos.X + 1, CaretPos.Y);
    finally
      Lines.EndUpdate();
      EndUpdate;
    end;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.KeyDown(var Key: Word; Shift: TShiftState);
var
  LChar: Char;
  LTextCaretPosition: TBCEditorTextPosition;
begin
  FSendToEditor := True;
  case Key of
    VK_RETURN, VK_TAB:
      begin
        if Assigned(FOnValidate) then
          FOnValidate(Self, Shift, BCEDITOR_NONE_CHAR);
          FSendToEditor := False;
      end;
    VK_ESCAPE:
      begin
        Editor.SetFocus;
        FSendToEditor := False;
      end;
    VK_LEFT:
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);
          TCustomBCEditor(Editor).CommandProcessor(ecLeft, BCEDITOR_NONE_CHAR, nil);
        end
        else
        begin
          TCustomBCEditor(Editor).CommandProcessor(ecLeft, BCEDITOR_NONE_CHAR, nil);
          Editor.SetFocus;
        end;
        FSendToEditor := False;
      end;
    VK_RIGHT:
      with TCustomBCEditor(Editor) do
      begin
        LTextCaretPosition := TextPosition(CaretPos);
        if LTextCaretPosition.Char < Length(Lines[LTextCaretPosition.Line]) then
          LChar := Lines[LTextCaretPosition.Line][1 + LTextCaretPosition.Char]
        else
          LChar := BCEDITOR_SPACE_CHAR;

        if not IsWordBreakChar(LChar) then
          CurrentString := FCurrentString + LChar
        else
          Editor.SetFocus;

        CommandProcessor(ecRight, BCEDITOR_NONE_CHAR, nil);
        FSendToEditor := False;
      end;
    VK_PRIOR:
      begin
        MoveSelectedLine(-GetVisibleLines);
        FSendToEditor := False;
      end;
    VK_NEXT:
      begin
        MoveSelectedLine(GetVisibleLines);
        FSendToEditor := False;
      end;
    VK_END:
      begin
        TopLine := Length(FItemIndexArray) - 1;
        FSendToEditor := False;
      end;
    VK_HOME:
      begin
        TopLine := 0;
        FSendToEditor := False;
      end;
    VK_UP:
      begin
        if ssCtrl in Shift then
          FSelectedLine := 0
        else
          MoveSelectedLine(-1);
        FSendToEditor := False;
      end;
    VK_DOWN:
      begin
        if ssCtrl in Shift then
          FSelectedLine := Length(FItemIndexArray) - 1
        else
          MoveSelectedLine(1);
        FSendToEditor := False;
      end;
    VK_BACK:
      if Shift = [] then
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);

          TCustomBCEditor(Editor).CommandProcessor(ecBackspace, BCEDITOR_NONE_CHAR, nil);
        end
        else
        begin
          TCustomBCEditor(Editor).CommandProcessor(ecBackspace, BCEDITOR_NONE_CHAR, nil);
          Editor.SetFocus;
        end;
        FSendToEditor := False;
      end;
    VK_DELETE:
      begin
        TCustomBCEditor(Editor).CommandProcessor(ecDeleteChar, BCEDITOR_NONE_CHAR, nil);
        FSendToEditor := False;
      end;
  end;
  Key := 0;
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.KeyPress(var Key: Char);
begin
  case Key of
    BCEDITOR_CARRIAGE_RETURN:
      Editor.SetFocus;
    BCEDITOR_SPACE_CHAR .. High(Char):
      begin
        if not (cpoAutoInvoke in FCompletionProposal.Options) then
          if TCustomBCEditor(Editor).IsWordBreakChar(Key) and Assigned(FOnValidate) then
            if Key = BCEDITOR_SPACE_CHAR then
              FOnValidate(Self, [], BCEDITOR_NONE_CHAR);
        CurrentString := FCurrentString + Key;
        if (cpoAutoInvoke in FCompletionProposal.Options) and (Length(FItemIndexArray) = 0) or
          (Pos(Key, FCompletionProposal.CloseChars) <> 0) then
          Editor.SetFocus
        else
        if Assigned(OnKeyPress) then
          OnKeyPress(Self, Key);
      end;
    BCEDITOR_BACKSPACE_CHAR:
      TCustomBCEditor(Editor).CommandProcessor(ecChar, Key, nil);
  end;
  if (FSendToEditor) then
    PostMessage(TCustomBCEditor(Editor).Handle, WM_CHAR, WParam(Key), 0);
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  FSelectedLine := Max(0, TopLine + ((Y - FTitleHeight) div FItemHeight));
  inherited MouseDown(AButton, AShift, X, Y);
  Refresh;
end;

procedure TBCEditorCompletionProposalPopupWindow.MouseWheel(AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint);
var
  LLinesToScroll: Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  if ssCtrl in aShift then
    LLinesToScroll := GetVisibleLines
  else
    LLinesToScroll := 1;

  if AWheelDelta > 0 then
    TopLine := Max(0, TopLine - LLinesToScroll)
  else
    TopLine := Min(GetItems.Count - GetVisibleLines, TopLine + LLinesToScroll);

  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.MoveSelectedLine(ALineCount: Integer);
begin
  FSelectedLine := MinMax(FSelectedLine + ALineCount, 0, Length(FItemIndexArray) - 1);
  if FSelectedLine >= TopLine + GetVisibleLines then
    TopLine := FSelectedLine - GetVisibleLines + 1;
  if FSelectedLine < TopLine then
    TopLine := FSelectedLine;
end;

procedure TBCEditorCompletionProposalPopupWindow.Paint;
var
  LColumn: TBCEditorCompletionProposal.TColumns.TColumn;
  LColumnIndex: Integer;
  LColumnWidth: Integer;
  LIndex: Integer;
  LItemIndex: Integer;
  LLeft: Integer;
  LRect: TRect;
begin
  with FBitmapBuffer do
  begin
    Canvas.Brush.Color := FCompletionProposal.Colors.Background;
    Height := 0;
    Width := ClientWidth;
    Height := ClientHeight;
    { Title }
    LRect := ClientRect;
    LRect.Height := FItemHeight;
    LColumnWidth := 0;
    if FTitleVisible then
    begin
      LRect.Height := FTitleHeight;
      for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
      begin
        LColumn := FCompletionProposal.Columns[LColumnIndex];
        if (LColumn.Visible) then
        begin
          LColumn := FCompletionProposal.Columns[LColumnIndex];
          Canvas.Brush.Color := LColumn.Title.Colors.Background;
          LRect.Left := LColumnWidth;
          LRect.Right := LColumnWidth + LColumn.Width;
          ExtTextOut(Canvas.Handle, 0, 0, ETO_OPAQUE, LRect, '', 0, nil);
          Canvas.Font.Assign(LColumn.Title.Font);
          if LColumn.Title.Visible then
            Canvas.TextOut(FMargin + LColumnWidth, 0, LColumn.Title.Caption);
          Canvas.Pen.Color := LColumn.Title.Colors.BottomBorder;
          Canvas.MoveTo(LRect.Left, LRect.Bottom - 1);
          Canvas.LineTo(LRect.Right, LRect.Bottom - 1);
          Canvas.Pen.Color := LColumn.Title.Colors.RightBorder;
          Canvas.MoveTo(LRect.Right - 1, LRect.Top - 1);
          Canvas.LineTo(LRect.Right - 1, LRect.Bottom - 1);
          LColumnWidth := LColumnWidth + LColumn.Width;
        end;
        LRect.Right := ClientRect.Right;
        LRect.Left := 0;
        LRect.Top := LRect.Bottom;
        LRect.Bottom := LRect.Top + FItemHeight;
      end;
    end;
    { Data }
    for LIndex := 0 to Min(GetVisibleLines, Length(FItemIndexArray) - 1) do
    begin
      if LIndex + TopLine >= Length(FItemIndexArray) then
        Break;

      if LIndex + TopLine = FSelectedLine then
      begin
        Canvas.Brush.Color := FCompletionProposal.Colors.SelectedBackground;
        Canvas.Pen.Color := FCompletionProposal.Colors.SelectedBackground;
        Canvas.Rectangle(LRect);
      end
      else
      begin
        Canvas.Brush.Color := FCompletionProposal.Colors.Background;
        Canvas.Pen.Color := FCompletionProposal.Colors.Background;
      end;
      LColumnWidth := 0;
      for LColumnIndex := 0 to FCompletionProposal.Columns.Count - 1 do
      begin
        LItemIndex := FItemIndexArray[TopLine + LIndex];
        LColumn := FCompletionProposal.Columns[LColumnIndex];
        if (LColumn.Visible) then
        begin
          Canvas.Font.Assign(LColumn.Font);

          if LIndex + TopLine = FSelectedLine then
            Canvas.Font.Color := FCompletionProposal.Colors.SelectedText
          else
            Canvas.Font.Color := FCompletionProposal.Colors.Foreground;

          if LItemIndex < LColumn.Items.Count then
          begin
            LLeft := 0;
            if LColumn.Items[LItemIndex].ImageIndex <> -1 then
            begin
              FCompletionProposal.Images.Draw(Canvas, FMargin + LColumnWidth, LRect.Top, LColumn.Items[LItemIndex].ImageIndex);
              Inc(LLeft, FCompletionProposal.Images.Width + FMargin);
            end;
            Canvas.TextOut(FMargin + LColumnWidth + LLeft, LRect.Top, LColumn.Items[LItemIndex].Value);
          end;
          LColumnWidth := LColumnWidth + LColumn.Width;
        end;
      end;
      LRect.Top := LRect.Bottom;
      LRect.Bottom := LRect.Top + FItemHeight;
    end;
  end;
  Canvas.Draw(0, 0, FBitmapBuffer);
end;

procedure TBCEditorCompletionProposalPopupWindow.SetCurrentString(const AValue: string);

  function MatchItem(AIndex: Integer): Boolean;
  var
    LCompareString: string;
  begin
    LCompareString := Copy(GetItems[AIndex].Value, 1, Length(AValue));

    if FCaseSensitive then
      Result := CompareStr(LCompareString, AValue) = 0
    else
      Result := AnsiCompareText(LCompareString, AValue) = 0;
  end;

  procedure RecalcList;
  var
    LIndex: Integer;
    LIndex2: Integer;
    LItemsCount: Integer;
  begin
    LIndex2 := 0;
    LItemsCount := GetItems.Count;
    SetLength(FItemIndexArray, 0);
    SetLength(FItemIndexArray, LItemsCount);
    for LIndex := 0 to LItemsCount - 1 do
      if MatchItem(LIndex) then
      begin
        FItemIndexArray[LIndex2] := LIndex;
        Inc(LIndex2);
      end;
    SetLength(FItemIndexArray, LIndex2);
  end;

var
  LIndex: Integer;
begin
  FCurrentString := AValue;

  if FFiltered then
  begin
    RecalcList;
    TopLine := 0;
    Repaint;
  end
  else
  begin
    LIndex := 0;
    while (LIndex < Items.Count) and (not MatchItem(LIndex)) do
      Inc(LIndex);

    if LIndex < Items.Count then
      TopLine := LIndex
    else
      TopLine := 0;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetTopLine(const AValue: Integer);
begin
  if TopLine <> AValue then
  begin
    FTopLine := AValue;
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.UpdateScrollBar;
var
  LScrollInfo: TScrollInfo;
begin
  LScrollInfo.cbSize := SizeOf(ScrollInfo);
  LScrollInfo.fMask := SIF_ALL;
  LScrollInfo.fMask := LScrollInfo.fMask or SIF_DISABLENOSCROLL;

  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);

  LScrollInfo.nMin := 0;
  LScrollInfo.nMax := Max(0, GetItems.Count - 2);
  LScrollInfo.nPage := GetVisibleLines;
  LScrollInfo.nPos := TopLine;

  ShowScrollBar(Handle, SB_VERT, (LScrollInfo.nMin = 0) or (LScrollInfo.nMax > GetVisibleLines));
  SetScrollInfo(Handle, SB_VERT, LScrollInfo, True);

  if GetItems.Count <= GetVisibleLines then
    EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH)
  else
  begin
    EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
    if TopLine <= 0 then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_UP)
    else
    if TopLine + GetVisibleLines >= GetItems.Count then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_DOWN);
  end;

  if Visible then
    SendMessage(Handle, WM_SETREDRAW, -1, 0);
end;

procedure TBCEditorCompletionProposalPopupWindow.WMVScroll(var AMessage: TWMScroll);
begin
  Invalidate;
  AMessage.Result := 0;

  case AMessage.ScrollCode of
    SB_TOP:
      TopLine := 0;
    SB_BOTTOM:
      TopLine := GetItems.Count - 1;
    SB_LINEDOWN:
      TopLine := Min(GetItems.Count - GetVisibleLines, TopLine + 1);
    SB_LINEUP:
      TopLine := Max(0, TopLine - 1);
    SB_PAGEDOWN:
      TopLine := Min(GetItems.Count - GetVisibleLines, TopLine + GetVisibleLines);
    SB_PAGEUP:
      TopLine := Max(0, TopLine - GetVisibleLines);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      TopLine := AMessage.Pos;
  end;
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_KEYDOWN) then
    Write;
  if (Msg.Msg = WM_SETFOCUS) then
    Write;

  inherited;
end;

end.
