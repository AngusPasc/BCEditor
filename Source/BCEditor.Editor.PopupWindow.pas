unit BCEditor.Editor.PopupWindow;

interface

uses
  Messages,
  Classes, Types,
  Forms, Controls;

type
  TBCEditorPopupWindow = class(TCustomControl)
  private
    FEditor: TCustomControl;
    FOriginalHeight: Integer;
    FOriginalWidth: Integer;
    FPopupParent: TCustomForm;
    procedure SetPopupParent(Value: TCustomForm);
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure WMEraseBkgnd(var AMessage: TMessage); message WM_ERASEBKGND;
  protected
    FActiveControl: TWinControl;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Hide; virtual;
    procedure Show(Origin: TPoint); virtual;
  public
    constructor Create(const AEditor: TCustomControl); reintroduce;
    procedure IncSize(const AWidth: Integer; const AHeight: Integer);
    procedure SetOriginalSize;
    property ActiveControl: TWinControl read FActiveControl;
    property Editor: TCustomControl read FEditor;
    property PopupParent: TCustomForm read FPopupParent write SetPopupParent;
  end;

implementation

uses
  Windows,
  SysUtils;

constructor TBCEditorPopupWindow.Create(const AEditor: TCustomControl);
begin
  inherited Create(nil);

  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];

  Ctl3D := False;
  FEditor := AEditor;
  FPopupParent := nil;
  ParentCtl3D := False;
  Visible := False;
end;

procedure TBCEditorPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  Params.Style := WS_POPUP or WS_BORDER;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  if (Assigned(PopupParent)) then
    Params.WndParent := PopupParent.Handle;
end;

procedure TBCEditorPopupWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER);
  Visible := False;
end;

procedure TBCEditorPopupWindow.IncSize(const AWidth: Integer; const AHeight: Integer);
var
  LHeight: Integer;
  LWidth: Integer;
begin
  LHeight := FOriginalHeight + AHeight;
  LWidth := FOriginalWidth + AWidth;

  if LHeight < Constraints.MinHeight then
    LHeight := Constraints.MinHeight;
  if (Constraints.MaxHeight > 0) and (LHeight > Constraints.MaxHeight) then
    LHeight := Constraints.MaxHeight;

  if LWidth < Constraints.MinWidth then
    LWidth := Constraints.MinWidth;
  if (Constraints.MaxWidth > 0) and (LWidth > Constraints.MaxWidth) then
    LWidth := Constraints.MaxWidth;

  SetBounds(Left, Top, LWidth, LHeight);
end;

procedure TBCEditorPopupWindow.SetOriginalSize;
begin
  FOriginalHeight := Height;
  FOriginalWidth := Width;
end;

procedure TBCEditorPopupWindow.SetPopupParent(Value: TCustomForm);
begin
  if (Value <> FPopupParent) then
  begin
    if FPopupParent <> nil then
      FPopupParent.RemoveFreeNotification(Self);
    FPopupParent := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
    if HandleAllocated and not (csDesigning in ComponentState) then
      RecreateWnd;
  end;
end;

procedure TBCEditorPopupWindow.Show(Origin: TPoint);
begin
  SetBounds(Origin.X, Origin.Y, Width, Height);

  SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER);

  Visible := True;
end;

procedure TBCEditorPopupWindow.WMActivate(var Msg: TWMActivate);
begin
  if ((Msg.Active <> WA_INACTIVE) and Assigned(PopupParent)) then
    SendMessage(PopupParent.Handle, WM_NCACTIVATE, WPARAM(TRUE), 0);

  inherited;

  if Msg.Active = WA_INACTIVE then
    Hide();
end;

procedure TBCEditorPopupWindow.WMEraseBkgnd(var AMessage: TMessage);
begin
  AMessage.Result := 1;
end;

end.
