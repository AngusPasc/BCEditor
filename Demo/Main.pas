unit Main;

interface

uses
  SysUtils, Variants, Classes,
  Windows, Messages,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  BCEditor.Editor, BCEditor.Highlighter;

type
  TMainForm = class(TForm)
    Editor: TBCEditor;
    ListBoxColors: TListBox;
    ListBoxHighlighters: TListBox;
    PanelLeft: TPanel;
    SplitterVertical: TSplitter;
    SplitterHorizontal: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxHighlightersClick(Sender: TObject);
    procedure ListBoxColorsClick(Sender: TObject);
  private
    ColorPath: TFileName;
    HighlighterPath: TFileName;
    procedure SetSelectedColor;
    procedure SetSelectedHighlighter;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure AddFileNamesFromPathIntoListBox(const APath: string; AListBox: TListBox);
var
  LSearchRec: TSearchRec;
begin
  if FindFirst(APath + '*.json', faNormal, LSearchRec) = 0 then
  try
    repeat
      AListBox.AddItem(LSearchRec.Name, nil);
    until FindNext(LSearchRec) <> 0;
  finally
    SysUtils.FindClose(LSearchRec);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  HighlighterPath := ExpandFileName('..\..\..\Highlighters\');
  ColorPath := ExpandFileName('..\..\..\Colors\');

  AddFileNamesFromPathIntoListBox(HighlighterPath, ListBoxHighlighters);
  AddFileNamesFromPathIntoListBox(ColorPath, ListBoxColors);

  with ListBoxHighlighters do
    if (Items.IndexOf('Object Pascal.json') >= 0) then
      Selected[Items.IndexOf('Object Pascal.json')] := True;

  with ListBoxColors do
    if (Items.IndexOf('Default.json') >= 0) then
      Selected[Items.IndexOf('Default.json')] := True;

  SetSelectedHighlighter;
  SetSelectedColor;
end;

procedure TMainForm.SetSelectedColor;
begin
  with ListBoxColors do
    if (ItemIndex >= 0) then
      Editor.Highlighter.Colors.LoadFromFile(ColorPath + Items[ItemIndex]);
end;

procedure TMainForm.SetSelectedHighlighter;
begin
  with ListBoxHighlighters do
    if (ItemIndex >= 0) then
      Editor.Highlighter.LoadFromFile(HighlighterPath + Items[ItemIndex]);
  Editor.Lines.Text := Editor.Highlighter.Info.General.Sample;
end;

procedure TMainForm.ListBoxColorsClick(Sender: TObject);
begin
  SetSelectedColor;
end;

procedure TMainForm.ListBoxHighlightersClick(Sender: TObject);
begin
  SetSelectedHighlighter;
end;

end.
