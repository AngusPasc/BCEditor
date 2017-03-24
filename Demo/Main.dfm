object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'BCEditor Demo'
  ClientHeight = 530
  ClientWidth = 706
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 106
  TextHeight = 14
  object SplitterVertical: TSplitter
    Left = 223
    Top = 0
    Height = 530
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 223
    Height = 530
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object SplitterHorizontal: TSplitter
      Left = 0
      Top = 390
      Width = 223
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object ListBoxColors: TListBox
      AlignWithMargins = True
      Left = 3
      Top = 393
      Width = 220
      Height = 134
      Margins.Top = 0
      Margins.Right = 0
      Align = alBottom
      ItemHeight = 14
      TabOrder = 0
      OnClick = ListBoxColorsClick
    end
    object ListBoxHighlighters: TListBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 220
      Height = 387
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      ItemHeight = 14
      TabOrder = 1
      OnClick = ListBoxHighlightersClick
    end
  end
  object Editor: TBCEditor
    AlignWithMargins = True
    Left = 226
    Top = 3
    Width = 477
    Height = 524
    Margins.Left = 0
    ActiveLine.Indicator.Visible = False
    Align = alClient
    CodeFolding.Hint.Font.Charset = DEFAULT_CHARSET
    CodeFolding.Hint.Font.Color = clWindowText
    CodeFolding.Hint.Font.Height = -11
    CodeFolding.Hint.Font.Name = 'Courier New'
    CodeFolding.Hint.Font.Style = []
    CodeFolding.Hint.Indicator.Glyph.Visible = False
    CompletionProposal.CloseChars = '()[]. '
    CompletionProposal.Columns = <
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Items = <>
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -11
        Title.Font.Name = 'Courier New'
        Title.Font.Style = []
      end>
    CompletionProposal.Trigger.Chars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    LeftMargin.Font.Charset = DEFAULT_CHARSET
    LeftMargin.Font.Color = 13408665
    LeftMargin.Font.Height = -11
    LeftMargin.Font.Name = 'Courier New'
    LeftMargin.Font.Style = []
    Minimap.Font.Charset = DEFAULT_CHARSET
    Minimap.Font.Color = clWindowText
    Minimap.Font.Height = -1
    Minimap.Font.Name = 'Courier New'
    Minimap.Font.Style = []
    TabOrder = 1
    TokenInfo.Font.Charset = DEFAULT_CHARSET
    TokenInfo.Font.Color = clWindowText
    TokenInfo.Font.Height = -11
    TokenInfo.Font.Name = 'Courier New'
    TokenInfo.Font.Style = []
    TokenInfo.Title.Font.Charset = DEFAULT_CHARSET
    TokenInfo.Title.Font.Color = clWindowText
    TokenInfo.Title.Font.Height = -11
    TokenInfo.Title.Font.Name = 'Courier New'
    TokenInfo.Title.Font.Style = []
  end
end
