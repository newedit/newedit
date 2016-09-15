inherited MainForm: TMainForm
  Caption = ''
  ClientHeight = 644
  ClientWidth = 1100
  Color = clWhite
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TBCSplitter [0]
    Left = 754
    Top = 0
    Height = 625
    Align = alRight
    SkinData.SkinSection = 'SPLITTER'
  end
  inherited StatusBar: TBCStatusBar
    Top = 625
    Width = 1100
    Panels = <
      item
        Alignment = taCenter
        Width = 86
      end
      item
        Width = 86
      end
      item
        Width = 86
      end
      item
        Width = 50
      end>
  end
  object PanelProperty: TBCPanel [2]
    Left = 760
    Top = 0
    Width = 340
    Height = 625
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alRight
    BevelOuter = bvNone
    DoubleBuffered = False
    ParentColor = True
    ParentDoubleBuffered = False
    TabOrder = 1
    SkinData.SkinSection = 'TRANSPARENT'
    object ObjectInspectorEh: TObjectInspectorEh
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 334
      Height = 615
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 6
      Margins.Bottom = 5
      Align = alClient
      Options = [goVertLineEh, goEditingEh, goAlwaysShowEditorEh]
    end
  end
  object PanelLeft: TBCPanel [3]
    AlignWithMargins = True
    Left = 6
    Top = 0
    Width = 748
    Height = 625
    Margins.Left = 6
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    SkinData.SkinSection = 'TRANSPARENT'
    object Editor: TBCEditor
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 748
      Height = 593
      Cursor = crIBeam
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 0
      ActiveLine.Indicator.Visible = False
      Align = alClient
      Caret.MultiEdit.Enabled = True
      Caret.NonBlinking.Enabled = False
      Caret.Options = []
      CodeFolding.Colors.Indent = clBlack
      CodeFolding.Hint.Font.Charset = DEFAULT_CHARSET
      CodeFolding.Hint.Font.Color = clWindowText
      CodeFolding.Hint.Font.Height = -11
      CodeFolding.Hint.Font.Name = 'Courier New'
      CodeFolding.Hint.Font.Style = []
      CompletionProposal.CloseChars = '()[]. '
      CompletionProposal.Columns = <
        item
        end>
      CompletionProposal.Font.Charset = DEFAULT_CHARSET
      CompletionProposal.Font.Color = clWindowText
      CompletionProposal.Font.Height = -11
      CompletionProposal.Font.Name = 'Courier New'
      CompletionProposal.Font.Style = []
      CompletionProposal.ShortCut = 16416
      CompletionProposal.Trigger.Chars = '.'
      CompletionProposal.Trigger.Enabled = False
      Ctl3D = True
      Directories.Colors = 'Colors'
      Directories.Highlighters = 'Highlighters'
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
      LeftMargin.Width = 55
      Lines.Strings = (
        '')
      LineSpacing = 0
      MatchingPair.Enabled = True
      Minimap.Font.Charset = DEFAULT_CHARSET
      Minimap.Font.Color = clWindowText
      Minimap.Font.Height = -4
      Minimap.Font.Name = 'Courier New'
      Minimap.Font.Style = []
      Minimap.Options = [moShowBookmarks]
      Minimap.Width = 140
      OnCaretChanged = EditorCaretChanged
      ParentCtl3D = False
      RightMargin.Position = 80
      RightMargin.Visible = True
      Scroll.Shadow.Visible = True
      Search.Enabled = False
      SpecialChars.EndOfLine.Visible = True
      SpecialChars.Selection.Visible = True
      SpecialChars.Style = scsDot
      SyncEdit.ShortCut = 24650
      TabOrder = 0
      WordWrap.Enabled = False
      WordWrap.Indicator.Glyph.Data = {
        7E030000424D7E0300000000000036000000280000000F0000000E0000000100
        2000000000004803000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
        000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
        0000FF00FF00FF00FF00FF00FF00FF00FF008000000080000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF008000000080000000800000008000000080000000FF00
        FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF008000000080000000FF00FF00FF00FF0080000000FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF0080000000FF00FF00FF00FF0080000000FF00FF00FF00
        FF00FF00FF000000000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00800000008000000080000000800000008000
        00008000000080000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00}
      WordWrap.Indicator.MaskColor = clFuchsia
      WordWrap.Style = wwsPageWidth
    end
    object PanelSearch: TBCPanel
      AlignWithMargins = True
      Left = 0
      Top = 601
      Width = 748
      Height = 21
      Margins.Left = 0
      Margins.Right = 0
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      SkinData.SkinSection = 'CHECKBOX'
      object BCSplitter1: TBCSplitter
        Left = 227
        Top = 0
        Height = 21
        SkinData.SkinSection = 'SPLITTER'
      end
      object SpeedButtonFindPrevious: TBCSpeedButton
        Left = 233
        Top = 0
        Width = 21
        Height = 21
        Action = ActionFindPrevious
        Align = alLeft
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 38
      end
      object SpeedButtonFindNext: TBCSpeedButton
        Left = 254
        Top = 0
        Width = 21
        Height = 21
        Action = ActionFindNext
        Align = alLeft
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 37
      end
      object SpeedButtonDivider: TBCSpeedButton
        AlignWithMargins = True
        Left = 275
        Top = 1
        Width = 10
        Height = 19
        Margins.Left = 0
        Margins.Top = 1
        Margins.Right = 0
        Margins.Bottom = 1
        Align = alLeft
        Flat = True
        ButtonStyle = tbsDivider
        SkinData.SkinSection = 'TOOLBUTTON'
        ImageIndex = 3
      end
      object SpeedButtonOptions: TBCSpeedButton
        Left = 337
        Top = 0
        Width = 21
        Height = 21
        Action = ActionOptions
        Align = alLeft
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 78
      end
      object SpeedButtonClose: TBCSpeedButton
        Left = 727
        Top = 0
        Width = 21
        Height = 21
        Action = ActionClose
        Align = alRight
        Flat = True
        Glyph.Data = {
          36060000424D3606000000000000360000002800000020000000100000000100
          18000000000000060000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF5B5B5B5B5B5BFF00FFFF00FFFF00FF5B5B5B5B5B5BFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171717171FF00FFFF
          00FFFF00FF717171717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF5B5B5B5B5B5B5B5B5B5B5B5BFF00FF5B5B5B5B5B5B5B5B5B5B5B5BFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFF717171FF
          00FF717171FFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFFFFFFFF71
          7171FFFFFFFFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF787878FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFF
          FFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5B5BFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFFFFFFFF71
          7171FFFFFFFFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FF5B5B5B5B5B5B5B5B5B5B5B5BFF00FF5B5B5B5B5B5B5B5B5B5B5B5BFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171FFFFFFFFFFFF717171FF
          00FF717171FFFFFFFFFFFF717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FF5B5B5B5B5B5BFF00FFFF00FFFF00FF5B5B5B5B5B5BFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF717171717171FF00FFFF
          00FFFF00FF717171717171FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        Margin = 0
        NumGlyphs = 2
        SkinData.SkinSection = 'CHECKBOX'
        Images = ImagesDataModule.ImageListSmall
      end
      object BCSpeedButton2: TBCSpeedButton
        Left = 285
        Top = 0
        Width = 21
        Height = 21
        Hint = 'Options'
        Align = alLeft
        Flat = True
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000979797C0A2A2
          A2FF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F
          9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFFA2A2A2FF979797C0A2A2A2FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA2A2A2FFA2A2A2FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA2A2A2FF9F9F9FFFFFFF
          FFFFFDFEFEFFFDFDFDFFFEFEFEFFFFFFFFFFFFFFFFFFFEFEFEFFFEFEFEFFFEFE
          FEFFFEFEFEFFFEFEFEFFFDFDFDFFFDFEFEFFFFFFFFFF9F9F9FFF9F9F9FFFFFFF
          FFFFFBF9F9FFFEFEFEFF403D3CFF4C4A49FF4C4A49FF413E3DFFFFFFFFFF8786
          85FF42403FFF868584FFFBFBFBFFFAF9F9FFFFFFFFFF9F9F9FFF9F9F9FFFFFFF
          FFFFF8F8F8FFF9F9F8FFFFFFFFFF535150FF535150FFFFFFFFFFFEFEFDFFFFFF
          FFFF575552FFFFFFFFFFF8F9F7FFF7F7F7FFFFFFFFFF9F9F9FFF9F9F9FFFFFFF
          FFFFF5F5F4FFF9F9F8FFFFFFFFFF535150FF535150FFFFFFFFFFFEFEFDFFFFFF
          FFFF575552FFFFFFFFFFF8F9F7FFF5F5F4FFFFFFFFFF9F9F9FFF9F9F9FFFFFFF
          FFFFF2F1F0FFF5F3F2FFFFFFFEFF686664FF686664FFFFFFFFFFFBFAF9FFFFFF
          FFFF73716FFFFFFFFEFFF8F7F6FFF4F3F2FFFFFFFFFF9F9F9FFF9F9F9FFFFFFF
          FFFFEFEFEEFFF1F1F0FFFBFCFBFF7C7A78FF7E7B79FFFFFFFFFF9A9A99FFFFFF
          FFFF6A6865FFFFFFFFFF989796FFF5F5F4FFFFFFFFFF9F9F9FFF9F9F9FFFFFFF
          FFFFEFEEEDFFF3F2F1FFFCFBFAFF757270FF767371FFFFFFFFFF514F4EFF5351
          50FF545150FF514E4DFF4A4847FFF4F3F2FFFFFFFFFF9F9F9FFF9F9F9FFFFFFF
          FFFFEFEFEEFF908E8EFFFFFFFFFF666463FF676463FFFFFFFFFF999798FFFCFC
          FBFFF8F8F7FFF6F6F5FFF3F3F2FFEDEDECFFFFFFFFFF9F9F9FFF9F9F9FFFFFFF
          FFFFEFEDECFF484645FF4F4C4BFF535150FF535150FF4F4D4CFF4A4847FFF2F0
          EFFFEBEAE9FFEAE9E8FFE9E8E7FFE8E7E5FFFFFFFFFF9F9F9FFF9F9F9FFFFFFF
          FFFFE7E7E6FFEDEDECFFF0F0EFFFF2F2F1FFF2F2F1FFF0F0EFFFEDEEEDFFE8E8
          E7FFE5E5E4FFE4E5E4FFE4E4E3FFE3E3E2FFFFFFFFFF9F9F9FFFA0A0A0FFFFFF
          FFFFE1E0DFFFE3E1E0FFE3E2E1FFE3E2E1FFE3E2E1FFE3E2E1FFE3E2E1FFE2E0
          DFFFE1E0DFFFE1E0DFFFE1E0DFFFE1DFDEFFFFFFFFFFA0A0A0FFA2A2A2FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA2A2A2FFA5A5A5EFA2A2
          A2FFA0A0A0FF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F
          9FFF9F9F9FFF9F9F9FFF9F9F9FFFA0A0A0FFA2A2A2FFA5A5A5EF}
        OnClick = ActionOptionsExecute
        ButtonStyle = tbsCheck
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
      end
      object BCSpeedButton3: TBCSpeedButton
        Left = 306
        Top = 0
        Width = 21
        Height = 21
        Hint = 'Options'
        Align = alLeft
        Flat = True
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000A4A4A2C0AFAF
          ADFFADADABFFADADABFFADADABFFADADABFFADADABFFADADABFFADADABFFADAD
          ABFFADADABFFADADABFFADADABFFADADABFFAFAFADFFA4A4A2C0AFAFADFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAFADFFAFAFADFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAFADFFAEAEABFFFFFF
          FFFFFFFFFFFFB05800FFB15B05FFB15A04FFB05A03FFB15A04FFB05A03FFB05A
          03FFB15A04FFB15B04FFB05700FFFFFFFFFFFFFFFFFFAEAEABFFADADABFFFFFF
          FFFFFFFFFFFFB15B04FFEBD1B6FFE9CEB1FFE8CDAFFFE9CEB1FFE8CDAFFFE8CD
          AFFFE9CEB1FFE9CFB5FFAE5A04FFFCFCFCFFFFFFFFFFADADABFFADADABFFFFFF
          FFFFFEFFFFFF3B1500FFA77C59FFA37753FFA17450FFE5C7A6FF4C2E14FFA477
          52FFA17450FFE4C9ACFFAD5804FFF9F9F9FFFFFFFFFFADADABFFADADABFFFFFF
          FFFFFCFCFDFFB15B06FFE7CBAAFFE5C6A1FFE4C49FFFE5C6A1FFE4C49FFFE4C4
          9FFFE5C6A1FFE5CAA9FFAE5804FFF8F7F7FFFFFFFFFFADADABFFADADABFFFFFF
          FFFFF9FBFAFF3C1601FFA97D57FFA47952FFA47751FFA37851FFA37550FFE3C6
          A1FF4E3016FFA67A54FF7C3502FFF5F6F6FFFFFFFFFFADADABFFADADABFFFFFF
          FFFFF6F6F7FFB15D09FFDAB384FFD8AF81FFD7AD7FFFD7AD80FFD9AE7FFFD7AE
          7FFFD9AF81FFD7AE81FFAA5807FFF2F2F2FFFFFFFFFFADADABFFADADABFFFFFF
          FFFFF3F5F5FF3D1601FF803A05FF7F3805FF7E3805FFAC5B0AFF3D1701FF8039
          05FF7E3805FFA95909FFA45201FFEEEEEDFFFFFFFFFFADADABFFADADABFFFFFF
          FFFFF1F0F0FFF7F9FAFFF7FAFCFFF5F8FAFFF4F6F8FFF4F6F7FFF5F8F9FFF5F7
          F8FFF5F7F9FFF2F4F5FFEFF0F0FFECECEBFFFFFFFFFFADADABFFADADABFFFFFF
          FFFFEDEDEEFF583F23FFB79A81FFB59880FFB4987FFFB4977EFFB4967CFFF1F2
          F5FF583F24FFB6997FFFB2947AFFEAEAEBFFFFFFFFFFADADABFFAEAEABFFFFFF
          FFFFE8E7E7FFECECEDFFECEDEDFFEBECEDFFEAECECFFEAEBECFFEAEBEBFFEAEA
          EAFFECEDEDFFECECEDFFE9EAEAFFE7E6E5FFFFFFFFFFAEAEABFFAEAEACFFFFFF
          FFFFE2E0DFFFE3E2E1FFE3E2E2FFE3E2E1FFE3E2E1FFE3E2E1FFE2E2E1FFE3E2
          E1FFE3E2E1FFE3E2E1FFE2E1E0FFE1E0DFFFFFFFFFFFAEAEACFFAFAFADFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAFADFFB3B3B1EFB0B0
          ADFFAEAEACFFAEAEABFFAEAEABFFAEAEABFFAEAEABFFAEAEABFFAEAEABFFAEAE
          ABFFAEAEABFFAEAEABFFAEAEABFFAEAEACFFB0B0ADFFB3B3B1EF}
        OnClick = ActionOptionsExecute
        ButtonStyle = tbsCheck
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
      end
      object BCSpeedButton1: TBCSpeedButton
        AlignWithMargins = True
        Left = 327
        Top = 1
        Width = 10
        Height = 19
        Margins.Left = 0
        Margins.Top = 1
        Margins.Right = 0
        Margins.Bottom = 1
        Align = alLeft
        Flat = True
        ButtonStyle = tbsDivider
        SkinData.SkinSection = 'TOOLBUTTON'
        ImageIndex = 3
      end
      object BCSpeedButton4: TBCSpeedButton
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 21
        Height = 21
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 6
        Margins.Bottom = 0
        Action = ActionSearchEngine
        Align = alLeft
        Flat = True
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageListSmall
        ImageIndex = 34
      end
      object ComboBoxSearchText: TBCComboBox
        Left = 27
        Top = 0
        Width = 200
        Height = 21
        Hint = 'Search text'
        Align = alLeft
        Alignment = taLeftJustify
        BoundLabel.Indent = 4
        BoundLabel.Layout = sclTopLeft
        SkinData.SkinSection = 'COMBOBOX'
        VerticalAlignment = taAlignTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemIndex = -1
        ParentFont = False
        TabOrder = 0
        OnChange = ComboBoxSearchTextChange
        OnKeyPress = ComboBoxSearchTextKeyPress
        UseMouseWheel = False
      end
      object PanelRight: TBCPanel
        Left = 358
        Top = 0
        Width = 369
        Height = 21
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        SkinData.SkinSection = 'CHECKBOX'
        object LabelSearchResultCount: TsLabel
          AlignWithMargins = True
          Left = 362
          Top = 0
          Width = 4
          Height = 21
          Margins.Left = 0
          Margins.Top = 0
          Margins.Bottom = 0
          Align = alRight
          ParentFont = False
          Layout = tlCenter
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
        end
      end
    end
  end
  inherited SkinManager: TBCSkinManager
    Effects.AllowGlowing = False
    IsDefault = True
    MenuSupport.UseExtraLine = False
    SkinInfo = 'N/A'
    ThirdParty.ThirdEdits = ' '#13#10'TBCEditorPrintPreview'#13#10
    ThirdParty.ThirdVirtualTrees = 'TVirtualDrawTree'#13#10
    Left = 166
    Top = 26
  end
  inherited TitleBar: TBCTitleBar
    Items = <
      item
        Caption = 'File'
        DropdownMenu = PopupMenuFile
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 0
        Name = 'TitleBarItemFile'
        ShowHint = True
        Style = bsMenu
        Width = 48
      end
      item
        Align = tbaCenterInSpace
        Caption = 'TBCEditor Control Demo v1.1'
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 21
        Index = 1
        Name = 'TitleBarItemCaption'
        ShowHint = False
        Style = bsInfo
        Width = 161
      end
      item
        Align = tbaRight
        Caption = 'Object Pascal'
        DropdownMenu = PopupMenuDummy
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 2
        Name = 'TitleBarItemHighlighter'
        ShowHint = False
        Style = bsMenu
        Width = 101
        OnClick = TitleBarItems4Click
      end
      item
        Align = tbaRight
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Index = 3
        Name = 'TitleBarItemSpacing2'
        ShowHint = False
        Style = bsSpacing
        Width = 6
      end
      item
        Align = tbaRight
        Caption = 'Default'
        DropdownMenu = PopupMenuDummy
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 4
        Name = 'TitleBarItemColors'
        ShowHint = False
        Style = bsMenu
        Width = 68
        OnClick = TitleBarItems6Click
      end
      item
        Align = tbaRight
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Index = 5
        Name = 'TitleBarItemSpacing3'
        ShowHint = False
        Style = bsSpacing
        Width = 2
      end>
    Left = 88
    Top = 22
  end
  inherited SkinProvider: TsSkinProvider
    Left = 248
    Top = 26
  end
  inherited ApplicationEvents: TApplicationEvents
    OnMessage = ApplicationEventsMessage
    Left = 88
    Top = 88
  end
  inherited ActionList: TActionList
    Images = ImagesDataModule.ImageListSmall
    Left = 164
    Top = 88
    object ActionSearch: TAction
      Caption = 'ActionSearch'
      ShortCut = 16454
      OnExecute = ActionSearchExecute
    end
    object ActionFileOpen: TAction
      Caption = 'Open...'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = ActionFileOpenExecute
    end
    object ActionPreview: TAction
      Caption = 'Print preview...'
      ImageIndex = 10
      OnExecute = ActionPreviewExecute
    end
    object ActionSkins: TAction
      Caption = 'Skins...'
      ImageIndex = 76
      OnExecute = ActionSkinsExecute
    end
    object ActionFindNext: TAction
      Hint = 'Find next'
      ImageIndex = 37
      ShortCut = 114
      OnExecute = ActionFindNextExecute
    end
    object ActionFindPrevious: TAction
      Hint = 'Find previous'
      ImageIndex = 38
      ShortCut = 8306
      OnExecute = ActionFindPreviousExecute
    end
    object ActionOptions: TAction
      Hint = 'Options'
      ImageIndex = 78
      OnExecute = ActionOptionsExecute
    end
    object ActionClose: TAction
      Hint = 'Close'
      OnExecute = ActionCloseExecute
    end
    object ActionSearchEngine: TAction
      Hint = 'Select search engine'
      ImageIndex = 34
      OnExecute = ActionSearchEngineExecute
    end
    object ActionCaseSensitive: TAction
      Caption = 'ActionCaseSensitive'
    end
    object ActionInSelection: TAction
      Hint = 'In selection'
    end
  end
  inherited MainMenu: TMainMenu
    Left = 81
    Top = 282
  end
  object PopupMenuFile: TPopupMenu
    Images = ImagesDataModule.ImageList
    Left = 84
    Top = 164
    object MenuItemFileOpen: TMenuItem
      Action = ActionFileOpen
      RadioItem = True
    end
    object MenuItemSeparator1: TMenuItem
      Caption = '-'
    end
    object MenuItemPrintPreview: TMenuItem
      Action = ActionPreview
      RadioItem = True
    end
    object MenuItemSkins: TMenuItem
      Action = ActionSkins
    end
    object MenuItemSeparator2: TMenuItem
      Caption = '-'
    end
    object MenuItemExit: TMenuItem
      Action = ActionFileExit
    end
  end
  object PopupMenuDummy: TPopupMenu
    Left = 84
    Top = 220
  end
  object MultiStringHolderFileTypes: TBCMultiStringHolder
    MultipleStrings = <
      item
        Name = 'Assembler (68HC11)'
        Strings.Strings = (
          '.asm')
      end
      item
        Name = 'AutoIt v3'
        Strings.Strings = (
          '.au3')
      end
      item
        Name = 'AWK'
        Strings.Strings = (
          '.awk')
      end
      item
        Name = 'C#'
        Strings.Strings = (
          '.cs')
      end
      item
        Name = 'C++'
        Strings.Strings = (
          '.c;.cpp;.h;.hpp')
      end
      item
        Name = 'CSS'
        Strings.Strings = (
          '.css')
      end
      item
        Name = 'Delphi Form Module'
        Strings.Strings = (
          '.dfm')
      end
      item
        Name = 'HTML with Scripts'
        Strings.Strings = (
          '.htm;.html')
      end
      item
        Name = 'Java'
        Strings.Strings = (
          '.java')
      end
      item
        Name = 'JavaScript'
        Strings.Strings = (
          '.js')
      end
      item
        Name = 'JSON'
        Strings.Strings = (
          '.json')
      end
      item
        Name = 'MS-DOS Batch'
        Strings.Strings = (
          '.bat')
      end
      item
        Name = 'Object Pascal'
        Strings.Strings = (
          '.pas;.dpr')
      end
      item
        Name = 'Perl'
        Strings.Strings = (
          '.pl')
      end
      item
        Name = 'PHP'
        Strings.Strings = (
          '.php')
      end
      item
        Name = 'Python'
        Strings.Strings = (
          '.py')
      end
      item
        Name = 'SQL (Standard)'
        Strings.Strings = (
          '.sql')
      end
      item
        Name = 'Visual Basic'
        Strings.Strings = (
          '.vb')
      end
      item
        Name = 'XML'
        Strings.Strings = (
          '.xml')
      end>
    Left = 344
    Top = 114
  end
  object OpenDialog: TsOpenDialog
    Left = 342
    Top = 52
  end
end
