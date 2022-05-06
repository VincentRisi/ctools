object FormMain: TFormMain
  Left = 536
  Top = 113
  Width = 640
  Height = 612
  Caption = 'CHRISTENSEN Stock Item Picture Server'
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010002002020100000000000E80200002600000010101000000000002801
    00000E0300002800000020000000400000000100040000000000800200000000
    0000000000000000000000000000000000000000800000800000008080008000
    0000800080008080000080808000C0C0C0000000FF0000FF000000FFFF00FF00
    0000FF00FF00FFFF0000FFFFFF00000000000000017000000000000000000000
    0000010088007700000000000000000000000007887000700000000000000000
    0000008888888700100000000000000000010788888888770010000000000000
    0070078888888888870000000000000000007888888888888877070000000000
    000788888888888888FF870100000000000088888888888888FFFF7070000000
    000888888888888888FFFFF700000000007888877787888888FFFFF700000000
    30700007007777888FFFFFF80010000000770000000000008FFFFFFF87000000
    007870000000007FFFFFFFFFF000000000088700000008FFFFFFFFFF80100000
    000788008FFFFFFFFFFFFFFF70000000010788708FFFFFFFFFFFFFFF00000000
    0077888008F778FF8FFFFFF7000000000088007008800770077FFFF710000000
    000770007FF70000078888070000000001078877788770007888877000000000
    0000000777770000000000701000000007077000000000000077777000000000
    70007870077888FFFF7077007000000070007880078888FFFF00787007000000
    031007870788888FFF70078870300000000007880788888FFF80007870000000
    000030078888888FF80077007700000000000100788888887007000007030000
    0000000000000000100000007003000000000000030143030000000003030000
    0000000000000000000000000010000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000280000001000000020000000010004000000
    0000C00000000000000000000000000000000000000000000000000080000080
    00000080800080000000800080008080000080808000C0C0C0000000FF0000FF
    000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0077777787777777780000
    00800000000700000F8F80000007000078888F8000070001F88888FF80070008
    8888888FF7070007000007FFFF07000770008FFFFF070007F0FFFFFFF8070008
    7087007FF707000077870077700700707000777670070000877FFF8087170000
    08788FF017070000007777000008000000000000000800000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    00000000000000000000000000000000000000000000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBarMain: TStatusBar
    Left = 0
    Top = 566
    Width = 632
    Height = 19
    Panels = <>
  end
  object PageControlMain: TPageControl
    Left = 0
    Top = 0
    Width = 632
    Height = 566
    ActivePage = BrowserTab
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    object BrowserTab: TTabSheet
      Caption = 'Browser'
      object BrowserPanel: TPanel
        Left = 0
        Top = 345
        Width = 624
        Height = 190
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          624
          190)
        object ItemsLabel: TLabel
          Left = 8
          Top = 16
          Width = 27
          Height = 13
          Caption = 'Items'
        end
        object UrlEdit: TEdit
          Left = 56
          Top = 32
          Width = 558
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 1
          Text = 
            'http://www.christensen.co.za/uploads/product-images/ck-3789h-120' +
            '8434014.jpg'
        end
        object PicMemo: TMemo
          Left = 0
          Top = 56
          Width = 622
          Height = 137
          TabStop = False
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
        object StockCombo: TComboBox
          Left = 56
          Top = 8
          Width = 558
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
          OnChange = StockComboChange
        end
      end
      object PicWebBrowser: TWebBrowser
        Left = 0
        Top = 0
        Width = 624
        Height = 345
        Align = alClient
        TabOrder = 1
        ControlData = {
          4C0000007E400000A82300000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
    object OptionsTab: TTabSheet
      Caption = 'Options'
      ImageIndex = 2
      object CSVLabel: TLabel
        Left = 8
        Top = 24
        Width = 67
        Height = 13
        Caption = 'CSV Stock File'
      end
      object PortLabel: TLabel
        Left = 24
        Top = 48
        Width = 20
        Height = 13
        Caption = 'Port'
      end
      object HostLabel: TLabel
        Left = 24
        Top = 80
        Width = 22
        Height = 13
        Caption = 'Host'
      end
      object CSVEdit: TEdit
        Left = 96
        Top = 16
        Width = 345
        Height = 21
        TabOrder = 0
        Text = 'C:\ctools\dev\source\pascal\data\products.csv'
      end
      object LoadCSVButton: TButton
        Left = 456
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Load'
        TabOrder = 1
        OnClick = LoadCSVButtonClick
      end
      object PortEdit: TEdit
        Left = 96
        Top = 48
        Width = 65
        Height = 21
        TabOrder = 2
        Text = '32145'
        OnChange = PortEditChange
      end
      object HostEdit: TEdit
        Left = 96
        Top = 80
        Width = 121
        Height = 21
        TabOrder = 3
        Text = 'localhost'
        OnChange = HostEditChange
      end
      object OpenButton: TButton
        Left = 8
        Top = 112
        Width = 75
        Height = 25
        Caption = 'OpenServer'
        TabOrder = 4
        OnClick = OpenButtonClick
      end
      object CloseButton: TButton
        Left = 96
        Top = 112
        Width = 75
        Height = 25
        Caption = 'CloseServer'
        Enabled = False
        TabOrder = 5
        OnClick = CloseButtonClick
      end
    end
    object LogTab: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      object LogMemo: TMemo
        Left = 0
        Top = 0
        Width = 603
        Height = 557
        Align = alClient
        TabOrder = 0
      end
      object LogPanel: TPanel
        Left = 0
        Top = 557
        Width = 603
        Height = 41
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
  end
  object PicTcpServer: TTcpServer
    OnAccept = PicTcpServerAccept
    OnListening = PicTcpServerListening
    OnCreateHandle = PicTcpServerCreateHandle
    OnDestroyHandle = PicTcpServerDestroyHandle
    Left = 188
    Top = 72
  end
  object ShowTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = ShowTimerTimer
    Left = 236
    Top = 75
  end
end
