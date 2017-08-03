object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 442
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LblSecurities: TLabel
    Left = 32
    Top = 16
    Width = 46
    Height = 13
    Caption = 'Securities'
  end
  object DateStart: TLabel
    Left = 184
    Top = 51
    Width = 24
    Height = 13
    Caption = 'Start'
  end
  object DateEnd: TLabel
    Left = 184
    Top = 78
    Width = 18
    Height = 13
    Caption = 'End'
  end
  object Run: TButton
    Left = 408
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Run Quandl'
    TabOrder = 0
    OnClick = RunClick
  end
  object ResultsDisplay: TMemo
    Left = 0
    Top = 144
    Width = 563
    Height = 298
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object SecuritiesList: TEdit
    Left = 104
    Top = 16
    Width = 281
    Height = 21
    Hint = 'List securities with comma separators'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = 'COF,GOOGL,MSFT'
  end
  object MaxDailyProfit: TCheckBox
    Left = 32
    Top = 62
    Width = 97
    Height = 17
    Hint = 
      'Highest amount of profit possible within date range if purchased' +
      ' on same day'#39's high and low.'
    Caption = 'Max Daily Profit'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object BusyDay: TCheckBox
    Left = 32
    Top = 85
    Width = 97
    Height = 17
    Hint = 
      'Days that generated volume at least 10% more than overall averag' +
      'e volume.'
    Caption = 'Busy Day'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
  end
  object BiggestLoser: TCheckBox
    Left = 32
    Top = 108
    Width = 97
    Height = 17
    Hint = 
      'Security which had the most days where the closing was lower tha' +
      'n the opening price.'
    Caption = 'Biggest Loser'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
  end
  object StartPick: TDateTimePicker
    Left = 232
    Top = 43
    Width = 153
    Height = 21
    Date = 42736.000000000000000000
    Time = 42736.000000000000000000
    TabOrder = 6
  end
  object EndPick: TDateTimePicker
    Left = 232
    Top = 70
    Width = 153
    Height = 21
    Date = 42916.000000000000000000
    Time = 42916.000000000000000000
    TabOrder = 7
  end
  object MonthlyAvg: TCheckBox
    Left = 32
    Top = 39
    Width = 97
    Height = 17
    Hint = 
      'Average monthly open and close prices for each security for each' +
      ' month of data in the date range requested.'
    Caption = 'Monthly Average'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
  end
  object NetHTTPRequest1: TNetHTTPRequest
    Asynchronous = False
    ConnectionTimeout = 60000
    ResponseTimeout = 60000
    URL = 
      'https://www.quandl.com/api/v3/datatables/WIKI/PRICES.json?date=2' +
      '0160912&api_key=s-GMZ_xkw6CrkGYUWs1p'
    Client = NetHTTPClient1
    Left = 528
    Top = 400
  end
  object NetHTTPClient1: TNetHTTPClient
    Asynchronous = False
    ConnectionTimeout = 60000
    ResponseTimeout = 60000
    AllowCookies = True
    HandleRedirects = True
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 488
    Top = 400
  end
end
