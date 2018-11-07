object SortFm: TSortFm
  Left = 0
  Top = 0
  Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1089#1083#1080#1103#1085#1080#1077#1084
  ClientHeight = 471
  ClientWidth = 934
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    934
    471)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 419
    Width = 84
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1048#1089#1093#1086#1076#1085#1099#1081' '#1092#1072#1081#1083':'
  end
  object Label2: TLabel
    Left = 36
    Top = 444
    Width = 57
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090':'
  end
  object btnStartStop: TButton
    Left = 864
    Top = 415
    Width = 61
    Height = 48
    Anchors = [akLeft, akBottom]
    Caption = #1057#1090#1072#1088#1090
    Default = True
    TabOrder = 0
    OnClick = btnStartStopClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 225
    Height = 377
    Anchors = [akLeft, akTop, akBottom]
    ReadOnly = True
    TabOrder = 5
  end
  object pb: TProgressBar
    Left = 8
    Top = 391
    Width = 918
    Height = 17
    Anchors = [akLeft, akBottom]
    Max = 1000
    TabOrder = 6
  end
  object Memo2: TMemo
    Left = 239
    Top = 8
    Width = 225
    Height = 377
    Anchors = [akLeft, akTop, akBottom]
    ReadOnly = True
    TabOrder = 7
  end
  object Memo3: TMemo
    Left = 470
    Top = 8
    Width = 225
    Height = 377
    Anchors = [akLeft, akTop, akBottom]
    ReadOnly = True
    TabOrder = 8
  end
  object Memo4: TMemo
    Left = 701
    Top = 8
    Width = 225
    Height = 377
    Anchors = [akLeft, akTop, akBottom]
    ReadOnly = True
    TabOrder = 9
  end
  object edInputFn: TEdit
    Left = 97
    Top = 416
    Width = 731
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
  end
  object edOutputFn: TEdit
    Left = 97
    Top = 441
    Width = 731
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
  end
  object btnSelectInputFile: TBitBtn
    Left = 833
    Top = 415
    Width = 23
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '...'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = btnSelectInputFileClick
  end
  object btnSelectOutputFile: TBitBtn
    Left = 833
    Top = 440
    Width = 23
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '...'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = btnSelectOutputFileClick
  end
  object OpenDialog: TOpenDialog
    Filter = #1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 280
    Top = 128
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'txt'
    FileName = 'Output.txt'
    Filter = #1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 352
    Top = 128
  end
end
