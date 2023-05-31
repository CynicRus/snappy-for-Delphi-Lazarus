object Form1: TForm1
  Left = 192
  Top = 125
  Caption = 'Google Snappy Delphi 7 Demo'
  ClientHeight = 394
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 689
    Height = 345
    Lines.Strings = (
      '')
    TabOrder = 0
  end
  object CompressBtn: TButton
    Left = 16
    Top = 360
    Width = 113
    Height = 25
    Caption = 'CompressBtn'
    TabOrder = 1
    OnClick = CompressBtnClick
  end
  object DecompressBtn: TButton
    Left = 136
    Top = 360
    Width = 137
    Height = 25
    Caption = 'DecompressBtn'
    TabOrder = 2
    OnClick = DecompressBtnClick
  end
  object CompressByStreamBtn: TButton
    Left = 280
    Top = 360
    Width = 185
    Height = 25
    Caption = 'CompressByStreamBtn'
    TabOrder = 3
    OnClick = CompressByStreamBtnClick
  end
  object DecompressByStreamBtn: TButton
    Left = 472
    Top = 360
    Width = 209
    Height = 25
    Caption = 'DecompressByStreamBtn'
    TabOrder = 4
    OnClick = DecompressByStreamBtnClick
  end
  object OpenFileDialog: TOpenDialog
    Left = 112
    Top = 104
  end
  object saveFileDialog: TSaveDialog
    Left = 216
    Top = 120
  end
end
