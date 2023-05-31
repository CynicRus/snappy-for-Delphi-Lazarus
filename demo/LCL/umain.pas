unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DateUtils, LazFileUtils,FileUtil,libsnappy
;

type

  { TForm1 }

  TForm1 = class(TForm)
    CompressBtn: TButton;
    DecompressBtn: TButton;
    CompressByStream: TButton;
    DecompressByStreamBtn: TButton;
    Memo1: TMemo;
    openFileDialog: TOpenDialog;
    saveFileDialog: TSaveDialog;
    procedure CompressBtnClick(Sender: TObject);
    procedure CompressByStreamBtnClick(Sender: TObject);
    procedure DecompressBtnClick(Sender: TObject);
    procedure DecompressByStreamBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public
    function SnappyCompress(InputFile, OutputFile: string): TSnappyStatus;
    function SnappyDecompress(InputFile, OutputFile: string): TSnappyStatus;
    procedure SnappyCompressByStream(InputFile, OutputFile: string);
    procedure SnappyDecompressByStream(InputFile, OutputFile: string);

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFNDEF SNAPPY_STATIC_INIT}
  InitSnappy();
{$ENDIF}
end;

procedure TForm1.CompressBtnClick(Sender: TObject);
var
  compressedFile: string;
  originalFile: string;
  currentStatus: TSnappyStatus;
   StartTime,StopTime: TDateTime;
begin
  openFileDialog.DefaultExt := '';
  openFileDialog.Filter := 'All files(*.*)|*.*';
  if openFileDialog.Execute() then
  begin
    originalFile := openFileDialog.FileName;
    saveFileDialog.FileName := ExtractFileNameWithoutExt(originalFile);
  end
  else
    exit;
  Memo1.Lines.Add(Format('File: %s', [originalFile]));
  Memo1.Lines.Add(Format('Original size:%d', [FileSize(originalFile)]));
  saveFileDialog.DefaultExt := 'snappy';
  saveFileDialog.Filter := 'Snappy compressed file(*.snappy)|*.snappy';
  if saveFileDialog.Execute() then
  begin
    compressedFile := saveFileDialog.FileName;
    if not(originalFile.IsEmpty()) then
    begin
      StartTime := Now;
      currentStatus := SnappyCompress(originalFile, compressedFile);
      StopTime := Now;
      if (currentStatus <> SNAPPY_OK) then
      begin
        case currentStatus of
          SNAPPY_INVALID_INPUT:
            Memo1.Lines.Add('Snappy failed with code: invalid input!');
          SNAPPY_BUFFER_TOO_SMALL:
            Memo1.Lines.Add('Snappy failed with code: buffer to small!');
        end;
      end
      else
      begin
        Memo1.Lines.Add('*************Successfully!*****************');
        Memo1.Lines.Add(Format('Compressed file: %s', [compressedFile]));
        Memo1.Lines.Add(Format('Compressed size: %d',
          [FileSize(compressedFile)]));
        Memo1.Lines.Add(Format('Elapsed time: %d ms',
          [MilliSecondsBetween(StopTime,StartTime)]));
      end;
    end;

  end;

end;

procedure TForm1.CompressByStreamBtnClick(Sender: TObject);
var
  compressedFile: string;
  originalFile: string;
  currentStatus: TSnappyStatus;
   StartTime,StopTime: TDateTime;
begin
  openFileDialog.DefaultExt := '';
  openFileDialog.Filter := 'All files(*.*)|*.*';
  if openFileDialog.Execute() then
  begin
    originalFile := openFileDialog.FileName;
    saveFileDialog.FileName := ExtractFileNameWithoutExt(originalFile);
  end
  else
    exit;
  Memo1.Lines.Add(Format('File: %s', [originalFile]));
  Memo1.Lines.Add(Format('Original size:%d', [FileSize(originalFile)]));
  saveFileDialog.DefaultExt := 'snappy';
  saveFileDialog.Filter := 'Snappy compressed file(*.snappy)|*.snappy';
  if saveFileDialog.Execute() then
  begin
    compressedFile := saveFileDialog.FileName;
    if not(originalFile.IsEmpty()) then
    begin
      StartTime := Now;
      SnappyCompressByStream(originalFile, compressedFile);
      StopTime := Now;
      Memo1.Lines.Add('*************Successfully!*****************');
      Memo1.Lines.Add(Format('Compressed file: %s', [compressedFile]));
      Memo1.Lines.Add(Format('Compressed size: %d',
        [FileSize(compressedFile)]));
        Memo1.Lines.Add(Format('Elapsed time: %d ms',
      [MilliSecondsBetween(StopTime,StartTime)]));
    end;
  end;
end;

procedure TForm1.DecompressBtnClick(Sender: TObject);
var
  compressedFile: string;
  originalFile: string;
  currentStatus: TSnappyStatus;
   StartTime,StopTime: TDateTime;
begin
  openFileDialog.DefaultExt := 'snappy';
  openFileDialog.Filter := 'Snappy compressed file(*.snappy)|*.snappy';
  if openFileDialog.Execute() then
  begin
    compressedFile := openFileDialog.FileName;
    saveFileDialog.FileName := ExtractFileNameWithoutExt(compressedFile);
  end
  else
    exit;
  Memo1.Lines.Add(Format('File: %s', [compressedFile]));
  Memo1.Lines.Add(Format('Compressed size:%d ',
    [FileSize(compressedFile)]));
  saveFileDialog.DefaultExt := '';
  saveFileDialog.Filter := 'All files(*.*)';
  if saveFileDialog.Execute() then
  begin
    originalFile := saveFileDialog.FileName;
    if not(originalFile.IsEmpty()) then
    begin
      StartTime := Now;
      SnappyDecompress(compressedFile, originalFile);
      StopTime := Now;
      if (currentStatus <> SNAPPY_OK) then
      begin
        case currentStatus of
          SNAPPY_INVALID_INPUT:
            Memo1.Lines.Add('Snappy failed with code: invalid input!');
          SNAPPY_BUFFER_TOO_SMALL:
            Memo1.Lines.Add('Snappy failed with code: buffer to small!');
        end;
      end
      else
      begin
        Memo1.Lines.Add('*************Successfully!*****************');
        Memo1.Lines.Add(Format('Decompressed file: %s', [originalFile]));
        Memo1.Lines.Add(Format('Decompressed size: %d ',
        [FileSize(originalFile)]));
        Memo1.Lines.Add(Format('Elapsed time: %d ms',
      [MilliSecondsBetween(StopTime,StartTime)]));
      end;
    end;

  end;

end;

procedure TForm1.DecompressByStreamBtnClick(Sender: TObject);
var
  compressedFile: string;
  originalFile: string;
   StartTime,StopTime: TDateTime;
begin
  openFileDialog.DefaultExt := 'snappy';
  openFileDialog.Filter := 'Snappy compressed file(*.snappy)|*.snappy';
  if openFileDialog.Execute() then
  begin
    compressedFile := openFileDialog.FileName;
    saveFileDialog.FileName := ExtractFileNameWithoutExt
      (compressedFile);
  end
  else
    exit;
  Memo1.Lines.Add(Format('File: %s', [compressedFile]));
  Memo1.Lines.Add(Format('Compressed size:%d ',
    [FileSize(compressedFile)]));
  saveFileDialog.DefaultExt := '';
  saveFileDialog.Filter := 'All files(*.*)';
  if saveFileDialog.Execute() then
  begin
    originalFile := saveFileDialog.FileName;
    if not(originalFile.IsEmpty()) then
    begin

      StartTime := Now;
      SnappyDecompressByStream(compressedFile, originalFile);
      StopTime := Now;
      Memo1.Lines.Add('*************Successfully!*****************');
      Memo1.Lines.Add(Format('Decompressed file: %s', [originalFile]));
      Memo1.Lines.Add(Format('Decompressed size: %d ',
      [FileSize(originalFile)]));
        Memo1.Lines.Add(Format('Elapsed time: %d ms',
      [MilliSecondsBetween(StopTime,StartTime)]));

    end;

  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
{$IFNDEF SNAPPY_STATIC_INIT}
  FreeSnappy();
{$ENDIF}
end;

function TForm1.SnappyCompress(InputFile, OutputFile: string): TSnappyStatus;
var
  fs: TFileStream;
  compressedFS: TFileStream;
  fileBuff: array of byte;
  compressedBuff: array of byte;
  compressedPtr: PByte;
  MaxLen: cardinal;
  S: TSnappyStatus;
begin
  S := SNAPPY_OK;
  MaxLen := 0;
  fs := TFileStream.Create(InputFile, fmOpenRead);
  compressedFS := TFileStream.Create(OutputFile, fmCreate or fmOpenWrite);
  try
    SetLength(fileBuff, fs.Size);
    fs.Seek(0, soFromBeginning);
    fs.Read(Pointer(@fileBuff[0])^, Length(fileBuff));
    MaxLen := snappy_max_compressed_length(Length(fileBuff));
    SetLength(compressedBuff, MaxLen);
    compressedPtr := PByte(@compressedBuff[0]);
    S := snappy_compress(@fileBuff[0], Length(fileBuff), compressedPtr,
      @(MaxLen));
    if (S <> SNAPPY_OK) then
    begin
      result := S;
      exit;
    end;
    S := snappy_validate_compressed_buffer(@compressedBuff[0], MaxLen);
    if (S <> SNAPPY_OK) then
    begin
      result := S;
      exit;
    end
    else
      compressedFS.Write(Pointer(@compressedBuff[0])^, MaxLen)

  finally
    result := S;
    fs.Free;
    compressedFS.Free;
  end;

end;

function TForm1.SnappyDecompress(InputFile, OutputFile: string): TSnappyStatus;
var
  fs: TFileStream;
  decompressedFS: TFileStream;
  fileBuff: array of byte;
  decompressedBuff: array of byte;
  decompressedPtr: PByte;
  DecLen: cardinal;
  S: TSnappyStatus;
begin
  S := SNAPPY_OK;
  DecLen := 0;
  fs := TFileStream.Create(InputFile, fmOpenRead);
  decompressedFS := TFileStream.Create(OutputFile, fmCreate or fmOpenWrite);
  try
    SetLength(fileBuff, fs.Size);
    fs.Seek(0, soFromBeginning);
    fs.Read(Pointer(@fileBuff[0])^, Length(fileBuff));
    S := snappy_uncompressed_length(@fileBuff[0], Length(fileBuff), @DecLen);
    if (S <> SNAPPY_OK) then
    begin
      result := S;
      exit;
    end;
    SetLength(decompressedBuff, DecLen);
    decompressedPtr := PByte(@decompressedBuff[0]);
    S := snappy_uncompress(@fileBuff[0], Length(fileBuff), decompressedPtr,
      @(DecLen));
    if (S <> SNAPPY_OK) then
    begin
      result := S;
      exit;
    end
    else
      decompressedFS.Write(Pointer(@decompressedBuff[0])^, DecLen)

  finally
    result := S;
    fs.Free;
    decompressedFS.Free;
  end;

end;

procedure TForm1.SnappyCompressByStream(InputFile, OutputFile: string);
var
  fs: TFileStream;
  compressedFS: TFileStream;
  fileBuff: array of byte;
  fileSize: integer;
  CompressedBytes: integer;
  SnappyStream: TSnappyCompressionStream;
begin
  CompressedBytes := 0;
  fileSize := 0;
  fs := TFileStream.Create(InputFile, fmOpenRead);
  compressedFS := TFileStream.Create(OutputFile, fmCreate or fmOpenWrite);
  SnappyStream := TSnappyCompressionStream.Create(compressedFS);
  try
    fileSize := fs.Size;
    SetLength(fileBuff, fileSize);
    fs.Seek(0, soFromBeginning);
    fs.Read(Pointer(@fileBuff[0])^,fileSize);
    CompressedBytes := SnappyStream.Write(Pointer(@fileBuff[0])^, fileSize);
  finally
    SnappyStream.Free;
    fs.Free;
    compressedFS.Free;
  end;

end;

procedure TForm1.SnappyDecompressByStream(InputFile, OutputFile: string);
var
  fs: TFileStream;
  compressedFS: TFileStream;
  fileBuff: array of byte;
  fileSize: integer;
  DecompressedBytes: integer;
  SnappyStream: TSnappyDecompressionStream;
begin
  DecompressedBytes := 0;
  fileSize := 0;
  compressedFS := TFileStream.Create(InputFile, fmOpenRead);
  fs := TFileStream.Create(OutputFile, fmCreate or fmOpenWrite);
  SnappyStream := TSnappyDecompressionStream.Create(compressedFS);
  try
    fileSize := compressedFS.Size;
    SetLength(fileBuff, filesize*2);
    DecompressedBytes := SnappyStream.Read(Pointer(@fileBuff[0])^, fileSize);
    fs.Write(Pointer(@fileBuff[0])^, DecompressedBytes);
  finally
    SnappyStream.Free;
    compressedFS.Free;
    fs.Free;
  end;
end;

end.

