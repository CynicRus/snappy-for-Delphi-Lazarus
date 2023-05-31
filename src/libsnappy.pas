unit libsnappy;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE Delphi7}
  {$DEFINE DELPHI_7}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  {$IFNDEF DELPHI_7}
  System.Classes, System.SysUtils {$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF};
  {$ELSE}
  Windows,Classes,Sysutils;
  {$ENDIF}
{$ENDIF}
{$IFDEF FPC}
 Classes,Sysutils,LCLIntf,dynlibs;
{$ENDIF}

const
{$IFDEF FPC}
{$IFDEF DARWIN}
  LIB_Name = '/usr/local/lib/libsnappy.dylib';
{$ENDIF}
{$IFDEF LINUX}
  LIB_Name = '/usr/lib/libsnappy.so';
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  LIB_Name = 'libsnappy.dll';
{$ENDIF}

type
  TSnappyStatus = (SNAPPY_OK = 0, SNAPPY_INVALID_INPUT = 1,
    SNAPPY_BUFFER_TOO_SMALL = 2);
  size_t = NativeInt;
  psize_t = ^size_t;
{$IFDEF SNAPPY_STATIC_INIT}
function snappy_compress(const input: pbyte; input_length: size_t;
  compressed: pbyte; compressed_length: psize_t): TSnappyStatus;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
external LIB_Name;

function snappy_uncompress(const compressed: pbyte; compressed_length: size_t;
  uncompressed: pbyte; uncompressed_length: psize_t): TSnappyStatus;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
external LIB_Name;

function snappy_max_compressed_length(source_length: size_t): size_t;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
external LIB_Name;

function snappy_uncompressed_length(const compressed: pbyte;
  compressed_length: size_t; result: psize_t): TSnappyStatus; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
external LIB_Name;

function snappy_validate_compressed_buffer(compressed: pbyte;
  compressed_length: size_t): TSnappyStatus; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
external LIB_Name;
{$ELSE}
  tsnappy_compress = function(const input: pbyte; input_length: size_t;
    compressed: pbyte; compressed_length: psize_t): TSnappyStatus;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  tsnappy_uncompress = function(const compressed: pbyte;
    compressed_length: size_t; uncompressed: pbyte;
    uncompressed_length: psize_t): TSnappyStatus; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  tsnappy_max_compressed_length = function(source_length: size_t): size_t;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  tsnappy_uncompressed_length = function(const compressed: pbyte;
    compressed_length: size_t; result: psize_t): TSnappyStatus;
{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  tsnappy_validate_compressed_buffer = function(compressed: pbyte;
    compressed_length: size_t): TSnappyStatus; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
procedure InitSnappy(libsnappy: string = LIB_Name);
procedure FreeSnappy();
{$ENDIF}

type
  TCustomSnappyStream = class(TStream)
  private
    FStrm: TStream;
    FStrmPos: Int64;
    // FBZRec: TBZStreamRec;
    FBuffer: array of byte;
  protected
    constructor Create(Strm: TStream);
  end;

  TSnappyCompressionStream = class(TCustomSnappyStream)
  public
    constructor Create(Dest: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteBuffer(const Buffer; Count: NativeInt); overload;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TSnappyDecompressionStream = class(TCustomSnappyStream)
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function GetDecompressedDataSize(Count: Longint): LongInt;
  end;

  ESnappyError = class(Exception);
  ESnappyCompressionError = class(ESnappyError);
  ESnappyDecompressionError = class(ESnappyError);
{$IFNDEF SNAPPY_STATIC_INIT}

var
  LibSnappyHandle: {$IFDEF MSWINDOWS}THandle{$ELSE}TLibHandle = NilHandle{$ENDIF};
  snappy_compress: tsnappy_compress;
  snappy_uncompress: tsnappy_uncompress;
  snappy_max_compressed_length: tsnappy_max_compressed_length;
  snappy_uncompressed_length: tsnappy_uncompressed_length;
  snappy_validate_compressed_buffer: tsnappy_validate_compressed_buffer;
{$ENDIF}

{$IFDEF DELPHI_7}
function ExtractFileNameWithoutExt(const FileName: string): string;
function FileSize( const Path : String ) : Integer;
{$ENDIF}

implementation

{$IFDEF DELPHI_7}
function ExtractFileNameWithoutExt(const FileName: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(FileName), '');
end;

function FileSize( const Path : String ) : Integer;
var FD : TWin32FindData;
    FH : THandle;
begin
  FH := FindFirstFile( PChar( Path ), FD );
  Result := 0;
  if FH = INVALID_HANDLE_VALUE then exit;
  Result := FD.nFileSizeLow;
  if ((FD.nFileSizeLow and $80000000) <> 0) or
     (FD.nFileSizeHigh <> 0) then Result := -1;
  Windows.FindClose( FH );
end;

{$ENDIF}

{$IFNDEF SNAPPY_STATIC_INIT}

procedure InitSnappy(libsnappy: string = LIB_Name);

begin
  LibSnappyHandle := {$IFNDEF FPC}SafeLoadLibrary{$ELSE}dynlibs.SafeLoadLibrary{$ENDIF}(libsnappy);
  if (LibSnappyHandle <>{$IFDEF MSWINDOWS} 0 {$ELSE}NilHandle{$ENDIF}) then
  begin
    @snappy_compress := GetProcAddress(LibSnappyHandle, ('snappy_compress'));
    @snappy_uncompress := GetProcAddress(LibSnappyHandle,
      ('snappy_uncompress'));
    @snappy_max_compressed_length := GetProcAddress(LibSnappyHandle,
      ('snappy_max_compressed_length'));
    @snappy_uncompressed_length := GetProcAddress(LibSnappyHandle,
      ('snappy_uncompressed_length'));
    @snappy_validate_compressed_buffer := GetProcAddress(LibSnappyHandle,
      ('snappy_validate_compressed_buffer'));
  end
  else
   {$IFDEF FPC}
    raise Exception.CreateFmt('Unable to load library %s!',[GetLoadErrorStr]);
   {$ELSE}
    raise Exception.CreateFmt('Unable to load library %s!', [LIB_Name]);
  {$ENDIF}

end;

procedure FreeSnappy();
begin
  if LibSnappyHandle > 0 then
    FreeLibrary(LibSnappyHandle);
end;
{$ENDIF}
{ TCustomSnappyStream }

constructor TCustomSnappyStream.Create(Strm: TStream);
begin
  inherited Create;
  FStrm := Strm;
  FStrmPos := Strm.Position;
end;

{ TSnappyCompressionStream }

constructor TSnappyCompressionStream.Create(Dest: TStream);
begin
 inherited Create(Dest);
end;

destructor TSnappyCompressionStream.Destroy;
begin

  inherited;
end;

function TSnappyCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise ESnappyCompressionError.Create('Invalid stream operation');
end;

function TSnappyCompressionStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  raise ESnappyCompressionError.Create('Invalid stream operation');
end;

function TSnappyCompressionStream.Write(const Buffer; Count: Longint): Longint;
var
  MaxLen: cardinal;
  bufferPtr, compressionPtr: pbyte;
  Status: TSnappyStatus;
begin
  if FStrm.Position <> FStrmPos then
    FStrm.Position := FStrmPos;
  MaxLen := snappy_max_compressed_length(Count);
  setLength(FBuffer, MaxLen);
  bufferPtr := PByte(@Buffer);
  compressionPtr := pbyte(@FBuffer[0]);
  Status := snappy_compress(bufferPtr, Count, compressionPtr, @MaxLen);
  if (Status <> SNAPPY_OK) then
  begin
    case Status of
      SNAPPY_INVALID_INPUT:
        raise ESnappyCompressionError.Create
          ('Snappy failed with code: invalid input!');
      SNAPPY_BUFFER_TOO_SMALL:
        raise ESnappyCompressionError.Create
          ('Snappy failed with code: buffer to small!');
    end;
  end
  else
  begin
    FStrm.Write(Pointer(@FBuffer[0])^, MaxLen);
    FStrmPos := FStrm.Position;
  end;
  result := MaxLen;
end;

procedure TSnappyCompressionStream.WriteBuffer(const Buffer; Count: NativeInt);
begin
 Write(Buffer,Count);
end;

{ TSnappyDecompressionStream }

constructor TSnappyDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);
end;

destructor TSnappyDecompressionStream.Destroy;
begin

  inherited;
end;

function TSnappyDecompressionStream.GetDecompressedDataSize(Count: Longint): LongInt;
var
  bufferPtr: PByte;
  Status: TSnappyStatus;
  DecLen: Cardinal;
begin
  if FStrm.Position <> FStrmPos then
  FStrm.Position := FStrmPos;
  setLength(FBuffer, Count);
  FStrm.Read(Pointer(@FBuffer[0])^,Count);
  Status := snappy_uncompressed_length(@FBuffer[0],Count,@DecLen);
  if (Status <> SNAPPY_OK) then
  begin
    case Status of
      SNAPPY_INVALID_INPUT:
        raise ESnappydecompressionError.Create
          ('Snappy failed with code: invalid input!');
      SNAPPY_BUFFER_TOO_SMALL:
        raise ESnappydecompressionError.Create
          ('Snappy failed with code: buffer to small!');
    end;
  end else
    begin
    FStrmPos := 0;
    FStrm.Position:=0;
    end;
    result := DecLen;
end;

function TSnappyDecompressionStream.Read(var Buffer; Count: Longint): Longint;
var
  DecLen: Cardinal;
  bufferPtr, compressionPtr: pbyte;
  Status: TSnappyStatus;
begin
  if FStrm.Position <> FStrmPos then
    FStrm.Position := FStrmPos;
  bufferPtr := PByte(@Buffer);
  DecLen := GetDecompressedDataSize(Count);
  compressionPtr := pbyte(@FBuffer[0]);
  Status := snappy_uncompress(compressionPtr, Count, bufferPtr, @DecLen);
  if (Status <> SNAPPY_OK) then
  begin
    case Status of
      SNAPPY_INVALID_INPUT:
        raise ESnappydecompressionError.Create
          ('Snappy failed with code: invalid input!');
      SNAPPY_BUFFER_TOO_SMALL:
        raise ESnappydecompressionError.Create
          ('Snappy failed with code: buffer to small!');
    end;
  end else
    FStrmPos := FStrm.Position;
  result := DecLen;
end;

function TSnappyDecompressionStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  raise ESnappyDecompressionError.Create('Invalid stream operation');
end;

function TSnappyDecompressionStream.Write(const Buffer; Count: Longint)
  : Longint;
begin
  raise ESnappyDecompressionError.Create('Invalid stream operation');
end;

end.
