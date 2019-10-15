unit MsgMon.System.StackRenderer;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,

  MsgMon.System.Data.Event,
  MsgMon.System.Data.Image;

type
  TStackRenderer = class
    class function Render(stack: TArrayOfByte; images, pid0images: TMMImages): string;
  end;

implementation

{ TStackRenderer }

type
  TSymbolInfo = record
    SizeOfStruct: ULONG;
    TypeIndex: ULONG;
    Reserved: array[0..1] of ULONG64;
    Index: ULONG;
    Size: ULONG;
    ModBase: ULONG64;
    Flags: ULONG;
    Value: ULONG64;
    Address: ULONG64;
    Register: ULONG;
    Scope: ULONG;
    Tag: ULONG;
    NameLen: ULONG;
    MaxNameLen: ULONG;
    Name: array[0..0] of CHAR;
  end;

  PSymbolInfo = ^TSymbolInfo;

// TODO: Lazy load (avoids issues with old dbghelp.dll)
function SymFromAddr(hProcess: THandle; Address: DWORD64; Displacement: PDWORD64; Symbol: PSymbolInfo): BOOL; stdcall; external 'dbghelp.dll' name 'SymFromAddrW'  delayed;
function SymSetOptions(SymOptions: DWORD): DWORD; stdcall; external '.\dbghelp.dll'  delayed;
function SymInitialize(hProcess: THandle; UserSearchPath: PWCHAR; fInvadeProcess: BOOL): BOOL; stdcall; external '.\dbghelp.dll' name 'SymInitializeW'  delayed;
function SymCleanup(hProcess: THandle): BOOL; stdcall; external '.\dbghelp.dll'  delayed;
function SymUnloadModule64(hProcess: THandle; BaseOfDll: ULONG64): BOOL; stdcall; external '.\dbghelp.dll' delayed;

type
  TMODLOAD_DATA = record
    ssize, ssig: DWORD;
    data: PVOID;
    size, flags: DWORD;
  end;

  PMODLOAD_DATA = ^TMODLOAD_DATA;

function SymLoadModuleExW(
  hProcess: THandle;
  hFile: THandle;
  ImageName: PWCHAR;
  ModuleName: PWCHAR;
  BaseOfDll: DWORD64;
  DllSize: DWORD;
  Data: PMODLOAD_DATA;
  Flags: DWORD
): DWORD64; stdcall; external '.\dbghelp.dll' delayed;

const
  SYMOPT_UNDNAME = $00000002;
  SYMOPT_DEFERRED_LOADS = $00000004;
  SYMOPT_INCLUDE_32BIT_MODULES = $00002000;
  SYMOPT_DEBUG = $80000000;

var
  hSymProcess: THandle;

class function TStackRenderer.Render(stack: TArrayOfByte; images, pid0images: TMMImages): string;
var
  len: Integer;
  i: Integer;
  p: PUInt64;
  d: UInt64;
  s: PSymbolInfo;
  base: array of UInt64;
  ims: TMMImages;
  m, j: Integer;
  sym: string;
const
  MaxNameInfo = 100;
begin
  Result := '';

  ims := TMMImages.Create(False);
  ims.AddRange(images);
  ims.AddRange(pid0images);

  hSymProcess := GetCurrentProcess;

  SymSetOptions(SYMOPT_UNDNAME or SYMOPT_DEFERRED_LOADS or SYMOPT_INCLUDE_32BIT_MODULES or SYMOPT_DEBUG);
  if not SymInitialize(hSymProcess, 'SRV*c:\symbols*http://msdl.microsoft.com/download/symbols', True) then
    RaiseLastOSError;

  SetLength(base, ims.Count);
  for i := 0 to ims.Count - 1 do
  begin
    base[i] := SymLoadModuleExW(hSymProcess, 0, PChar(ExtractFileName(ims[i].filename)), nil, ims[i].imagebase, ims[i].imagesize, nil, 0);
  end;

  // TODO: Note: we are not currently supporting images unloaded and reloaded at a new base address
  // Or where one image is unloaded and another is loaded at an overlapping address
  len := Length(stack) div 8; // stack is 8-byte aligned
  p := @stack[0];
  s := AllocMem(sizeof(TSymbolInfo) + MaxNameInfo);
  for i := 0 to len - 1 do
  begin
    FillChar(s^, sizeof(TSymbolInfo) + MaxNameInfo, 0);
    s.SizeOfStruct := sizeof(TSymbolInfo);
    s.MaxNameLen := MaxNameInfo;
    m := -1;
    for j := 0 to ims.Count - 1 do
      if (ims[j].imagebase <= p^) and (p^ < ims[j].ImageBase + ims[j].imagesize) then
      begin
        m := j;
        Break;
      end;

    if SymFromAddr(hSymProcess, p^, @d, s) then
    begin
//      for i := 0 to High(base) do
      if d > 0
        then sym := Format(' %s+0x%x', [s.Name, d])
        else sym := s.Name;
    end
    else
    begin
      if m >= 0 then
        sym := Format('+0x%x', [p^ - ims[m].imagebase])
      else
        sym := 'Unknown';
    end;

    if m >= 0
      then Result := Result + Format('%016.16x [%s]%s %x'#13#10, [p^, ExtractFileName(ims[m].filename), sym, s.Flags])
      else Result := Result + Format('%016.16x [unknown]%s %x'#13#10, [p^, sym, s.Flags]);
//    else
//      Result := Result + Format('%016.16x %s'#13#10, [p^, SysErrorMessage(GetLastError)]);
    Inc(p);
  end;

//  for i := 0 to images.Count - 1 do
//  begin
//    if base[i] <> 0 then
//      if not SymUnLoadModule64(hSymProcess, base[i]) then
//        RaiseLastOSError;
//  end;

  if not SymCleanup(hSymProcess) then
    RaiseLastOSError;

  Result := Result + #13#10;

  for i := 0 to ims.Count - 1 do
    Result := Result + Format('%016.16x - %016.16x %s'#13#10, [ims[i].imagebase, ims[i].imagebase + ims[i].imagesize, ExtractFileName(ims[i].filename)]);

  ims.Free;
end;

initialization
  if LoadLibrary(PChar(ExtractFilePath(ParamStr(0))+'dbghelp.dll')) = 0 then
    RaiseLastOSError;
  LoadLibrary(PChar(ExtractFilePath(ParamStr(0))+'symsrv.dll'));
end.
