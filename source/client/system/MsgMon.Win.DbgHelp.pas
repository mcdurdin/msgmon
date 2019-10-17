unit MsgMon.Win.DbgHelp;

interface

uses
  Winapi.Windows;

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

type
  TMODLOAD_DATA = record
    ssize, ssig: DWORD;
    data: PVOID;
    size, flags: DWORD;
  end;

  PMODLOAD_DATA = ^TMODLOAD_DATA;

//  PSYMBOL_REGISTERED_CALLBACK = function(hProcess: THandle; ActionCode: ULONG; CallbackData: PVOID; UserContext: PVOID): BOOL; stdcall;
  PSYMBOL_REGISTERED_CALLBACK64 = function(hProcess: THandle; ActionCode: ULONG; CallbackData: ULONG64; UserContext: ULONG64): BOOL; stdcall;
  IMAGEHLP_CBA_EVENT = record
    severity: DWord;
    code: DWord;
    desc: PChar;
    object_: Pointer;
  end;

  PIMAGEHLP_CBA_EVENT = ^IMAGEHLP_CBA_EVENT;

function LoadDbgHelp(const path: string): Boolean;
function UnloadDbgHelp: Boolean;

function SymFromAddr(hProcess: THandle; Address: DWORD64; Displacement: PDWORD64; Symbol: PSymbolInfo): BOOL; stdcall;
function SymSetOptions(SymOptions: DWORD): DWORD; stdcall;
function SymInitialize(hProcess: THandle; UserSearchPath: PWCHAR; fInvadeProcess: BOOL): BOOL; stdcall;
function SymCleanup(hProcess: THandle): BOOL; stdcall;
function SymUnloadModule64(hProcess: THandle; BaseOfDll: ULONG64): BOOL; stdcall;
function SymLoadModuleExW(
  hProcess: THandle;
  hFile: THandle;
  ImageName: PWCHAR;
  ModuleName: PWCHAR;
  BaseOfDll: DWORD64;
  DllSize: DWORD;
  Data: PMODLOAD_DATA;
  Flags: DWORD
): DWORD64; stdcall;

function SymRegisterCallback64(
  hProcess: THandle;
  CallbackFunction: PSYMBOL_REGISTERED_CALLBACK64;
  UserContext: ULONG64
): BOOL; stdcall;

type
  PFINDFILEINPATHCALLBACK = function(filename: PCHAR; context: PVOID): BOOL; stdcall;

function SymFindFileInPath(
  hProcess: THandle;
  SearchPath: PCHAR;
  FileName: PCHAR;
  id: PVOID;
  two: DWORD;
  three: DWORD;
  flags: DWORD;
  FoundFile: PCHAR;
  callback: PFINDFILEINPATHCALLBACK;
  context: PVOID
): BOOL; stdcall;

const
  SYMOPT_UNDNAME = $00000002;
  SYMOPT_DEFERRED_LOADS = $00000004;
  SYMOPT_INCLUDE_32BIT_MODULES = $00002000;
  SYMOPT_DEBUG = $80000000;
  SYMOPT_NO_IMAGE_SEARCH = $00020000;

  SLMFLAG_VIRTUAL = 1;

  SSRVOPT_DWORD = $2;

const
  CBA_EVENT = $10;

implementation

var
  hDbghelpModule: THandle = 0;

var FSymFromAddr: function(hProcess: THandle; Address: DWORD64; Displacement: PDWORD64; Symbol: PSymbolInfo): BOOL; stdcall = nil;
function SymFromAddr(hProcess: THandle; Address: DWORD64; Displacement: PDWORD64; Symbol: PSymbolInfo): BOOL; stdcall;
begin
  Assert(Assigned(FSymFromAddr));
  Result := FSymFromAddr(hProcess, Address, Displacement, Symbol);
end;

var FSymSetOptions: function(SymOptions: DWORD): DWORD; stdcall = nil;
function SymSetOptions(SymOptions: DWORD): DWORD; stdcall;
begin
  Assert(Assigned(FSymSetOptions));
  Result := FSymSetOptions(SymOptions);
end;

var FSymInitialize: function(hProcess: THandle; UserSearchPath: PWCHAR; fInvadeProcess: BOOL): BOOL; stdcall = nil;
function SymInitialize(hProcess: THandle; UserSearchPath: PWCHAR; fInvadeProcess: BOOL): BOOL; stdcall;
begin
  Assert(Assigned(FSymInitialize));
  Result := FSymInitialize(hProcess, UserSearchPath, fInvadeProcess);
end;

var FSymCleanup: function(hProcess: THandle): BOOL; stdcall = nil;
function SymCleanup(hProcess: THandle): BOOL; stdcall;
begin
  Assert(Assigned(FSymCleanup));
  Result := FSymCleanup(hProcess);
end;

var FSymUnloadModule64: function(hProcess: THandle; BaseOfDll: ULONG64): BOOL; stdcall = nil;
function SymUnloadModule64(hProcess: THandle; BaseOfDll: ULONG64): BOOL; stdcall;
begin
  Assert(Assigned(FSymUnloadModule64));
  Result := FSymUnloadModule64(hProcess, BaseOfDll);
end;

var FSymLoadModuleExW: function(hProcess: THandle; hFile: THandle; ImageName: PWCHAR; ModuleName: PWCHAR; BaseOfDll: DWORD64; DllSize: DWORD; Data: PMODLOAD_DATA; Flags: DWORD): DWORD64; stdcall = nil;
function SymLoadModuleExW(hProcess: THandle; hFile: THandle; ImageName: PWCHAR; ModuleName: PWCHAR; BaseOfDll: DWORD64; DllSize: DWORD; Data: PMODLOAD_DATA; Flags: DWORD): DWORD64; stdcall;
begin
  Assert(Assigned(FSymLoadModuleExW));
  Result := FSymLoadModuleExW(hProcess, hFile, ImageName, ModuleName, BaseOfDll, DllSize, Data, Flags);
end;

var FSymRegisterCallback64: function(hProcess: THandle; CallbackFunction: PSYMBOL_REGISTERED_CALLBACK64; UserContext: ULONG64): BOOL; stdcall = nil;
function SymRegisterCallback64(hProcess: THandle; CallbackFunction: PSYMBOL_REGISTERED_CALLBACK64; UserContext: ULONG64): BOOL; stdcall;
begin
  Assert(Assigned(FSymRegisterCallback64));
  Result := FSymRegisterCallback64(hProcess, CallbackFunction, UserContext);
end;

var FSymFindFileInPath: function(hProcess: THandle; SearchPath: PCHAR; FileName: PCHAR; id: PVOID; two: DWORD; three: DWORD; flags: DWORD; FoundFile: PCHAR; callback: PFINDFILEINPATHCALLBACK; context: PVOID): BOOL; stdcall = nil;
function SymFindFileInPath(hProcess: THandle; SearchPath: PCHAR; FileName: PCHAR; id: PVOID; two: DWORD; three: DWORD; flags: DWORD; FoundFile: PCHAR; callback: PFINDFILEINPATHCALLBACK; context: PVOID): BOOL; stdcall;
begin
  Assert(Assigned(FSymFindFileInPath));
  Result := FSymFindFileInPath(hProcess, SearchPath, FileName, id, two, three, flags, FoundFile, callback, context);
end;

function LoadDbgHelp(const path: string): Boolean;
begin
  UnloadDbgHelp;
  if path <> ''
    then hDbghelpModule := LoadLibrary(PChar(path))
    else hDbghelpModule := LoadLibrary('dbghelp.dll');

  if hDbghelpModule = 0 then
    Exit(False);

  FSymFromAddr := GetProcAddress(hDbghelpModule, 'SymFromAddrW');
  FSymSetOptions := GetProcAddress(hDbghelpModule, 'SymSetOptions');
  FSymInitialize := GetProcAddress(hDbghelpModule, 'SymInitializeW');
  FSymCleanup := GetProcAddress(hDbghelpModule, 'SymCleanup');
  FSymUnloadModule64 := GetProcAddress(hDbghelpModule, 'SymUnloadModule64');
  FSymLoadModuleExW := GetProcAddress(hDbghelpModule, 'SymLoadModuleExW');
  FSymRegisterCallback64 := GetProcAddress(hDbghelpModule, 'SymRegisterCallbackW64');
  FSymFindFileInPath := GetProcAddress(hDbghelpModule, 'SymFindFileInPathW');

  Result :=
    Assigned(FSymFromAddr) and
    Assigned(FSymSetOptions) and
    Assigned(FSymInitialize) and
    Assigned(FSymCleanup) and
    Assigned(FSymUnloadModule64) and
    Assigned(FSymLoadModuleExW) and
    Assigned(FSymRegisterCallback64) and
    Assigned(FSymFindFileInPath);
end;

function UnloadDbgHelp: Boolean;
begin
  if hDbghelpModule = 0 then
    Exit(True);

  FSymFromAddr := nil;
  FSymSetOptions := nil;
  FSymInitialize := nil;
  FSymCleanup := nil;
  FSymUnloadModule64 := nil;
  FSymLoadModuleExW := nil;
  FSymRegisterCallback64 := nil;
  FSymFindFileInPath := nil;

  Result := FreeLibrary(hDbghelpModule);
  hDbghelpModule := 0;
end;

end.
