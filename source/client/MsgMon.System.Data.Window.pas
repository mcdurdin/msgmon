unit MsgMon.System.Data.Window;

interface

uses
  System.Generics.Collections,
  Winapi.Windows;

type
  TMMWindow = class
    base: Integer;  // First reference in messages index
    hwnd: DWORD;
    pid, tid: DWORD;
    hwndOwner, hwndParent: DWORD;
    ClassName, RealClassName: string;
    constructor Create(hwnd, pid, tid, hwndOwner, hwndParent: Integer; className, realClassName: string; ABase: Integer);
    function Render(IncludeHandle: Boolean): string;
  end;

  TMMWindows = class(TObjectList<TMMWindow>)
    function FromBase(ABase: Integer): TMMWindow;
  end;

  TMMWindowDictionary = class(TObjectDictionary<DWORD,TMMWindows>)
  end;

implementation

uses
  System.SysUtils;

{ TMsgMonWindow }

constructor TMMWindow.Create(hwnd, pid, tid, hwndOwner, hwndParent: Integer; className, realClassName: string; ABase: Integer);
var
  name, value: string;
  valueInt: Int64;
begin
  inherited Create;

  Self.hwnd := hwnd;
  Self.pid := pid;
  Self.tid := tid;
  Self.hwndOwner := hwndOwner;
  Self.hwndParent := hwndParent;
  Self.ClassName := className;
  Self.RealClassName := realClassName;

  Self.base := ABase;
end;

{ TMsgMonWindows }

function TMMWindows.FromBase(ABase: Integer): TMMWindow;
var
  i: Integer;
begin
  for i := Count-1 downto 0 do
    if Items[i].base <= ABase then
      Exit(Items[i]);
  if Count = 0 then
    Exit(nil);
  Result := Items[0];
end;

function TMMWindow.Render(IncludeHandle: Boolean): string;
begin
  Result := ClassName;

  if IncludeHandle then
    Result := Result + ' ['+IntToStr(hwnd)+']'
  else if RealClassName <> ClassName
    then Result := Result + ' ('+RealClassName+')';
end;

end.
