unit MsgMon.System.Data.Window;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Winapi.Windows,

  MsgMon.System.Data.Event;

type
  TMMWindows = class;

  TMMWindow = class(TMMEvent)
    hwnd: Integer;
    ownerPid, ownerTid: Integer;
    hwndOwner, hwndParent: Integer;
    ClassName, RealClassName: string;
  private
  public
    constructor Create(
      timestamp: Int64;
      pid,
      tid: Integer;
      event_id: Int64;
      const stack: string;

      hwnd,
      ownerPid,  // TODO: rename or eliminate
      ownerTid,  // TODO: rename or eliminate
      hwndOwner,
      hwndParent: Integer;
      const className,
      realClassName: string);
    function Render(IncludeHandle: Boolean): string;
    class function BaseRender(hwnd: Cardinal): string; overload; static;
    class function BaseRender(IncludeHandle: Boolean; hwnd: Cardinal;
      const ClassName, RealClassName: string): string; overload; static;
  end;

  TMMWindows = class(TObjectList<TMMWindow>)
  end;

  TMMWindowDictionary = class(TObjectDictionary<DWORD,TMMWindow>)
  end;

implementation

{ TMsgMonWindow }

constructor TMMWindow.Create(
  timestamp: Int64;
  pid,
  tid: Integer;
  event_id: Int64;
  const stack: string;

  hwnd,
  ownerPid,
  ownerTid,
  hwndOwner,
  hwndParent:
  Integer;
  const className,
  realClassName: string);
begin
  inherited Create(timestamp, pid, tid, event_id, stack);

  Self.hwnd := hwnd;
  Self.ownerPid := ownerPid;
  Self.ownerTid := ownerTid;
  Self.hwndOwner := hwndOwner;
  Self.hwndParent := hwndParent;
  Self.ClassName := className;
  Self.RealClassName := realClassName;
end;

{ TMsgMonWindows }

function TMMWindow.Render(IncludeHandle: Boolean): string;
begin
  Result := BaseRender(IncludeHandle, hwnd, ClassName, RealClassName);
end;

class function TMMWindow.BaseRender(hwnd: Cardinal): string;
begin
  Result := IntToHex(hwnd, 8);
end;

class function TMMWindow.BaseRender(IncludeHandle: Boolean; hwnd: Cardinal; const ClassName, RealClassName: string): string;
begin
  Result := ClassName;

  if IncludeHandle then
    Result := Result + ' ['+IntToHex(hwnd, 8)+']'
  else if (RealClassName <> ClassName) and (RealClassName <> '') then
    Result := Result + ' ('+RealClassName+')';
end;

end.
