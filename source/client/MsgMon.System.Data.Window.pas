unit MsgMon.System.Data.Window;

interface

uses
  System.Generics.Collections,
  Winapi.Windows;

type
  TMMWindows = class;

  TMMWindow = class
    base: Integer;  // First reference in messages index
    hwnd: DWORD;
    pid, tid: DWORD;
    hwndOwner, hwndParent: DWORD;
    ClassName, RealClassName: string;
  private
    FChildWindows: TMMWindows;
  public
    constructor Create(hwnd, pid, tid, hwndOwner, hwndParent: Integer; className, realClassName: string; ABase: Integer);
    destructor Destroy; override;
    function Render(IncludeHandle: Boolean): string;
    property ChildWindows: TMMWindows read FChildWindows;
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
begin
  inherited Create;

  FChildWindows := TMMWindows.Create(False); // This object does not own the child window objects

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

destructor TMMWindow.Destroy;
begin
  FreeAndNil(FChildWindows);
  inherited Destroy;
end;

function TMMWindow.Render(IncludeHandle: Boolean): string;
begin
  // TODO: Merge with TMMColumn render
  Result := ClassName;

  if IncludeHandle then
    Result := Result + ' ['+IntToHex(hwnd, 8)+']'
  else if RealClassName <> ClassName
    then Result := Result + ' ('+RealClassName+')';
end;

end.
