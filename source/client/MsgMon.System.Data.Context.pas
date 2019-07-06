unit MsgMon.System.Data.Context;

interface

uses
  System.Generics.Collections,

  MsgMon.System.Data.Message,
  MsgMon.System.Data.MessageName,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Thread,
  MsgMon.System.Data.Window;

type
  TMMDataContext = class
  private
    FProcesses: TMMProcessDictionary;
    FMessageNames: TMMMessageNameDictionary;
    FWindows: TMMWindowDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure PrepareTrees;
    property MessageNames: TMMMessageNameDictionary read FMessageNames;
    property Processes: TMMProcessDictionary read FProcesses;
    property Windows: TMMWindowDictionary read FWindows;
  end;

implementation

{ TMsgMonContext }

procedure TMMDataContext.Clear;
begin
  FMessageNames.Clear;
  FMessageNames.FillDefault;
  FProcesses.Clear;
  FWindows.Clear;
end;

constructor TMMDataContext.Create;
begin
  inherited Create;
  FMessageNames := TMMMessageNameDictionary.Create;
  FProcesses := TMMProcessDictionary.Create;
  FWindows := TMMWindowDictionary.Create;
end;

destructor TMMDataContext.Destroy;
begin
  FWindows.Free;
  FProcesses.Free;
  FMessageNames.Free;
  inherited Destroy;
end;

procedure TMMDataContext.PrepareTrees;
var
  ws: TPair<Cardinal,TMMWindows>;
  ps: TMMProcesses;
  t: TMMThread;
  w: TMMWindow;
  tws: TMMWindows;
  pws: TMMWindows;
begin
  // For each process, get the windows
  for ws in FWindows do
  begin
    w := ws.Value[0];
    if w.pid = 0 then
      // TODO: Why do we have windows with PID=0? Permissions?
      Continue;

    if not FProcesses.TryGetValue(w.pid, ps) or (ps.Count = 0) then
    begin
      // TODO: Why do we have windows with no corresponding process?
      Continue; //
    end;

    if not ps[0].Threads.TryGetValue(w.tid, t) then
    begin
      t := TMMThread.Create(w.tid, w.pid);
      ps[0].Threads.Add(w.tid, t);
    end;

    if not t.Windows.TryGetValue(w.hwnd, tws) then
    begin
      // thread window lists don't own the window objects
      tws := TMMWindows.Create(False);
      t.Windows.Add(w.hwnd, tws);
    end;
    tws.Add(w);

    if w.hwndParent <> 0 then
    begin
      if FWindows.TryGetValue(w.hwndParent, pws) and (pws.Count > 0) then
      begin
        pws[0].ChildWindows.Add(w);
      end;
    end;
  end;
end;

end.
