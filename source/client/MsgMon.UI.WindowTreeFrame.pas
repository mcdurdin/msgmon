unit MsgMon.UI.WindowTreeFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,

  System.Generics.Collections,

  MsgMon.Data.Database,
  MsgMon.System.Data.Process,
  MsgMon.System.Data.Thread,
  MsgMon.System.Data.Window;

type
  TMMWindowTreeFrame = class(TForm)
    tvWindows: TTreeView;
  private
    db: TMMDatabase;
    procedure RefreshTree;
  public
    { Public declarations }
    procedure SetDatabase(Adb: TMMDatabase);
    procedure CloseDatabase;
  end;

implementation

{$R *.dfm}

procedure TMMWindowTreeFrame.CloseDatabase;
begin
  db := nil;
end;

procedure TMMWindowTreeFrame.RefreshTree;
  procedure AddWindow(p: TTreeNode; w: TMMWindow);
  var
    wnode: TTreeNode;
    w0: TMMWindow;
  begin
    wnode := tvWindows.Items.AddChild(p, IntToStr(w.hwnd) + ' ' + w.ClassName);
    for w0 in w.ChildWindows do
    begin
      AddWindow(wnode, w0);
    end;
  end;
var
  ws: TMMWindows;
  pp: TPair<DWORD, TMMProcesses>;
  tp: TPair<DWORD, TMMThread>;
  pnode, tnode: TTreeNode;
  wp: TPair<DWORD, TMMWindows>;
  w0, w: TMMWindow;
begin
  tvWindows.Items.BeginUpdate;
  try
    tvWindows.Items.Clear;

    // For each process, get the windows
    for pp in db.Context.Processes do
    begin
      pnode := tvWindows.Items.Add(nil, IntToStr(pp.Value[0].PID) + ' ' + pp.Value[0].processName);
      for tp in pp.Value[0].Threads do
      begin
        tnode := tvWindows.Items.AddChild(pnode, IntToStr(tp.Value.TID));
        for wp in tp.Value.Windows do
        begin
          w := wp.Value[0];
          if w.hwndParent = 0 then
          begin
            AddWindow(tnode, w);
          end;
        end;
      end;
      //ps.Value

//      tvn := TTreeNode.Create
    end;

    tvWindows.FullExpand;
  finally
    tvWindows.Items.EndUpdate;
  end;
end;

procedure TMMWindowTreeFrame.SetDatabase(Adb: TMMDatabase);
begin
  db := Adb;
  RefreshTree;
end;

end.
