unit MsgMon.System.Data.Thread;

interface

uses
  System.Generics.Collections,

  MsgMon.System.Data.Window;

type
  TMMThread = class
  private
    FTID: Cardinal;
    FWindows: TMMWindowDictionary;
  public
    constructor Create(ATID: Cardinal);
    destructor Destroy; override;
    property TID: Cardinal read FTID;
    property Windows: TMMWindowDictionary read FWindows;
  end;

  TMMThreads = class(TDictionary<Cardinal, TMMThread>)
  end;

implementation

uses
  System.SysUtils;

{ TMMThread }

constructor TMMThread.Create(ATID: Cardinal);
begin
  inherited Create;
  FTID := ATID;
  FWindows := TMMWindowDictionary.Create;
end;

destructor TMMThread.Destroy;
begin
  FreeAndNil(FWindows);
  inherited Destroy;
end;

end.
