unit MsgMon.System.ProgressManager;

interface

type
  IProgressUI = interface
    ['{86E70E91-E12A-4FA3-900C-C6D7B919994A}']
    procedure SetCanCancel(const Value: Boolean);
    procedure SetMax(const Value: Integer);
    procedure SetMessage(const Value: string);
    procedure SetPosition(const Value: Integer);
    procedure SetTitle(const Value: string);
    function GetCanCancel: Boolean;
    function GetMax: Integer;
    function GetMessage: string;
    function GetPosition: Integer;
    function GetTitle: string;
    function GetCancelled: Boolean;
    procedure Yield;
    property Title: string read GetTitle write SetTitle;
    property Message: string read GetMessage write SetMessage;
    property Max: Integer read GetMax write SetMax;
    property Position: Integer read GetPosition write SetPosition;
    property CanCancel: Boolean read GetCanCancel write SetCanCancel;
    property Cancelled: Boolean read GetCancelled;
  end;

  TProgressCallback = reference to procedure(Sender: IProgressUI);

implementation

end.
