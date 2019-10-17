unit MsgMon.System.StackBase;

interface

type
  TStackRow = record
    IsKernel: Boolean;
    Frame: Integer;
    Module: string;
    Location: string;
    Address: UInt64;
    Path: string;
  end;

  TStackRows = array of TStackRow;

implementation

end.
