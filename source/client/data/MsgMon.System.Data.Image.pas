unit MsgMon.System.Data.Image;

interface

uses
  System.Generics.Collections;

type
  TMMImage = class
  public
    filename: string;
    pid: Integer;
    checksum: Integer;
    timedatestamp: Integer;
    imagesize: UInt64;
    imagebase: UInt64;
    constructor Create(
      filename: string;
      pid: Integer;
      checksum: Integer;
      timedatestamp: Integer;
      imagesize: UInt64;
      imagebase: UInt64);
  end;

  TMMImages = class(TObjectList<TMMImage>)
  end;

implementation

{ TMMImage }

constructor TMMImage.Create(filename: string; pid, checksum,
  timedatestamp: Integer; imagesize, imagebase: UInt64);
begin
  inherited Create;
  Self.filename := filename;
  Self.pid := pid;
  Self.checksum := checksum;
  Self.timedatestamp := timedatestamp;
  Self.imagesize := imagesize;
  Self.imagebase := imagebase;
end;

end.
