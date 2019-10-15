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
    imagesize: Int64;
    imagebase: Int64;
    constructor Create(
      filename: string;
      pid: Integer;
      checksum: Integer;
      timedatestamp: Integer;
      imagesize: Int64;
      imagebase: Int64);
  end;

  TMMImages = class(TObjectList<TMMImage>)
  end;

implementation

{ TMMImage }

constructor TMMImage.Create(filename: string; pid, checksum,
  timedatestamp: Integer; imagesize, imagebase: Int64);
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
