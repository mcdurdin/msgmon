unit MsgMon.System.PlainStackRenderer;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  Winapi.Windows,

  MsgMon.System.Data.Event,
  MsgMon.System.Data.Image,
  MSgMon.System.StackBase; // todo: rename?

type
  TPlainStackRenderer = class
  private
    FSystemModules: TMMImages;
  public
    constructor Create(ASystemModules: TMMImages);
    destructor Destroy; override;
    function Render(stack: TArrayOfByte; images_in: TMMImages): TStackRows;
  end;

implementation

{ TPlainStackRenderer }

constructor TPlainStackRenderer.Create(ASystemModules: TMMImages);
begin
  inherited Create;
  FSystemModules := ASystemModules.Clone;
end;

destructor TPlainStackRenderer.Destroy;
begin
  FSystemModules.Free;
  inherited Destroy;
end;

function TPlainStackRenderer.Render(stack: TArrayOfByte; images_in: TMMImages): TStackRows;
var
  len: Integer;
  i: Integer;
  p: PUInt64;
  ims: TMMImages;
  im: TMMImage;
  j: Integer;
  r: TStackRow;
  FFirstKernelModule: Integer;
begin
  SetLength(Result, 0);

  ims := TMMImages.Create(False);
  try
    ims.AddRange(images_in);
    FFirstKernelModule := ims.Count;
    ims.AddRange(FSystemModules);

    len := Length(stack) div 8; // we have setup the stack as 8-byte aligned even on 32-bit systems

    SetLength(Result, len);

    p := @stack[0];
    for i := 0 to len - 1 do
    begin
      im := nil;

      r.IsKernel := False;

      for j := 0 to ims.Count - 1 do
        if (ims[j].imagebase <= p^) and (p^ < ims[j].ImageBase + ims[j].imagesize) then
        begin
          im := ims[j];
          r.IsKernel := j >= FFirstKernelModule;
          Break;
        end;

      r.Frame := i;
      if Assigned(im)
        then r.Module := ExtractFileName(im.filename)
        else r.Module := 'Unknown';

      if Assigned(im)
        then r.Location := Format('%s + 0x%x', [r.Module, p^ - im.imagebase])
        else r.Location := 'Unknown';

      r.Address := p^;
      if Assigned(im)
        then r.Path := im.filename
        else r.Path := '';

      Result[i] := r;
      Inc(p);
    end;
  finally
    ims.Free;
  end;
end;

end.
