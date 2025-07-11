program IfThenElse;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  _tmp0: string;
  msg: string;
  x: integer;

begin
  x := 12;
  if (x > 10) then
    begin
      _tmp0 := 'yes';
    end
  else
    begin
      _tmp0 := 'no';
    end;
  msg := _tmp0;
  writeln(msg);
end.
