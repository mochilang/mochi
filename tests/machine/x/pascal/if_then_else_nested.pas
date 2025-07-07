program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: string;
  msg: string;
  x: integer;

begin
  x := 8;
  if (x > 10) then
    begin
      _tmp0 := 'big';
    end
  else if (x > 5) then
         begin
           _tmp0 := 'medium';
         end
  else
    begin
      _tmp0 := 'small';
    end;
  msg := _tmp0;
  writeln(msg);
end.
