program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  i: integer;

begin
  for i := 1 to 4 - 1 do
    begin
      writeln(i);
    end;
end.
