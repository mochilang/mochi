program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  n: integer;

begin
  for n in specialize TArray<integer>([1, 2, 3]) do
    begin
      writeln(n);
    end;
end.
