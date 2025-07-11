program ForListCollection;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  n: Variant;

begin
  for n in specialize TArray<integer>([1, 2, 3]) do
    begin
      writeln(n);
    end;
end.
