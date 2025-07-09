program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  xs: specialize TArray<integer>;

begin
  xs := specialize TArray<integer>([1, 2, 3]);
  writeln((2 in xs));
  writeln(not (5 in xs));
end.
