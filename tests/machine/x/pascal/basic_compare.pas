program BasicCompare;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  a: integer;
  b: integer;

begin
  a := 10 - 3;
  b := 2 + 2;
  writeln(a);
  writeln((a = 7));
  writeln((b < 5));
end.
