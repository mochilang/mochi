program LetAndPrint;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  a: integer;
  b: integer;

begin
  a := 10;
  b := 20;
  writeln(a + b);
end.
