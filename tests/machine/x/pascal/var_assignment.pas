program VarAssignment;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  x: integer;

begin
  x := 1;
  x := 2;
  writeln(x);
end.
