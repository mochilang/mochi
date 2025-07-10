program FunThreeArgs;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

function sum3(a: integer; b: integer; c: integer): integer;
begin
  result := a + b + c;
  exit;
end;

begin
  writeln(sum3(1, 2, 3));
end.
