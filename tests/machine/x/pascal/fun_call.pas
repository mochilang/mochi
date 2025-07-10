program FunCall;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

function add(a: integer; b: integer): integer;
begin
  result := a + b;
  exit;
end;

begin
  writeln(add(2, 3));
end.
