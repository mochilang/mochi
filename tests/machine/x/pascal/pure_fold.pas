program PureFold;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

function triple(x: integer): integer;
begin
  result := x * 3;
  exit;
end;

begin
  writeln(triple(1 + 2));
end.
