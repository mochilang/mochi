program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

function inc(x: integer): integer;
begin
  result := x + k;
  exit;
end;

var
  k: integer;

begin
  k := 2;
  writeln(inc(3));
end.
