program ShortCircuit;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

function boom(a: integer; b: integer): boolean;
begin
  writeln('boom');
  result := True;
  exit;
end;

begin
  writeln((False and boom(1, 2)));
  writeln((True or boom(1, 2)));
end.
