program RecordAssign;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

type Counter = record
  n: integer;
end;

function inc(c: Counter): integer;
begin
  c := c.n + 1;
end;

var
  _tmp0: Counter;
  c: Counter;

begin
  _tmp0.n := 0;
  c := _tmp0;
  inc(c);
  writeln(c.n);
end.
