program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TArray<double>;
  n: integer;
  nums: specialize TArray<integer>;
  _result: double;

begin
  nums := specialize TArray<integer>([1, 2, 3]);
  SetLength(_tmp0, 0);
  for n in nums do
    begin
      if not ((n > 1)) then continue;
      _tmp0 := Concat(_tmp0, [0]);
    end;
  _result := _tmp0;
  writeln(_result);
end.
