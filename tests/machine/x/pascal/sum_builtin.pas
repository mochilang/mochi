program SumBuiltin;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic function _sumList<T>(arr: specialize TArray<T>): double;

var i: integer;
  s: double;
begin
  s := 0;
  for i := 0 to High(arr) do
    s := s + arr[i];
  Result := s;
end;

begin
  writeln(specialize _sumList<integer>(specialize TArray<integer>([1, 2, 3])));
end.
