// Generated by Mochi compiler v0.10.27 on 2025-07-17T18:08:39Z
program AvgBuiltin;
{$mode objfpc}
{$modeswitch nestedprocvars}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;
generic function _sumList<T>(arr: specialize TArray<T>): double;
var i: integer; s: double;
begin
  s := 0;
  for i := 0 to High(arr) do
    s := s + arr[i];
  Result := s;
end;

generic function _avgList<T>(arr: specialize TArray<T>): double;
begin
  if Length(arr) = 0 then exit(0);
  Result := specialize _sumList<T>(arr) / Length(arr);
end;


begin
  writeln(specialize _avgList<integer>(specialize TArray<integer>([1, 2, 3])));
end.
