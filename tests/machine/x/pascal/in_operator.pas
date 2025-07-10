program InOperator;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic function _containsList<T>(arr: specialize TArray<T>; v: T): boolean;

var i: Integer;
begin
  Result := False;
  for i := 0 to High(arr) do
    if arr[i] = v then exit(True);
end;

var
  xs: specialize TArray<integer>;

begin
  xs := specialize TArray<integer>([1, 2, 3]);
  writeln(specialize _containsList<Variant>(xs, 2));
  writeln(not specialize _containsList<Variant>(xs, 5));
end.
