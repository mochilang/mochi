program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic function _indexList<T>(arr: specialize TArray<T>; i: integer): T;
begin
  if i < 0 then i := Length(arr) + i;
  if (i < 0) or (i >= Length(arr)) then
    raise Exception.Create('index out of range');
  Result := arr[i];
end;

var
  matrix: specialize TArray<specialize TArray<integer>>;

begin
  matrix := specialize TArray<specialize TArray<integer>>([specialize TArray<integer>([1, 2]),
            specialize TArray<integer>([3, 4])]);
  matrix[1][0] := 5;
  writeln(specialize _indexList<specialize TArray<integer>>(specialize _indexList<specialize TArray<
          integer>>(matrix, 1), 0));
end.
