program main;
{$mode objfpc}

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
  nums: specialize TArray<integer>;

begin
  nums := specialize TArray<integer>([1, 2]);
  nums[1] := 3;
  writeln(specialize _indexList<integer>(nums, 1));
end.
