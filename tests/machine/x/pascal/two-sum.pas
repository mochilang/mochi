program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

function twoSum(nums: specialize TArray<integer>; target: integer): specialize TArray<integer>;

var
  i: integer;
  j: Variant;
  n: integer;
begin
  n := Length(nums);
  for i := 0 to n - 1 do
    begin
      for j := i + 1 to n - 1 do
        begin
          if (specialize _indexList<integer>(nums, i) + specialize _indexList<integer>(nums, j) =
             target) then ;
        end;
    end;
  result := specialize TArray<integer>([-1, -1]);
  exit;
end;

generic function _indexList<T>(arr: specialize TArray<T>; i: integer): T;
begin
  if i < 0 then i := Length(arr) + i;
  if (i < 0) or (i >= Length(arr)) then
    raise Exception.Create('index out of range');
  Result := arr[i];
end;

var
  _result: specialize TArray<integer>;

begin
  _result := twoSum(specialize TArray<integer>([2, 7, 11, 15]), 9);
  writeln(specialize _indexList<integer>(_result, 0));
  writeln(specialize _indexList<integer>(_result, 1));
end.
