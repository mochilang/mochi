program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  nums: specialize TArray<integer>;

begin
  nums := specialize TArray<integer>([3, 1, 4]);
  writeln(min(nums));
  writeln(max(nums));
end.
