program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  nums: specialize TArray<integer>;

begin
  nums := specialize TArray<integer>([1, 2, 3]);
  writeln((2 in nums));
  writeln((4 in nums));
end.
