program MinMaxBuiltin;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  nums: specialize TArray<integer>;

begin
  nums := specialize TArray<integer>([3, 1, 4]);
  writeln(min(nums));
  writeln(max(nums));
end.
