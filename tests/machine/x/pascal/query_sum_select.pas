program QuerySumSelect;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

  generic procedure _printList<T>(arr: specialize TArray<T>);

var i: Integer;
begin
  for i := 0 to High(arr) do
    begin
      if i > 0 then Write(' ');
      Write(arr[i]);
    end;
  writeln();
end;

var
  _tmp0: specialize TArray<integer>;
  n: integer;
  nums: specialize TArray<integer>;
  _result: specialize TArray<Variant>;

begin
  nums := specialize TArray<integer>([1, 2, 3]);
  SetLength(_tmp0, 0);
  for n in nums do
    begin
      if not ((n > 1)) then continue;
      _tmp0 := Concat(_tmp0, [0]);
    end;
  _result := _tmp0;
  specialize _printList<Variant>(_result);
end.
