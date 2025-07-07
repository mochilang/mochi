program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<integer, Variant>;
  _tmp1: specialize TArray<specialize TFPGMap<string, Variant>>;
  b: boolean;
  bools: specialize TArray<boolean>;
  c: specialize TFPGMap<string, Variant>;
  combos: specialize TArray<specialize TFPGMap<string, Variant>>;
  l: string;
  letters: specialize TArray<string>;
  n: integer;
  nums: specialize TArray<integer>;

begin
  nums := specialize TArray<integer>([1, 2]);
  letters := specialize TArray<string>(['A', 'B']);
  bools := specialize TArray<boolean>([True, False]);
  _tmp0 := specialize TFPGMap<integer, Variant>.Create;
  _tmp0.AddOrSetData('n', n);
  _tmp0.AddOrSetData('l', l);
  _tmp0.AddOrSetData('b', b);
  SetLength(_tmp1, 0);
  for n in nums do
    begin
      for l in letters do
        begin
          for b in bools do
            begin
              _tmp1 := Concat(_tmp1, [_tmp0]);
            end;
        end;
    end;
  combos := _tmp1;
  writeln('--- Cross Join of three lists ---');
  for c in combos do
    begin
      writeln(c.n, ' ', c.l, ' ', c.b);
    end;
end.
