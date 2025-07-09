program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp1: specialize TFPGMap<string, Variant>;
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
  SetLength(_tmp0, 0);
  for n in nums do
    begin
      for l in letters do
        begin
          for b in bools do
            begin
              _tmp1 := specialize TFPGMap<string, Variant>.Create;
              _tmp1.AddOrSetData('n', n);
              _tmp1.AddOrSetData('l', l);
              _tmp1.AddOrSetData('b', b);
              _tmp0 := Concat(_tmp0, [_tmp1]);
            end;
        end;
    end;
  combos := _tmp0;
  writeln('--- Cross Join of three lists ---');
  for c in combos do
    begin
      writeln(c.n, ' ', c.l, ' ', c.b);
    end;
end.
