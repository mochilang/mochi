program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<integer, Variant>;
  _tmp1: specialize TArray<specialize TFPGMap<string, Variant>>;
  l: string;
  letters: specialize TArray<string>;
  n: integer;
  nums: specialize TArray<integer>;
  p: specialize TFPGMap<string, Variant>;
  pairs: specialize TArray<specialize TFPGMap<string, Variant>>;

begin
  nums := specialize TArray<integer>([1, 2, 3]);
  letters := specialize TArray<string>(['A', 'B']);
  _tmp0 := specialize TFPGMap<integer, Variant>.Create;
  _tmp0.AddOrSetData('n', n);
  _tmp0.AddOrSetData('l', l);
  SetLength(_tmp1, 0);
  for n in nums do
    begin
      for l in letters do
        begin
          if not ((n mod 2 = 0)) then continue;
          _tmp1 := Concat(_tmp1, [_tmp0]);
        end;
    end;
  pairs := _tmp1;
  writeln('--- Even pairs ---');
  for p in pairs do
    begin
      writeln(p.n, ' ', p.l);
    end;
end.
