program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TArray<specialize TFPGMap<string, Variant>>;
  _tmp1: specialize TFPGMap<string, Variant>;
  l: string;
  letters: specialize TArray<string>;
  n: integer;
  nums: specialize TArray<integer>;
  p: specialize TFPGMap<string, Variant>;
  pairs: specialize TArray<specialize TFPGMap<string, Variant>>;

begin
  nums := specialize TArray<integer>([1, 2, 3]);
  letters := specialize TArray<string>(['A', 'B']);
  SetLength(_tmp0, 0);
  for n in nums do
    begin
      for l in letters do
        begin
          if not ((n mod 2 = 0)) then continue;
          _tmp1 := specialize TFPGMap<string, Variant>.Create;
          _tmp1.AddOrSetData('n', n);
          _tmp1.AddOrSetData('l', l);
          _tmp0 := Concat(_tmp0, [_tmp1]);
        end;
    end;
  pairs := _tmp0;
  writeln('--- Even pairs ---');
  for p in pairs do
    begin
      writeln(p.n, ' ', p.l);
    end;
end.
