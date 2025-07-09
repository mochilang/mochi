program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TArray<integer>;
  _tmp1: specialize TFPGMap<Variant, integer>;
  m: specialize TFPGMap<string, integer>;
  s: string;
  x: integer;
  xs: specialize TArray<integer>;
  ys: specialize TArray<integer>;

begin
  xs := specialize TArray<integer>([1, 2, 3]);
  SetLength(_tmp0, 0);
  for x in xs do
    begin
      if not ((x mod 2 = 1)) then continue;
      _tmp0 := Concat(_tmp0, [x]);
    end;
  ys := _tmp0;
  writeln((1 in ys));
  writeln((2 in ys));
  _tmp1 := specialize TFPGMap<Variant, integer>.Create;
  _tmp1.AddOrSetData('a', 1);
  m := _tmp1;
  writeln((m.IndexOf('a') >= 0));
  writeln((m.IndexOf('b') >= 0));
  s := 'hello';
  writeln(('ell' in s));
  writeln(('foo' in s));
end.
