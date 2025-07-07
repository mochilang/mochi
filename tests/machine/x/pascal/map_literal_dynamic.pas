program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<string, integer>;
  m: specialize TFPGMap<string, integer>;
  x: integer;
  y: integer;

begin
  x := 3;
  y := 4;
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('a', x);
  _tmp0.AddOrSetData('b', y);
  m := _tmp0;
  writeln(m.KeyData['a'], ' ', m.KeyData['b']);
end.
