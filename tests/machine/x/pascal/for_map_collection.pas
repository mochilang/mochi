program ForMapCollection;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<string, integer>;
  _tmp1: integer;
  k: string;
  m: specialize TFPGMap<string, integer>;

begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('a', 1);
  _tmp0.AddOrSetData('b', 2);
  m := _tmp0;
  for _tmp1 := 0 to m.Count - 1 do
    begin
      k := m.Keys[_tmp1];
      writeln(k);
    end;
end.
