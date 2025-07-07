program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<string, Variant>;
  _tmp1: specialize TFPGMap<string, integer>;
  data: specialize TFPGMap<string, specialize TFPGMap<string, integer>>;

begin
  _tmp0 := specialize TFPGMap<string, Variant>.Create;
  _tmp1 := specialize TFPGMap<string, integer>.Create;
  _tmp1.AddOrSetData('inner', 1);
  _tmp0.AddOrSetData('outer', _tmp1);
  data := _tmp0;
  data['outer']['inner'] := 2;
  writeln(data.KeyData['outer'].KeyData['inner']);
end.
