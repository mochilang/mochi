program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<integer, Variant>;
  m: specialize TFPGMap<integer, string>;

begin
  _tmp0 := specialize TFPGMap<integer, Variant>.Create;
  _tmp0.AddOrSetData(1, 'a');
  _tmp0.AddOrSetData(2, 'b');
  m := _tmp0;
  writeln(m.KeyData[1]);
end.
