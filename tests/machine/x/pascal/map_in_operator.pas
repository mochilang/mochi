program MapInOperator;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<integer, string>;
  m: specialize TFPGMap<integer, string>;

begin
  _tmp0 := specialize TFPGMap<integer, string>.Create;
  _tmp0.AddOrSetData(1, 'a');
  _tmp0.AddOrSetData(2, 'b');
  m := _tmp0;
  writeln((m.IndexOf(1) >= 0));
  writeln((m.IndexOf(3) >= 0));
end.
