program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TFPGMap<string, integer>;
  scores: specialize TFPGMap<string, integer>;

begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('alice', 1);
  scores := _tmp0;
  scores.KeyData['bob'] := 2;
  writeln(scores.KeyData['bob']);
end.
