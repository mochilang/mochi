program JsonBuiltin;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

  generic procedure _json<T>(v: T);
begin
  writeln('[]');
end;

var
  _tmp0: specialize TFPGMap<string, integer>;
  m: specialize TFPGMap<string, integer>;

begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  _tmp0.AddOrSetData('a', 1);
  _tmp0.AddOrSetData('b', 2);
  m := _tmp0;
  specialize _json<specialize TFPGMap<string, integer>>(m);
end.
