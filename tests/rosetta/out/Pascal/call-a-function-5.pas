// Generated by Mochi compiler v0.10.26 on 2025-07-16T11:36:18Z
program CallAFunction5;
{$mode objfpc}
{$modeswitch nestedprocvars}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

function doIt(p: specialize TFPGMap<string, integer>): integer;
var
  b: integer;
begin
  b := 0;
  if (p.IndexOf('b') >= 0) then ;
  result := p.KeyData['a'] + b + p.KeyData['c'];
  exit;
end;

function main(): integer;
var
  _tmp0: specialize TFPGMap<string, integer>;
  p: specialize TFPGMap<string, integer>;
begin
  _tmp0 := specialize TFPGMap<string, integer>.Create;
  p := _tmp0;
  p['a'] := 1;
  p['c'] := 9;
  writeln(IntToStr(doIt(p)));
end;

begin
  main();
end.
