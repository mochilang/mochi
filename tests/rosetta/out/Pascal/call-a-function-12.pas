// Generated by Mochi compiler v0.10.26 on 2025-07-16T11:36:18Z
program CallAFunction12;
{$mode objfpc}
{$modeswitch nestedprocvars}
uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;
type
  TFunc0 = function(p0: integer): integer is nested;


function mkAdd(a: integer): TFunc0;
function _lambda0(b: integer): integer;
begin
  result := a + b;
  exit;
end;

begin
  result := @_lambda0;
  exit;
end;

function mysum(x: integer; y: integer): integer;
begin
  result := x + y;
  exit;
end;

function partialSum(x: integer): TFunc0;
function _lambda0(y: integer): integer;
begin
  result := mysum(x, y);
  exit;
end;

begin
  result := @_lambda0;
  exit;
end;

function main(): integer;
var
  add2: Variant;
  add3: Variant;
  partial: Variant;
begin
  add2 := mkAdd(2);
  add3 := mkAdd(3);
  writeln(IntToStr(add2(5)) + ' ' + IntToStr(add3(6)));
  partial := partialSum(13);
  writeln(IntToStr(partial(5)));
end;

begin
  main();
end.
