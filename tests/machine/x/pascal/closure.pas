program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

type
  TFunc0 = function (p0: integer): integer is nested;


function makeAdder(n: integer): TFunc0;
function _lambda0(x: integer): integer;
begin
  result := x + n;
  exit;
end;

begin
  result := @_lambda0;
  exit;
end;

var
  add10: function (p0: integer): integer is nested;

begin
  add10 := makeAdder(10);
  writeln(add10(7));
end.
