program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

type
  TFunc0 = function (p0: integer): integer;


function makeAdder(n: integer): TFunc0;
begin
  result := _lambda0;
  exit;
end;

function _lambda0(x: integer): integer;
begin
  result := x + n;
  exit;
end;

var
  add10: function (integer): integer;

begin
  add10 := makeAdder(10);
  writeln(add10(7));
end.
