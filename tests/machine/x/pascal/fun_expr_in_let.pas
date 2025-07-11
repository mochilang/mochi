program FunExprInLet;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

function _lambda0(x: integer): integer;
begin
  result := x * x;
  exit;
end;

var
  square: function (p0: integer): integer;

begin
  square := @_lambda0;
  writeln(square(6));
end.
