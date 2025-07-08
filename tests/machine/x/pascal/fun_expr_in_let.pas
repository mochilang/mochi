program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

function _lambda0(x: integer): integer;
begin
  result := x * x;
  exit;
end;

var
  square: function (integer): integer;

begin
  square := @_lambda0;
  writeln(square(6));
end.
