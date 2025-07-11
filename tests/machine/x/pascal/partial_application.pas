program PartialApplication;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

function add(a: integer; b: integer): integer;
begin
  result := a + b;
  exit;
end;

var
  add5: function (p0: integer): integer;

begin
  add5 := add(5);
  writeln(add5(3));
end.
