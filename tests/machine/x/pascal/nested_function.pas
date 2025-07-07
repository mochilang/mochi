program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

function outer(x: integer): integer;
begin
  function inner(y: integer): integer;
  begin
    result := x + y;
    exit;
  end;
  result := inner(5);
  exit;
end;

begin
  writeln(outer(3));
end.
