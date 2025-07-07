program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

function sum_rec(n: integer; acc: integer): integer;
begin
  if (n = 0) then ;
  result := sum_rec(n - 1, acc + n);
  exit;
end;

begin
  writeln(sum_rec(10, 0));
end.
