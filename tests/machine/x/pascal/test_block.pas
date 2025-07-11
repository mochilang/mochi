program TestBlock;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

procedure test_addition_works;

var
  x: integer;
begin
  x := 1 + 2;
  if not ((x = 3)) then raise Exception.Create('expect failed');
end;

begin
  writeln('ok');
  test_addition_works;
end.
