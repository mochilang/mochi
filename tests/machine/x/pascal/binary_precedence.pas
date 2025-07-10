program BinaryPrecedence;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

begin
  writeln(1 + 2 * 3);
  writeln(1 + 2 * 3);
  writeln(2 * 3 + 1);
  writeln(2 * 3 + 1);
end.
