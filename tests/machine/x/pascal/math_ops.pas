program MathOps;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

begin
  writeln(6 * 7);
  writeln(7 div 2);
  writeln(7 mod 2);
end.
