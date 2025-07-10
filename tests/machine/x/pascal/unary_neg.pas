program UnaryNeg;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

begin
  writeln(-3);
  writeln(5 + -2);
end.
