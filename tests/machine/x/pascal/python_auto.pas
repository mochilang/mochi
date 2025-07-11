program PythonAuto;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

begin
  writeln(math.sqrt(16));
  writeln(math.pi);
end.
