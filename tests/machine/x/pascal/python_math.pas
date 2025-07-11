program PythonMath;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  area: double;
  log_e: double;
  r: double;
  root: double;
  sin45: double;

begin
  r := 3;
  area := math.pi * math.pow(r, 2);
  root := math.sqrt(49);
  sin45 := math.sin(math.pi / 4);
  log_e := math.log(math.e);
  writeln('Circle area with r =', ' ', r, ' ', '=>', ' ', area);
  writeln('Square root of 49:', ' ', root);
  writeln('sin(π/4):', ' ', sin45);
  writeln('log(e):', ' ', log_e);
end.
