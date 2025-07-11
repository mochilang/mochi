program StringInOperator;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  s: string;

begin
  s := 'catch';
  writeln((Pos('cat', s) > 0));
  writeln((Pos('dog', s) > 0));
end.
