program main;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  s: string;

begin
  s := 'catch';
  writeln(('cat' in s));
  writeln(('dog' in s));
end.
