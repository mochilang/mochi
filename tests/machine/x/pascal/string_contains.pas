program StringContains;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  s: string;

begin
  s := 'catch';
  writeln(s.contains('cat'));
  writeln(s.contains('dog'));
end.
