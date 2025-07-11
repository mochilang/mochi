program StringCompare;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

begin
  writeln(('a' < 'b'));
  writeln(('a' <= 'a'));
  writeln(('b' > 'a'));
  writeln(('b' >= 'b'));
end.
