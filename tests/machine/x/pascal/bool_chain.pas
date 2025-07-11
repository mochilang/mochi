program BoolChain;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

function boom(): boolean;
begin
  writeln('boom');
  result := True;
  exit;
end;

begin
  writeln((((1 < 2) and (2 < 3)) and (3 < 4)));
  writeln((((1 < 2) and (2 > 3)) and boom()));
  writeln(((((1 < 2) and (2 < 3)) and (3 > 4)) and boom()));
end.
