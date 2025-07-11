program BreakContinue;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  n: integer;
  numbers: specialize TArray<integer>;

begin
  numbers := specialize TArray<integer>([1, 2, 3, 4, 5, 6, 7, 8, 9]);
  for n in numbers do
    begin
      if (n mod 2 = 0) then ;
      if (n > 7) then ;
      writeln('odd number:', ' ', n);
    end;
end.
