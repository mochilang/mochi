program ExistsBuiltin;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

var
  _tmp0: specialize TArray<integer>;
  data: specialize TArray<integer>;
  flag: boolean;
  x: integer;

begin
  data := specialize TArray<integer>([1, 2]);
  SetLength(_tmp0, 0);
  for x in data do
    begin
      if not ((x = 1)) then continue;
      _tmp0 := Concat(_tmp0, [x]);
    end;
  flag := (Length(_tmp0) > 0);
  writeln(flag);
end.
