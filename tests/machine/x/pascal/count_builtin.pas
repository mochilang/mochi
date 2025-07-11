program CountBuiltin;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

  generic function _countList<T>(arr: specialize TArray<T>): integer;
begin
  Result := Length(arr);
end;

begin
  writeln(specialize _countList<integer>(specialize TArray<integer>([1, 2, 3])));
end.
