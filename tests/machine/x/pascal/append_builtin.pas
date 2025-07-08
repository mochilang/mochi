program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic function _appendList<T>(arr: specialize TArray<T>; val: T): specialize TArray<T>;

var i,n: Integer;
begin
  n := Length(arr);
  SetLength(Result, n + 1);
  for i := 0 to n - 1 do
    Result[i] := arr[i];
  Result[n] := val;
end;

generic procedure _printList<T>(arr: specialize TArray<T>);

var i: Integer;
begin
  for i := 0 to High(arr) do
    begin
      if i > 0 then Write(' ');
      Write(arr[i]);
    end;
  writeln();
end;

var
  a: specialize TArray<integer>;

begin
  a := specialize TArray<integer>([1, 2]);
  specialize _printList<integer>(specialize _appendList<integer>(a, 3));
end.
