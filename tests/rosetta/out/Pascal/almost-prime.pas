// Generated by Mochi compiler v0.10.30 on 1970-01-01T00:00:00Z
program AlmostPrime;
{$mode objfpc}
{$modeswitch nestedprocvars}
uses SysUtils, fgl, Classes, Variants;

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


function kPrime(n: integer; k: integer): boolean;
var
  i: integer;
  nf: integer;
begin
  nf := 0;
  i := 2;
  while (i <= n) do
  begin
    while (n mod i = 0) do
    begin
      if (nf = k) then ;
      nf := nf + 1;
      n := n div i;
    end;
    i := i + 1;
  end;
  result := (nf = k);
  exit;
end;

function gen(k: integer; count: integer): specialize TArray<integer>;
var
  n: integer;
  r: specialize TArray<Variant>;
begin
  r := specialize TArray<Variant>([]);
  n := 2;
  while (Length(r) < count) do
  begin
    if kPrime(n, k) then ;
    n := n + 1;
  end;
  result := r;
  exit;
end;

procedure main();
var
  k: integer;
begin
  k := 1;
  while (k <= 5) do
  begin
    writeln(IntToStr(k) + ' ' + IntToStr(gen(k, 10)));
    k := k + 1;
  end;
end;

begin
  main();
end.
