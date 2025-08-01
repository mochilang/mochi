// Generated by Mochi compiler v0.10.30 on 1970-01-01T00:00:00Z
program ApplyACallbackToAnArray2;
{$mode objfpc}
{$modeswitch nestedprocvars}
uses SysUtils, fgl, Classes, Variants;

type
  generic TArray<T> = array of T;
type
  TFunc0 = function(p0: integer): integer is nested;

generic function _appendList<T>(arr: specialize TArray<T>; val: T): specialize TArray<T>;
var i,n: Integer;
begin
  n := Length(arr);
  SetLength(Result, n + 1);
  for i := 0 to n - 1 do
    Result[i] := arr[i];
  Result[n] := val;
end;


procedure each(xs: specialize TArray<integer>; f: TFunc0);
var
  x: Variant;
begin
  for x in xs do
  begin
    f(x);
  end;
end;

function Map(xs: specialize TArray<integer>; f: TFunc0): specialize TArray<integer>;
var
  r: specialize TArray<Variant>;
  x: Variant;
begin
  r := specialize TArray<Variant>([]);
  for x in xs do
  begin
    r := specialize _appendList<Variant>(r, f(x));
  end;
  result := r;
  exit;
end;

procedure main();
var
  s: specialize TArray<Variant>;
function _lambda0(i: integer): any;
begin
  result := writeln(IntToStr(i * i));
  exit;
end;

function _lambda0(i: integer): integer;
begin
  result := i * i;
  exit;
end;

begin
  s := specialize TArray<Variant>([1, 2, 3, 4, 5]);
  each(s, _lambda0);
  writeln(IntToStr(Map(s, _lambda0)));
end;

begin
  main();
end.
