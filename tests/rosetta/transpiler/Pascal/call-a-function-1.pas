{$mode objfpc}
program Main;
type VariantArray = array of Variant;
type IntArray = array of integer;
function f(): VariantArray;
begin
  exit([0, 0]);
end;
function g(a: integer; b: real): integer;
begin
  exit(0);
end;
procedure h(s: string; nums: IntArray);
begin
end;
begin
end.
