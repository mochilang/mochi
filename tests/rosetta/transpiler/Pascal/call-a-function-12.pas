{$mode objfpc}
program Main;
uses SysUtils;
type FuncType1 = function(b: integer): integer;
type FuncType2 = function(p0: integer): integer;
type FuncType3 = function(y: integer): integer;
var
  anon0_cap0: integer;
  anon4_cap0: integer;
  main_add2: FuncType2;
  main_add3: FuncType2;
  main_partial: FuncType2;
function anon0(b: integer): integer;
begin
  exit(anon0_cap0 + b);
end;
function anon0_make(a: integer): FuncType1;
begin
  anon0_cap0 := a;
  exit(anon0);
end;
function mkAdd(a: integer): FuncType2;
begin
  exit(anon0_make(a));
end;
function mysum(x: integer; y: integer): integer;
begin
  exit(x + y);
end;
function anon4(y: integer): integer;
begin
  exit(mysum(anon4_cap0, y));
end;
function anon4_make(x: integer): FuncType3;
begin
  anon4_cap0 := x;
  exit(anon4);
end;
function partialSum(x: integer): FuncType2;
begin
  exit(anon4_make(x));
end;
procedure main();
begin
  main_add2 := mkAdd(2);
  main_add3 := mkAdd(3);
  writeln((IntToStr(add2(5)) + ' ') + IntToStr(add3(6)));
  main_partial := partialSum(13);
  writeln(IntToStr(partial(5)));
end;
begin
  main();
end.
