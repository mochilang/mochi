{$mode objfpc}
program Main;
uses SysUtils;
var _nowSeed: int64 = 0;
var _nowSeeded: boolean = false;
procedure init_now();
var s: string; v: int64;
begin
  s := GetEnvironmentVariable('MOCHI_NOW_SEED');
  if s <> '' then begin
    Val(s, v);
    _nowSeed := v;
    _nowSeeded := true;
  end;
end;
function _now(): integer;
begin
  if _nowSeeded then begin
    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;
    _now := _nowSeed;
  end else begin
    _now := Integer(GetTickCount64()*1000);
  end;
end;
function _bench_now(): int64;
begin
  _bench_now := GetTickCount64()*1000;
end;
function _mem(): int64;
var h: TFPCHeapStatus;
begin
  h := GetFPCHeapStatus;
  _mem := h.CurrHeapUsed;
end;
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
end;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  input_str: string;
  node_count: integer;
  bst: integer;
  bt: integer;
  k: integer;
  n: integer;
function binomial_coefficient(n: integer; k: integer): integer; forward;
function catalan_number(node_count: integer): integer; forward;
function factorial(n: integer): integer; forward;
function binary_tree_count(node_count: integer): integer; forward;
function binomial_coefficient(n: integer; k: integer): integer;
var
  binomial_coefficient_result_: integer;
  binomial_coefficient_kk: integer;
  binomial_coefficient_i: integer;
begin
  binomial_coefficient_result_ := 1;
  binomial_coefficient_kk := k;
  if k > (n - k) then begin
  binomial_coefficient_kk := n - k;
end;
  for binomial_coefficient_i := 0 to (binomial_coefficient_kk - 1) do begin
  binomial_coefficient_result_ := binomial_coefficient_result_ * (n - binomial_coefficient_i);
  binomial_coefficient_result_ := binomial_coefficient_result_ div (binomial_coefficient_i + 1);
end;
  exit(binomial_coefficient_result_);
end;
function catalan_number(node_count: integer): integer;
begin
  exit(binomial_coefficient(2 * node_count, node_count) div (node_count + 1));
end;
function factorial(n: integer): integer;
var
  factorial_result_: integer;
  factorial_i: integer;
begin
  if n < 0 then begin
  writeln('factorial() not defined for negative values');
  exit(0);
end;
  factorial_result_ := 1;
  for factorial_i := 1 to (n + 1 - 1) do begin
  factorial_result_ := factorial_result_ * factorial_i;
end;
  exit(factorial_result_);
end;
function binary_tree_count(node_count: integer): integer;
begin
  exit(catalan_number(node_count) * factorial(node_count));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('Enter the number of nodes:');
  input_str := _input();
  node_count := StrToInt(input_str);
  if node_count <= 0 then begin
  writeln('We need some nodes to work with.');
end else begin
  bst := catalan_number(node_count);
  bt := binary_tree_count(node_count);
  writeln('Given', ' ', node_count, ' ', 'nodes, there are', ' ', bt, ' ', 'binary trees and', ' ', bst, ' ', 'binary search trees.');
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
