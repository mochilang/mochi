{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type IntArrayArray = array of IntArray;
type MatrixChainResult = record
  matrix: array of IntArray;
  solution: array of IntArray;
end;
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
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function makeMatrixChainResult(matrix: IntArrayArray; solution: IntArrayArray): MatrixChainResult; forward;
function make_2d(make_2d_n: int64): IntArrayArray; forward;
function matrix_chain_order(matrix_chain_order_arr: IntArray): MatrixChainResult; forward;
function optimal_parenthesization(optimal_parenthesization_s: IntArrayArray; optimal_parenthesization_i: int64; optimal_parenthesization_j: int64): string; forward;
procedure main(); forward;
function makeMatrixChainResult(matrix: IntArrayArray; solution: IntArrayArray): MatrixChainResult;
begin
  Result.matrix := matrix;
  Result.solution := solution;
end;
function make_2d(make_2d_n: int64): IntArrayArray;
var
  make_2d_res: array of IntArray;
  make_2d_i: int64;
  make_2d_row: array of int64;
  make_2d_j: int64;
begin
  make_2d_res := [];
  make_2d_i := 0;
  while make_2d_i < make_2d_n do begin
  make_2d_row := [];
  make_2d_j := 0;
  while make_2d_j < make_2d_n do begin
  make_2d_row := concat(make_2d_row, IntArray([0]));
  make_2d_j := make_2d_j + 1;
end;
  make_2d_res := concat(make_2d_res, [make_2d_row]);
  make_2d_i := make_2d_i + 1;
end;
  exit(make_2d_res);
end;
function matrix_chain_order(matrix_chain_order_arr: IntArray): MatrixChainResult;
var
  matrix_chain_order_n: integer;
  matrix_chain_order_m: array of IntArray;
  matrix_chain_order_s: array of IntArray;
  matrix_chain_order_chain_length: int64;
  matrix_chain_order_a: int64;
  matrix_chain_order_b: int64;
  matrix_chain_order_c: int64;
  matrix_chain_order_cost: int64;
begin
  matrix_chain_order_n := Length(matrix_chain_order_arr);
  matrix_chain_order_m := make_2d(matrix_chain_order_n);
  matrix_chain_order_s := make_2d(matrix_chain_order_n);
  matrix_chain_order_chain_length := 2;
  while matrix_chain_order_chain_length < matrix_chain_order_n do begin
  matrix_chain_order_a := 1;
  while matrix_chain_order_a < ((matrix_chain_order_n - matrix_chain_order_chain_length) + 1) do begin
  matrix_chain_order_b := (matrix_chain_order_a + matrix_chain_order_chain_length) - 1;
  matrix_chain_order_m[matrix_chain_order_a][matrix_chain_order_b] := 1000000000;
  matrix_chain_order_c := matrix_chain_order_a;
  while matrix_chain_order_c < matrix_chain_order_b do begin
  matrix_chain_order_cost := (matrix_chain_order_m[matrix_chain_order_a][matrix_chain_order_c] + matrix_chain_order_m[matrix_chain_order_c + 1][matrix_chain_order_b]) + ((matrix_chain_order_arr[matrix_chain_order_a - 1] * matrix_chain_order_arr[matrix_chain_order_c]) * matrix_chain_order_arr[matrix_chain_order_b]);
  if matrix_chain_order_cost < matrix_chain_order_m[matrix_chain_order_a][matrix_chain_order_b] then begin
  matrix_chain_order_m[matrix_chain_order_a][matrix_chain_order_b] := matrix_chain_order_cost;
  matrix_chain_order_s[matrix_chain_order_a][matrix_chain_order_b] := matrix_chain_order_c;
end;
  matrix_chain_order_c := matrix_chain_order_c + 1;
end;
  matrix_chain_order_a := matrix_chain_order_a + 1;
end;
  matrix_chain_order_chain_length := matrix_chain_order_chain_length + 1;
end;
  exit(makeMatrixChainResult(matrix_chain_order_m, matrix_chain_order_s));
end;
function optimal_parenthesization(optimal_parenthesization_s: IntArrayArray; optimal_parenthesization_i: int64; optimal_parenthesization_j: int64): string;
var
  optimal_parenthesization_left: string;
  optimal_parenthesization_right: string;
begin
  if optimal_parenthesization_i = optimal_parenthesization_j then begin
  exit('A' + IntToStr(optimal_parenthesization_i));
end else begin
  optimal_parenthesization_left := optimal_parenthesization(optimal_parenthesization_s, optimal_parenthesization_i, optimal_parenthesization_s[optimal_parenthesization_i][optimal_parenthesization_j]);
  optimal_parenthesization_right := optimal_parenthesization(optimal_parenthesization_s, optimal_parenthesization_s[optimal_parenthesization_i][optimal_parenthesization_j] + 1, optimal_parenthesization_j);
  exit(((('( ' + optimal_parenthesization_left) + ' ') + optimal_parenthesization_right) + ' )');
end;
end;
procedure main();
var
  main_arr: array of int64;
  main_n: integer;
  main_res: MatrixChainResult;
  main_m: array of IntArray;
  main_s: array of IntArray;
  main_seq: string;
begin
  main_arr := [30, 35, 15, 5, 10, 20, 25];
  main_n := Length(main_arr);
  main_res := matrix_chain_order(main_arr);
  main_m := main_res.matrix;
  main_s := main_res.solution;
  writeln('No. of Operation required: ' + IntToStr(main_m[1][main_n - 1]));
  main_seq := optimal_parenthesization(main_s, 1, main_n - 1);
  writeln(main_seq);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
