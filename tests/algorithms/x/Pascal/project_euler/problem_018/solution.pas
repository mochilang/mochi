{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type IntArray = array of int64;
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
function solution(): int64; forward;
function solution(): int64;
var
  solution_triangle: array of IntArray;
  solution_i: int64;
  solution_j: int64;
  solution_prev_row: array of int64;
  solution_number1: int64;
  solution_number2: int64;
  solution_max_val: int64;
  solution_last: array of int64;
  solution_k: int64;
  solution_best: int64;
begin
  solution_triangle := [[75], [95, 64], [17, 47, 82], [18, 35, 87, 10], [20, 4, 82, 47, 65], [19, 1, 23, 75, 3, 34], [88, 2, 77, 73, 7, 63, 67], [99, 65, 4, 28, 6, 16, 70, 92], [41, 41, 26, 56, 83, 40, 80, 70, 33], [41, 48, 72, 33, 47, 32, 37, 16, 94, 29], [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14], [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57], [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48], [63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31], [4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23]];
  solution_i := 1;
  while solution_i < Length(solution_triangle) do begin
  solution_j := 0;
  while solution_j < Length(solution_triangle[solution_i]) do begin
  solution_prev_row := solution_triangle[solution_i - 1];
  if solution_j <> Length(solution_prev_row) then begin
  solution_number1 := solution_prev_row[solution_j];
end else begin
  solution_number1 := 0;
end;
  if solution_j > 0 then begin
  solution_number2 := solution_prev_row[solution_j - 1];
end else begin
  solution_number2 := 0;
end;
  if solution_number1 > solution_number2 then begin
  solution_max_val := solution_number1;
end else begin
  solution_max_val := solution_number2;
end;
  solution_triangle[solution_i][solution_j] := solution_triangle[solution_i][solution_j] + solution_max_val;
  solution_j := solution_j + 1;
end;
  solution_i := solution_i + 1;
end;
  solution_last := solution_triangle[Length(solution_triangle) - 1];
  solution_k := 0;
  solution_best := 0;
  while solution_k < Length(solution_last) do begin
  if solution_last[solution_k] > solution_best then begin
  solution_best := solution_last[solution_k];
end;
  solution_k := solution_k + 1;
end;
  exit(solution_best);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(solution()));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
