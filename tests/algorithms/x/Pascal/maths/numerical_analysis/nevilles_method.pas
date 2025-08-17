{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type NevilleResult = record
  value: real;
  table: array of RealArray;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  x0: real;
  x_points: RealArray;
  y_points: RealArray;
function makeNevilleResult(value: real; table: RealArrayArray): NevilleResult; forward;
function neville_interpolate(x_points: RealArray; y_points: RealArray; x0: real): NevilleResult; forward;
procedure test_neville(); forward;
procedure main(); forward;
function makeNevilleResult(value: real; table: RealArrayArray): NevilleResult;
begin
  Result.value := value;
  Result.table := table;
end;
function neville_interpolate(x_points: RealArray; y_points: RealArray; x0: real): NevilleResult;
var
  neville_interpolate_n: integer;
  neville_interpolate_q: array of RealArray;
  neville_interpolate_i: integer;
  neville_interpolate_row: array of real;
  neville_interpolate_j: integer;
  neville_interpolate_col: integer;
  neville_interpolate_row_idx: integer;
begin
  neville_interpolate_n := Length(x_points);
  neville_interpolate_q := [];
  neville_interpolate_i := 0;
  while neville_interpolate_i < neville_interpolate_n do begin
  neville_interpolate_row := [];
  neville_interpolate_j := 0;
  while neville_interpolate_j < neville_interpolate_n do begin
  neville_interpolate_row := concat(neville_interpolate_row, [0]);
  neville_interpolate_j := neville_interpolate_j + 1;
end;
  neville_interpolate_q := concat(neville_interpolate_q, [neville_interpolate_row]);
  neville_interpolate_i := neville_interpolate_i + 1;
end;
  neville_interpolate_i := 0;
  while neville_interpolate_i < neville_interpolate_n do begin
  neville_interpolate_q[neville_interpolate_i][1] := y_points[neville_interpolate_i];
  neville_interpolate_i := neville_interpolate_i + 1;
end;
  neville_interpolate_col := 2;
  while neville_interpolate_col < neville_interpolate_n do begin
  neville_interpolate_row_idx := neville_interpolate_col;
  while neville_interpolate_row_idx < neville_interpolate_n do begin
  neville_interpolate_q[neville_interpolate_row_idx][neville_interpolate_col] := (((x0 - x_points[(neville_interpolate_row_idx - neville_interpolate_col) + 1]) * neville_interpolate_q[neville_interpolate_row_idx][neville_interpolate_col - 1]) - ((x0 - x_points[neville_interpolate_row_idx]) * neville_interpolate_q[neville_interpolate_row_idx - 1][neville_interpolate_col - 1])) / (x_points[neville_interpolate_row_idx] - x_points[(neville_interpolate_row_idx - neville_interpolate_col) + 1]);
  neville_interpolate_row_idx := neville_interpolate_row_idx + 1;
end;
  neville_interpolate_col := neville_interpolate_col + 1;
end;
  exit(makeNevilleResult(neville_interpolate_q[neville_interpolate_n - 1][neville_interpolate_n - 1], neville_interpolate_q));
end;
procedure test_neville();
var
  test_neville_xs: array of real;
  test_neville_ys: array of real;
  test_neville_r1: NevilleResult;
  test_neville_r2: NevilleResult;
begin
  test_neville_xs := [1, 2, 3, 4, 6];
  test_neville_ys := [6, 7, 8, 9, 11];
  test_neville_r1 := neville_interpolate(test_neville_xs, test_neville_ys, 5);
  if test_neville_r1.value <> 10 then begin
  panic('neville_interpolate at 5 failed');
end;
  test_neville_r2 := neville_interpolate(test_neville_xs, test_neville_ys, 99);
  if test_neville_r2.value <> 104 then begin
  panic('neville_interpolate at 99 failed');
end;
end;
procedure main();
var
  main_xs: array of real;
  main_ys: array of real;
  main_r: NevilleResult;
begin
  test_neville();
  main_xs := [1, 2, 3, 4, 6];
  main_ys := [6, 7, 8, 9, 11];
  main_r := neville_interpolate(main_xs, main_ys, 5);
  writeln(main_r.value);
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
end.
