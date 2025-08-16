{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
  num: real;
  x: IntArray;
function abs_val(num: real): real; forward;
function abs_min(x: IntArray): integer; forward;
function abs_max(x: IntArray): integer; forward;
function abs_max_sort(x: IntArray): integer; forward;
procedure test_abs_val(); forward;
procedure main(); forward;
function abs_val(num: real): real;
begin
  if num < 0 then begin
  exit(-num);
end;
  exit(num);
end;
function abs_min(x: IntArray): integer;
var
  abs_min_j: integer;
  abs_min_idx: integer;
  abs_min_i: integer;
begin
  if Length(x) = 0 then begin
  panic('abs_min() arg is an empty sequence');
end;
  abs_min_j := x[0];
  abs_min_idx := 0;
  while abs_min_idx < Length(x) do begin
  abs_min_i := x[abs_min_idx];
  if abs_val(Double(abs_min_i)) < abs_val(Double(abs_min_j)) then begin
  abs_min_j := abs_min_i;
end;
  abs_min_idx := abs_min_idx + 1;
end;
  exit(abs_min_j);
end;
function abs_max(x: IntArray): integer;
var
  abs_max_j: integer;
  abs_max_idx: integer;
  abs_max_i: integer;
begin
  if Length(x) = 0 then begin
  panic('abs_max() arg is an empty sequence');
end;
  abs_max_j := x[0];
  abs_max_idx := 0;
  while abs_max_idx < Length(x) do begin
  abs_max_i := x[abs_max_idx];
  if abs_val(Double(abs_max_i)) > abs_val(Double(abs_max_j)) then begin
  abs_max_j := abs_max_i;
end;
  abs_max_idx := abs_max_idx + 1;
end;
  exit(abs_max_j);
end;
function abs_max_sort(x: IntArray): integer;
var
  abs_max_sort_arr: array of integer;
  abs_max_sort_i: integer;
  abs_max_sort_n: integer;
  abs_max_sort_a: integer;
  abs_max_sort_b: integer;
  abs_max_sort_temp: integer;
begin
  if Length(x) = 0 then begin
  panic('abs_max_sort() arg is an empty sequence');
end;
  abs_max_sort_arr := [];
  abs_max_sort_i := 0;
  while abs_max_sort_i < Length(x) do begin
  abs_max_sort_arr := concat(abs_max_sort_arr, IntArray([x[abs_max_sort_i]]));
  abs_max_sort_i := abs_max_sort_i + 1;
end;
  abs_max_sort_n := Length(abs_max_sort_arr);
  abs_max_sort_a := 0;
  while abs_max_sort_a < abs_max_sort_n do begin
  abs_max_sort_b := 0;
  while abs_max_sort_b < ((abs_max_sort_n - abs_max_sort_a) - 1) do begin
  if abs_val(Double(abs_max_sort_arr[abs_max_sort_b])) > abs_val(Double(abs_max_sort_arr[abs_max_sort_b + 1])) then begin
  abs_max_sort_temp := abs_max_sort_arr[abs_max_sort_b];
  abs_max_sort_arr[abs_max_sort_b] := abs_max_sort_arr[abs_max_sort_b + 1];
  abs_max_sort_arr[abs_max_sort_b + 1] := abs_max_sort_temp;
end;
  abs_max_sort_b := abs_max_sort_b + 1;
end;
  abs_max_sort_a := abs_max_sort_a + 1;
end;
  exit(abs_max_sort_arr[abs_max_sort_n - 1]);
end;
procedure test_abs_val();
var
  test_abs_val_a: array of integer;
begin
  if abs_val(0) <> 0 then begin
  panic('abs_val(0) failed');
end;
  if abs_val(34) <> 34 then begin
  panic('abs_val(34) failed');
end;
  if abs_val(-1e+11) <> 1e+11 then begin
  panic('abs_val large failed');
end;
  test_abs_val_a := [-3, -1, 2, -11];
  if abs_max(test_abs_val_a) <> -11 then begin
  panic('abs_max failed');
end;
  if abs_max_sort(test_abs_val_a) <> -11 then begin
  panic('abs_max_sort failed');
end;
  if abs_min(test_abs_val_a) <> -1 then begin
  panic('abs_min failed');
end;
end;
procedure main();
begin
  test_abs_val();
  writeln(abs_val(-34));
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
