{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
function minimum_subarray_sum(minimum_subarray_sum_target: int64; minimum_subarray_sum_numbers: IntArray): int64; forward;
function minimum_subarray_sum(minimum_subarray_sum_target: int64; minimum_subarray_sum_numbers: IntArray): int64;
var
  minimum_subarray_sum_n: integer;
  minimum_subarray_sum_i: int64;
  minimum_subarray_sum_left: int64;
  minimum_subarray_sum_right: int64;
  minimum_subarray_sum_curr_sum: int64;
  minimum_subarray_sum_min_len: integer;
  minimum_subarray_sum_current_len: int64;
begin
  minimum_subarray_sum_n := Length(minimum_subarray_sum_numbers);
  if minimum_subarray_sum_n = 0 then begin
  exit(0);
end;
  if minimum_subarray_sum_target = 0 then begin
  minimum_subarray_sum_i := 0;
  while minimum_subarray_sum_i < minimum_subarray_sum_n do begin
  if minimum_subarray_sum_numbers[minimum_subarray_sum_i] = 0 then begin
  exit(0);
end;
  minimum_subarray_sum_i := minimum_subarray_sum_i + 1;
end;
end;
  minimum_subarray_sum_left := 0;
  minimum_subarray_sum_right := 0;
  minimum_subarray_sum_curr_sum := 0;
  minimum_subarray_sum_min_len := minimum_subarray_sum_n + 1;
  while minimum_subarray_sum_right < minimum_subarray_sum_n do begin
  minimum_subarray_sum_curr_sum := minimum_subarray_sum_curr_sum + minimum_subarray_sum_numbers[minimum_subarray_sum_right];
  while (minimum_subarray_sum_curr_sum >= minimum_subarray_sum_target) and (minimum_subarray_sum_left <= minimum_subarray_sum_right) do begin
  minimum_subarray_sum_current_len := (minimum_subarray_sum_right - minimum_subarray_sum_left) + 1;
  if minimum_subarray_sum_current_len < minimum_subarray_sum_min_len then begin
  minimum_subarray_sum_min_len := minimum_subarray_sum_current_len;
end;
  minimum_subarray_sum_curr_sum := minimum_subarray_sum_curr_sum - minimum_subarray_sum_numbers[minimum_subarray_sum_left];
  minimum_subarray_sum_left := minimum_subarray_sum_left + 1;
end;
  minimum_subarray_sum_right := minimum_subarray_sum_right + 1;
end;
  if minimum_subarray_sum_min_len = (minimum_subarray_sum_n + 1) then begin
  exit(0);
end;
  exit(minimum_subarray_sum_min_len);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(minimum_subarray_sum(7, [2, 3, 1, 2, 4, 3])));
  writeln(IntToStr(minimum_subarray_sum(7, [2, 3, -1, 2, 4, -3])));
  writeln(IntToStr(minimum_subarray_sum(11, [1, 1, 1, 1, 1, 1, 1, 1])));
  writeln(IntToStr(minimum_subarray_sum(0, [1, 2, 3])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
