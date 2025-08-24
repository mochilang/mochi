{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type IntArrayArray = array of IntArray;
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
function list_int_to_str(xs: array of int64): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  RADIX: int64;
function make_buckets(): IntArrayArray; forward;
function max_value(max_value_xs: IntArray): int64; forward;
function radix_sort(radix_sort_list_of_ints: IntArray): IntArray; forward;
function make_buckets(): IntArrayArray;
var
  make_buckets_buckets: array of IntArray;
  make_buckets_i: int64;
begin
  make_buckets_buckets := [];
  make_buckets_i := 0;
  while make_buckets_i < RADIX do begin
  make_buckets_buckets := concat(make_buckets_buckets, [[]]);
  make_buckets_i := make_buckets_i + 1;
end;
  exit(make_buckets_buckets);
end;
function max_value(max_value_xs: IntArray): int64;
var
  max_value_max_val: int64;
  max_value_i: int64;
begin
  max_value_max_val := max_value_xs[0];
  max_value_i := 1;
  while max_value_i < Length(max_value_xs) do begin
  if max_value_xs[max_value_i] > max_value_max_val then begin
  max_value_max_val := max_value_xs[max_value_i];
end;
  max_value_i := max_value_i + 1;
end;
  exit(max_value_max_val);
end;
function radix_sort(radix_sort_list_of_ints: IntArray): IntArray;
var
  radix_sort_placement: int64;
  radix_sort_max_digit: int64;
  radix_sort_buckets: IntArrayArray;
  radix_sort_i: int64;
  radix_sort_value: int64;
  radix_sort_tmp: int64;
  radix_sort_a: int64;
  radix_sort_b: int64;
  radix_sort_bucket: array of int64;
  radix_sort_j: int64;
begin
  radix_sort_placement := 1;
  radix_sort_max_digit := max_value(radix_sort_list_of_ints);
  while radix_sort_placement <= radix_sort_max_digit do begin
  radix_sort_buckets := make_buckets();
  radix_sort_i := 0;
  while radix_sort_i < Length(radix_sort_list_of_ints) do begin
  radix_sort_value := radix_sort_list_of_ints[radix_sort_i];
  radix_sort_tmp := (radix_sort_value div radix_sort_placement) mod RADIX;
  radix_sort_buckets[radix_sort_tmp] := concat(radix_sort_buckets[radix_sort_tmp], IntArray([radix_sort_value]));
  radix_sort_i := radix_sort_i + 1;
end;
  radix_sort_a := 0;
  radix_sort_b := 0;
  while radix_sort_b < RADIX do begin
  radix_sort_bucket := radix_sort_buckets[radix_sort_b];
  radix_sort_j := 0;
  while radix_sort_j < Length(radix_sort_bucket) do begin
  radix_sort_list_of_ints[radix_sort_a] := radix_sort_bucket[radix_sort_j];
  radix_sort_a := radix_sort_a + 1;
  radix_sort_j := radix_sort_j + 1;
end;
  radix_sort_b := radix_sort_b + 1;
end;
  radix_sort_placement := radix_sort_placement * RADIX;
end;
  exit(radix_sort_list_of_ints);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  RADIX := 10;
  writeln(list_int_to_str(radix_sort([0, 5, 3, 2, 2])));
  writeln(list_int_to_str(radix_sort([1, 100, 10, 1000])));
  writeln(list_int_to_str(radix_sort([15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
