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
function flip(flip_arr: IntArray; flip_k: int64): IntArray; forward;
function find_max_index(find_max_index_arr: IntArray; find_max_index_n: int64): int64; forward;
function pancake_sort(pancake_sort_arr: IntArray): IntArray; forward;
procedure main(); forward;
function flip(flip_arr: IntArray; flip_k: int64): IntArray;
var
  flip_start: int64;
  flip_end_: int64;
  flip_temp: int64;
begin
  flip_start := 0;
  flip_end_ := flip_k;
  while flip_start < flip_end_ do begin
  flip_temp := flip_arr[flip_start];
  flip_arr[flip_start] := flip_arr[flip_end_];
  flip_arr[flip_end_] := flip_temp;
  flip_start := flip_start + 1;
  flip_end_ := flip_end_ - 1;
end;
  exit(flip_arr);
end;
function find_max_index(find_max_index_arr: IntArray; find_max_index_n: int64): int64;
var
  find_max_index_mi: int64;
  find_max_index_i: int64;
begin
  find_max_index_mi := 0;
  find_max_index_i := 1;
  while find_max_index_i < find_max_index_n do begin
  if find_max_index_arr[find_max_index_i] > find_max_index_arr[find_max_index_mi] then begin
  find_max_index_mi := find_max_index_i;
end;
  find_max_index_i := find_max_index_i + 1;
end;
  exit(find_max_index_mi);
end;
function pancake_sort(pancake_sort_arr: IntArray): IntArray;
var
  pancake_sort_cur: integer;
  pancake_sort_mi: int64;
begin
  pancake_sort_cur := Length(pancake_sort_arr);
  while pancake_sort_cur > 1 do begin
  pancake_sort_mi := find_max_index(pancake_sort_arr, pancake_sort_cur);
  pancake_sort_arr := flip(pancake_sort_arr, pancake_sort_mi);
  pancake_sort_arr := flip(pancake_sort_arr, pancake_sort_cur - 1);
  pancake_sort_cur := pancake_sort_cur - 1;
end;
  exit(pancake_sort_arr);
end;
procedure main();
var
  main_data: array of int64;
  main_sorted: array of int64;
begin
  main_data := [3, 6, 1, 10, 2];
  main_sorted := pancake_sort(main_data);
  writeln(list_int_to_str(main_sorted));
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
