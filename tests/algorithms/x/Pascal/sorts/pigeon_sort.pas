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
function make_list(make_list_n: int64; make_list_value: int64): IntArray; forward;
function min_value(min_value_arr: IntArray): int64; forward;
function max_value(max_value_arr: IntArray): int64; forward;
function pigeon_sort(pigeon_sort_array_: IntArray): IntArray; forward;
function make_list(make_list_n: int64; make_list_value: int64): IntArray;
var
  make_list_result_: array of int64;
  make_list_i: int64;
begin
  make_list_result_ := [];
  make_list_i := 0;
  while make_list_i < make_list_n do begin
  make_list_result_ := concat(make_list_result_, IntArray([make_list_value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_result_);
end;
function min_value(min_value_arr: IntArray): int64;
var
  min_value_m: int64;
  min_value_i: int64;
begin
  min_value_m := min_value_arr[0];
  min_value_i := 1;
  while min_value_i < Length(min_value_arr) do begin
  if min_value_arr[min_value_i] < min_value_m then begin
  min_value_m := min_value_arr[min_value_i];
end;
  min_value_i := min_value_i + 1;
end;
  exit(min_value_m);
end;
function max_value(max_value_arr: IntArray): int64;
var
  max_value_m: int64;
  max_value_i: int64;
begin
  max_value_m := max_value_arr[0];
  max_value_i := 1;
  while max_value_i < Length(max_value_arr) do begin
  if max_value_arr[max_value_i] > max_value_m then begin
  max_value_m := max_value_arr[max_value_i];
end;
  max_value_i := max_value_i + 1;
end;
  exit(max_value_m);
end;
function pigeon_sort(pigeon_sort_array_: IntArray): IntArray;
var
  pigeon_sort_mn: int64;
  pigeon_sort_mx: int64;
  pigeon_sort_holes_range: int64;
  pigeon_sort_holes: array of int64;
  pigeon_sort_holes_repeat: array of int64;
  pigeon_sort_i: int64;
  pigeon_sort_index: int64;
  pigeon_sort_array_index: int64;
  pigeon_sort_h: int64;
begin
  if Length(pigeon_sort_array_) = 0 then begin
  exit(pigeon_sort_array_);
end;
  pigeon_sort_mn := min_value(pigeon_sort_array_);
  pigeon_sort_mx := max_value(pigeon_sort_array_);
  pigeon_sort_holes_range := (pigeon_sort_mx - pigeon_sort_mn) + 1;
  pigeon_sort_holes := make_list(pigeon_sort_holes_range, 0);
  pigeon_sort_holes_repeat := make_list(pigeon_sort_holes_range, 0);
  pigeon_sort_i := 0;
  while pigeon_sort_i < Length(pigeon_sort_array_) do begin
  pigeon_sort_index := pigeon_sort_array_[pigeon_sort_i] - pigeon_sort_mn;
  pigeon_sort_holes[pigeon_sort_index] := pigeon_sort_array_[pigeon_sort_i];
  pigeon_sort_holes_repeat[pigeon_sort_index] := pigeon_sort_holes_repeat[pigeon_sort_index] + 1;
  pigeon_sort_i := pigeon_sort_i + 1;
end;
  pigeon_sort_array_index := 0;
  pigeon_sort_h := 0;
  while pigeon_sort_h < pigeon_sort_holes_range do begin
  while pigeon_sort_holes_repeat[pigeon_sort_h] > 0 do begin
  pigeon_sort_array_[pigeon_sort_array_index] := pigeon_sort_holes[pigeon_sort_h];
  pigeon_sort_array_index := pigeon_sort_array_index + 1;
  pigeon_sort_holes_repeat[pigeon_sort_h] := pigeon_sort_holes_repeat[pigeon_sort_h] - 1;
end;
  pigeon_sort_h := pigeon_sort_h + 1;
end;
  exit(pigeon_sort_array_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(pigeon_sort([0, 5, 3, 2, 2])));
  writeln(list_int_to_str(pigeon_sort([])));
  writeln(list_int_to_str(pigeon_sort([-2, -5, -45])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
