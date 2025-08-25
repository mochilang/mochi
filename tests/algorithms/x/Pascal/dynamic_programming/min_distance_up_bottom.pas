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
function min3(min3_a: int64; min3_b: int64; min3_c: int64): int64; forward;
function helper(helper_word1: string; helper_word2: string; helper_cache: IntArrayArray; helper_i: int64; helper_j: int64; helper_len1: int64; helper_len2: int64): int64; forward;
function min_distance_up_bottom(min_distance_up_bottom_word1: string; min_distance_up_bottom_word2: string): int64; forward;
function min3(min3_a: int64; min3_b: int64; min3_c: int64): int64;
var
  min3_m: int64;
begin
  min3_m := min3_a;
  if min3_b < min3_m then begin
  min3_m := min3_b;
end;
  if min3_c < min3_m then begin
  min3_m := min3_c;
end;
  exit(min3_m);
end;
function helper(helper_word1: string; helper_word2: string; helper_cache: IntArrayArray; helper_i: int64; helper_j: int64; helper_len1: int64; helper_len2: int64): int64;
var
  helper_diff: int64;
  helper_delete_cost: int64;
  helper_insert_cost: int64;
  helper_replace_cost: int64;
begin
  if helper_i >= helper_len1 then begin
  exit(helper_len2 - helper_j);
end;
  if helper_j >= helper_len2 then begin
  exit(helper_len1 - helper_i);
end;
  if helper_cache[helper_i][helper_j] <> (0 - 1) then begin
  exit(helper_cache[helper_i][helper_j]);
end;
  helper_diff := 0;
  if copy(helper_word1, helper_i+1, (helper_i + 1 - (helper_i))) <> copy(helper_word2, helper_j+1, (helper_j + 1 - (helper_j))) then begin
  helper_diff := 1;
end;
  helper_delete_cost := 1 + helper(helper_word1, helper_word2, helper_cache, helper_i + 1, helper_j, helper_len1, helper_len2);
  helper_insert_cost := 1 + helper(helper_word1, helper_word2, helper_cache, helper_i, helper_j + 1, helper_len1, helper_len2);
  helper_replace_cost := helper_diff + helper(helper_word1, helper_word2, helper_cache, helper_i + 1, helper_j + 1, helper_len1, helper_len2);
  helper_cache[helper_i][helper_j] := min3(helper_delete_cost, helper_insert_cost, helper_replace_cost);
  exit(helper_cache[helper_i][helper_j]);
end;
function min_distance_up_bottom(min_distance_up_bottom_word1: string; min_distance_up_bottom_word2: string): int64;
var
  min_distance_up_bottom_len1: integer;
  min_distance_up_bottom_len2: integer;
  min_distance_up_bottom_cache: array of IntArray;
  min_distance_up_bottom__: int64;
  min_distance_up_bottom_row: array of int64;
  min_distance_up_bottom__2: int64;
begin
  min_distance_up_bottom_len1 := Length(min_distance_up_bottom_word1);
  min_distance_up_bottom_len2 := Length(min_distance_up_bottom_word2);
  min_distance_up_bottom_cache := [];
  for min_distance_up_bottom__ := 0 to (min_distance_up_bottom_len1 - 1) do begin
  min_distance_up_bottom_row := [];
  for min_distance_up_bottom__2 := 0 to (min_distance_up_bottom_len2 - 1) do begin
  min_distance_up_bottom_row := concat(min_distance_up_bottom_row, IntArray([0 - 1]));
end;
  min_distance_up_bottom_cache := concat(min_distance_up_bottom_cache, [min_distance_up_bottom_row]);
end;
  exit(helper(min_distance_up_bottom_word1, min_distance_up_bottom_word2, min_distance_up_bottom_cache, 0, 0, min_distance_up_bottom_len1, min_distance_up_bottom_len2));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(min_distance_up_bottom('intention', 'execution')));
  writeln(IntToStr(min_distance_up_bottom('intention', '')));
  writeln(IntToStr(min_distance_up_bottom('', '')));
  writeln(IntToStr(min_distance_up_bottom('zooicoarchaeologist', 'zoologist')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
