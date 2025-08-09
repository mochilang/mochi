{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  i: integer;
  len1: integer;
  len2: integer;
  word1: string;
  c: integer;
  word2: string;
  cache: IntArrayArray;
  j: integer;
  a: integer;
  b: integer;
function min3(a: integer; b: integer; c: integer): integer; forward;
function helper(word1: string; word2: string; cache: IntArrayArray; i: integer; j: integer; len1: integer; len2: integer): integer; forward;
function min_distance_up_bottom(word1: string; word2: string): integer; forward;
function min3(a: integer; b: integer; c: integer): integer;
var
  min3_m: integer;
begin
  min3_m := a;
  if b < min3_m then begin
  min3_m := b;
end;
  if c < min3_m then begin
  min3_m := c;
end;
  exit(min3_m);
end;
function helper(word1: string; word2: string; cache: IntArrayArray; i: integer; j: integer; len1: integer; len2: integer): integer;
var
  helper_diff: integer;
  helper_delete_cost: integer;
  helper_insert_cost: integer;
  helper_replace_cost: integer;
begin
  if i >= len1 then begin
  exit(len2 - j);
end;
  if j >= len2 then begin
  exit(len1 - i);
end;
  if cache[i][j] <> (0 - 1) then begin
  exit(cache[i][j]);
end;
  helper_diff := 0;
  if copy(word1, i+1, (i + 1 - (i))) <> copy(word2, j+1, (j + 1 - (j))) then begin
  helper_diff := 1;
end;
  helper_delete_cost := 1 + helper(word1, word2, cache, i + 1, j, len1, len2);
  helper_insert_cost := 1 + helper(word1, word2, cache, i, j + 1, len1, len2);
  helper_replace_cost := helper_diff + helper(word1, word2, cache, i + 1, j + 1, len1, len2);
  cache[i][j] := min3(helper_delete_cost, helper_insert_cost, helper_replace_cost);
  exit(cache[i][j]);
end;
function min_distance_up_bottom(word1: string; word2: string): integer;
var
  min_distance_up_bottom_len1: integer;
  min_distance_up_bottom_len2: integer;
  min_distance_up_bottom_cache: array of IntArray;
  min_distance_up_bottom__: int64;
  min_distance_up_bottom_row: array of integer;
  min_distance_up_bottom__2: int64;
begin
  min_distance_up_bottom_len1 := Length(word1);
  min_distance_up_bottom_len2 := Length(word2);
  min_distance_up_bottom_cache := [];
  for min_distance_up_bottom__ := 0 to (min_distance_up_bottom_len1 - 1) do begin
  min_distance_up_bottom_row := [];
  for min_distance_up_bottom__2 := 0 to (min_distance_up_bottom_len2 - 1) do begin
  min_distance_up_bottom_row := concat(min_distance_up_bottom_row, IntArray([0 - 1]));
end;
  min_distance_up_bottom_cache := concat(min_distance_up_bottom_cache, [min_distance_up_bottom_row]);
end;
  exit(helper(word1, word2, min_distance_up_bottom_cache, 0, 0, min_distance_up_bottom_len1, min_distance_up_bottom_len2));
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
end.
