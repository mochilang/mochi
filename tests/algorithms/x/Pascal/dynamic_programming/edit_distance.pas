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
  b: integer;
  c: integer;
  a: integer;
  dp: IntArrayArray;
  i: integer;
  j: integer;
  word1: string;
  word2: string;
function min3(a: integer; b: integer; c: integer): integer; forward;
function helper_top_down(word1: string; word2: string; dp: IntArrayArray; i: integer; j: integer): integer; forward;
function min_dist_top_down(word1: string; word2: string): integer; forward;
function min_dist_bottom_up(word1: string; word2: string): integer; forward;
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
function helper_top_down(word1: string; word2: string; dp: IntArrayArray; i: integer; j: integer): integer;
var
  helper_top_down_insert: integer;
  helper_top_down_delete: integer;
  helper_top_down_replace: integer;
begin
  if i < 0 then begin
  exit(j + 1);
end;
  if j < 0 then begin
  exit(i + 1);
end;
  if dp[i][j] <> (0 - 1) then begin
  exit(dp[i][j]);
end;
  if copy(word1, i+1, (i + 1 - (i))) = copy(word2, j+1, (j + 1 - (j))) then begin
  dp[i][j] := helper_top_down(word1, word2, dp, i - 1, j - 1);
end else begin
  helper_top_down_insert := helper_top_down(word1, word2, dp, i, j - 1);
  helper_top_down_delete := helper_top_down(word1, word2, dp, i - 1, j);
  helper_top_down_replace := helper_top_down(word1, word2, dp, i - 1, j - 1);
  dp[i][j] := 1 + min3(helper_top_down_insert, helper_top_down_delete, helper_top_down_replace);
end;
  exit(dp[i][j]);
end;
function min_dist_top_down(word1: string; word2: string): integer;
var
  min_dist_top_down_m: integer;
  min_dist_top_down_n: integer;
  min_dist_top_down_dp: array of IntArray;
  min_dist_top_down__: int64;
  min_dist_top_down_row: array of integer;
  min_dist_top_down__2: int64;
begin
  min_dist_top_down_m := Length(word1);
  min_dist_top_down_n := Length(word2);
  min_dist_top_down_dp := [];
  for min_dist_top_down__ := 0 to (min_dist_top_down_m - 1) do begin
  min_dist_top_down_row := [];
  for min_dist_top_down__2 := 0 to (min_dist_top_down_n - 1) do begin
  min_dist_top_down_row := concat(min_dist_top_down_row, IntArray([0 - 1]));
end;
  min_dist_top_down_dp := concat(min_dist_top_down_dp, [min_dist_top_down_row]);
end;
  exit(helper_top_down(word1, word2, min_dist_top_down_dp, min_dist_top_down_m - 1, min_dist_top_down_n - 1));
end;
function min_dist_bottom_up(word1: string; word2: string): integer;
var
  min_dist_bottom_up_m: integer;
  min_dist_bottom_up_n: integer;
  min_dist_bottom_up_dp: array of IntArray;
  min_dist_bottom_up__: int64;
  min_dist_bottom_up_row: array of integer;
  min_dist_bottom_up__2: int64;
  min_dist_bottom_up_i: int64;
  min_dist_bottom_up_j: int64;
  min_dist_bottom_up_insert: integer;
  min_dist_bottom_up_delete: integer;
  min_dist_bottom_up_replace: integer;
begin
  min_dist_bottom_up_m := Length(word1);
  min_dist_bottom_up_n := Length(word2);
  min_dist_bottom_up_dp := [];
  for min_dist_bottom_up__ := 0 to (min_dist_bottom_up_m + 1 - 1) do begin
  min_dist_bottom_up_row := [];
  for min_dist_bottom_up__2 := 0 to (min_dist_bottom_up_n + 1 - 1) do begin
  min_dist_bottom_up_row := concat(min_dist_bottom_up_row, IntArray([0]));
end;
  min_dist_bottom_up_dp := concat(min_dist_bottom_up_dp, [min_dist_bottom_up_row]);
end;
  for min_dist_bottom_up_i := 0 to (min_dist_bottom_up_m + 1 - 1) do begin
  for min_dist_bottom_up_j := 0 to (min_dist_bottom_up_n + 1 - 1) do begin
  if min_dist_bottom_up_i = 0 then begin
  min_dist_bottom_up_dp[min_dist_bottom_up_i][min_dist_bottom_up_j] := min_dist_bottom_up_j;
end else begin
  if min_dist_bottom_up_j = 0 then begin
  min_dist_bottom_up_dp[min_dist_bottom_up_i][min_dist_bottom_up_j] := min_dist_bottom_up_i;
end else begin
  if copy(word1, min_dist_bottom_up_i - 1+1, (min_dist_bottom_up_i - (min_dist_bottom_up_i - 1))) = copy(word2, min_dist_bottom_up_j - 1+1, (min_dist_bottom_up_j - (min_dist_bottom_up_j - 1))) then begin
  min_dist_bottom_up_dp[min_dist_bottom_up_i][min_dist_bottom_up_j] := min_dist_bottom_up_dp[min_dist_bottom_up_i - 1][min_dist_bottom_up_j - 1];
end else begin
  min_dist_bottom_up_insert := min_dist_bottom_up_dp[min_dist_bottom_up_i][min_dist_bottom_up_j - 1];
  min_dist_bottom_up_delete := min_dist_bottom_up_dp[min_dist_bottom_up_i - 1][min_dist_bottom_up_j];
  min_dist_bottom_up_replace := min_dist_bottom_up_dp[min_dist_bottom_up_i - 1][min_dist_bottom_up_j - 1];
  min_dist_bottom_up_dp[min_dist_bottom_up_i][min_dist_bottom_up_j] := 1 + min3(min_dist_bottom_up_insert, min_dist_bottom_up_delete, min_dist_bottom_up_replace);
end;
end;
end;
end;
end;
  exit(min_dist_bottom_up_dp[min_dist_bottom_up_m][min_dist_bottom_up_n]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(min_dist_top_down('intention', 'execution')));
  writeln(IntToStr(min_dist_top_down('intention', '')));
  writeln(IntToStr(min_dist_top_down('', '')));
  writeln(IntToStr(min_dist_bottom_up('intention', 'execution')));
  writeln(IntToStr(min_dist_bottom_up('intention', '')));
  writeln(IntToStr(min_dist_bottom_up('', '')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
