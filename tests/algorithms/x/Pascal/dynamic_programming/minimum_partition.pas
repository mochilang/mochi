{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type BoolArray = array of boolean;
type BoolArrayArray = array of BoolArray;
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
  numbers: IntArray;
function find_min(numbers: IntArray): integer; forward;
function find_min(numbers: IntArray): integer;
var
  find_min_n: integer;
  find_min_s: integer;
  find_min_idx: integer;
  find_min_dp: array of BoolArray;
  find_min_i: integer;
  find_min_row: array of boolean;
  find_min_j: integer;
  find_min_diff: integer;
begin
  find_min_n := Length(numbers);
  find_min_s := 0;
  find_min_idx := 0;
  while find_min_idx < find_min_n do begin
  find_min_s := find_min_s + numbers[find_min_idx];
  find_min_idx := find_min_idx + 1;
end;
  find_min_dp := [];
  find_min_i := 0;
  while find_min_i <= find_min_n do begin
  find_min_row := [];
  find_min_j := 0;
  while find_min_j <= find_min_s do begin
  find_min_row := concat(find_min_row, [false]);
  find_min_j := find_min_j + 1;
end;
  find_min_dp := concat(find_min_dp, [find_min_row]);
  find_min_i := find_min_i + 1;
end;
  find_min_i := 0;
  while find_min_i <= find_min_n do begin
  find_min_dp[find_min_i][0] := true;
  find_min_i := find_min_i + 1;
end;
  find_min_j := 1;
  while find_min_j <= find_min_s do begin
  find_min_dp[0][find_min_j] := false;
  find_min_j := find_min_j + 1;
end;
  find_min_i := 1;
  while find_min_i <= find_min_n do begin
  find_min_j := 1;
  while find_min_j <= find_min_s do begin
  find_min_dp[find_min_i][find_min_j] := find_min_dp[find_min_i - 1][find_min_j];
  if numbers[find_min_i - 1] <= find_min_j then begin
  if find_min_dp[find_min_i - 1][find_min_j - numbers[find_min_i - 1]] then begin
  find_min_dp[find_min_i][find_min_j] := true;
end;
end;
  find_min_j := find_min_j + 1;
end;
  find_min_i := find_min_i + 1;
end;
  find_min_diff := 0;
  find_min_j := find_min_s div 2;
  while find_min_j >= 0 do begin
  if find_min_dp[find_min_n][find_min_j] then begin
  find_min_diff := find_min_s - (2 * find_min_j);
  break;
end;
  find_min_j := find_min_j - 1;
end;
  exit(find_min_diff);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(find_min([1, 2, 3, 4, 5])));
  writeln(IntToStr(find_min([5, 5, 5, 5, 5])));
  writeln(IntToStr(find_min([5, 5, 5, 5])));
  writeln(IntToStr(find_min([3])));
  writeln(IntToStr(find_min([])));
  writeln(IntToStr(find_min([1, 2, 3, 4])));
  writeln(IntToStr(find_min([0, 0, 0, 0])));
  writeln(IntToStr(find_min([-1, -5, 5, 1])));
  writeln(IntToStr(find_min([9, 9, 9, 9, 9])));
  writeln(IntToStr(find_min([1, 5, 10, 3])));
  writeln(IntToStr(find_min([-1, 0, 1])));
  writeln(IntToStr(find_min([10, 9, 8, 7, 6, 5, 4, 3, 2, 1])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
