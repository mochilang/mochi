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
  text2: string;
  text1: string;
function longest_common_substring(text1: string; text2: string): string; forward;
function longest_common_substring(text1: string; text2: string): string;
var
  longest_common_substring_m: integer;
  longest_common_substring_n: integer;
  longest_common_substring_dp: array of IntArray;
  longest_common_substring_i: integer;
  longest_common_substring_row: array of integer;
  longest_common_substring_j: integer;
  longest_common_substring_end_pos: integer;
  longest_common_substring_max_len: integer;
  longest_common_substring_ii: integer;
  longest_common_substring_jj: integer;
begin
  if (Length(text1) = 0) or (Length(text2) = 0) then begin
  exit('');
end;
  longest_common_substring_m := Length(text1);
  longest_common_substring_n := Length(text2);
  longest_common_substring_dp := [];
  longest_common_substring_i := 0;
  while longest_common_substring_i < (longest_common_substring_m + 1) do begin
  longest_common_substring_row := [];
  longest_common_substring_j := 0;
  while longest_common_substring_j < (longest_common_substring_n + 1) do begin
  longest_common_substring_row := concat(longest_common_substring_row, IntArray([0]));
  longest_common_substring_j := longest_common_substring_j + 1;
end;
  longest_common_substring_dp := concat(longest_common_substring_dp, [longest_common_substring_row]);
  longest_common_substring_i := longest_common_substring_i + 1;
end;
  longest_common_substring_end_pos := 0;
  longest_common_substring_max_len := 0;
  longest_common_substring_ii := 1;
  while longest_common_substring_ii <= longest_common_substring_m do begin
  longest_common_substring_jj := 1;
  while longest_common_substring_jj <= longest_common_substring_n do begin
  if copy(text1, longest_common_substring_ii - 1+1, (longest_common_substring_ii - (longest_common_substring_ii - 1))) = copy(text2, longest_common_substring_jj - 1+1, (longest_common_substring_jj - (longest_common_substring_jj - 1))) then begin
  longest_common_substring_dp[longest_common_substring_ii][longest_common_substring_jj] := 1 + longest_common_substring_dp[longest_common_substring_ii - 1][longest_common_substring_jj - 1];
  if longest_common_substring_dp[longest_common_substring_ii][longest_common_substring_jj] > longest_common_substring_max_len then begin
  longest_common_substring_max_len := longest_common_substring_dp[longest_common_substring_ii][longest_common_substring_jj];
  longest_common_substring_end_pos := longest_common_substring_ii;
end;
end;
  longest_common_substring_jj := longest_common_substring_jj + 1;
end;
  longest_common_substring_ii := longest_common_substring_ii + 1;
end;
  exit(copy(text1, longest_common_substring_end_pos - longest_common_substring_max_len+1, (longest_common_substring_end_pos - (longest_common_substring_end_pos - longest_common_substring_max_len))));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(longest_common_substring('abcdef', 'xabded'));
  writeln('' + #10 + '');
  writeln(longest_common_substring('zxabcdezy', 'yzabcdezx'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
