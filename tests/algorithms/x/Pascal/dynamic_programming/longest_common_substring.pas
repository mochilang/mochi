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
function longest_common_substring(longest_common_substring_text1: string; longest_common_substring_text2: string): string; forward;
function longest_common_substring(longest_common_substring_text1: string; longest_common_substring_text2: string): string;
var
  longest_common_substring_m: integer;
  longest_common_substring_n: integer;
  longest_common_substring_dp: array of IntArray;
  longest_common_substring_i: int64;
  longest_common_substring_row: array of int64;
  longest_common_substring_j: int64;
  longest_common_substring_end_pos: int64;
  longest_common_substring_max_len: int64;
  longest_common_substring_ii: int64;
  longest_common_substring_jj: int64;
begin
  if (Length(longest_common_substring_text1) = 0) or (Length(longest_common_substring_text2) = 0) then begin
  exit('');
end;
  longest_common_substring_m := Length(longest_common_substring_text1);
  longest_common_substring_n := Length(longest_common_substring_text2);
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
  if copy(longest_common_substring_text1, longest_common_substring_ii - 1+1, (longest_common_substring_ii - (longest_common_substring_ii - 1))) = copy(longest_common_substring_text2, longest_common_substring_jj - 1+1, (longest_common_substring_jj - (longest_common_substring_jj - 1))) then begin
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
  exit(copy(longest_common_substring_text1, longest_common_substring_end_pos - longest_common_substring_max_len+1, (longest_common_substring_end_pos - (longest_common_substring_end_pos - longest_common_substring_max_len))));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(longest_common_substring('abcdef', 'xabded'));
  writeln(#10);
  writeln(longest_common_substring('zxabcdezy', 'yzabcdezx'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
