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
  s: string;
  a: integer;
  b: integer;
function reverse(s: string): string; forward;
function max_int(a: integer; b: integer): integer; forward;
function longest_palindromic_subsequence(s: string): integer; forward;
function reverse(s: string): string;
var
  reverse_result_: string;
  reverse_i: integer;
begin
  reverse_result_ := '';
  reverse_i := Length(s) - 1;
  while reverse_i >= 0 do begin
  reverse_result_ := reverse_result_ + copy(s, reverse_i+1, (reverse_i + 1 - (reverse_i)));
  reverse_i := reverse_i - 1;
end;
  exit(reverse_result_);
end;
function max_int(a: integer; b: integer): integer;
begin
  if a > b then begin
  exit(a);
end;
  exit(b);
end;
function longest_palindromic_subsequence(s: string): integer;
var
  longest_palindromic_subsequence_rev: string;
  longest_palindromic_subsequence_n: integer;
  longest_palindromic_subsequence_m: integer;
  longest_palindromic_subsequence_dp: array of IntArray;
  longest_palindromic_subsequence_i: integer;
  longest_palindromic_subsequence_row: array of integer;
  longest_palindromic_subsequence_j: integer;
  longest_palindromic_subsequence_a_char: string;
  longest_palindromic_subsequence_b_char: string;
begin
  longest_palindromic_subsequence_rev := reverse(s);
  longest_palindromic_subsequence_n := Length(s);
  longest_palindromic_subsequence_m := Length(longest_palindromic_subsequence_rev);
  longest_palindromic_subsequence_dp := [];
  longest_palindromic_subsequence_i := 0;
  while longest_palindromic_subsequence_i <= longest_palindromic_subsequence_n do begin
  longest_palindromic_subsequence_row := [];
  longest_palindromic_subsequence_j := 0;
  while longest_palindromic_subsequence_j <= longest_palindromic_subsequence_m do begin
  longest_palindromic_subsequence_row := concat(longest_palindromic_subsequence_row, IntArray([0]));
  longest_palindromic_subsequence_j := longest_palindromic_subsequence_j + 1;
end;
  longest_palindromic_subsequence_dp := concat(longest_palindromic_subsequence_dp, [longest_palindromic_subsequence_row]);
  longest_palindromic_subsequence_i := longest_palindromic_subsequence_i + 1;
end;
  longest_palindromic_subsequence_i := 1;
  while longest_palindromic_subsequence_i <= longest_palindromic_subsequence_n do begin
  longest_palindromic_subsequence_j := 1;
  while longest_palindromic_subsequence_j <= longest_palindromic_subsequence_m do begin
  longest_palindromic_subsequence_a_char := copy(s, longest_palindromic_subsequence_i - 1+1, (longest_palindromic_subsequence_i - (longest_palindromic_subsequence_i - 1)));
  longest_palindromic_subsequence_b_char := copy(longest_palindromic_subsequence_rev, longest_palindromic_subsequence_j - 1+1, (longest_palindromic_subsequence_j - (longest_palindromic_subsequence_j - 1)));
  if longest_palindromic_subsequence_a_char = longest_palindromic_subsequence_b_char then begin
  longest_palindromic_subsequence_dp[longest_palindromic_subsequence_i][longest_palindromic_subsequence_j] := 1 + longest_palindromic_subsequence_dp[longest_palindromic_subsequence_i - 1][longest_palindromic_subsequence_j - 1];
end else begin
  longest_palindromic_subsequence_dp[longest_palindromic_subsequence_i][longest_palindromic_subsequence_j] := max_int(longest_palindromic_subsequence_dp[longest_palindromic_subsequence_i - 1][longest_palindromic_subsequence_j], longest_palindromic_subsequence_dp[longest_palindromic_subsequence_i][longest_palindromic_subsequence_j - 1]);
end;
  longest_palindromic_subsequence_j := longest_palindromic_subsequence_j + 1;
end;
  longest_palindromic_subsequence_i := longest_palindromic_subsequence_i + 1;
end;
  exit(longest_palindromic_subsequence_dp[longest_palindromic_subsequence_n][longest_palindromic_subsequence_m]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(longest_palindromic_subsequence('bbbab')));
  writeln(IntToStr(longest_palindromic_subsequence('bbabcbcab')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
