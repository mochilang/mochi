{$mode objfpc}
program Main;
uses SysUtils;
type LcsResult = record
  length_: integer;
  sequence: string;
end;
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
  a: string;
  b: string;
  res: LcsResult;
  y: string;
  rows: integer;
  x: string;
  cols: integer;
function makeLcsResult(length_: integer; sequence: string): LcsResult; forward;
function zeros_matrix(rows: integer; cols: integer): IntArrayArray; forward;
function longest_common_subsequence(x: string; y: string): LcsResult; forward;
function makeLcsResult(length_: integer; sequence: string): LcsResult;
begin
  Result.length_ := length_;
  Result.sequence := sequence;
end;
function zeros_matrix(rows: integer; cols: integer): IntArrayArray;
var
  zeros_matrix_matrix: array of IntArray;
  zeros_matrix_i: integer;
  zeros_matrix_row: array of integer;
  zeros_matrix_j: integer;
begin
  zeros_matrix_matrix := [];
  zeros_matrix_i := 0;
  while zeros_matrix_i <= rows do begin
  zeros_matrix_row := [];
  zeros_matrix_j := 0;
  while zeros_matrix_j <= cols do begin
  zeros_matrix_row := concat(zeros_matrix_row, IntArray([0]));
  zeros_matrix_j := zeros_matrix_j + 1;
end;
  zeros_matrix_matrix := concat(zeros_matrix_matrix, [zeros_matrix_row]);
  zeros_matrix_i := zeros_matrix_i + 1;
end;
  exit(zeros_matrix_matrix);
end;
function longest_common_subsequence(x: string; y: string): LcsResult;
var
  longest_common_subsequence_m: integer;
  longest_common_subsequence_n: integer;
  longest_common_subsequence_dp: IntArrayArray;
  longest_common_subsequence_i: integer;
  longest_common_subsequence_j: integer;
  longest_common_subsequence_seq: string;
  longest_common_subsequence_i2: integer;
  longest_common_subsequence_j2: integer;
begin
  longest_common_subsequence_m := Length(x);
  longest_common_subsequence_n := Length(y);
  longest_common_subsequence_dp := zeros_matrix(longest_common_subsequence_m, longest_common_subsequence_n);
  longest_common_subsequence_i := 1;
  while longest_common_subsequence_i <= longest_common_subsequence_m do begin
  longest_common_subsequence_j := 1;
  while longest_common_subsequence_j <= longest_common_subsequence_n do begin
  if x[longest_common_subsequence_i - 1+1] = y[longest_common_subsequence_j - 1+1] then begin
  longest_common_subsequence_dp[longest_common_subsequence_i][longest_common_subsequence_j] := longest_common_subsequence_dp[longest_common_subsequence_i - 1][longest_common_subsequence_j - 1] + 1;
end else begin
  if longest_common_subsequence_dp[longest_common_subsequence_i - 1][longest_common_subsequence_j] > longest_common_subsequence_dp[longest_common_subsequence_i][longest_common_subsequence_j - 1] then begin
  longest_common_subsequence_dp[longest_common_subsequence_i][longest_common_subsequence_j] := longest_common_subsequence_dp[longest_common_subsequence_i - 1][longest_common_subsequence_j];
end else begin
  longest_common_subsequence_dp[longest_common_subsequence_i][longest_common_subsequence_j] := longest_common_subsequence_dp[longest_common_subsequence_i][longest_common_subsequence_j - 1];
end;
end;
  longest_common_subsequence_j := longest_common_subsequence_j + 1;
end;
  longest_common_subsequence_i := longest_common_subsequence_i + 1;
end;
  longest_common_subsequence_seq := '';
  longest_common_subsequence_i2 := longest_common_subsequence_m;
  longest_common_subsequence_j2 := longest_common_subsequence_n;
  while (longest_common_subsequence_i2 > 0) and (longest_common_subsequence_j2 > 0) do begin
  if x[longest_common_subsequence_i2 - 1+1] = y[longest_common_subsequence_j2 - 1+1] then begin
  longest_common_subsequence_seq := x[longest_common_subsequence_i2 - 1+1] + longest_common_subsequence_seq;
  longest_common_subsequence_i2 := longest_common_subsequence_i2 - 1;
  longest_common_subsequence_j2 := longest_common_subsequence_j2 - 1;
end else begin
  if longest_common_subsequence_dp[longest_common_subsequence_i2 - 1][longest_common_subsequence_j2] >= longest_common_subsequence_dp[longest_common_subsequence_i2][longest_common_subsequence_j2 - 1] then begin
  longest_common_subsequence_i2 := longest_common_subsequence_i2 - 1;
end else begin
  longest_common_subsequence_j2 := longest_common_subsequence_j2 - 1;
end;
end;
end;
  exit(makeLcsResult(longest_common_subsequence_dp[longest_common_subsequence_m][longest_common_subsequence_n], longest_common_subsequence_seq));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  a := 'AGGTAB';
  b := 'GXTXAYB';
  res := longest_common_subsequence(a, b);
  writeln((('len = ' + IntToStr(res.length_)) + ', sub-sequence = ') + res.sequence);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
