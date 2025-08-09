{$mode objfpc}
program Main;
uses SysUtils;
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
  b: boolean;
  rows: integer;
  cols: integer;
  s: string;
  n: integer;
  p: string;
function make_bool_list(n: integer): BoolArray; forward;
function make_bool_matrix(rows: integer; cols: integer): BoolArrayArray; forward;
function is_match(s: string; p: string): boolean; forward;
procedure print_bool(b: boolean); forward;
function make_bool_list(n: integer): BoolArray;
var
  make_bool_list_row: array of boolean;
  make_bool_list_i: integer;
begin
  make_bool_list_row := [];
  make_bool_list_i := 0;
  while make_bool_list_i < n do begin
  make_bool_list_row := concat(make_bool_list_row, [false]);
  make_bool_list_i := make_bool_list_i + 1;
end;
  exit(make_bool_list_row);
end;
function make_bool_matrix(rows: integer; cols: integer): BoolArrayArray;
var
  make_bool_matrix_matrix: array of BoolArray;
  make_bool_matrix_i: integer;
begin
  make_bool_matrix_matrix := [];
  make_bool_matrix_i := 0;
  while make_bool_matrix_i < rows do begin
  make_bool_matrix_matrix := concat(make_bool_matrix_matrix, [make_bool_list(cols)]);
  make_bool_matrix_i := make_bool_matrix_i + 1;
end;
  exit(make_bool_matrix_matrix);
end;
function is_match(s: string; p: string): boolean;
var
  is_match_n: integer;
  is_match_m: integer;
  is_match_dp: BoolArrayArray;
  is_match_j: integer;
  is_match_i: integer;
  is_match_j2: integer;
  is_match_pc: string;
  is_match_sc: string;
begin
  is_match_n := Length(s);
  is_match_m := Length(p);
  is_match_dp := make_bool_matrix(is_match_n + 1, is_match_m + 1);
  is_match_dp[0][0] := true;
  is_match_j := 1;
  while is_match_j <= is_match_m do begin
  if copy(p, is_match_j - 1+1, (is_match_j - (is_match_j - 1))) = '*' then begin
  is_match_dp[0][is_match_j] := is_match_dp[0][is_match_j - 1];
end;
  is_match_j := is_match_j + 1;
end;
  is_match_i := 1;
  while is_match_i <= is_match_n do begin
  is_match_j2 := 1;
  while is_match_j2 <= is_match_m do begin
  is_match_pc := copy(p, is_match_j2 - 1+1, (is_match_j2 - (is_match_j2 - 1)));
  is_match_sc := copy(s, is_match_i - 1+1, (is_match_i - (is_match_i - 1)));
  if (is_match_pc = is_match_sc) or (is_match_pc = '?') then begin
  is_match_dp[is_match_i][is_match_j2] := is_match_dp[is_match_i - 1][is_match_j2 - 1];
end else begin
  if is_match_pc = '*' then begin
  if is_match_dp[is_match_i - 1][is_match_j2] or is_match_dp[is_match_i][is_match_j2 - 1] then begin
  is_match_dp[is_match_i][is_match_j2] := true;
end;
end;
end;
  is_match_j2 := is_match_j2 + 1;
end;
  is_match_i := is_match_i + 1;
end;
  exit(is_match_dp[is_match_n][is_match_m]);
end;
procedure print_bool(b: boolean);
begin
  if b then begin
  writeln(Ord(true));
end else begin
  writeln(Ord(false));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  print_bool(is_match('abc', 'a*c'));
  print_bool(is_match('abc', 'a*d'));
  print_bool(is_match('baaabab', '*****ba*****ab'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
