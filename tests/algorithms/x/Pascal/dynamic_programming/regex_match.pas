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
  text: string;
  pattern: string;
  b: boolean;
function recursive_match(text: string; pattern: string): boolean; forward;
function dp_match(text: string; pattern: string): boolean; forward;
procedure print_bool(b: boolean); forward;
function recursive_match(text: string; pattern: string): boolean;
var
  recursive_match_last_text: string;
  recursive_match_last_pattern: string;
begin
  if Length(pattern) = 0 then begin
  exit(Length(text) = 0);
end;
  if Length(text) = 0 then begin
  if (Length(pattern) >= 2) and (copy(pattern, Length(pattern) - 1+1, (Length(pattern) - (Length(pattern) - 1))) = '*') then begin
  exit(recursive_match(text, copy(pattern, 0+1, (Length(pattern) - 2 - (0)))));
end;
  exit(false);
end;
  recursive_match_last_text := copy(text, Length(text) - 1+1, (Length(text) - (Length(text) - 1)));
  recursive_match_last_pattern := copy(pattern, Length(pattern) - 1+1, (Length(pattern) - (Length(pattern) - 1)));
  if (recursive_match_last_text = recursive_match_last_pattern) or (recursive_match_last_pattern = '.') then begin
  exit(recursive_match(copy(text, 0+1, (Length(text) - 1 - (0))), copy(pattern, 0+1, (Length(pattern) - 1 - (0)))));
end;
  if recursive_match_last_pattern = '*' then begin
  if recursive_match(copy(text, 0+1, (Length(text) - 1 - (0))), pattern) then begin
  exit(true);
end;
  exit(recursive_match(text, copy(pattern, 0+1, (Length(pattern) - 2 - (0)))));
end;
  exit(false);
end;
function dp_match(text: string; pattern: string): boolean;
var
  dp_match_m: integer;
  dp_match_n: integer;
  dp_match_dp: array of BoolArray;
  dp_match_i: integer;
  dp_match_row: array of boolean;
  dp_match_j: integer;
  dp_match_p_char: string;
  dp_match_t_char: string;
  dp_match_prev_p: string;
begin
  dp_match_m := Length(text);
  dp_match_n := Length(pattern);
  dp_match_dp := [];
  dp_match_i := 0;
  while dp_match_i <= dp_match_m do begin
  dp_match_row := [];
  dp_match_j := 0;
  while dp_match_j <= dp_match_n do begin
  dp_match_row := concat(dp_match_row, [false]);
  dp_match_j := dp_match_j + 1;
end;
  dp_match_dp := concat(dp_match_dp, [dp_match_row]);
  dp_match_i := dp_match_i + 1;
end;
  dp_match_dp[0][0] := true;
  dp_match_j := 1;
  while dp_match_j <= dp_match_n do begin
  if (copy(pattern, dp_match_j - 1+1, (dp_match_j - (dp_match_j - 1))) = '*') and (dp_match_j >= 2) then begin
  if dp_match_dp[0][dp_match_j - 2] then begin
  dp_match_dp[0][dp_match_j] := true;
end;
end;
  dp_match_j := dp_match_j + 1;
end;
  dp_match_i := 1;
  while dp_match_i <= dp_match_m do begin
  dp_match_j := 1;
  while dp_match_j <= dp_match_n do begin
  dp_match_p_char := copy(pattern, dp_match_j - 1+1, (dp_match_j - (dp_match_j - 1)));
  dp_match_t_char := copy(text, dp_match_i - 1+1, (dp_match_i - (dp_match_i - 1)));
  if (dp_match_p_char = '.') or (dp_match_p_char = dp_match_t_char) then begin
  if dp_match_dp[dp_match_i - 1][dp_match_j - 1] then begin
  dp_match_dp[dp_match_i][dp_match_j] := true;
end;
end else begin
  if dp_match_p_char = '*' then begin
  if dp_match_j >= 2 then begin
  if dp_match_dp[dp_match_i][dp_match_j - 2] then begin
  dp_match_dp[dp_match_i][dp_match_j] := true;
end;
  dp_match_prev_p := copy(pattern, dp_match_j - 2+1, (dp_match_j - 1 - (dp_match_j - 2)));
  if (dp_match_prev_p = '.') or (dp_match_prev_p = dp_match_t_char) then begin
  if dp_match_dp[dp_match_i - 1][dp_match_j] then begin
  dp_match_dp[dp_match_i][dp_match_j] := true;
end;
end;
end;
end else begin
  dp_match_dp[dp_match_i][dp_match_j] := false;
end;
end;
  dp_match_j := dp_match_j + 1;
end;
  dp_match_i := dp_match_i + 1;
end;
  exit(dp_match_dp[dp_match_m][dp_match_n]);
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
  print_bool(recursive_match('abc', 'a.c'));
  print_bool(recursive_match('abc', 'af*.c'));
  print_bool(recursive_match('abc', 'a.c*'));
  print_bool(recursive_match('abc', 'a.c*d'));
  print_bool(recursive_match('aa', '.*'));
  print_bool(dp_match('abc', 'a.c'));
  print_bool(dp_match('abc', 'af*.c'));
  print_bool(dp_match('abc', 'a.c*'));
  print_bool(dp_match('abc', 'a.c*d'));
  print_bool(dp_match('aa', '.*'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
