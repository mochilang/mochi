{$mode objfpc}{$modeswitch nestedprocvars}
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
function recursive_match(recursive_match_text: string; recursive_match_pattern: string): boolean; forward;
function dp_match(dp_match_text: string; dp_match_pattern: string): boolean; forward;
procedure print_bool(print_bool_b: boolean); forward;
function recursive_match(recursive_match_text: string; recursive_match_pattern: string): boolean;
var
  recursive_match_last_text: string;
  recursive_match_last_pattern: string;
begin
  if Length(recursive_match_pattern) = 0 then begin
  exit(Length(recursive_match_text) = 0);
end;
  if Length(recursive_match_text) = 0 then begin
  if (Length(recursive_match_pattern) >= 2) and (copy(recursive_match_pattern, Length(recursive_match_pattern) - 1+1, (Length(recursive_match_pattern) - (Length(recursive_match_pattern) - 1))) = '*') then begin
  exit(recursive_match(recursive_match_text, copy(recursive_match_pattern, 1, (Length(recursive_match_pattern) - 2 - (0)))));
end;
  exit(false);
end;
  recursive_match_last_text := copy(recursive_match_text, Length(recursive_match_text) - 1+1, (Length(recursive_match_text) - (Length(recursive_match_text) - 1)));
  recursive_match_last_pattern := copy(recursive_match_pattern, Length(recursive_match_pattern) - 1+1, (Length(recursive_match_pattern) - (Length(recursive_match_pattern) - 1)));
  if (recursive_match_last_text = recursive_match_last_pattern) or (recursive_match_last_pattern = '.') then begin
  exit(recursive_match(copy(recursive_match_text, 1, (Length(recursive_match_text) - 1 - (0))), copy(recursive_match_pattern, 1, (Length(recursive_match_pattern) - 1 - (0)))));
end;
  if recursive_match_last_pattern = '*' then begin
  if recursive_match(copy(recursive_match_text, 1, (Length(recursive_match_text) - 1 - (0))), recursive_match_pattern) then begin
  exit(true);
end;
  exit(recursive_match(recursive_match_text, copy(recursive_match_pattern, 1, (Length(recursive_match_pattern) - 2 - (0)))));
end;
  exit(false);
end;
function dp_match(dp_match_text: string; dp_match_pattern: string): boolean;
var
  dp_match_m: integer;
  dp_match_n: integer;
  dp_match_dp: array of BoolArray;
  dp_match_i: int64;
  dp_match_row: array of boolean;
  dp_match_j: int64;
  dp_match_p_char: string;
  dp_match_t_char: string;
  dp_match_prev_p: string;
begin
  dp_match_m := Length(dp_match_text);
  dp_match_n := Length(dp_match_pattern);
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
  if (copy(dp_match_pattern, dp_match_j - 1+1, (dp_match_j - (dp_match_j - 1))) = '*') and (dp_match_j >= 2) then begin
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
  dp_match_p_char := copy(dp_match_pattern, dp_match_j - 1+1, (dp_match_j - (dp_match_j - 1)));
  dp_match_t_char := copy(dp_match_text, dp_match_i - 1+1, (dp_match_i - (dp_match_i - 1)));
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
  dp_match_prev_p := copy(dp_match_pattern, dp_match_j - 2+1, (dp_match_j - 1 - (dp_match_j - 2)));
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
procedure print_bool(print_bool_b: boolean);
begin
  if print_bool_b then begin
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
  writeln('');
end.
