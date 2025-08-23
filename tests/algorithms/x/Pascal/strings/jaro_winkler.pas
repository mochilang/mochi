{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type BoolArray = array of boolean;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function min_int(min_int_a: integer; min_int_b: integer): integer; forward;
function max_int(max_int_a: integer; max_int_b: integer): integer; forward;
function repeat_bool(repeat_bool_n: integer; repeat_bool_value: boolean): BoolArray; forward;
function set_bool(set_bool_xs: BoolArray; set_bool_idx: integer; set_bool_value: boolean): BoolArray; forward;
function jaro_winkler(jaro_winkler_s1: string; jaro_winkler_s2: string): real; forward;
function min_int(min_int_a: integer; min_int_b: integer): integer;
begin
  if min_int_a < min_int_b then begin
  exit(min_int_a);
end else begin
  exit(min_int_b);
end;
end;
function max_int(max_int_a: integer; max_int_b: integer): integer;
begin
  if max_int_a > max_int_b then begin
  exit(max_int_a);
end else begin
  exit(max_int_b);
end;
end;
function repeat_bool(repeat_bool_n: integer; repeat_bool_value: boolean): BoolArray;
var
  repeat_bool_res: array of boolean;
  repeat_bool_i: integer;
begin
  repeat_bool_res := [];
  repeat_bool_i := 0;
  while repeat_bool_i < repeat_bool_n do begin
  repeat_bool_res := concat(repeat_bool_res, [repeat_bool_value]);
  repeat_bool_i := repeat_bool_i + 1;
end;
  exit(repeat_bool_res);
end;
function set_bool(set_bool_xs: BoolArray; set_bool_idx: integer; set_bool_value: boolean): BoolArray;
var
  set_bool_res: array of boolean;
  set_bool_i: integer;
begin
  set_bool_res := [];
  set_bool_i := 0;
  while set_bool_i < Length(set_bool_xs) do begin
  if set_bool_i = set_bool_idx then begin
  set_bool_res := concat(set_bool_res, [set_bool_value]);
end else begin
  set_bool_res := concat(set_bool_res, [set_bool_xs[set_bool_i]]);
end;
  set_bool_i := set_bool_i + 1;
end;
  exit(set_bool_res);
end;
function jaro_winkler(jaro_winkler_s1: string; jaro_winkler_s2: string): real;
var
  jaro_winkler_len1: integer;
  jaro_winkler_len2: integer;
  jaro_winkler_limit: integer;
  jaro_winkler_match1: BoolArray;
  jaro_winkler_match2: BoolArray;
  jaro_winkler_matches: integer;
  jaro_winkler_i: integer;
  jaro_winkler_start: integer;
  jaro_winkler_end_: integer;
  jaro_winkler_j: integer;
  jaro_winkler_transpositions: integer;
  jaro_winkler_k: integer;
  jaro_winkler_m: real;
  jaro_winkler_jaro: real;
  jaro_winkler_prefix_len: integer;
begin
  jaro_winkler_len1 := Length(jaro_winkler_s1);
  jaro_winkler_len2 := Length(jaro_winkler_s2);
  jaro_winkler_limit := min_int(jaro_winkler_len1, jaro_winkler_len2) div 2;
  jaro_winkler_match1 := repeat_bool(jaro_winkler_len1, false);
  jaro_winkler_match2 := repeat_bool(jaro_winkler_len2, false);
  jaro_winkler_matches := 0;
  jaro_winkler_i := 0;
  while jaro_winkler_i < jaro_winkler_len1 do begin
  jaro_winkler_start := max_int(0, jaro_winkler_i - jaro_winkler_limit);
  jaro_winkler_end_ := min_int((jaro_winkler_i + jaro_winkler_limit) + 1, jaro_winkler_len2);
  jaro_winkler_j := jaro_winkler_start;
  while jaro_winkler_j < jaro_winkler_end_ do begin
  if not jaro_winkler_match2[jaro_winkler_j] and (copy(jaro_winkler_s1, jaro_winkler_i+1, (jaro_winkler_i + 1 - (jaro_winkler_i))) = copy(jaro_winkler_s2, jaro_winkler_j+1, (jaro_winkler_j + 1 - (jaro_winkler_j)))) then begin
  jaro_winkler_match1 := set_bool(jaro_winkler_match1, jaro_winkler_i, true);
  jaro_winkler_match2 := set_bool(jaro_winkler_match2, jaro_winkler_j, true);
  jaro_winkler_matches := jaro_winkler_matches + 1;
  break;
end;
  jaro_winkler_j := jaro_winkler_j + 1;
end;
  jaro_winkler_i := jaro_winkler_i + 1;
end;
  if jaro_winkler_matches = 0 then begin
  exit(0);
end;
  jaro_winkler_transpositions := 0;
  jaro_winkler_k := 0;
  jaro_winkler_i := 0;
  while jaro_winkler_i < jaro_winkler_len1 do begin
  if jaro_winkler_match1[jaro_winkler_i] then begin
  while not jaro_winkler_match2[jaro_winkler_k] do begin
  jaro_winkler_k := jaro_winkler_k + 1;
end;
  if copy(jaro_winkler_s1, jaro_winkler_i+1, (jaro_winkler_i + 1 - (jaro_winkler_i))) <> copy(jaro_winkler_s2, jaro_winkler_k+1, (jaro_winkler_k + 1 - (jaro_winkler_k))) then begin
  jaro_winkler_transpositions := jaro_winkler_transpositions + 1;
end;
  jaro_winkler_k := jaro_winkler_k + 1;
end;
  jaro_winkler_i := jaro_winkler_i + 1;
end;
  jaro_winkler_m := Double(jaro_winkler_matches);
  jaro_winkler_jaro := (((jaro_winkler_m / Double(jaro_winkler_len1)) + (jaro_winkler_m / Double(jaro_winkler_len2))) + ((jaro_winkler_m - (Double(jaro_winkler_transpositions) / 2)) / jaro_winkler_m)) / 3;
  jaro_winkler_prefix_len := 0;
  jaro_winkler_i := 0;
  while ((jaro_winkler_i < 4) and (jaro_winkler_i < jaro_winkler_len1)) and (jaro_winkler_i < jaro_winkler_len2) do begin
  if copy(jaro_winkler_s1, jaro_winkler_i+1, (jaro_winkler_i + 1 - (jaro_winkler_i))) = copy(jaro_winkler_s2, jaro_winkler_i+1, (jaro_winkler_i + 1 - (jaro_winkler_i))) then begin
  jaro_winkler_prefix_len := jaro_winkler_prefix_len + 1;
end else begin
  break;
end;
  jaro_winkler_i := jaro_winkler_i + 1;
end;
  exit(jaro_winkler_jaro + ((0.1 * Double(jaro_winkler_prefix_len)) * (1 - jaro_winkler_jaro)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(jaro_winkler('hello', 'world')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
