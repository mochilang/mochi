{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function get_value(keys: StrArray; values: StrArray; key: string): string; forward;
function contains_value(values: StrArray; value: string): boolean; forward;
function backtrack(pattern: string; input_string: string; pi: integer; si: integer; keys: StrArray; values: StrArray): boolean; forward;
function match_word_pattern(pattern: string; input_string: string): boolean; forward;
procedure main(); forward;
function get_value(keys: StrArray; values: StrArray; key: string): string;
var
  get_value_i: integer;
begin
  get_value_i := 0;
  while get_value_i < Length(keys) do begin
  if keys[get_value_i] = key then begin
  exit(values[get_value_i]);
end;
  get_value_i := get_value_i + 1;
end;
  exit('');
end;
function contains_value(values: StrArray; value: string): boolean;
var
  contains_value_i: integer;
begin
  contains_value_i := 0;
  while contains_value_i < Length(values) do begin
  if values[contains_value_i] = value then begin
  exit(true);
end;
  contains_value_i := contains_value_i + 1;
end;
  exit(false);
end;
function backtrack(pattern: string; input_string: string; pi: integer; si: integer; keys: StrArray; values: StrArray): boolean;
var
  backtrack_ch: string;
  backtrack_mapped: string;
  backtrack_end: integer;
  backtrack_substr: string;
  backtrack_new_keys: array of string;
  backtrack_new_values: array of string;
begin
  if (pi = Length(pattern)) and (si = Length(input_string)) then begin
  exit(true);
end;
  if (pi = Length(pattern)) or (si = Length(input_string)) then begin
  exit(false);
end;
  backtrack_ch := copy(pattern, pi+1, (pi + 1 - (pi)));
  backtrack_mapped := get_value(keys, values, backtrack_ch);
  if backtrack_mapped <> '' then begin
  if copy(input_string, si+1, (si + Length(backtrack_mapped) - (si))) = backtrack_mapped then begin
  exit(backtrack(pattern, input_string, pi + 1, si + Length(backtrack_mapped), keys, values));
end;
  exit(false);
end;
  backtrack_end := si + 1;
  while backtrack_end <= Length(input_string) do begin
  backtrack_substr := copy(input_string, si+1, (backtrack_end - (si)));
  if contains_value(values, backtrack_substr) then begin
  backtrack_end := backtrack_end + 1;
  continue;
end;
  backtrack_new_keys := concat(keys, [backtrack_ch]);
  backtrack_new_values := concat(values, [backtrack_substr]);
  if backtrack(pattern, input_string, pi + 1, backtrack_end, backtrack_new_keys, backtrack_new_values) then begin
  exit(true);
end;
  backtrack_end := backtrack_end + 1;
end;
  exit(false);
end;
function match_word_pattern(pattern: string; input_string: string): boolean;
var
  match_word_pattern_keys: array of string;
  match_word_pattern_values: array of string;
begin
  match_word_pattern_keys := [];
  match_word_pattern_values := [];
  exit(backtrack(pattern, input_string, 0, 0, match_word_pattern_keys, match_word_pattern_values));
end;
procedure main();
begin
  writeln(Ord(match_word_pattern('aba', 'GraphTreesGraph')));
  writeln(Ord(match_word_pattern('xyx', 'PythonRubyPython')));
  writeln(Ord(match_word_pattern('GG', 'PythonJavaPython')));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
