{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
  LETTERS: string;
  LOWERCASE: string;
  can_string_be_rearranged_as_palindrome_counter_key_idx: integer;
  can_string_be_rearranged_as_palindrome_character_key_idx: integer;
function char_to_lower(char_to_lower_c: string): string; forward;
function normalize(normalize_input_str: string): string; forward;
function can_string_be_rearranged_as_palindrome_counter(can_string_be_rearranged_as_palindrome_counter_input_str: string): boolean; forward;
function can_string_be_rearranged_as_palindrome(can_string_be_rearranged_as_palindrome_input_str: string): boolean; forward;
function char_to_lower(char_to_lower_c: string): string;
var
  char_to_lower_i: integer;
begin
  char_to_lower_i := 0;
  while char_to_lower_i < Length(LETTERS) do begin
  if char_to_lower_c = copy(LETTERS, char_to_lower_i+1, (char_to_lower_i + 1 - (char_to_lower_i))) then begin
  exit(copy(LOWERCASE, char_to_lower_i+1, (char_to_lower_i + 1 - (char_to_lower_i))));
end;
  char_to_lower_i := char_to_lower_i + 1;
end;
  exit(char_to_lower_c);
end;
function normalize(normalize_input_str: string): string;
var
  normalize_res: string;
  normalize_i: integer;
  normalize_ch: string;
  normalize_lc: string;
begin
  normalize_res := '';
  normalize_i := 0;
  while normalize_i < Length(normalize_input_str) do begin
  normalize_ch := copy(normalize_input_str, normalize_i+1, (normalize_i + 1 - (normalize_i)));
  normalize_lc := char_to_lower(normalize_ch);
  if (normalize_lc >= 'a') and (normalize_lc <= 'z') then begin
  normalize_res := normalize_res + normalize_lc;
end;
  normalize_i := normalize_i + 1;
end;
  exit(normalize_res);
end;
function can_string_be_rearranged_as_palindrome_counter(can_string_be_rearranged_as_palindrome_counter_input_str: string): boolean;
var
  can_string_be_rearranged_as_palindrome_counter_s: string;
  can_string_be_rearranged_as_palindrome_counter_freq: specialize TFPGMap<string, integer>;
  can_string_be_rearranged_as_palindrome_counter_i: integer;
  can_string_be_rearranged_as_palindrome_counter_ch: string;
  can_string_be_rearranged_as_palindrome_counter_odd: integer;
  can_string_be_rearranged_as_palindrome_counter_key: string;
begin
  can_string_be_rearranged_as_palindrome_counter_s := normalize(can_string_be_rearranged_as_palindrome_counter_input_str);
  can_string_be_rearranged_as_palindrome_counter_freq := specialize TFPGMap<string, integer>.Create();
  can_string_be_rearranged_as_palindrome_counter_i := 0;
  while can_string_be_rearranged_as_palindrome_counter_i < Length(can_string_be_rearranged_as_palindrome_counter_s) do begin
  can_string_be_rearranged_as_palindrome_counter_ch := copy(can_string_be_rearranged_as_palindrome_counter_s, can_string_be_rearranged_as_palindrome_counter_i+1, (can_string_be_rearranged_as_palindrome_counter_i + 1 - (can_string_be_rearranged_as_palindrome_counter_i)));
  if can_string_be_rearranged_as_palindrome_counter_freq.IndexOf(can_string_be_rearranged_as_palindrome_counter_ch) <> -1 then begin
  can_string_be_rearranged_as_palindrome_counter_freq[can_string_be_rearranged_as_palindrome_counter_ch] := can_string_be_rearranged_as_palindrome_counter_freq[can_string_be_rearranged_as_palindrome_counter_ch] + 1;
end else begin
  can_string_be_rearranged_as_palindrome_counter_freq[can_string_be_rearranged_as_palindrome_counter_ch] := 1;
end;
  can_string_be_rearranged_as_palindrome_counter_i := can_string_be_rearranged_as_palindrome_counter_i + 1;
end;
  can_string_be_rearranged_as_palindrome_counter_odd := 0;
  for can_string_be_rearranged_as_palindrome_counter_key_idx := 0 to (can_string_be_rearranged_as_palindrome_counter_freq.Count - 1) do begin
  can_string_be_rearranged_as_palindrome_counter_key := can_string_be_rearranged_as_palindrome_counter_freq.Keys[can_string_be_rearranged_as_palindrome_counter_key_idx];
  if (can_string_be_rearranged_as_palindrome_counter_freq[can_string_be_rearranged_as_palindrome_counter_key] mod 2) <> 0 then begin
  can_string_be_rearranged_as_palindrome_counter_odd := can_string_be_rearranged_as_palindrome_counter_odd + 1;
end;
end;
  exit(can_string_be_rearranged_as_palindrome_counter_odd < 2);
end;
function can_string_be_rearranged_as_palindrome(can_string_be_rearranged_as_palindrome_input_str: string): boolean;
var
  can_string_be_rearranged_as_palindrome_s: string;
  can_string_be_rearranged_as_palindrome_character_freq_dict: specialize TFPGMap<string, integer>;
  can_string_be_rearranged_as_palindrome_i: integer;
  can_string_be_rearranged_as_palindrome_character: string;
  can_string_be_rearranged_as_palindrome_odd_char: integer;
  can_string_be_rearranged_as_palindrome_character_key: string;
  can_string_be_rearranged_as_palindrome_character_count: integer;
  can_string_be_rearranged_as_palindrome_character_count_idx: integer;
begin
  can_string_be_rearranged_as_palindrome_s := normalize(can_string_be_rearranged_as_palindrome_input_str);
  if Length(can_string_be_rearranged_as_palindrome_s) = 0 then begin
  exit(true);
end;
  can_string_be_rearranged_as_palindrome_character_freq_dict := specialize TFPGMap<string, integer>.Create();
  can_string_be_rearranged_as_palindrome_i := 0;
  while can_string_be_rearranged_as_palindrome_i < Length(can_string_be_rearranged_as_palindrome_s) do begin
  can_string_be_rearranged_as_palindrome_character := copy(can_string_be_rearranged_as_palindrome_s, can_string_be_rearranged_as_palindrome_i+1, (can_string_be_rearranged_as_palindrome_i + 1 - (can_string_be_rearranged_as_palindrome_i)));
  if can_string_be_rearranged_as_palindrome_character_freq_dict.IndexOf(can_string_be_rearranged_as_palindrome_character) <> -1 then begin
  can_string_be_rearranged_as_palindrome_character_freq_dict[can_string_be_rearranged_as_palindrome_character] := can_string_be_rearranged_as_palindrome_character_freq_dict[can_string_be_rearranged_as_palindrome_character] + 1;
end else begin
  can_string_be_rearranged_as_palindrome_character_freq_dict[can_string_be_rearranged_as_palindrome_character] := 1;
end;
  can_string_be_rearranged_as_palindrome_i := can_string_be_rearranged_as_palindrome_i + 1;
end;
  can_string_be_rearranged_as_palindrome_odd_char := 0;
  for can_string_be_rearranged_as_palindrome_character_key_idx := 0 to (can_string_be_rearranged_as_palindrome_character_freq_dict.Count - 1) do begin
  can_string_be_rearranged_as_palindrome_character_key := can_string_be_rearranged_as_palindrome_character_freq_dict.Keys[can_string_be_rearranged_as_palindrome_character_key_idx];
  can_string_be_rearranged_as_palindrome_character_count_idx := can_string_be_rearranged_as_palindrome_character_freq_dict.IndexOf(can_string_be_rearranged_as_palindrome_character_key);
  if can_string_be_rearranged_as_palindrome_character_count_idx <> -1 then begin
  can_string_be_rearranged_as_palindrome_character_count := can_string_be_rearranged_as_palindrome_character_freq_dict.Data[can_string_be_rearranged_as_palindrome_character_count_idx];
end else begin
  can_string_be_rearranged_as_palindrome_character_count := 0;
end;
  if (can_string_be_rearranged_as_palindrome_character_count mod 2) <> 0 then begin
  can_string_be_rearranged_as_palindrome_odd_char := can_string_be_rearranged_as_palindrome_odd_char + 1;
end;
end;
  exit(not (can_string_be_rearranged_as_palindrome_odd_char > 1));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  LETTERS := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  LOWERCASE := 'abcdefghijklmnopqrstuvwxyz';
  writeln(Ord(can_string_be_rearranged_as_palindrome_counter('Momo')));
  writeln(Ord(can_string_be_rearranged_as_palindrome_counter('Mother')));
  writeln(Ord(can_string_be_rearranged_as_palindrome('Momo')));
  writeln(Ord(can_string_be_rearranged_as_palindrome('Mother')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
