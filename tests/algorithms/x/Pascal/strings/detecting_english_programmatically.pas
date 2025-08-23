{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math, fgl;
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
  LETTERS_AND_SPACE: string;
  LOWER: string;
  UPPER: string;
  ENGLISH_WORDS: specialize TFPGMap<string, boolean>;
function to_upper(to_upper_s: string): string; forward;
function char_in(char_in_chars: string; char_in_c: string): boolean; forward;
function remove_non_letters(remove_non_letters_message: string): string; forward;
function split_spaces(split_spaces_text: string): StrArray; forward;
function load_dictionary(): specialize TFPGMap<string, boolean>; forward;
function get_english_count(get_english_count_message: string): real; forward;
function is_english(is_english_message: string; is_english_word_percentage: integer; is_english_letter_percentage: integer): boolean; forward;
function to_upper(to_upper_s: string): string;
var
  to_upper_res: string;
  to_upper_i: integer;
  to_upper_c: string;
  to_upper_j: integer;
  to_upper_up: string;
begin
  to_upper_res := '';
  to_upper_i := 0;
  while to_upper_i < Length(to_upper_s) do begin
  to_upper_c := copy(to_upper_s, to_upper_i+1, (to_upper_i + 1 - (to_upper_i)));
  to_upper_j := 0;
  to_upper_up := to_upper_c;
  while to_upper_j < Length(LOWER) do begin
  if to_upper_c = copy(LOWER, to_upper_j+1, (to_upper_j + 1 - (to_upper_j))) then begin
  to_upper_up := copy(UPPER, to_upper_j+1, (to_upper_j + 1 - (to_upper_j)));
  break;
end;
  to_upper_j := to_upper_j + 1;
end;
  to_upper_res := to_upper_res + to_upper_up;
  to_upper_i := to_upper_i + 1;
end;
  exit(to_upper_res);
end;
function char_in(char_in_chars: string; char_in_c: string): boolean;
var
  char_in_i: integer;
begin
  char_in_i := 0;
  while char_in_i < Length(char_in_chars) do begin
  if copy(char_in_chars, char_in_i+1, (char_in_i + 1 - (char_in_i))) = char_in_c then begin
  exit(true);
end;
  char_in_i := char_in_i + 1;
end;
  exit(false);
end;
function remove_non_letters(remove_non_letters_message: string): string;
var
  remove_non_letters_res: string;
  remove_non_letters_i: integer;
  remove_non_letters_ch: string;
begin
  remove_non_letters_res := '';
  remove_non_letters_i := 0;
  while remove_non_letters_i < Length(remove_non_letters_message) do begin
  remove_non_letters_ch := copy(remove_non_letters_message, remove_non_letters_i+1, (remove_non_letters_i + 1 - (remove_non_letters_i)));
  if char_in(LETTERS_AND_SPACE, remove_non_letters_ch) then begin
  remove_non_letters_res := remove_non_letters_res + remove_non_letters_ch;
end;
  remove_non_letters_i := remove_non_letters_i + 1;
end;
  exit(remove_non_letters_res);
end;
function split_spaces(split_spaces_text: string): StrArray;
var
  split_spaces_res: array of string;
  split_spaces_current: string;
  split_spaces_i: integer;
  split_spaces_ch: string;
begin
  split_spaces_res := [];
  split_spaces_current := '';
  split_spaces_i := 0;
  while split_spaces_i < Length(split_spaces_text) do begin
  split_spaces_ch := copy(split_spaces_text, split_spaces_i+1, (split_spaces_i + 1 - (split_spaces_i)));
  if split_spaces_ch = ' ' then begin
  split_spaces_res := concat(split_spaces_res, StrArray([split_spaces_current]));
  split_spaces_current := '';
end else begin
  split_spaces_current := split_spaces_current + split_spaces_ch;
end;
  split_spaces_i := split_spaces_i + 1;
end;
  split_spaces_res := concat(split_spaces_res, StrArray([split_spaces_current]));
  exit(split_spaces_res);
end;
function load_dictionary(): specialize TFPGMap<string, boolean>;
var
  load_dictionary_words: array of string;
  load_dictionary_dict: specialize TFPGMap<string, boolean>;
  load_dictionary_w: string;
begin
  load_dictionary_words := ['HELLO', 'WORLD', 'HOW', 'ARE', 'YOU', 'THE', 'QUICK', 'BROWN', 'FOX', 'JUMPS', 'OVER', 'LAZY', 'DOG'];
  load_dictionary_dict := specialize TFPGMap<string, boolean>.Create();
  for load_dictionary_w in load_dictionary_words do begin
  load_dictionary_dict[load_dictionary_w] := true;
end;
  exit(load_dictionary_dict);
end;
function get_english_count(get_english_count_message: string): real;
var
  get_english_count_upper: string;
  get_english_count_cleaned: string;
  get_english_count_possible: StrArray;
  get_english_count_matches: integer;
  get_english_count_total: integer;
  get_english_count_w: string;
begin
  get_english_count_upper := to_upper(get_english_count_message);
  get_english_count_cleaned := remove_non_letters(get_english_count_upper);
  get_english_count_possible := split_spaces(get_english_count_cleaned);
  get_english_count_matches := 0;
  get_english_count_total := 0;
  for get_english_count_w in get_english_count_possible do begin
  if get_english_count_w <> '' then begin
  get_english_count_total := get_english_count_total + 1;
  if ENGLISH_WORDS.IndexOf(get_english_count_w) <> -1 then begin
  get_english_count_matches := get_english_count_matches + 1;
end;
end;
end;
  if get_english_count_total = 0 then begin
  exit(0);
end;
  exit(Double(get_english_count_matches) / Double(get_english_count_total));
end;
function is_english(is_english_message: string; is_english_word_percentage: integer; is_english_letter_percentage: integer): boolean;
var
  is_english_words_match: boolean;
  is_english_num_letters: integer;
  is_english_letters_pct: real;
  is_english_letters_match: boolean;
begin
  is_english_words_match := (get_english_count(is_english_message) * 100) >= Double(is_english_word_percentage);
  is_english_num_letters := Length(remove_non_letters(is_english_message));
  if Length(is_english_message) = 0 then begin
  is_english_letters_pct := 0;
end else begin
  is_english_letters_pct := (Double(is_english_num_letters) / Double(Length(is_english_message))) * 100;
end;
  is_english_letters_match := is_english_letters_pct >= Double(is_english_letter_percentage);
  exit(is_english_words_match and is_english_letters_match);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  LETTERS_AND_SPACE := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz 	' + #10;
  LOWER := 'abcdefghijklmnopqrstuvwxyz';
  UPPER := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ENGLISH_WORDS := load_dictionary();
  writeln(LowerCase(BoolToStr(is_english('Hello World', 20, 85), true)));
  writeln(LowerCase(BoolToStr(is_english('llold HorWd', 20, 85), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
