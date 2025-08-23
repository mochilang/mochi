{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, StrUtils;
type StrArray = array of string;
type StrArrayArray = array of StrArray;
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
function split_words(split_words_s: string): StrArray; forward;
function is_alnum(is_alnum_c: string): boolean; forward;
function split_input(split_input_text: string): StrArrayArray; forward;
function capitalize(capitalize_word: string): string; forward;
function to_simple_case(to_simple_case_text: string): string; forward;
function to_complex_case(to_complex_case_text: string; to_complex_case_upper_flag: boolean; to_complex_case_sep: string): string; forward;
function to_pascal_case(to_pascal_case_text: string): string; forward;
function to_camel_case(to_camel_case_text: string): string; forward;
function to_snake_case(to_snake_case_text: string; to_snake_case_upper_flag: boolean): string; forward;
function to_kebab_case(to_kebab_case_text: string; to_kebab_case_upper_flag: boolean): string; forward;
function split_words(split_words_s: string): StrArray;
var
  split_words_words: array of string;
  split_words_current: string;
  split_words_ch: int64;
begin
  split_words_words := [];
  split_words_current := '';
  for split_words_ch in split_words_s do begin
  if split_words_ch = ' ' then begin
  if split_words_current <> '' then begin
  split_words_words := concat(split_words_words, StrArray([split_words_current]));
  split_words_current := '';
end;
end else begin
  split_words_current := split_words_current + split_words_ch;
end;
end;
  if split_words_current <> '' then begin
  split_words_words := concat(split_words_words, StrArray([split_words_current]));
end;
  exit(split_words_words);
end;
function is_alnum(is_alnum_c: string): boolean;
begin
  exit((((Pos(is_alnum_c, '0123456789') <> 0) or (Pos(is_alnum_c, 'abcdefghijklmnopqrstuvwxyz') <> 0)) or (Pos(is_alnum_c, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ') <> 0)) or (is_alnum_c = ' '));
end;
function split_input(split_input_text: string): StrArrayArray;
var
  split_input_result_: array of StrArray;
  split_input_current: string;
  split_input_ch: int64;
begin
  split_input_result_ := [];
  split_input_current := '';
  for split_input_ch in split_input_text do begin
  if is_alnum(split_input_ch) then begin
  split_input_current := split_input_current + split_input_ch;
end else begin
  if split_input_current <> '' then begin
  split_input_result_ := concat(split_input_result_, [split_words(split_input_current)]);
  split_input_current := '';
end;
end;
end;
  if split_input_current <> '' then begin
  split_input_result_ := concat(split_input_result_, [split_words(split_input_current)]);
end;
  exit(split_input_result_);
end;
function capitalize(capitalize_word: string): string;
begin
  if Length(capitalize_word) = 0 then begin
  exit('');
end;
  if Length(capitalize_word) = 1 then begin
  exit(UpperCase(capitalize_word));
end;
  exit(UpperCase(copy(capitalize_word, 1, 1)) + LowerCase(copy(capitalize_word, 2, Length(capitalize_word))));
end;
function to_simple_case(to_simple_case_text: string): string;
var
  to_simple_case_parts: StrArrayArray;
  to_simple_case_res: string;
  to_simple_case_sub: StrArray;
  to_simple_case_w: string;
begin
  to_simple_case_parts := split_input(to_simple_case_text);
  to_simple_case_res := '';
  for to_simple_case_sub in to_simple_case_parts do begin
  for to_simple_case_w in to_simple_case_sub do begin
  to_simple_case_res := to_simple_case_res + capitalize(to_simple_case_w);
end;
end;
  exit(to_simple_case_res);
end;
function to_complex_case(to_complex_case_text: string; to_complex_case_upper_flag: boolean; to_complex_case_sep: string): string;
var
  to_complex_case_parts: StrArrayArray;
  to_complex_case_res: string;
  to_complex_case_sub: StrArray;
  to_complex_case_first: boolean;
  to_complex_case_w: string;
  to_complex_case_word: string;
begin
  to_complex_case_parts := split_input(to_complex_case_text);
  to_complex_case_res := '';
  for to_complex_case_sub in to_complex_case_parts do begin
  to_complex_case_first := true;
  for to_complex_case_w in to_complex_case_sub do begin
  to_complex_case_word := IfThen(to_complex_case_upper_flag, UpperCase(to_complex_case_w), LowerCase(to_complex_case_w));
  if to_complex_case_first then begin
  to_complex_case_res := to_complex_case_res + to_complex_case_word;
  to_complex_case_first := false;
end else begin
  to_complex_case_res := (to_complex_case_res + to_complex_case_sep) + to_complex_case_word;
end;
end;
end;
  exit(to_complex_case_res);
end;
function to_pascal_case(to_pascal_case_text: string): string;
begin
  exit(to_simple_case(to_pascal_case_text));
end;
function to_camel_case(to_camel_case_text: string): string;
var
  to_camel_case_s: string;
begin
  to_camel_case_s := to_simple_case(to_camel_case_text);
  if Length(to_camel_case_s) = 0 then begin
  exit('');
end;
  exit(LowerCase(copy(to_camel_case_s, 1, 1)) + copy(to_camel_case_s, 2, Length(to_camel_case_s)));
end;
function to_snake_case(to_snake_case_text: string; to_snake_case_upper_flag: boolean): string;
begin
  exit(to_complex_case(to_snake_case_text, to_snake_case_upper_flag, '_'));
end;
function to_kebab_case(to_kebab_case_text: string; to_kebab_case_upper_flag: boolean): string;
begin
  exit(to_complex_case(to_kebab_case_text, to_kebab_case_upper_flag, '-'));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(to_pascal_case('one two 31235three4four'));
  writeln(to_camel_case('one two 31235three4four'));
  writeln(to_snake_case('one two 31235three4four', true));
  writeln(to_snake_case('one two 31235three4four', false));
  writeln(to_kebab_case('one two 31235three4four', true));
  writeln(to_kebab_case('one two 31235three4four', false));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
