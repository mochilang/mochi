{$mode objfpc}{$modeswitch nestedprocvars}
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
function split(split_s: string; split_sep: string): StrArray; forward;
function capitalize(capitalize_word: string): string; forward;
function snake_to_camel_case(snake_to_camel_case_input_str: string; snake_to_camel_case_use_pascal: boolean): string; forward;
function split(split_s: string; split_sep: string): StrArray;
var
  split_res: array of string;
  split_current: string;
  split_i: integer;
  split_ch: string;
begin
  split_res := [];
  split_current := '';
  split_i := 0;
  while split_i < Length(split_s) do begin
  split_ch := copy(split_s, split_i+1, (split_i + 1 - (split_i)));
  if split_ch = split_sep then begin
  split_res := concat(split_res, StrArray([split_current]));
  split_current := '';
end else begin
  split_current := split_current + split_ch;
end;
  split_i := split_i + 1;
end;
  split_res := concat(split_res, StrArray([split_current]));
  exit(split_res);
end;
function capitalize(capitalize_word: string): string;
var
  capitalize_first: string;
  capitalize_rest: string;
begin
  if Length(capitalize_word) = 0 then begin
  exit('');
end;
  capitalize_first := UpperCase(copy(capitalize_word, 1, 1));
  capitalize_rest := copy(capitalize_word, 2, (Length(capitalize_word) - (1)));
  exit(capitalize_first + capitalize_rest);
end;
function snake_to_camel_case(snake_to_camel_case_input_str: string; snake_to_camel_case_use_pascal: boolean): string;
var
  snake_to_camel_case_words: StrArray;
  snake_to_camel_case_result_: string;
  snake_to_camel_case_index: integer;
  snake_to_camel_case_word: string;
begin
  snake_to_camel_case_words := split(snake_to_camel_case_input_str, '_');
  snake_to_camel_case_result_ := '';
  snake_to_camel_case_index := 0;
  if not snake_to_camel_case_use_pascal then begin
  if Length(snake_to_camel_case_words) > 0 then begin
  snake_to_camel_case_result_ := snake_to_camel_case_words[0];
  snake_to_camel_case_index := 1;
end;
end;
  while snake_to_camel_case_index < Length(snake_to_camel_case_words) do begin
  snake_to_camel_case_word := snake_to_camel_case_words[snake_to_camel_case_index];
  snake_to_camel_case_result_ := snake_to_camel_case_result_ + capitalize(snake_to_camel_case_word);
  snake_to_camel_case_index := snake_to_camel_case_index + 1;
end;
  exit(snake_to_camel_case_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(snake_to_camel_case('some_random_string', false));
  writeln(snake_to_camel_case('some_random_string', true));
  writeln(snake_to_camel_case('some_random_string_with_numbers_123', false));
  writeln(snake_to_camel_case('some_random_string_with_numbers_123', true));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
