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
  UPPER: string;
  LOWER: string;
  keyword: string;
  plaintext: string;
  xs: StrArray;
  s: string;
  keys: StrArray;
  x: string;
  verbose: boolean;
  key: string;
  ch: string;
  values: StrArray;
function to_upper(s: string): string; forward;
function contains(xs: StrArray; x: string): boolean; forward;
function contains_char(s: string; ch: string): boolean; forward;
function get_value(keys: StrArray; values: StrArray; key: string): string; forward;
procedure print_mapping(keys: StrArray; values: StrArray); forward;
function mixed_keyword(keyword: string; plaintext: string; verbose: boolean): string; forward;
function to_upper(s: string): string;
var
  to_upper_res: string;
  to_upper_i: integer;
  to_upper_ch: string;
  to_upper_j: integer;
  to_upper_found: boolean;
begin
  to_upper_res := '';
  to_upper_i := 0;
  while to_upper_i < Length(s) do begin
  to_upper_ch := s[to_upper_i+1];
  to_upper_j := 0;
  to_upper_found := false;
  while to_upper_j < 26 do begin
  if to_upper_ch = LOWER[to_upper_j+1] then begin
  to_upper_res := to_upper_res + UPPER[to_upper_j+1];
  to_upper_found := true;
  break;
end;
  to_upper_j := to_upper_j + 1;
end;
  if to_upper_found = false then begin
  to_upper_res := to_upper_res + to_upper_ch;
end;
  to_upper_i := to_upper_i + 1;
end;
  exit(to_upper_res);
end;
function contains(xs: StrArray; x: string): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(xs) do begin
  if xs[contains_i] = x then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function contains_char(s: string; ch: string): boolean;
var
  contains_char_i: integer;
begin
  contains_char_i := 0;
  while contains_char_i < Length(s) do begin
  if s[contains_char_i+1] = ch then begin
  exit(true);
end;
  contains_char_i := contains_char_i + 1;
end;
  exit(false);
end;
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
procedure print_mapping(keys: StrArray; values: StrArray);
var
  print_mapping_s: string;
  print_mapping_i: integer;
begin
  print_mapping_s := '{';
  print_mapping_i := 0;
  while print_mapping_i < Length(keys) do begin
  print_mapping_s := ((((print_mapping_s + '''') + keys[print_mapping_i]) + ''': ''') + values[print_mapping_i]) + '''';
  if (print_mapping_i + 1) < Length(keys) then begin
  print_mapping_s := print_mapping_s + ', ';
end;
  print_mapping_i := print_mapping_i + 1;
end;
  print_mapping_s := print_mapping_s + '}';
  writeln(print_mapping_s);
end;
function mixed_keyword(keyword: string; plaintext: string; verbose: boolean): string;
var
  mixed_keyword_alphabet: string;
  mixed_keyword_keyword_u: string;
  mixed_keyword_plaintext_u: string;
  mixed_keyword_unique: array of string;
  mixed_keyword_i: integer;
  mixed_keyword_ch: string;
  mixed_keyword_num_unique: integer;
  mixed_keyword_shifted: array of string;
  mixed_keyword_modified: array of StrArray;
  mixed_keyword_k: integer;
  mixed_keyword_row: array of string;
  mixed_keyword_r: integer;
  mixed_keyword_keys: array of string;
  mixed_keyword_values: array of string;
  mixed_keyword_column: integer;
  mixed_keyword_letter_index: integer;
  mixed_keyword_row_idx: integer;
  mixed_keyword_result_: string;
  mixed_keyword_mapped: string;
begin
  mixed_keyword_alphabet := UPPER;
  mixed_keyword_keyword_u := to_upper(keyword);
  mixed_keyword_plaintext_u := to_upper(plaintext);
  mixed_keyword_unique := [];
  mixed_keyword_i := 0;
  while mixed_keyword_i < Length(mixed_keyword_keyword_u) do begin
  mixed_keyword_ch := mixed_keyword_keyword_u[mixed_keyword_i+1];
  if contains_char(mixed_keyword_alphabet, mixed_keyword_ch) and (contains(mixed_keyword_unique, mixed_keyword_ch) = false) then begin
  mixed_keyword_unique := concat(mixed_keyword_unique, StrArray([mixed_keyword_ch]));
end;
  mixed_keyword_i := mixed_keyword_i + 1;
end;
  mixed_keyword_num_unique := Length(mixed_keyword_unique);
  mixed_keyword_shifted := [];
  mixed_keyword_i := 0;
  while mixed_keyword_i < Length(mixed_keyword_unique) do begin
  mixed_keyword_shifted := concat(mixed_keyword_shifted, StrArray([mixed_keyword_unique[mixed_keyword_i]]));
  mixed_keyword_i := mixed_keyword_i + 1;
end;
  mixed_keyword_i := 0;
  while mixed_keyword_i < Length(mixed_keyword_alphabet) do begin
  mixed_keyword_ch := mixed_keyword_alphabet[mixed_keyword_i+1];
  if contains(mixed_keyword_unique, mixed_keyword_ch) = false then begin
  mixed_keyword_shifted := concat(mixed_keyword_shifted, StrArray([mixed_keyword_ch]));
end;
  mixed_keyword_i := mixed_keyword_i + 1;
end;
  mixed_keyword_modified := [];
  mixed_keyword_k := 0;
  while mixed_keyword_k < Length(mixed_keyword_shifted) do begin
  mixed_keyword_row := [];
  mixed_keyword_r := 0;
  while (mixed_keyword_r < mixed_keyword_num_unique) and ((mixed_keyword_k + mixed_keyword_r) < Length(mixed_keyword_shifted)) do begin
  mixed_keyword_row := concat(mixed_keyword_row, StrArray([mixed_keyword_shifted[mixed_keyword_k + mixed_keyword_r]]));
  mixed_keyword_r := mixed_keyword_r + 1;
end;
  mixed_keyword_modified := concat(mixed_keyword_modified, [mixed_keyword_row]);
  mixed_keyword_k := mixed_keyword_k + mixed_keyword_num_unique;
end;
  mixed_keyword_keys := [];
  mixed_keyword_values := [];
  mixed_keyword_column := 0;
  mixed_keyword_letter_index := 0;
  while mixed_keyword_column < mixed_keyword_num_unique do begin
  mixed_keyword_row_idx := 0;
  while mixed_keyword_row_idx < Length(mixed_keyword_modified) do begin
  mixed_keyword_row := mixed_keyword_modified[mixed_keyword_row_idx];
  if Length(mixed_keyword_row) <= mixed_keyword_column then begin
  break;
end;
  mixed_keyword_keys := concat(mixed_keyword_keys, StrArray([mixed_keyword_alphabet[mixed_keyword_letter_index+1]]));
  mixed_keyword_values := concat(mixed_keyword_values, StrArray([mixed_keyword_row[mixed_keyword_column]]));
  mixed_keyword_letter_index := mixed_keyword_letter_index + 1;
  mixed_keyword_row_idx := mixed_keyword_row_idx + 1;
end;
  mixed_keyword_column := mixed_keyword_column + 1;
end;
  if verbose then begin
  print_mapping(mixed_keyword_keys, mixed_keyword_values);
end;
  mixed_keyword_result_ := '';
  mixed_keyword_i := 0;
  while mixed_keyword_i < Length(mixed_keyword_plaintext_u) do begin
  mixed_keyword_ch := mixed_keyword_plaintext_u[mixed_keyword_i+1];
  mixed_keyword_mapped := get_value(mixed_keyword_keys, mixed_keyword_values, mixed_keyword_ch);
  if mixed_keyword_mapped = '' then begin
  mixed_keyword_result_ := mixed_keyword_result_ + mixed_keyword_ch;
end else begin
  mixed_keyword_result_ := mixed_keyword_result_ + mixed_keyword_mapped;
end;
  mixed_keyword_i := mixed_keyword_i + 1;
end;
  exit(mixed_keyword_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  UPPER := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  LOWER := 'abcdefghijklmnopqrstuvwxyz';
  writeln(mixed_keyword('college', 'UNIVERSITY', true));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
