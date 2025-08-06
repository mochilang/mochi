{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  UPPER: string;
  LOWER: string;
  BASE_TOP: string;
  BASE_BOTTOM: string;
  s: string;
  k: integer;
  c: string;
  ch: string;
  key: string;
  table: StrArray;
  words: string;
function to_upper(s: string): string; forward;
function char_index(c: string): integer; forward;
function rotate_right(s: string; k: integer): string; forward;
function table_for(c: string): StrArray; forward;
function generate_table(key: string): StrArrayArray; forward;
function str_index(s: string; ch: string): integer; forward;
function get_position(table: StrArray; ch: string): IntArray; forward;
function get_opponent(table: StrArray; ch: string): string; forward;
function encrypt(key: string; words: string): string; forward;
function decrypt(key: string; words: string): string; forward;
procedure main(); forward;
function to_upper(s: string): string;
var
  to_upper_res: string;
  to_upper_i: integer;
  to_upper_ch: string;
  to_upper_j: integer;
  to_upper_replaced: boolean;
begin
  to_upper_res := '';
  to_upper_i := 0;
  while to_upper_i < Length(s) do begin
  to_upper_ch := copy(s, to_upper_i+1, (to_upper_i + 1 - (to_upper_i)));
  to_upper_j := 0;
  to_upper_replaced := false;
  while to_upper_j < Length(LOWER) do begin
  if copy(LOWER, to_upper_j+1, (to_upper_j + 1 - (to_upper_j))) = to_upper_ch then begin
  to_upper_res := to_upper_res + copy(UPPER, to_upper_j+1, (to_upper_j + 1 - (to_upper_j)));
  to_upper_replaced := true;
  break;
end;
  to_upper_j := to_upper_j + 1;
end;
  if not to_upper_replaced then begin
  to_upper_res := to_upper_res + to_upper_ch;
end;
  to_upper_i := to_upper_i + 1;
end;
  exit(to_upper_res);
end;
function char_index(c: string): integer;
var
  char_index_i: integer;
begin
  char_index_i := 0;
  while char_index_i < Length(UPPER) do begin
  if copy(UPPER, char_index_i+1, (char_index_i + 1 - (char_index_i))) = c then begin
  exit(char_index_i);
end;
  char_index_i := char_index_i + 1;
end;
  exit(-1);
end;
function rotate_right(s: string; k: integer): string;
var
  rotate_right_n: integer;
  rotate_right_shift: integer;
begin
  rotate_right_n := Length(s);
  rotate_right_shift := k mod rotate_right_n;
  exit(copy(s, rotate_right_n - rotate_right_shift+1, (rotate_right_n - (rotate_right_n - rotate_right_shift))) + copy(s, 0+1, (rotate_right_n - rotate_right_shift - (0))));
end;
function table_for(c: string): StrArray;
var
  table_for_idx: integer;
  table_for_shift: integer;
  table_for_row1: string;
  table_for_pair: array of string;
begin
  table_for_idx := char_index(c);
  table_for_shift := table_for_idx div 2;
  table_for_row1 := rotate_right(BASE_BOTTOM, table_for_shift);
  table_for_pair := [BASE_TOP, table_for_row1];
  exit(table_for_pair);
end;
function generate_table(key: string): StrArrayArray;
var
  generate_table_up: string;
  generate_table_i: integer;
  generate_table_result_: array of StrArray;
  generate_table_ch: string;
  generate_table_pair: StrArray;
begin
  generate_table_up := to_upper(key);
  generate_table_i := 0;
  generate_table_result_ := [];
  while generate_table_i < Length(generate_table_up) do begin
  generate_table_ch := copy(generate_table_up, generate_table_i+1, (generate_table_i + 1 - (generate_table_i)));
  generate_table_pair := table_for(generate_table_ch);
  generate_table_result_ := concat(generate_table_result_, [generate_table_pair]);
  generate_table_i := generate_table_i + 1;
end;
  exit(generate_table_result_);
end;
function str_index(s: string; ch: string): integer;
var
  str_index_i: integer;
begin
  str_index_i := 0;
  while str_index_i < Length(s) do begin
  if copy(s, str_index_i+1, (str_index_i + 1 - (str_index_i))) = ch then begin
  exit(str_index_i);
end;
  str_index_i := str_index_i + 1;
end;
  exit(0 - 1);
end;
function get_position(table: StrArray; ch: string): IntArray;
var
  get_position_row: integer;
  get_position_col: integer;
begin
  get_position_row := 0;
  if str_index(table[0], ch) = (0 - 1) then begin
  get_position_row := 1;
end;
  get_position_col := str_index(table[get_position_row], ch);
  exit([get_position_row, get_position_col]);
end;
function get_opponent(table: StrArray; ch: string): string;
var
  get_opponent_pos: IntArray;
  get_opponent_row: integer;
  get_opponent_col: integer;
begin
  get_opponent_pos := get_position(table, ch);
  get_opponent_row := get_opponent_pos[0];
  get_opponent_col := get_opponent_pos[1];
  if get_opponent_col = (0 - 1) then begin
  exit(ch);
end;
  if get_opponent_row = 1 then begin
  exit(copy(table[0], get_opponent_col+1, (get_opponent_col + 1 - (get_opponent_col))));
end;
  exit(copy(table[1], get_opponent_col+1, (get_opponent_col + 1 - (get_opponent_col))));
end;
function encrypt(key: string; words: string): string;
var
  encrypt_table: StrArrayArray;
  encrypt_up_words: string;
  encrypt_cipher: string;
  encrypt_count: integer;
  encrypt_i: integer;
  encrypt_ch: string;
begin
  encrypt_table := generate_table(key);
  encrypt_up_words := to_upper(words);
  encrypt_cipher := '';
  encrypt_count := 0;
  encrypt_i := 0;
  while encrypt_i < Length(encrypt_up_words) do begin
  encrypt_ch := copy(encrypt_up_words, encrypt_i+1, (encrypt_i + 1 - (encrypt_i)));
  encrypt_cipher := encrypt_cipher + get_opponent(encrypt_table[encrypt_count], encrypt_ch);
  encrypt_count := (encrypt_count + 1) mod Length(encrypt_table);
  encrypt_i := encrypt_i + 1;
end;
  exit(encrypt_cipher);
end;
function decrypt(key: string; words: string): string;
var
  decrypt_res: string;
begin
  decrypt_res := encrypt(key, words);
  exit(decrypt_res);
end;
procedure main();
begin
  writeln(encrypt('marvin', 'jessica'));
  writeln(decrypt('marvin', 'QRACRWU'));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  UPPER := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  LOWER := 'abcdefghijklmnopqrstuvwxyz';
  BASE_TOP := 'ABCDEFGHIJKLM';
  BASE_BOTTOM := 'NOPQRSTUVWXYZ';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
