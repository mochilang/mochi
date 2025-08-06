{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
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
  SQUARE: array of StrArray;
  row: integer;
  message: string;
  ch: string;
  replace_j: boolean;
  letter: string;
  col: integer;
  s: string;
function index_of(s: string; ch: string): integer; forward;
function to_lower_without_spaces(message: string; replace_j: boolean): string; forward;
function letter_to_numbers(letter: string): IntArray; forward;
function numbers_to_letter(row: integer; col: integer): string; forward;
function encode(message: string): string; forward;
function decode(message: string): string; forward;
function index_of(s: string; ch: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(s) do begin
  if s[index_of_i+1] = ch then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function to_lower_without_spaces(message: string; replace_j: boolean): string;
var
  to_lower_without_spaces_upper: string;
  to_lower_without_spaces_lower: string;
  to_lower_without_spaces_res: string;
  to_lower_without_spaces_i: integer;
  to_lower_without_spaces_ch: string;
  to_lower_without_spaces_pos: integer;
begin
  to_lower_without_spaces_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  to_lower_without_spaces_lower := 'abcdefghijklmnopqrstuvwxyz';
  to_lower_without_spaces_res := '';
  to_lower_without_spaces_i := 0;
  while to_lower_without_spaces_i < Length(message) do begin
  to_lower_without_spaces_ch := message[to_lower_without_spaces_i+1];
  to_lower_without_spaces_pos := index_of(to_lower_without_spaces_upper, to_lower_without_spaces_ch);
  if to_lower_without_spaces_pos >= 0 then begin
  to_lower_without_spaces_ch := to_lower_without_spaces_lower[to_lower_without_spaces_pos+1];
end;
  if to_lower_without_spaces_ch <> ' ' then begin
  if replace_j and (to_lower_without_spaces_ch = 'j') then begin
  to_lower_without_spaces_ch := 'i';
end;
  to_lower_without_spaces_res := to_lower_without_spaces_res + to_lower_without_spaces_ch;
end;
  to_lower_without_spaces_i := to_lower_without_spaces_i + 1;
end;
  exit(to_lower_without_spaces_res);
end;
function letter_to_numbers(letter: string): IntArray;
var
  letter_to_numbers_r: integer;
  letter_to_numbers_c: integer;
begin
  letter_to_numbers_r := 0;
  while letter_to_numbers_r < Length(SQUARE) do begin
  letter_to_numbers_c := 0;
  while letter_to_numbers_c < Length(SQUARE[letter_to_numbers_r]) do begin
  if SQUARE[letter_to_numbers_r][letter_to_numbers_c] = letter then begin
  exit(IntArray([letter_to_numbers_r + 1, letter_to_numbers_c + 1]));
end;
  letter_to_numbers_c := letter_to_numbers_c + 1;
end;
  letter_to_numbers_r := letter_to_numbers_r + 1;
end;
  exit(IntArray([0, 0]));
end;
function numbers_to_letter(row: integer; col: integer): string;
begin
  exit(SQUARE[row - 1][col - 1]);
end;
function encode(message: string): string;
var
  encode_clean: string;
  encode_l: integer;
  encode_rows: array of integer;
  encode_cols: array of integer;
  encode_i: integer;
  encode_nums: IntArray;
  encode_seq: array of integer;
  encode_encoded: string;
  encode_r: integer;
  encode_c: integer;
begin
  encode_clean := to_lower_without_spaces(message, true);
  encode_l := Length(encode_clean);
  encode_rows := IntArray([]);
  encode_cols := IntArray([]);
  encode_i := 0;
  while encode_i < encode_l do begin
  encode_nums := letter_to_numbers(encode_clean[encode_i+1]);
  encode_rows := concat(encode_rows, IntArray([encode_nums[0]]));
  encode_cols := concat(encode_cols, IntArray([encode_nums[1]]));
  encode_i := encode_i + 1;
end;
  encode_seq := IntArray([]);
  encode_i := 0;
  while encode_i < encode_l do begin
  encode_seq := concat(encode_seq, IntArray([encode_rows[encode_i]]));
  encode_i := encode_i + 1;
end;
  encode_i := 0;
  while encode_i < encode_l do begin
  encode_seq := concat(encode_seq, IntArray([encode_cols[encode_i]]));
  encode_i := encode_i + 1;
end;
  encode_encoded := '';
  encode_i := 0;
  while encode_i < encode_l do begin
  encode_r := encode_seq[2 * encode_i];
  encode_c := encode_seq[(2 * encode_i) + 1];
  encode_encoded := encode_encoded + numbers_to_letter(encode_r, encode_c);
  encode_i := encode_i + 1;
end;
  exit(encode_encoded);
end;
function decode(message: string): string;
var
  decode_clean: string;
  decode_l: integer;
  decode_first: array of integer;
  decode_i: integer;
  decode_nums: IntArray;
  decode_top: array of integer;
  decode_bottom: array of integer;
  decode_decoded: string;
  decode_r: integer;
  decode_c: integer;
begin
  decode_clean := to_lower_without_spaces(message, false);
  decode_l := Length(decode_clean);
  decode_first := IntArray([]);
  decode_i := 0;
  while decode_i < decode_l do begin
  decode_nums := letter_to_numbers(decode_clean[decode_i+1]);
  decode_first := concat(decode_first, IntArray([decode_nums[0]]));
  decode_first := concat(decode_first, IntArray([decode_nums[1]]));
  decode_i := decode_i + 1;
end;
  decode_top := IntArray([]);
  decode_bottom := IntArray([]);
  decode_i := 0;
  while decode_i < decode_l do begin
  decode_top := concat(decode_top, IntArray([decode_first[decode_i]]));
  decode_bottom := concat(decode_bottom, IntArray([decode_first[decode_i + decode_l]]));
  decode_i := decode_i + 1;
end;
  decode_decoded := '';
  decode_i := 0;
  while decode_i < decode_l do begin
  decode_r := decode_top[decode_i];
  decode_c := decode_bottom[decode_i];
  decode_decoded := decode_decoded + numbers_to_letter(decode_r, decode_c);
  decode_i := decode_i + 1;
end;
  exit(decode_decoded);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  SQUARE := [['a', 'b', 'c', 'd', 'e'], ['f', 'g', 'h', 'i', 'k'], ['l', 'm', 'n', 'o', 'p'], ['q', 'r', 's', 't', 'u'], ['v', 'w', 'x', 'y', 'z']];
  writeln(encode('testmessage'));
  writeln(encode('Test Message'));
  writeln(encode('test j'));
  writeln(encode('test i'));
  writeln(decode('qtltbdxrxlk'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
