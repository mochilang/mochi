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
  square: array of StrArray;
  index2: integer;
  message: string;
  letter: string;
  index1: integer;
  ch: string;
function letter_to_numbers(letter: string): IntArray; forward;
function numbers_to_letter(index1: integer; index2: integer): string; forward;
function char_to_int(ch: string): integer; forward;
function encode(message: string): string; forward;
function decode(message: string): string; forward;
function letter_to_numbers(letter: string): IntArray;
var
  letter_to_numbers_i: integer;
  letter_to_numbers_j: integer;
begin
  letter_to_numbers_i := 0;
  while letter_to_numbers_i < Length(square) do begin
  letter_to_numbers_j := 0;
  while letter_to_numbers_j < Length(square[letter_to_numbers_i]) do begin
  if square[letter_to_numbers_i][letter_to_numbers_j] = letter then begin
  exit([letter_to_numbers_i + 1, letter_to_numbers_j + 1]);
end;
  letter_to_numbers_j := letter_to_numbers_j + 1;
end;
  letter_to_numbers_i := letter_to_numbers_i + 1;
end;
  exit([0, 0]);
end;
function numbers_to_letter(index1: integer; index2: integer): string;
begin
  exit(square[index1 - 1][index2 - 1]);
end;
function char_to_int(ch: string): integer;
begin
  if ch = '1' then begin
  exit(1);
end;
  if ch = '2' then begin
  exit(2);
end;
  if ch = '3' then begin
  exit(3);
end;
  if ch = '4' then begin
  exit(4);
end;
  if ch = '5' then begin
  exit(5);
end;
  exit(0);
end;
function encode(message: string): string;
var
  encode_encoded: string;
  encode_i: integer;
  encode_ch: string;
  encode_nums: IntArray;
begin
  message := LowerCase(message);
  encode_encoded := '';
  encode_i := 0;
  while encode_i < Length(message) do begin
  encode_ch := message[encode_i+1];
  if encode_ch = 'j' then begin
  encode_ch := 'i';
end;
  if encode_ch <> ' ' then begin
  encode_nums := letter_to_numbers(encode_ch);
  encode_encoded := (encode_encoded + IntToStr(encode_nums[0])) + IntToStr(encode_nums[1]);
end else begin
  encode_encoded := encode_encoded + ' ';
end;
  encode_i := encode_i + 1;
end;
  exit(encode_encoded);
end;
function decode(message: string): string;
var
  decode_decoded: string;
  decode_i: integer;
  decode_index1: integer;
  decode_index2: integer;
  decode_letter: string;
begin
  decode_decoded := '';
  decode_i := 0;
  while decode_i < Length(message) do begin
  if message[decode_i+1] = ' ' then begin
  decode_decoded := decode_decoded + ' ';
  decode_i := decode_i + 1;
end else begin
  decode_index1 := char_to_int(message[decode_i+1]);
  decode_index2 := char_to_int(message[decode_i + 1+1]);
  decode_letter := numbers_to_letter(decode_index1, decode_index2);
  decode_decoded := decode_decoded + decode_letter;
  decode_i := decode_i + 2;
end;
end;
  exit(decode_decoded);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  square := [['a', 'b', 'c', 'd', 'e'], ['f', 'g', 'h', 'i', 'k'], ['l', 'm', 'n', 'o', 'p'], ['q', 'r', 's', 't', 'u'], ['v', 'w', 'x', 'y', 'z']];
  writeln(encode('test message'));
  writeln(encode('Test Message'));
  writeln(decode('44154344 32154343112215'));
  writeln(decode('4415434432154343112215'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
