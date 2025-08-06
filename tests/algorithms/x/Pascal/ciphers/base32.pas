{$mode objfpc}
program Main;
uses SysUtils;
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
  B32_CHARSET: string;
  s: string;
  bits: string;
  data: string;
  n: integer;
  code: integer;
  ch: string;
function indexOfChar(s: string; ch: string): integer; forward;
function ord(ch: string): integer; forward;
function chr(code: integer): string; forward;
function repeat_(s: string; n: integer): string; forward;
function to_binary(n: integer; bits: integer): string; forward;
function binary_to_int(bits: string): integer; forward;
function base32_encode(data: string): string; forward;
function base32_decode(data: string): string; forward;
function indexOfChar(s: string; ch: string): integer;
var
  indexOfChar_i: integer;
begin
  indexOfChar_i := 0;
  while indexOfChar_i < Length(s) do begin
  if s[indexOfChar_i+1] = ch then begin
  exit(indexOfChar_i);
end;
  indexOfChar_i := indexOfChar_i + 1;
end;
  exit(-1);
end;
function ord(ch: string): integer;
var
  ord_upper: string;
  ord_lower: string;
  ord_digits: string;
  ord_idx: integer;
begin
  ord_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ord_lower := 'abcdefghijklmnopqrstuvwxyz';
  ord_digits := '0123456789';
  ord_idx := indexOfChar(ord_upper, ch);
  if ord_idx >= 0 then begin
  exit(65 + ord_idx);
end;
  ord_idx := indexOfChar(ord_lower, ch);
  if ord_idx >= 0 then begin
  exit(97 + ord_idx);
end;
  ord_idx := indexOfChar(ord_digits, ch);
  if ord_idx >= 0 then begin
  exit(48 + ord_idx);
end;
  if ch = ' ' then begin
  exit(32);
end;
  if ch = '!' then begin
  exit(33);
end;
  exit(0);
end;
function chr(code: integer): string;
var
  chr_upper: string;
  chr_lower: string;
  chr_digits: string;
  chr_idx: integer;
begin
  chr_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  chr_lower := 'abcdefghijklmnopqrstuvwxyz';
  chr_digits := '0123456789';
  if code = 32 then begin
  exit(' ');
end;
  if code = 33 then begin
  exit('!');
end;
  chr_idx := code - 65;
  if (chr_idx >= 0) and (chr_idx < Length(chr_upper)) then begin
  exit(chr_upper[chr_idx+1]);
end;
  chr_idx := code - 97;
  if (chr_idx >= 0) and (chr_idx < Length(chr_lower)) then begin
  exit(chr_lower[chr_idx+1]);
end;
  chr_idx := code - 48;
  if (chr_idx >= 0) and (chr_idx < Length(chr_digits)) then begin
  exit(chr_digits[chr_idx+1]);
end;
  exit('');
end;
function repeat_(s: string; n: integer): string;
var
  repeat__out: string;
  repeat__i: integer;
begin
  repeat__out := '';
  repeat__i := 0;
  while repeat__i < n do begin
  repeat__out := repeat__out + s;
  repeat__i := repeat__i + 1;
end;
  exit(repeat__out);
end;
function to_binary(n: integer; bits: integer): string;
var
  to_binary_v: integer;
  to_binary_out: string;
  to_binary_i: integer;
begin
  to_binary_v := n;
  to_binary_out := '';
  to_binary_i := 0;
  while to_binary_i < bits do begin
  to_binary_out := IntToStr(to_binary_v mod 2) + to_binary_out;
  to_binary_v := to_binary_v div 2;
  to_binary_i := to_binary_i + 1;
end;
  exit(to_binary_out);
end;
function binary_to_int(bits: string): integer;
var
  binary_to_int_n: integer;
  binary_to_int_i: integer;
begin
  binary_to_int_n := 0;
  binary_to_int_i := 0;
  while binary_to_int_i < Length(bits) do begin
  binary_to_int_n := binary_to_int_n * 2;
  if bits[binary_to_int_i+1] = '1' then begin
  binary_to_int_n := binary_to_int_n + 1;
end;
  binary_to_int_i := binary_to_int_i + 1;
end;
  exit(binary_to_int_n);
end;
function base32_encode(data: string): string;
var
  base32_encode_binary_data: string;
  base32_encode_i: integer;
  base32_encode_remainder: integer;
  base32_encode_b32_result: string;
  base32_encode_j: integer;
  base32_encode_chunk: string;
  base32_encode_index: integer;
  base32_encode_rem: integer;
begin
  base32_encode_binary_data := '';
  base32_encode_i := 0;
  while base32_encode_i < Length(data) do begin
  base32_encode_binary_data := base32_encode_binary_data + to_binary(ord(data[base32_encode_i+1]), 8);
  base32_encode_i := base32_encode_i + 1;
end;
  base32_encode_remainder := Length(base32_encode_binary_data) mod 5;
  if base32_encode_remainder <> 0 then begin
  base32_encode_binary_data := base32_encode_binary_data + repeat_('0', 5 - base32_encode_remainder);
end;
  base32_encode_b32_result := '';
  base32_encode_j := 0;
  while base32_encode_j < Length(base32_encode_binary_data) do begin
  base32_encode_chunk := copy(base32_encode_binary_data, base32_encode_j+1, (base32_encode_j + 5 - (base32_encode_j)));
  base32_encode_index := binary_to_int(base32_encode_chunk);
  base32_encode_b32_result := base32_encode_b32_result + B32_CHARSET[base32_encode_index+1];
  base32_encode_j := base32_encode_j + 5;
end;
  base32_encode_rem := Length(base32_encode_b32_result) mod 8;
  if base32_encode_rem <> 0 then begin
  base32_encode_b32_result := base32_encode_b32_result + repeat_('=', 8 - base32_encode_rem);
end;
  exit(base32_encode_b32_result);
end;
function base32_decode(data: string): string;
var
  base32_decode_clean: string;
  base32_decode_i: integer;
  base32_decode_ch: string;
  base32_decode_binary_chunks: string;
  base32_decode_idx: integer;
  base32_decode_result_: string;
  base32_decode_j: integer;
  base32_decode_byte_bits: string;
  base32_decode_code: integer;
begin
  base32_decode_clean := '';
  base32_decode_i := 0;
  while base32_decode_i < Length(data) do begin
  base32_decode_ch := data[base32_decode_i+1];
  if base32_decode_ch <> '=' then begin
  base32_decode_clean := base32_decode_clean + base32_decode_ch;
end;
  base32_decode_i := base32_decode_i + 1;
end;
  base32_decode_binary_chunks := '';
  base32_decode_i := 0;
  while base32_decode_i < Length(base32_decode_clean) do begin
  base32_decode_idx := indexOfChar(B32_CHARSET, base32_decode_clean[base32_decode_i+1]);
  base32_decode_binary_chunks := base32_decode_binary_chunks + to_binary(base32_decode_idx, 5);
  base32_decode_i := base32_decode_i + 1;
end;
  base32_decode_result_ := '';
  base32_decode_j := 0;
  while (base32_decode_j + 8) <= Length(base32_decode_binary_chunks) do begin
  base32_decode_byte_bits := copy(base32_decode_binary_chunks, base32_decode_j+1, (base32_decode_j + 8 - (base32_decode_j)));
  base32_decode_code := binary_to_int(base32_decode_byte_bits);
  base32_decode_result_ := base32_decode_result_ + chr(base32_decode_code);
  base32_decode_j := base32_decode_j + 8;
end;
  exit(base32_decode_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  B32_CHARSET := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
  writeln(base32_encode('Hello World!'));
  writeln(base32_encode('123456'));
  writeln(base32_encode('some long complex string'));
  writeln(base32_decode('JBSWY3DPEBLW64TMMQQQ===='));
  writeln(base32_decode('GEZDGNBVGY======'));
  writeln(base32_decode('ONXW2ZJANRXW4ZZAMNXW24DMMV4CA43UOJUW4ZY='));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
