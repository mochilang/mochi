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
  ascii85_chars: string;
  s: string;
  digits: string;
  n: integer;
  ch: string;
  data: string;
  d: integer;
  bits: string;
function indexOf(s: string; ch: string): integer; forward;
function ord(ch: string): integer; forward;
function chr(n: integer): string; forward;
function to_binary(n: integer; bits: integer): string; forward;
function bin_to_int(bits: string): integer; forward;
function reverse(s: string): string; forward;
function base10_to_85(d: integer): string; forward;
function base85_to_10(digits: string): integer; forward;
function ascii85_encode(data: string): string; forward;
function ascii85_decode(data: string): string; forward;
function indexOf(s: string; ch: string): integer;
var
  indexOf_i: integer;
begin
  indexOf_i := 0;
  while indexOf_i < Length(s) do begin
  if s[indexOf_i+1] = ch then begin
  exit(indexOf_i);
end;
  indexOf_i := indexOf_i + 1;
end;
  exit(-1);
end;
function ord(ch: string): integer;
var
  ord_idx: integer;
begin
  ord_idx := indexOf(ascii85_chars, ch);
  if ord_idx >= 0 then begin
  exit(33 + ord_idx);
end;
  exit(0);
end;
function chr(n: integer): string;
begin
  if (n >= 33) and (n <= 117) then begin
  exit(copy(ascii85_chars, n - 33+1, (n - 32 - (n - 33))));
end;
  exit('?');
end;
function to_binary(n: integer; bits: integer): string;
var
  to_binary_b: string;
  to_binary_val: integer;
begin
  to_binary_b := '';
  to_binary_val := n;
  while to_binary_val > 0 do begin
  to_binary_b := IntToStr(to_binary_val mod 2) + to_binary_b;
  to_binary_val := to_binary_val div 2;
end;
  while Length(to_binary_b) < bits do begin
  to_binary_b := '0' + to_binary_b;
end;
  if Length(to_binary_b) = 0 then begin
  to_binary_b := '0';
end;
  exit(to_binary_b);
end;
function bin_to_int(bits: string): integer;
var
  bin_to_int_n: integer;
  bin_to_int_i: integer;
begin
  bin_to_int_n := 0;
  bin_to_int_i := 0;
  while bin_to_int_i < Length(bits) do begin
  if bits[bin_to_int_i+1] = '1' then begin
  bin_to_int_n := (bin_to_int_n * 2) + 1;
end else begin
  bin_to_int_n := bin_to_int_n * 2;
end;
  bin_to_int_i := bin_to_int_i + 1;
end;
  exit(bin_to_int_n);
end;
function reverse(s: string): string;
var
  reverse_res: string;
  reverse_i: integer;
begin
  reverse_res := '';
  reverse_i := Length(s) - 1;
  while reverse_i >= 0 do begin
  reverse_res := reverse_res + s[reverse_i+1];
  reverse_i := reverse_i - 1;
end;
  exit(reverse_res);
end;
function base10_to_85(d: integer): string;
begin
  if d > 0 then begin
  exit(chr((d mod 85) + 33) + base10_to_85(d div 85));
end;
  exit('');
end;
function base85_to_10(digits: string): integer;
var
  base85_to_10_value: integer;
  base85_to_10_i: integer;
begin
  base85_to_10_value := 0;
  base85_to_10_i := 0;
  while base85_to_10_i < Length(digits) do begin
  base85_to_10_value := (base85_to_10_value * 85) + (ord(digits[base85_to_10_i+1]) - 33);
  base85_to_10_i := base85_to_10_i + 1;
end;
  exit(base85_to_10_value);
end;
function ascii85_encode(data: string): string;
var
  ascii85_encode_binary_data: string;
  ascii85_encode_ch: integer;
  ascii85_encode_null_values: integer;
  ascii85_encode_total_bits: integer;
  ascii85_encode_result_: string;
  ascii85_encode_i: integer;
  ascii85_encode_chunk_bits: string;
  ascii85_encode_chunk_val: integer;
  ascii85_encode_encoded: string;
begin
  ascii85_encode_binary_data := '';
  for ascii85_encode_ch in data do begin
  ascii85_encode_binary_data := ascii85_encode_binary_data + to_binary(ord(ascii85_encode_ch), 8);
end;
  ascii85_encode_null_values := ((32 * ((Length(ascii85_encode_binary_data) div 32) + 1)) - Length(ascii85_encode_binary_data)) div 8;
  ascii85_encode_total_bits := 32 * ((Length(ascii85_encode_binary_data) div 32) + 1);
  while Length(ascii85_encode_binary_data) < ascii85_encode_total_bits do begin
  ascii85_encode_binary_data := ascii85_encode_binary_data + '0';
end;
  ascii85_encode_result_ := '';
  ascii85_encode_i := 0;
  while ascii85_encode_i < Length(ascii85_encode_binary_data) do begin
  ascii85_encode_chunk_bits := copy(ascii85_encode_binary_data, ascii85_encode_i+1, (ascii85_encode_i + 32 - (ascii85_encode_i)));
  ascii85_encode_chunk_val := bin_to_int(ascii85_encode_chunk_bits);
  ascii85_encode_encoded := reverse(base10_to_85(ascii85_encode_chunk_val));
  ascii85_encode_result_ := ascii85_encode_result_ + ascii85_encode_encoded;
  ascii85_encode_i := ascii85_encode_i + 32;
end;
  if (ascii85_encode_null_values mod 4) <> 0 then begin
  ascii85_encode_result_ := copy(ascii85_encode_result_, 0+1, (Length(ascii85_encode_result_) - ascii85_encode_null_values - (0)));
end;
  exit(ascii85_encode_result_);
end;
function ascii85_decode(data: string): string;
var
  ascii85_decode_null_values: integer;
  ascii85_decode_binary_data: string;
  ascii85_decode_i: integer;
  ascii85_decode_result_: string;
  ascii85_decode_chunk: string;
  ascii85_decode_value: integer;
  ascii85_decode_bits: string;
  ascii85_decode_j: integer;
  ascii85_decode_byte_bits: string;
  ascii85_decode_c: string;
  ascii85_decode_trim: integer;
begin
  ascii85_decode_null_values := (5 * ((Length(data) div 5) + 1)) - Length(data);
  ascii85_decode_binary_data := data;
  ascii85_decode_i := 0;
  while ascii85_decode_i < ascii85_decode_null_values do begin
  ascii85_decode_binary_data := ascii85_decode_binary_data + 'u';
  ascii85_decode_i := ascii85_decode_i + 1;
end;
  ascii85_decode_result_ := '';
  ascii85_decode_i := 0;
  while ascii85_decode_i < Length(ascii85_decode_binary_data) do begin
  ascii85_decode_chunk := copy(ascii85_decode_binary_data, ascii85_decode_i+1, (ascii85_decode_i + 5 - (ascii85_decode_i)));
  ascii85_decode_value := base85_to_10(ascii85_decode_chunk);
  ascii85_decode_bits := to_binary(ascii85_decode_value, 32);
  ascii85_decode_j := 0;
  while ascii85_decode_j < 32 do begin
  ascii85_decode_byte_bits := copy(ascii85_decode_bits, ascii85_decode_j+1, (ascii85_decode_j + 8 - (ascii85_decode_j)));
  ascii85_decode_c := chr(bin_to_int(ascii85_decode_byte_bits));
  ascii85_decode_result_ := ascii85_decode_result_ + ascii85_decode_c;
  ascii85_decode_j := ascii85_decode_j + 8;
end;
  ascii85_decode_i := ascii85_decode_i + 5;
end;
  ascii85_decode_trim := ascii85_decode_null_values;
  if (ascii85_decode_null_values mod 5) = 0 then begin
  ascii85_decode_trim := ascii85_decode_null_values - 1;
end;
  exit(copy(ascii85_decode_result_, 0+1, (Length(ascii85_decode_result_) - ascii85_decode_trim - (0))));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  ascii85_chars := '!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstu';
  writeln(ascii85_encode(''));
  writeln(ascii85_encode('12345'));
  writeln(ascii85_encode('base 85'));
  writeln(ascii85_decode(''));
  writeln(ascii85_decode('0etOA2#'));
  writeln(ascii85_decode('@UX=h+?24'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
