{$mode objfpc}
program Main;
uses SysUtils;
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
  B64_CHARSET: string;
  n: integer;
  width: integer;
  ch: string;
  c: string;
  s: string;
  data: IntArray;
  times: integer;
function to_binary(n: integer): string; forward;
function zfill(s: string; width: integer): string; forward;
function from_binary(s: string): integer; forward;
function repeat_(ch: string; times: integer): string; forward;
function char_index(s: string; c: string): integer; forward;
function base64_encode(data: IntArray): string; forward;
function base64_decode(s: string): IntArray; forward;
procedure main(); forward;
function to_binary(n: integer): string;
var
  to_binary_num: integer;
  to_binary_res: string;
  to_binary_bit: integer;
begin
  if n = 0 then begin
  exit('0');
end;
  to_binary_num := n;
  to_binary_res := '';
  while to_binary_num > 0 do begin
  to_binary_bit := to_binary_num mod 2;
  to_binary_res := IntToStr(to_binary_bit) + to_binary_res;
  to_binary_num := to_binary_num div 2;
end;
  exit(to_binary_res);
end;
function zfill(s: string; width: integer): string;
var
  zfill_res: string;
  zfill_pad: integer;
begin
  zfill_res := s;
  zfill_pad := width - Length(s);
  while zfill_pad > 0 do begin
  zfill_res := '0' + zfill_res;
  zfill_pad := zfill_pad - 1;
end;
  exit(zfill_res);
end;
function from_binary(s: string): integer;
var
  from_binary_i: integer;
  from_binary_result_: integer;
begin
  from_binary_i := 0;
  from_binary_result_ := 0;
  while from_binary_i < Length(s) do begin
  from_binary_result_ := from_binary_result_ * 2;
  if copy(s, from_binary_i+1, (from_binary_i + 1 - (from_binary_i))) = '1' then begin
  from_binary_result_ := from_binary_result_ + 1;
end;
  from_binary_i := from_binary_i + 1;
end;
  exit(from_binary_result_);
end;
function repeat_(ch: string; times: integer): string;
var
  repeat__res: string;
  repeat__i: integer;
begin
  repeat__res := '';
  repeat__i := 0;
  while repeat__i < times do begin
  repeat__res := repeat__res + ch;
  repeat__i := repeat__i + 1;
end;
  exit(repeat__res);
end;
function char_index(s: string; c: string): integer;
var
  char_index_i: integer;
begin
  char_index_i := 0;
  while char_index_i < Length(s) do begin
  if copy(s, char_index_i+1, (char_index_i + 1 - (char_index_i))) = c then begin
  exit(char_index_i);
end;
  char_index_i := char_index_i + 1;
end;
  exit(-1);
end;
function base64_encode(data: IntArray): string;
var
  base64_encode_bits: string;
  base64_encode_i: integer;
  base64_encode_pad_bits: integer;
  base64_encode_j: integer;
  base64_encode_encoded: string;
  base64_encode_chunk: string;
  base64_encode_idx: integer;
  base64_encode_pad: integer;
begin
  base64_encode_bits := '';
  base64_encode_i := 0;
  while base64_encode_i < Length(data) do begin
  base64_encode_bits := base64_encode_bits + zfill(to_binary(data[base64_encode_i]), 8);
  base64_encode_i := base64_encode_i + 1;
end;
  base64_encode_pad_bits := 0;
  if (Length(base64_encode_bits) mod 6) <> 0 then begin
  base64_encode_pad_bits := 6 - (Length(base64_encode_bits) mod 6);
  base64_encode_bits := base64_encode_bits + repeat_('0', base64_encode_pad_bits);
end;
  base64_encode_j := 0;
  base64_encode_encoded := '';
  while base64_encode_j < Length(base64_encode_bits) do begin
  base64_encode_chunk := copy(base64_encode_bits, base64_encode_j+1, (base64_encode_j + 6 - (base64_encode_j)));
  base64_encode_idx := from_binary(base64_encode_chunk);
  base64_encode_encoded := base64_encode_encoded + copy(B64_CHARSET, base64_encode_idx+1, (base64_encode_idx + 1 - (base64_encode_idx)));
  base64_encode_j := base64_encode_j + 6;
end;
  base64_encode_pad := base64_encode_pad_bits div 2;
  while base64_encode_pad > 0 do begin
  base64_encode_encoded := base64_encode_encoded + '=';
  base64_encode_pad := base64_encode_pad - 1;
end;
  exit(base64_encode_encoded);
end;
function base64_decode(s: string): IntArray;
var
  base64_decode_padding: integer;
  base64_decode_end: integer;
  base64_decode_bits: string;
  base64_decode_k: integer;
  base64_decode_c: string;
  base64_decode_idx: integer;
  base64_decode_bytes: array of integer;
  base64_decode_m: integer;
  base64_decode_byte: integer;
begin
  base64_decode_padding := 0;
  base64_decode_end := Length(s);
  while (base64_decode_end > 0) and (copy(s, base64_decode_end - 1+1, (base64_decode_end - (base64_decode_end - 1))) = '=') do begin
  base64_decode_padding := base64_decode_padding + 1;
  base64_decode_end := base64_decode_end - 1;
end;
  base64_decode_bits := '';
  base64_decode_k := 0;
  while base64_decode_k < base64_decode_end do begin
  base64_decode_c := copy(s, base64_decode_k+1, (base64_decode_k + 1 - (base64_decode_k)));
  base64_decode_idx := char_index(B64_CHARSET, base64_decode_c);
  base64_decode_bits := base64_decode_bits + zfill(to_binary(base64_decode_idx), 6);
  base64_decode_k := base64_decode_k + 1;
end;
  if base64_decode_padding > 0 then begin
  base64_decode_bits := copy(base64_decode_bits, 0+1, (Length(base64_decode_bits) - (base64_decode_padding * 2) - (0)));
end;
  base64_decode_bytes := [];
  base64_decode_m := 0;
  while base64_decode_m < Length(base64_decode_bits) do begin
  base64_decode_byte := from_binary(copy(base64_decode_bits, base64_decode_m+1, (base64_decode_m + 8 - (base64_decode_m))));
  base64_decode_bytes := concat(base64_decode_bytes, IntArray([base64_decode_byte]));
  base64_decode_m := base64_decode_m + 8;
end;
  exit(base64_decode_bytes);
end;
procedure main();
var
  main_data: array of integer;
  main_encoded: string;
begin
  main_data := [77, 111, 99, 104, 105];
  main_encoded := base64_encode(main_data);
  writeln(main_encoded);
  json(base64_decode(main_encoded));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  B64_CHARSET := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
