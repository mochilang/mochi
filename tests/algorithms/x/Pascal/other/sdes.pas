{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type IntArrayArray = array of IntArray;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  p4_table: array of int64;
  key: string;
  message: string;
  p8_table: array of int64;
  p10_table: array of int64;
  IP: array of int64;
  IP_inv: array of int64;
  expansion: array of int64;
  s0: array of IntArray;
  s1: array of IntArray;
  temp: string;
  left: string;
  right: string;
  key1: string;
  key2: string;
  CT: string;
  PT: string;
function apply_table(apply_table_inp: string; apply_table_table: IntArray): string; forward;
function left_shift(left_shift_data: string): string; forward;
function xor_(xor__a: string; xor__b: string): string; forward;
function int_to_binary(int_to_binary_n: int64): string; forward;
function pad_left(pad_left_s: string; pad_left_width: int64): string; forward;
function bin_to_int(bin_to_int_s: string): int64; forward;
function apply_sbox(apply_sbox_s: IntArrayArray; apply_sbox_data: string): string; forward;
function f(f_expansion: IntArray; f_s0: IntArrayArray; f_s1: IntArrayArray; f_key: string; f_message: string): string; forward;
function apply_table(apply_table_inp: string; apply_table_table: IntArray): string;
var
  apply_table_res: string;
  apply_table_i: int64;
  apply_table_idx: int64;
begin
  apply_table_res := '';
  apply_table_i := 0;
  while apply_table_i < Length(apply_table_table) do begin
  apply_table_idx := apply_table_table[apply_table_i] - 1;
  if apply_table_idx < 0 then begin
  apply_table_idx := Length(apply_table_inp) - 1;
end;
  apply_table_res := apply_table_res + copy(apply_table_inp, apply_table_idx+1, (apply_table_idx + 1 - (apply_table_idx)));
  apply_table_i := apply_table_i + 1;
end;
  exit(apply_table_res);
end;
function left_shift(left_shift_data: string): string;
begin
  exit(copy(left_shift_data, 2, (Length(left_shift_data) - (1))) + copy(left_shift_data, 1, 1));
end;
function xor_(xor__a: string; xor__b: string): string;
var
  xor__res: string;
  xor__i: int64;
begin
  xor__res := '';
  xor__i := 0;
  while (xor__i < Length(xor__a)) and (xor__i < Length(xor__b)) do begin
  if copy(xor__a, xor__i+1, (xor__i + 1 - (xor__i))) = copy(xor__b, xor__i+1, (xor__i + 1 - (xor__i))) then begin
  xor__res := xor__res + '0';
end else begin
  xor__res := xor__res + '1';
end;
  xor__i := xor__i + 1;
end;
  exit(xor__res);
end;
function int_to_binary(int_to_binary_n: int64): string;
var
  int_to_binary_res: string;
  int_to_binary_num: int64;
begin
  if int_to_binary_n = 0 then begin
  exit('0');
end;
  int_to_binary_res := '';
  int_to_binary_num := int_to_binary_n;
  while int_to_binary_num > 0 do begin
  int_to_binary_res := IntToStr(int_to_binary_num mod 2) + int_to_binary_res;
  int_to_binary_num := _floordiv(int_to_binary_num, 2);
end;
  exit(int_to_binary_res);
end;
function pad_left(pad_left_s: string; pad_left_width: int64): string;
var
  pad_left_res: string;
begin
  pad_left_res := pad_left_s;
  while Length(pad_left_res) < pad_left_width do begin
  pad_left_res := '0' + pad_left_res;
end;
  exit(pad_left_res);
end;
function bin_to_int(bin_to_int_s: string): int64;
var
  bin_to_int_result_: int64;
  bin_to_int_i: int64;
  bin_to_int_digit: integer;
begin
  bin_to_int_result_ := 0;
  bin_to_int_i := 0;
  while bin_to_int_i < Length(bin_to_int_s) do begin
  bin_to_int_digit := StrToInt(copy(bin_to_int_s, bin_to_int_i+1, (bin_to_int_i + 1 - (bin_to_int_i))));
  bin_to_int_result_ := (bin_to_int_result_ * 2) + bin_to_int_digit;
  bin_to_int_i := bin_to_int_i + 1;
end;
  exit(bin_to_int_result_);
end;
function apply_sbox(apply_sbox_s: IntArrayArray; apply_sbox_data: string): string;
var
  apply_sbox_row_bits: string;
  apply_sbox_col_bits: string;
  apply_sbox_row: int64;
  apply_sbox_col: int64;
  apply_sbox_val: int64;
  apply_sbox_out_: string;
begin
  apply_sbox_row_bits := copy(apply_sbox_data, 1, 1) + copy(apply_sbox_data, Length(apply_sbox_data) - 1+1, (Length(apply_sbox_data) - (Length(apply_sbox_data) - 1)));
  apply_sbox_col_bits := copy(apply_sbox_data, 2, 2);
  apply_sbox_row := bin_to_int(apply_sbox_row_bits);
  apply_sbox_col := bin_to_int(apply_sbox_col_bits);
  apply_sbox_val := apply_sbox_s[apply_sbox_row][apply_sbox_col];
  apply_sbox_out_ := int_to_binary(apply_sbox_val);
  exit(apply_sbox_out_);
end;
function f(f_expansion: IntArray; f_s0: IntArrayArray; f_s1: IntArrayArray; f_key: string; f_message: string): string;
var
  f_left: string;
  f_right: string;
  f_temp: string;
  f_left_bin_str: string;
  f_right_bin_str: string;
begin
  f_left := copy(f_message, 1, 4);
  f_right := copy(f_message, 5, 4);
  f_temp := apply_table(f_right, f_expansion);
  f_temp := xor_(f_temp, f_key);
  f_left_bin_str := apply_sbox(f_s0, copy(f_temp, 1, 4));
  f_right_bin_str := apply_sbox(f_s1, copy(f_temp, 5, 4));
  f_left_bin_str := pad_left(f_left_bin_str, 2);
  f_right_bin_str := pad_left(f_right_bin_str, 2);
  f_temp := apply_table(f_left_bin_str + f_right_bin_str, p4_table);
  f_temp := xor_(f_left, f_temp);
  exit(f_temp + f_right);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  p4_table := [2, 4, 3, 1];
  key := '1010000010';
  message := '11010111';
  p8_table := [6, 3, 7, 4, 8, 5, 10, 9];
  p10_table := [3, 5, 2, 7, 4, 10, 1, 9, 8, 6];
  IP := [2, 6, 3, 1, 4, 8, 5, 7];
  IP_inv := [4, 1, 3, 5, 7, 2, 8, 6];
  expansion := [4, 1, 2, 3, 2, 3, 4, 1];
  s0 := [[1, 0, 3, 2], [3, 2, 1, 0], [0, 2, 1, 3], [3, 1, 3, 2]];
  s1 := [[0, 1, 2, 3], [2, 0, 1, 3], [3, 0, 1, 0], [2, 1, 0, 3]];
  temp := apply_table(key, p10_table);
  left := copy(temp, 1, 5);
  right := copy(temp, 6, 5);
  left := left_shift(left);
  right := left_shift(right);
  key1 := apply_table(left + right, p8_table);
  left := left_shift(left);
  right := left_shift(right);
  left := left_shift(left);
  right := left_shift(right);
  key2 := apply_table(left + right, p8_table);
  temp := apply_table(message, IP);
  temp := f(expansion, s0, s1, key1, temp);
  temp := copy(temp, 5, 4) + copy(temp, 1, 4);
  temp := f(expansion, s0, s1, key2, temp);
  CT := apply_table(temp, IP_inv);
  writeln('Cipher text is: ' + CT);
  temp := apply_table(CT, IP);
  temp := f(expansion, s0, s1, key2, temp);
  temp := copy(temp, 5, 4) + copy(temp, 1, 4);
  temp := f(expansion, s0, s1, key1, temp);
  PT := apply_table(temp, IP_inv);
  writeln('Plain text after decypting is: ' + PT);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
