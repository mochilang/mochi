{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
  p4_table: array of integer;
  key: string;
  message: string;
  p8_table: array of integer;
  p10_table: array of integer;
  IP: array of integer;
  IP_inv: array of integer;
  expansion: array of integer;
  s0: array of IntArray;
  s1: array of IntArray;
  temp: string;
  left: string;
  right: string;
  key1: string;
  key2: string;
  CT: string;
  PT: string;
  data: string;
  a: string;
  n: integer;
  b: string;
  inp: string;
  table: IntArray;
  s: IntArrayArray;
  width: integer;
function apply_table(inp: string; table: IntArray): string; forward;
function left_shift(data: string): string; forward;
function xor_(a: string; b: string): string; forward;
function int_to_binary(n: integer): string; forward;
function pad_left(s: string; width: integer): string; forward;
function bin_to_int(s: string): integer; forward;
function apply_sbox(s: IntArrayArray; data: string): string; forward;
function f(expansion: IntArray; s0: IntArrayArray; s1: IntArrayArray; key: string; message: string): string; forward;
function apply_table(inp: string; table: IntArray): string;
var
  apply_table_res: string;
  apply_table_i: integer;
  apply_table_idx: integer;
begin
  apply_table_res := '';
  apply_table_i := 0;
  while apply_table_i < Length(table) do begin
  apply_table_idx := table[apply_table_i] - 1;
  if apply_table_idx < 0 then begin
  apply_table_idx := Length(inp) - 1;
end;
  apply_table_res := apply_table_res + copy(inp, apply_table_idx+1, (apply_table_idx + 1 - (apply_table_idx)));
  apply_table_i := apply_table_i + 1;
end;
  exit(apply_table_res);
end;
function left_shift(data: string): string;
begin
  exit(copy(data, 2, (Length(data) - (1))) + copy(data, 1, 1));
end;
function xor_(a: string; b: string): string;
var
  xor__res: string;
  xor__i: integer;
begin
  xor__res := '';
  xor__i := 0;
  while (xor__i < Length(a)) and (xor__i < Length(b)) do begin
  if copy(a, xor__i+1, (xor__i + 1 - (xor__i))) = copy(b, xor__i+1, (xor__i + 1 - (xor__i))) then begin
  xor__res := xor__res + '0';
end else begin
  xor__res := xor__res + '1';
end;
  xor__i := xor__i + 1;
end;
  exit(xor__res);
end;
function int_to_binary(n: integer): string;
var
  int_to_binary_res: string;
  int_to_binary_num: integer;
begin
  if n = 0 then begin
  exit('0');
end;
  int_to_binary_res := '';
  int_to_binary_num := n;
  while int_to_binary_num > 0 do begin
  int_to_binary_res := IntToStr(int_to_binary_num mod 2) + int_to_binary_res;
  int_to_binary_num := int_to_binary_num div 2;
end;
  exit(int_to_binary_res);
end;
function pad_left(s: string; width: integer): string;
var
  pad_left_res: string;
begin
  pad_left_res := s;
  while Length(pad_left_res) < width do begin
  pad_left_res := '0' + pad_left_res;
end;
  exit(pad_left_res);
end;
function bin_to_int(s: string): integer;
var
  bin_to_int_result_: integer;
  bin_to_int_i: integer;
  bin_to_int_digit: integer;
begin
  bin_to_int_result_ := 0;
  bin_to_int_i := 0;
  while bin_to_int_i < Length(s) do begin
  bin_to_int_digit := StrToInt(copy(s, bin_to_int_i+1, (bin_to_int_i + 1 - (bin_to_int_i))));
  bin_to_int_result_ := (bin_to_int_result_ * 2) + bin_to_int_digit;
  bin_to_int_i := bin_to_int_i + 1;
end;
  exit(bin_to_int_result_);
end;
function apply_sbox(s: IntArrayArray; data: string): string;
var
  apply_sbox_row_bits: string;
  apply_sbox_col_bits: string;
  apply_sbox_row: integer;
  apply_sbox_col: integer;
  apply_sbox_val: integer;
  apply_sbox_out_: string;
begin
  apply_sbox_row_bits := copy(data, 1, 1) + copy(data, Length(data) - 1+1, (Length(data) - (Length(data) - 1)));
  apply_sbox_col_bits := copy(data, 2, 2);
  apply_sbox_row := bin_to_int(apply_sbox_row_bits);
  apply_sbox_col := bin_to_int(apply_sbox_col_bits);
  apply_sbox_val := s[apply_sbox_row][apply_sbox_col];
  apply_sbox_out_ := int_to_binary(apply_sbox_val);
  exit(apply_sbox_out_);
end;
function f(expansion: IntArray; s0: IntArrayArray; s1: IntArrayArray; key: string; message: string): string;
var
  f_left: string;
  f_right: string;
  f_temp: string;
  f_left_bin_str: string;
  f_right_bin_str: string;
begin
  f_left := copy(message, 1, 4);
  f_right := copy(message, 5, 4);
  f_temp := apply_table(f_right, expansion);
  f_temp := xor_(f_temp, key);
  f_left_bin_str := apply_sbox(s0, copy(f_temp, 1, 4));
  f_right_bin_str := apply_sbox(s1, copy(f_temp, 5, 4));
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
end.
