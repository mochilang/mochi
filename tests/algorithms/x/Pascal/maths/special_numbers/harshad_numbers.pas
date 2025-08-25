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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
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
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
procedure panic_(panic__msg: string); forward;
function char_to_value(char_to_value_c: string): int64; forward;
function int_to_base(int_to_base_number: int64; int_to_base_base: int64): string; forward;
function base_to_int(base_to_int_num_str: string; base_to_int_base: int64): int64; forward;
function sum_of_digits(sum_of_digits_num: int64; sum_of_digits_base: int64): string; forward;
function harshad_numbers_in_base(harshad_numbers_in_base_limit: int64; harshad_numbers_in_base_base: int64): StrArray; forward;
function is_harshad_number_in_base(is_harshad_number_in_base_num: int64; is_harshad_number_in_base_base: int64): boolean; forward;
procedure main(); forward;
procedure panic_(panic__msg: string);
begin
end;
function char_to_value(char_to_value_c: string): int64;
var
  char_to_value_digits: string;
  char_to_value_i: int64;
begin
  char_to_value_digits := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  char_to_value_i := 0;
  while char_to_value_i < Length(char_to_value_digits) do begin
  if char_to_value_digits[char_to_value_i+1] = char_to_value_c then begin
  exit(char_to_value_i);
end;
  char_to_value_i := char_to_value_i + 1;
end;
  panic_('invalid digit');
end;
function int_to_base(int_to_base_number: int64; int_to_base_base: int64): string;
var
  int_to_base_digits: string;
  int_to_base_n: int64;
  int_to_base_result_: string;
  int_to_base_remainder: int64;
begin
  if (int_to_base_base < 2) or (int_to_base_base > 36) then begin
  panic_('''base'' must be between 2 and 36 inclusive');
end;
  if int_to_base_number < 0 then begin
  panic_('number must be a positive integer');
end;
  int_to_base_digits := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  int_to_base_n := int_to_base_number;
  int_to_base_result_ := '';
  while int_to_base_n > 0 do begin
  int_to_base_remainder := int_to_base_n mod int_to_base_base;
  int_to_base_result_ := int_to_base_digits[int_to_base_remainder+1] + int_to_base_result_;
  int_to_base_n := _floordiv(int_to_base_n, int_to_base_base);
end;
  if int_to_base_result_ = '' then begin
  int_to_base_result_ := '0';
end;
  exit(int_to_base_result_);
end;
function base_to_int(base_to_int_num_str: string; base_to_int_base: int64): int64;
var
  base_to_int_value: int64;
  base_to_int_i: int64;
  base_to_int_c: string;
begin
  base_to_int_value := 0;
  base_to_int_i := 0;
  while base_to_int_i < Length(base_to_int_num_str) do begin
  base_to_int_c := base_to_int_num_str[base_to_int_i+1];
  base_to_int_value := (base_to_int_value * base_to_int_base) + char_to_value(base_to_int_c);
  base_to_int_i := base_to_int_i + 1;
end;
  exit(base_to_int_value);
end;
function sum_of_digits(sum_of_digits_num: int64; sum_of_digits_base: int64): string;
var
  sum_of_digits_num_str: string;
  sum_of_digits_total: int64;
  sum_of_digits_i: int64;
  sum_of_digits_c: string;
begin
  if (sum_of_digits_base < 2) or (sum_of_digits_base > 36) then begin
  panic_('''base'' must be between 2 and 36 inclusive');
end;
  sum_of_digits_num_str := int_to_base(sum_of_digits_num, sum_of_digits_base);
  sum_of_digits_total := 0;
  sum_of_digits_i := 0;
  while sum_of_digits_i < Length(sum_of_digits_num_str) do begin
  sum_of_digits_c := sum_of_digits_num_str[sum_of_digits_i+1];
  sum_of_digits_total := sum_of_digits_total + char_to_value(sum_of_digits_c);
  sum_of_digits_i := sum_of_digits_i + 1;
end;
  exit(int_to_base(sum_of_digits_total, sum_of_digits_base));
end;
function harshad_numbers_in_base(harshad_numbers_in_base_limit: int64; harshad_numbers_in_base_base: int64): StrArray;
var
  harshad_numbers_in_base_numbers: array of string;
  harshad_numbers_in_base_i: int64;
  harshad_numbers_in_base_s: string;
  harshad_numbers_in_base_divisor: int64;
begin
  if (harshad_numbers_in_base_base < 2) or (harshad_numbers_in_base_base > 36) then begin
  panic_('''base'' must be between 2 and 36 inclusive');
end;
  if harshad_numbers_in_base_limit < 0 then begin
  exit([]);
end;
  harshad_numbers_in_base_numbers := [];
  harshad_numbers_in_base_i := 1;
  while harshad_numbers_in_base_i < harshad_numbers_in_base_limit do begin
  harshad_numbers_in_base_s := sum_of_digits(harshad_numbers_in_base_i, harshad_numbers_in_base_base);
  harshad_numbers_in_base_divisor := base_to_int(harshad_numbers_in_base_s, harshad_numbers_in_base_base);
  if (harshad_numbers_in_base_i mod harshad_numbers_in_base_divisor) = 0 then begin
  harshad_numbers_in_base_numbers := concat(harshad_numbers_in_base_numbers, StrArray([int_to_base(harshad_numbers_in_base_i, harshad_numbers_in_base_base)]));
end;
  harshad_numbers_in_base_i := harshad_numbers_in_base_i + 1;
end;
  exit(harshad_numbers_in_base_numbers);
end;
function is_harshad_number_in_base(is_harshad_number_in_base_num: int64; is_harshad_number_in_base_base: int64): boolean;
var
  is_harshad_number_in_base_n: string;
  is_harshad_number_in_base_d: string;
  is_harshad_number_in_base_n_val: int64;
  is_harshad_number_in_base_d_val: int64;
begin
  if (is_harshad_number_in_base_base < 2) or (is_harshad_number_in_base_base > 36) then begin
  panic_('''base'' must be between 2 and 36 inclusive');
end;
  if is_harshad_number_in_base_num < 0 then begin
  exit(false);
end;
  is_harshad_number_in_base_n := int_to_base(is_harshad_number_in_base_num, is_harshad_number_in_base_base);
  is_harshad_number_in_base_d := sum_of_digits(is_harshad_number_in_base_num, is_harshad_number_in_base_base);
  is_harshad_number_in_base_n_val := base_to_int(is_harshad_number_in_base_n, is_harshad_number_in_base_base);
  is_harshad_number_in_base_d_val := base_to_int(is_harshad_number_in_base_d, is_harshad_number_in_base_base);
  exit((is_harshad_number_in_base_n_val mod is_harshad_number_in_base_d_val) = 0);
end;
procedure main();
begin
  writeln(int_to_base(0, 21));
  writeln(int_to_base(23, 2));
  writeln(int_to_base(58, 5));
  writeln(int_to_base(167, 16));
  writeln(sum_of_digits(103, 12));
  writeln(sum_of_digits(1275, 4));
  writeln(sum_of_digits(6645, 2));
  writeln(list_to_str(harshad_numbers_in_base(15, 2)));
  writeln(list_to_str(harshad_numbers_in_base(12, 34)));
  writeln(list_to_str(harshad_numbers_in_base(12, 4)));
  writeln(Ord(is_harshad_number_in_base(18, 10)));
  writeln(Ord(is_harshad_number_in_base(21, 10)));
  writeln(Ord(is_harshad_number_in_base(-21, 5)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
