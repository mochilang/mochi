{$mode objfpc}{$modeswitch nestedprocvars}
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
  digits: string;
  i: integer;
  digit_pos_to_extract: integer;
  exponent: integer;
  digit_position: integer;
  precision: integer;
  modulus: integer;
  x: real;
  n: integer;
  denominator_addend: integer;
  base: real;
function mod_pow(base: integer; exponent: integer; modulus: integer): integer; forward;
function pow_float(base: real; exponent: integer): real; forward;
function hex_digit(n: integer): string; forward;
function floor_float(x: real): real; forward;
function subsum(digit_pos_to_extract: integer; denominator_addend: integer; precision: integer): real; forward;
function bailey_borwein_plouffe(digit_position: integer; precision: integer): string; forward;
function mod_pow(base: integer; exponent: integer; modulus: integer): integer;
var
  mod_pow_result_: integer;
  mod_pow_b: integer;
  mod_pow_e: integer;
begin
  mod_pow_result_ := 1;
  mod_pow_b := base mod modulus;
  mod_pow_e := exponent;
  while mod_pow_e > 0 do begin
  if (mod_pow_e mod 2) = 1 then begin
  mod_pow_result_ := (mod_pow_result_ * mod_pow_b) mod modulus;
end;
  mod_pow_b := (mod_pow_b * mod_pow_b) mod modulus;
  mod_pow_e := mod_pow_e div 2;
end;
  exit(mod_pow_result_);
end;
function pow_float(base: real; exponent: integer): real;
var
  pow_float_exp_: integer;
  pow_float_result_: real;
  pow_float_i: integer;
begin
  pow_float_exp_ := exponent;
  pow_float_result_ := 1;
  if pow_float_exp_ < 0 then begin
  pow_float_exp_ := -pow_float_exp_;
end;
  pow_float_i := 0;
  while pow_float_i < pow_float_exp_ do begin
  pow_float_result_ := pow_float_result_ * base;
  pow_float_i := pow_float_i + 1;
end;
  if exponent < 0 then begin
  pow_float_result_ := 1 / pow_float_result_;
end;
  exit(pow_float_result_);
end;
function hex_digit(n: integer): string;
var
  hex_digit_letters: array of string;
begin
  if n < 10 then begin
  exit(IntToStr(n));
end;
  hex_digit_letters := ['a', 'b', 'c', 'd', 'e', 'f'];
  exit(hex_digit_letters[n - 10]);
end;
function floor_float(x: real): real;
var
  floor_float_i: integer;
begin
  floor_float_i := Trunc(x);
  if Double(floor_float_i) > x then begin
  floor_float_i := floor_float_i - 1;
end;
  exit(Double(floor_float_i));
end;
function subsum(digit_pos_to_extract: integer; denominator_addend: integer; precision: integer): real;
var
  subsum_total: real;
  subsum_sum_index: integer;
  subsum_denominator: integer;
  subsum_exponent: integer;
  subsum_exponential_term: integer;
  subsum_exponent_17: integer;
  subsum_exponential_term_18: real;
begin
  subsum_total := 0;
  subsum_sum_index := 0;
  while subsum_sum_index < (digit_pos_to_extract + precision) do begin
  subsum_denominator := (8 * subsum_sum_index) + denominator_addend;
  if subsum_sum_index < digit_pos_to_extract then begin
  subsum_exponent := (digit_pos_to_extract - 1) - subsum_sum_index;
  subsum_exponential_term := mod_pow(16, subsum_exponent, subsum_denominator);
  subsum_total := subsum_total + (Double(subsum_exponential_term) / Double(subsum_denominator));
end else begin
  subsum_exponent_17 := (digit_pos_to_extract - 1) - subsum_sum_index;
  subsum_exponential_term_18 := pow_float(16, subsum_exponent_17);
  subsum_total := subsum_total + (subsum_exponential_term_18 / Double(subsum_denominator));
end;
  subsum_sum_index := subsum_sum_index + 1;
end;
  exit(subsum_total);
end;
function bailey_borwein_plouffe(digit_position: integer; precision: integer): string;
var
  bailey_borwein_plouffe_sum_result: real;
  bailey_borwein_plouffe_fraction: real;
  bailey_borwein_plouffe_digit: integer;
  bailey_borwein_plouffe_hd: string;
begin
  if digit_position <= 0 then begin
  panic('Digit position must be a positive integer');
end;
  if precision < 0 then begin
  panic('Precision must be a nonnegative integer');
end;
  bailey_borwein_plouffe_sum_result := (((4 * subsum(digit_position, 1, precision)) - (2 * subsum(digit_position, 4, precision))) - (1 * subsum(digit_position, 5, precision))) - (1 * subsum(digit_position, 6, precision));
  bailey_borwein_plouffe_fraction := bailey_borwein_plouffe_sum_result - floor_float(bailey_borwein_plouffe_sum_result);
  bailey_borwein_plouffe_digit := Trunc(bailey_borwein_plouffe_fraction * 16);
  bailey_borwein_plouffe_hd := hex_digit(bailey_borwein_plouffe_digit);
  exit(bailey_borwein_plouffe_hd);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  digits := '';
  i := 1;
  while i <= 10 do begin
  digits := digits + bailey_borwein_plouffe(i, 1000);
  i := i + 1;
end;
  writeln(digits);
  writeln(bailey_borwein_plouffe(5, 10000));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
