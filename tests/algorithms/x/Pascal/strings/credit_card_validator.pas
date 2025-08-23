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
function validate_initial_digits(validate_initial_digits_cc: string): boolean; forward;
function luhn_validation(luhn_validation_cc: string): boolean; forward;
function is_digit_string(is_digit_string_s: string): boolean; forward;
function validate_credit_card_number(validate_credit_card_number_cc: string): boolean; forward;
procedure main(); forward;
function validate_initial_digits(validate_initial_digits_cc: string): boolean;
begin
  exit((((((copy(validate_initial_digits_cc, 1, 2) = '34') or (copy(validate_initial_digits_cc, 1, 2) = '35')) or (copy(validate_initial_digits_cc, 1, 2) = '37')) or (copy(validate_initial_digits_cc, 1, 1) = '4')) or (copy(validate_initial_digits_cc, 1, 1) = '5')) or (copy(validate_initial_digits_cc, 1, 1) = '6'));
end;
function luhn_validation(luhn_validation_cc: string): boolean;
var
  luhn_validation_sum: integer;
  luhn_validation_double_digit: boolean;
  luhn_validation_i: integer;
  luhn_validation_n: integer;
begin
  luhn_validation_sum := 0;
  luhn_validation_double_digit := false;
  luhn_validation_i := Length(luhn_validation_cc) - 1;
  while luhn_validation_i >= 0 do begin
  luhn_validation_n := StrToInt(copy(luhn_validation_cc, luhn_validation_i+1, (luhn_validation_i + 1 - (luhn_validation_i))));
  if luhn_validation_double_digit then begin
  luhn_validation_n := luhn_validation_n * 2;
  if luhn_validation_n > 9 then begin
  luhn_validation_n := luhn_validation_n - 9;
end;
end;
  luhn_validation_sum := luhn_validation_sum + luhn_validation_n;
  luhn_validation_double_digit := not luhn_validation_double_digit;
  luhn_validation_i := luhn_validation_i - 1;
end;
  exit((luhn_validation_sum mod 10) = 0);
end;
function is_digit_string(is_digit_string_s: string): boolean;
var
  is_digit_string_i: integer;
  is_digit_string_c: string;
begin
  is_digit_string_i := 0;
  while is_digit_string_i < Length(is_digit_string_s) do begin
  is_digit_string_c := copy(is_digit_string_s, is_digit_string_i+1, (is_digit_string_i + 1 - (is_digit_string_i)));
  if (is_digit_string_c < '0') or (is_digit_string_c > '9') then begin
  exit(false);
end;
  is_digit_string_i := is_digit_string_i + 1;
end;
  exit(true);
end;
function validate_credit_card_number(validate_credit_card_number_cc: string): boolean;
var
  validate_credit_card_number_error_message: string;
begin
  validate_credit_card_number_error_message := validate_credit_card_number_cc + ' is an invalid credit card number because';
  if not is_digit_string(validate_credit_card_number_cc) then begin
  writeln(validate_credit_card_number_error_message + ' it has nonnumerical characters.');
  exit(false);
end;
  if not ((Length(validate_credit_card_number_cc) >= 13) and (Length(validate_credit_card_number_cc) <= 16)) then begin
  writeln(validate_credit_card_number_error_message + ' of its length.');
  exit(false);
end;
  if not validate_initial_digits(validate_credit_card_number_cc) then begin
  writeln(validate_credit_card_number_error_message + ' of its first two digits.');
  exit(false);
end;
  if not luhn_validation(validate_credit_card_number_cc) then begin
  writeln(validate_credit_card_number_error_message + ' it fails the Luhn check.');
  exit(false);
end;
  writeln(validate_credit_card_number_cc + ' is a valid credit card number.');
  exit(true);
end;
procedure main();
begin
  validate_credit_card_number('4111111111111111');
  validate_credit_card_number('32323');
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
