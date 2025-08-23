{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type IntArray = array of int64;
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
procedure show_list_int64(xs: array of int64);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  res: array of int64;
  x: int64;
function has_alpha(has_alpha_s: string): boolean; forward;
function parse_decimal(parse_decimal_s: string): int64; forward;
function get_barcode(get_barcode_barcode: string): int64; forward;
function get_check_digit(get_check_digit_barcode: int64): int64; forward;
function is_valid(is_valid_barcode: int64): boolean; forward;
function has_alpha(has_alpha_s: string): boolean;
var
  has_alpha_i: int64;
  has_alpha_c: string;
begin
  has_alpha_i := 0;
  while has_alpha_i < Length(has_alpha_s) do begin
  has_alpha_c := has_alpha_s[has_alpha_i+1];
  if ((has_alpha_c >= 'a') and (has_alpha_c <= 'z')) or ((has_alpha_c >= 'A') and (has_alpha_c <= 'Z')) then begin
  exit(true);
end;
  has_alpha_i := has_alpha_i + 1;
end;
  exit(false);
end;
function parse_decimal(parse_decimal_s: string): int64;
var
  parse_decimal_value: int64;
  parse_decimal_i: int64;
  parse_decimal_c: string;
begin
  parse_decimal_value := 0;
  parse_decimal_i := 0;
  while parse_decimal_i < Length(parse_decimal_s) do begin
  parse_decimal_c := parse_decimal_s[parse_decimal_i+1];
  if (parse_decimal_c < '0') or (parse_decimal_c > '9') then begin
  panic('Non-digit character encountered');
end;
  parse_decimal_value := (parse_decimal_value * 10) + StrToInt(parse_decimal_c);
  parse_decimal_i := parse_decimal_i + 1;
end;
  exit(parse_decimal_value);
end;
function get_barcode(get_barcode_barcode: string): int64;
begin
  if has_alpha(get_barcode_barcode) then begin
  panic(('Barcode ''' + get_barcode_barcode) + ''' has alphabetic characters.');
end;
  if (Length(get_barcode_barcode) > 0) and (get_barcode_barcode[0+1] = '-') then begin
  panic('The entered barcode has a negative value. Try again.');
end;
  exit(parse_decimal(get_barcode_barcode));
end;
function get_check_digit(get_check_digit_barcode: int64): int64;
var
  get_check_digit_num: int64;
  get_check_digit_s: int64;
  get_check_digit_position: int64;
  get_check_digit_mult: int64;
begin
  get_check_digit_num := get_check_digit_barcode div 10;
  get_check_digit_s := 0;
  get_check_digit_position := 0;
  while get_check_digit_num <> 0 do begin
  if (get_check_digit_position mod 2) = 0 then begin
  get_check_digit_mult := 3;
end else begin
  get_check_digit_mult := 1;
end;
  get_check_digit_s := get_check_digit_s + (get_check_digit_mult * (get_check_digit_num mod 10));
  get_check_digit_num := get_check_digit_num div 10;
  get_check_digit_position := get_check_digit_position + 1;
end;
  exit((10 - (get_check_digit_s mod 10)) mod 10);
end;
function is_valid(is_valid_barcode: int64): boolean;
begin
  exit((Length(IntToStr(is_valid_barcode)) = 13) and (get_check_digit(is_valid_barcode) = (is_valid_barcode mod 10)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(get_check_digit(8718452538119)));
  writeln(IntToStr(get_check_digit(87184523)));
  writeln(IntToStr(get_check_digit(87193425381086)));
  res := [];
  x := 0;
  while x < 100 do begin
  res := concat(res, IntArray([get_check_digit(x)]));
  x := x + 10;
end;
  show_list_int64(res);
  writeln(LowerCase(BoolToStr(is_valid(8718452538119), true)));
  writeln(LowerCase(BoolToStr(is_valid(87184525), true)));
  writeln(LowerCase(BoolToStr(is_valid(87193425381089), true)));
  writeln(LowerCase(BoolToStr(is_valid(0), true)));
  writeln(IntToStr(get_barcode('8718452538119')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
