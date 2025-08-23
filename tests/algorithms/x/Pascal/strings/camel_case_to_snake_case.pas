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
  LOWER: string;
  UPPER: string;
  DIGITS: string;
function is_lower(is_lower_ch: string): boolean; forward;
function is_upper(is_upper_ch: string): boolean; forward;
function is_digit(is_digit_ch: string): boolean; forward;
function is_alpha(is_alpha_ch: string): boolean; forward;
function is_alnum(is_alnum_ch: string): boolean; forward;
function to_lower(to_lower_ch: string): string; forward;
function camel_to_snake_case(camel_to_snake_case_input_str: string): string; forward;
procedure main(); forward;
function is_lower(is_lower_ch: string): boolean;
var
  is_lower_i: integer;
begin
  is_lower_i := 0;
  while is_lower_i < Length(LOWER) do begin
  if LOWER[is_lower_i+1] = is_lower_ch then begin
  exit(true);
end;
  is_lower_i := is_lower_i + 1;
end;
  exit(false);
end;
function is_upper(is_upper_ch: string): boolean;
var
  is_upper_i: integer;
begin
  is_upper_i := 0;
  while is_upper_i < Length(UPPER) do begin
  if UPPER[is_upper_i+1] = is_upper_ch then begin
  exit(true);
end;
  is_upper_i := is_upper_i + 1;
end;
  exit(false);
end;
function is_digit(is_digit_ch: string): boolean;
var
  is_digit_i: integer;
begin
  is_digit_i := 0;
  while is_digit_i < Length(DIGITS) do begin
  if DIGITS[is_digit_i+1] = is_digit_ch then begin
  exit(true);
end;
  is_digit_i := is_digit_i + 1;
end;
  exit(false);
end;
function is_alpha(is_alpha_ch: string): boolean;
begin
  if is_lower(is_alpha_ch) then begin
  exit(true);
end;
  if is_upper(is_alpha_ch) then begin
  exit(true);
end;
  exit(false);
end;
function is_alnum(is_alnum_ch: string): boolean;
begin
  if is_alpha(is_alnum_ch) then begin
  exit(true);
end;
  if is_digit(is_alnum_ch) then begin
  exit(true);
end;
  exit(false);
end;
function to_lower(to_lower_ch: string): string;
var
  to_lower_i: integer;
begin
  to_lower_i := 0;
  while to_lower_i < Length(UPPER) do begin
  if UPPER[to_lower_i+1] = to_lower_ch then begin
  exit(LOWER[to_lower_i+1]);
end;
  to_lower_i := to_lower_i + 1;
end;
  exit(to_lower_ch);
end;
function camel_to_snake_case(camel_to_snake_case_input_str: string): string;
var
  camel_to_snake_case_snake_str: string;
  camel_to_snake_case_i: integer;
  camel_to_snake_case_prev_is_digit: boolean;
  camel_to_snake_case_prev_is_alpha: boolean;
  camel_to_snake_case_ch: string;
begin
  camel_to_snake_case_snake_str := '';
  camel_to_snake_case_i := 0;
  camel_to_snake_case_prev_is_digit := false;
  camel_to_snake_case_prev_is_alpha := false;
  while camel_to_snake_case_i < Length(camel_to_snake_case_input_str) do begin
  camel_to_snake_case_ch := camel_to_snake_case_input_str[camel_to_snake_case_i+1];
  if is_upper(camel_to_snake_case_ch) then begin
  camel_to_snake_case_snake_str := (camel_to_snake_case_snake_str + '_') + to_lower(camel_to_snake_case_ch);
end else begin
  if camel_to_snake_case_prev_is_digit and is_lower(camel_to_snake_case_ch) then begin
  camel_to_snake_case_snake_str := (camel_to_snake_case_snake_str + '_') + camel_to_snake_case_ch;
end else begin
  if camel_to_snake_case_prev_is_alpha and is_digit(camel_to_snake_case_ch) then begin
  camel_to_snake_case_snake_str := (camel_to_snake_case_snake_str + '_') + camel_to_snake_case_ch;
end else begin
  if not is_alnum(camel_to_snake_case_ch) then begin
  camel_to_snake_case_snake_str := camel_to_snake_case_snake_str + '_';
end else begin
  camel_to_snake_case_snake_str := camel_to_snake_case_snake_str + camel_to_snake_case_ch;
end;
end;
end;
end;
  camel_to_snake_case_prev_is_digit := is_digit(camel_to_snake_case_ch);
  camel_to_snake_case_prev_is_alpha := is_alpha(camel_to_snake_case_ch);
  camel_to_snake_case_i := camel_to_snake_case_i + 1;
end;
  if (Length(camel_to_snake_case_snake_str) > 0) and (camel_to_snake_case_snake_str[0+1] = '_') then begin
  camel_to_snake_case_snake_str := copy(camel_to_snake_case_snake_str, 2, (Length(camel_to_snake_case_snake_str) - (1)));
end;
  exit(camel_to_snake_case_snake_str);
end;
procedure main();
begin
  writeln(camel_to_snake_case('someRandomString'));
  writeln(camel_to_snake_case('SomeRandomStr#ng'));
  writeln(camel_to_snake_case('123SomeRandom123String123'));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  LOWER := 'abcdefghijklmnopqrstuvwxyz';
  UPPER := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  DIGITS := '0123456789';
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
