{$mode objfpc}
program Main;
uses SysUtils;
type Fraction = record
  numerator: integer;
  denominator: integer;
end;
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
  num: integer;
  fr: Fraction;
  n: integer;
  s: string;
  x: real;
  name: string;
  den: integer;
  b: integer;
  a: integer;
function makeFraction(numerator: integer; denominator: integer): Fraction; forward;
function pow10(n: integer): integer; forward;
function gcd(a: integer; b: integer): integer; forward;
function parse_decimal(s: string): Fraction; forward;
function reduce(fr: Fraction): Fraction; forward;
function decimal_to_fraction_str(s: string): Fraction; forward;
function decimal_to_fraction(x: real): Fraction; forward;
procedure assert_fraction(name: string; fr: Fraction; num: integer; den: integer); forward;
procedure test_decimal_to_fraction(); forward;
procedure main(); forward;
function makeFraction(numerator: integer; denominator: integer): Fraction;
begin
  Result.numerator := numerator;
  Result.denominator := denominator;
end;
function pow10(n: integer): integer;
var
  pow10_result_: integer;
  pow10_i: integer;
begin
  pow10_result_ := 1;
  pow10_i := 0;
  while pow10_i < n do begin
  pow10_result_ := pow10_result_ * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_result_);
end;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_r: integer;
begin
  gcd_x := a;
  gcd_y := b;
  if gcd_x < 0 then begin
  gcd_x := -gcd_x;
end;
  if gcd_y < 0 then begin
  gcd_y := -gcd_y;
end;
  while gcd_y <> 0 do begin
  gcd_r := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_r;
end;
  exit(gcd_x);
end;
function parse_decimal(s: string): Fraction;
var
  parse_decimal_idx: integer;
  parse_decimal_sign: integer;
  parse_decimal_first: string;
  parse_decimal_int_part: string;
  parse_decimal_c: string;
  parse_decimal_frac_part: string;
  parse_decimal_exp: integer;
  parse_decimal_exp_sign: integer;
  parse_decimal_exp_str: string;
  parse_decimal_num_str: string;
  parse_decimal_numerator: integer;
  parse_decimal_denominator: integer;
begin
  if Length(s) = 0 then begin
  panic('invalid number');
end;
  parse_decimal_idx := 0;
  parse_decimal_sign := 1;
  parse_decimal_first := copy(s, 1, 1);
  if parse_decimal_first = '-' then begin
  parse_decimal_sign := -1;
  parse_decimal_idx := 1;
end else begin
  if parse_decimal_first = '+' then begin
  parse_decimal_idx := 1;
end;
end;
  parse_decimal_int_part := '';
  while parse_decimal_idx < Length(s) do begin
  parse_decimal_c := copy(s, parse_decimal_idx+1, (parse_decimal_idx + 1 - (parse_decimal_idx)));
  if (parse_decimal_c >= '0') and (parse_decimal_c <= '9') then begin
  parse_decimal_int_part := parse_decimal_int_part + parse_decimal_c;
  parse_decimal_idx := parse_decimal_idx + 1;
end else begin
  break;
end;
end;
  parse_decimal_frac_part := '';
  if (parse_decimal_idx < Length(s)) and (copy(s, parse_decimal_idx+1, (parse_decimal_idx + 1 - (parse_decimal_idx))) = '.') then begin
  parse_decimal_idx := parse_decimal_idx + 1;
  while parse_decimal_idx < Length(s) do begin
  parse_decimal_c := copy(s, parse_decimal_idx+1, (parse_decimal_idx + 1 - (parse_decimal_idx)));
  if (parse_decimal_c >= '0') and (parse_decimal_c <= '9') then begin
  parse_decimal_frac_part := parse_decimal_frac_part + parse_decimal_c;
  parse_decimal_idx := parse_decimal_idx + 1;
end else begin
  break;
end;
end;
end;
  parse_decimal_exp := 0;
  if (parse_decimal_idx < Length(s)) and ((copy(s, parse_decimal_idx+1, (parse_decimal_idx + 1 - (parse_decimal_idx))) = 'e') or (copy(s, parse_decimal_idx+1, (parse_decimal_idx + 1 - (parse_decimal_idx))) = 'E')) then begin
  parse_decimal_idx := parse_decimal_idx + 1;
  parse_decimal_exp_sign := 1;
  if (parse_decimal_idx < Length(s)) and (copy(s, parse_decimal_idx+1, (parse_decimal_idx + 1 - (parse_decimal_idx))) = '-') then begin
  parse_decimal_exp_sign := -1;
  parse_decimal_idx := parse_decimal_idx + 1;
end else begin
  if (parse_decimal_idx < Length(s)) and (copy(s, parse_decimal_idx+1, (parse_decimal_idx + 1 - (parse_decimal_idx))) = '+') then begin
  parse_decimal_idx := parse_decimal_idx + 1;
end;
end;
  parse_decimal_exp_str := '';
  while parse_decimal_idx < Length(s) do begin
  parse_decimal_c := copy(s, parse_decimal_idx+1, (parse_decimal_idx + 1 - (parse_decimal_idx)));
  if (parse_decimal_c >= '0') and (parse_decimal_c <= '9') then begin
  parse_decimal_exp_str := parse_decimal_exp_str + parse_decimal_c;
  parse_decimal_idx := parse_decimal_idx + 1;
end else begin
  panic('invalid number');
end;
end;
  if Length(parse_decimal_exp_str) = 0 then begin
  panic('invalid number');
end;
  parse_decimal_exp := parse_decimal_exp_sign * StrToInt(parse_decimal_exp_str);
end;
  if parse_decimal_idx <> Length(s) then begin
  panic('invalid number');
end;
  if Length(parse_decimal_int_part) = 0 then begin
  parse_decimal_int_part := '0';
end;
  parse_decimal_num_str := parse_decimal_int_part + parse_decimal_frac_part;
  parse_decimal_numerator := StrToInt(parse_decimal_num_str);
  if parse_decimal_sign = (0 - 1) then begin
  parse_decimal_numerator := 0 - parse_decimal_numerator;
end;
  parse_decimal_denominator := pow10(Length(parse_decimal_frac_part));
  if parse_decimal_exp > 0 then begin
  parse_decimal_numerator := parse_decimal_numerator * pow10(parse_decimal_exp);
end else begin
  if parse_decimal_exp < 0 then begin
  parse_decimal_denominator := parse_decimal_denominator * pow10(-parse_decimal_exp);
end;
end;
  exit(makeFraction(parse_decimal_numerator, parse_decimal_denominator));
end;
function reduce(fr: Fraction): Fraction;
var
  reduce_g: integer;
begin
  reduce_g := gcd(fr.numerator, fr.denominator);
  exit(makeFraction(fr.numerator div reduce_g, fr.denominator div reduce_g));
end;
function decimal_to_fraction_str(s: string): Fraction;
begin
  exit(reduce(parse_decimal(s)));
end;
function decimal_to_fraction(x: real): Fraction;
begin
  exit(decimal_to_fraction_str(FloatToStr(x)));
end;
procedure assert_fraction(name: string; fr: Fraction; num: integer; den: integer);
begin
  if (fr.numerator <> num) or (fr.denominator <> den) then begin
  panic(name);
end;
end;
procedure test_decimal_to_fraction();
begin
  assert_fraction('case1', decimal_to_fraction(2), 2, 1);
  assert_fraction('case2', decimal_to_fraction(89), 89, 1);
  assert_fraction('case3', decimal_to_fraction_str('67'), 67, 1);
  assert_fraction('case4', decimal_to_fraction_str('45.0'), 45, 1);
  assert_fraction('case5', decimal_to_fraction(1.5), 3, 2);
  assert_fraction('case6', decimal_to_fraction_str('6.25'), 25, 4);
  assert_fraction('case7', decimal_to_fraction(0), 0, 1);
  assert_fraction('case8', decimal_to_fraction(-2.5), -5, 2);
  assert_fraction('case9', decimal_to_fraction(0.125), 1, 8);
  assert_fraction('case10', decimal_to_fraction(1.00000025e+06), 4000001, 4);
  assert_fraction('case11', decimal_to_fraction(1.3333), 13333, 10000);
  assert_fraction('case12', decimal_to_fraction_str('1.23e2'), 123, 1);
  assert_fraction('case13', decimal_to_fraction_str('0.500'), 1, 2);
end;
procedure main();
var
  main_fr: Fraction;
begin
  test_decimal_to_fraction();
  main_fr := decimal_to_fraction(1.5);
  writeln((IntToStr(main_fr.numerator) + '/') + IntToStr(main_fr.denominator));
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
end.
