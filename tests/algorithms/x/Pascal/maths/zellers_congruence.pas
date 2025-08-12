{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
  s: string;
  date_input: string;
function Map1(): specialize TFPGMap<integer, string>; forward;
function parse_decimal(s: string): integer; forward;
function zeller_day(date_input: string): string; forward;
function zeller(date_input: string): string; forward;
procedure test_zeller(); forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<integer, string>;
begin
  Result := specialize TFPGMap<integer, string>.Create();
  Result.AddOrSetData(0, Variant('Sunday'));
  Result.AddOrSetData(1, Variant('Monday'));
  Result.AddOrSetData(2, Variant('Tuesday'));
  Result.AddOrSetData(3, Variant('Wednesday'));
  Result.AddOrSetData(4, Variant('Thursday'));
  Result.AddOrSetData(5, Variant('Friday'));
  Result.AddOrSetData(6, Variant('Saturday'));
end;
function parse_decimal(s: string): integer;
var
  parse_decimal_value: integer;
  parse_decimal_i: integer;
  parse_decimal_c: string;
begin
  parse_decimal_value := 0;
  parse_decimal_i := 0;
  while parse_decimal_i < Length(s) do begin
  parse_decimal_c := s[parse_decimal_i+1];
  if (parse_decimal_c < '0') or (parse_decimal_c > '9') then begin
  panic('invalid literal');
end;
  parse_decimal_value := (parse_decimal_value * 10) + StrToInt(parse_decimal_c);
  parse_decimal_i := parse_decimal_i + 1;
end;
  exit(parse_decimal_value);
end;
function zeller_day(date_input: string): string;
var
  zeller_day_days: specialize TFPGMap<integer, string>;
  zeller_day_m: integer;
  zeller_day_sep1: string;
  zeller_day_d: integer;
  zeller_day_sep2: string;
  zeller_day_y: integer;
  zeller_day_year: integer;
  zeller_day_month: integer;
  zeller_day_c: integer;
  zeller_day_k: integer;
  zeller_day_t: integer;
  zeller_day_u: integer;
  zeller_day_v: integer;
  zeller_day_x: integer;
  zeller_day_z: integer;
  zeller_day_w: integer;
  zeller_day_f: integer;
begin
  zeller_day_days := Map1();
  if Length(date_input) <> 10 then begin
  panic('Must be 10 characters long');
end;
  zeller_day_m := parse_decimal(copy(date_input, 1, 2));
  if (zeller_day_m <= 0) or (zeller_day_m >= 13) then begin
  panic('Month must be between 1 - 12');
end;
  zeller_day_sep1 := date_input[2+1];
  if (zeller_day_sep1 <> '-') and (zeller_day_sep1 <> '/') then begin
  panic('Date separator must be ''-'' or ''/''');
end;
  zeller_day_d := parse_decimal(copy(date_input, 4, 2));
  if (zeller_day_d <= 0) or (zeller_day_d >= 32) then begin
  panic('Date must be between 1 - 31');
end;
  zeller_day_sep2 := date_input[5+1];
  if (zeller_day_sep2 <> '-') and (zeller_day_sep2 <> '/') then begin
  panic('Date separator must be ''-'' or ''/''');
end;
  zeller_day_y := parse_decimal(copy(date_input, 7, 4));
  if (zeller_day_y <= 45) or (zeller_day_y >= 8500) then begin
  panic('Year out of range. There has to be some sort of limit...right?');
end;
  zeller_day_year := zeller_day_y;
  zeller_day_month := zeller_day_m;
  if zeller_day_month <= 2 then begin
  zeller_day_year := zeller_day_year - 1;
  zeller_day_month := zeller_day_month + 12;
end;
  zeller_day_c := zeller_day_year div 100;
  zeller_day_k := zeller_day_year mod 100;
  zeller_day_t := Trunc((2.6 * Double(zeller_day_month)) - 5.39);
  zeller_day_u := zeller_day_c div 4;
  zeller_day_v := zeller_day_k div 4;
  zeller_day_x := zeller_day_d + zeller_day_k;
  zeller_day_z := ((zeller_day_t + zeller_day_u) + zeller_day_v) + zeller_day_x;
  zeller_day_w := zeller_day_z - (2 * zeller_day_c);
  zeller_day_f := zeller_day_w mod 7;
  if zeller_day_f < 0 then begin
  zeller_day_f := zeller_day_f + 7;
end;
  exit(zeller_day_days[zeller_day_f]);
end;
function zeller(date_input: string): string;
var
  zeller_day_var: string;
begin
  zeller_day_var := zeller_day(date_input);
  exit(((('Your date ' + date_input) + ', is a ') + zeller_day_var) + '!');
end;
procedure test_zeller();
var
  test_zeller_inputs: array of string;
  test_zeller_expected: array of string;
  test_zeller_i: integer;
  test_zeller_res: string;
begin
  test_zeller_inputs := ['01-31-2010', '02-01-2010', '11-26-2024', '07-04-1776'];
  test_zeller_expected := ['Sunday', 'Monday', 'Tuesday', 'Thursday'];
  test_zeller_i := 0;
  while test_zeller_i < Length(test_zeller_inputs) do begin
  test_zeller_res := zeller_day(test_zeller_inputs[test_zeller_i]);
  if test_zeller_res <> test_zeller_expected[test_zeller_i] then begin
  panic('zeller test failed');
end;
  test_zeller_i := test_zeller_i + 1;
end;
end;
procedure main();
begin
  test_zeller();
  writeln(zeller('01-31-2010'));
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
