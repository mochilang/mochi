{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  valid_colors: array of string;
  significant_figures_color_values: specialize TFPGMap<string, int64>;
  multiplier_color_values: specialize TFPGMap<string, real>;
  tolerance_color_values: specialize TFPGMap<string, real>;
  temperature_coeffecient_color_values: specialize TFPGMap<string, int64>;
function Map4(): specialize TFPGMap<string, int64>; forward;
function Map3(): specialize TFPGMap<string, real>; forward;
function Map2(): specialize TFPGMap<string, real>; forward;
function Map1(): specialize TFPGMap<string, int64>; forward;
function contains(contains_list: StrArray; contains_value: string): boolean; forward;
function get_significant_digits(get_significant_digits_colors: StrArray): int64; forward;
function get_multiplier(get_multiplier_color: string): real; forward;
function get_tolerance(get_tolerance_color: string): real; forward;
function get_temperature_coeffecient(get_temperature_coeffecient_color: string): int64; forward;
function get_band_type_count(get_band_type_count_total: int64; get_band_type_count_typ: string): int64; forward;
function check_validity(check_validity_number_of_bands: int64; check_validity_colors: StrArray): boolean; forward;
function calculate_resistance(calculate_resistance_number_of_bands: int64; calculate_resistance_color_code_list: StrArray): string; forward;
function Map4(): specialize TFPGMap<string, int64>;
begin
  Result := specialize TFPGMap<string, int64>.Create();
  Result.AddOrSetData('Black', 250);
  Result.AddOrSetData('Brown', 100);
  Result.AddOrSetData('Red', 50);
  Result.AddOrSetData('Orange', 15);
  Result.AddOrSetData('Yellow', 25);
  Result.AddOrSetData('Green', 20);
  Result.AddOrSetData('Blue', 10);
  Result.AddOrSetData('Violet', 5);
  Result.AddOrSetData('Grey', 1);
end;
function Map3(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('Brown', 1);
  Result.AddOrSetData('Red', 2);
  Result.AddOrSetData('Orange', 0.05);
  Result.AddOrSetData('Yellow', 0.02);
  Result.AddOrSetData('Green', 0.5);
  Result.AddOrSetData('Blue', 0.25);
  Result.AddOrSetData('Violet', 0.1);
  Result.AddOrSetData('Grey', 0.01);
  Result.AddOrSetData('Gold', 5);
  Result.AddOrSetData('Silver', 10);
end;
function Map2(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('Black', 1);
  Result.AddOrSetData('Brown', 10);
  Result.AddOrSetData('Red', 100);
  Result.AddOrSetData('Orange', 1000);
  Result.AddOrSetData('Yellow', 10000);
  Result.AddOrSetData('Green', 100000);
  Result.AddOrSetData('Blue', 1e+06);
  Result.AddOrSetData('Violet', 1e+07);
  Result.AddOrSetData('Grey', 1e+08);
  Result.AddOrSetData('White', 1e+09);
  Result.AddOrSetData('Gold', 0.1);
  Result.AddOrSetData('Silver', 0.01);
end;
function Map1(): specialize TFPGMap<string, int64>;
begin
  Result := specialize TFPGMap<string, int64>.Create();
  Result.AddOrSetData('Black', 0);
  Result.AddOrSetData('Brown', 1);
  Result.AddOrSetData('Red', 2);
  Result.AddOrSetData('Orange', 3);
  Result.AddOrSetData('Yellow', 4);
  Result.AddOrSetData('Green', 5);
  Result.AddOrSetData('Blue', 6);
  Result.AddOrSetData('Violet', 7);
  Result.AddOrSetData('Grey', 8);
  Result.AddOrSetData('White', 9);
end;
function contains(contains_list: StrArray; contains_value: string): boolean;
var
  contains_c: string;
begin
  for contains_c in contains_list do begin
  if contains_c = contains_value then begin
  exit(true);
end;
end;
  exit(false);
end;
function get_significant_digits(get_significant_digits_colors: StrArray): int64;
var
  get_significant_digits_digit: int64;
  get_significant_digits_color: string;
begin
  get_significant_digits_digit := 0;
  for get_significant_digits_color in get_significant_digits_colors do begin
  if not significant_figures_color_values.IndexOf(get_significant_digits_color) <> -1 then begin
  panic(get_significant_digits_color + ' is not a valid color for significant figure bands');
end;
  get_significant_digits_digit := (get_significant_digits_digit * 10) + significant_figures_color_values[get_significant_digits_color];
end;
  exit(get_significant_digits_digit);
end;
function get_multiplier(get_multiplier_color: string): real;
begin
  if not multiplier_color_values.IndexOf(get_multiplier_color) <> -1 then begin
  panic(get_multiplier_color + ' is not a valid color for multiplier band');
end;
  exit(multiplier_color_values[get_multiplier_color]);
end;
function get_tolerance(get_tolerance_color: string): real;
begin
  if not tolerance_color_values.IndexOf(get_tolerance_color) <> -1 then begin
  panic(get_tolerance_color + ' is not a valid color for tolerance band');
end;
  exit(tolerance_color_values[get_tolerance_color]);
end;
function get_temperature_coeffecient(get_temperature_coeffecient_color: string): int64;
begin
  if not temperature_coeffecient_color_values.IndexOf(get_temperature_coeffecient_color) <> -1 then begin
  panic(get_temperature_coeffecient_color + ' is not a valid color for temperature coeffecient band');
end;
  exit(temperature_coeffecient_color_values[get_temperature_coeffecient_color]);
end;
function get_band_type_count(get_band_type_count_total: int64; get_band_type_count_typ: string): int64;
begin
  if get_band_type_count_total = 3 then begin
  if get_band_type_count_typ = 'significant' then begin
  exit(2);
end;
  if get_band_type_count_typ = 'multiplier' then begin
  exit(1);
end;
  panic(get_band_type_count_typ + ' is not valid for a 3 band resistor');
end else begin
  if get_band_type_count_total = 4 then begin
  if get_band_type_count_typ = 'significant' then begin
  exit(2);
end;
  if get_band_type_count_typ = 'multiplier' then begin
  exit(1);
end;
  if get_band_type_count_typ = 'tolerance' then begin
  exit(1);
end;
  panic(get_band_type_count_typ + ' is not valid for a 4 band resistor');
end else begin
  if get_band_type_count_total = 5 then begin
  if get_band_type_count_typ = 'significant' then begin
  exit(3);
end;
  if get_band_type_count_typ = 'multiplier' then begin
  exit(1);
end;
  if get_band_type_count_typ = 'tolerance' then begin
  exit(1);
end;
  panic(get_band_type_count_typ + ' is not valid for a 5 band resistor');
end else begin
  if get_band_type_count_total = 6 then begin
  if get_band_type_count_typ = 'significant' then begin
  exit(3);
end;
  if get_band_type_count_typ = 'multiplier' then begin
  exit(1);
end;
  if get_band_type_count_typ = 'tolerance' then begin
  exit(1);
end;
  if get_band_type_count_typ = 'temp_coeffecient' then begin
  exit(1);
end;
  panic(get_band_type_count_typ + ' is not valid for a 6 band resistor');
end else begin
  panic(IntToStr(get_band_type_count_total) + ' is not a valid number of bands');
end;
end;
end;
end;
end;
function check_validity(check_validity_number_of_bands: int64; check_validity_colors: StrArray): boolean;
var
  check_validity_color: string;
begin
  if (check_validity_number_of_bands < 3) or (check_validity_number_of_bands > 6) then begin
  panic('Invalid number of bands. Resistor bands must be 3 to 6');
end;
  if check_validity_number_of_bands <> Length(check_validity_colors) then begin
  panic(((('Expecting ' + IntToStr(check_validity_number_of_bands)) + ' colors, provided ') + IntToStr(Length(check_validity_colors))) + ' colors');
end;
  for check_validity_color in check_validity_colors do begin
  if not contains(valid_colors, check_validity_color) then begin
  panic(check_validity_color + ' is not a valid color');
end;
end;
  exit(true);
end;
function calculate_resistance(calculate_resistance_number_of_bands: int64; calculate_resistance_color_code_list: StrArray): string;
var
  calculate_resistance_sig_count: int64;
  calculate_resistance_significant_colors: array of string;
  calculate_resistance_significant_digits: int64;
  calculate_resistance_multiplier_color: string;
  calculate_resistance_multiplier: real;
  calculate_resistance_tolerance: real;
  calculate_resistance_tolerance_color: string;
  calculate_resistance_temp_coeff: int64;
  calculate_resistance_temp_color: string;
  calculate_resistance_resistance_value: real;
  calculate_resistance_resistance_str: string;
  calculate_resistance_answer: string;
begin
  check_validity(calculate_resistance_number_of_bands, calculate_resistance_color_code_list);
  calculate_resistance_sig_count := get_band_type_count(calculate_resistance_number_of_bands, 'significant');
  calculate_resistance_significant_colors := copy(calculate_resistance_color_code_list, 0, (calculate_resistance_sig_count - (0)));
  calculate_resistance_significant_digits := get_significant_digits(calculate_resistance_significant_colors);
  calculate_resistance_multiplier_color := calculate_resistance_color_code_list[calculate_resistance_sig_count];
  calculate_resistance_multiplier := get_multiplier(calculate_resistance_multiplier_color);
  calculate_resistance_tolerance := 20;
  if calculate_resistance_number_of_bands >= 4 then begin
  calculate_resistance_tolerance_color := calculate_resistance_color_code_list[calculate_resistance_sig_count + 1];
  calculate_resistance_tolerance := get_tolerance(calculate_resistance_tolerance_color);
end;
  calculate_resistance_temp_coeff := 0;
  if calculate_resistance_number_of_bands = 6 then begin
  calculate_resistance_temp_color := calculate_resistance_color_code_list[calculate_resistance_sig_count + 2];
  calculate_resistance_temp_coeff := get_temperature_coeffecient(calculate_resistance_temp_color);
end;
  calculate_resistance_resistance_value := calculate_resistance_multiplier * calculate_resistance_significant_digits;
  calculate_resistance_resistance_str := FloatToStr(calculate_resistance_resistance_value);
  if calculate_resistance_resistance_value = Trunc(calculate_resistance_resistance_value) then begin
  calculate_resistance_resistance_str := IntToStr(Trunc(calculate_resistance_resistance_value));
end;
  calculate_resistance_answer := ((calculate_resistance_resistance_str + 'Ω ±') + FloatToStr(calculate_resistance_tolerance)) + '% ';
  if calculate_resistance_temp_coeff <> 0 then begin
  calculate_resistance_answer := (calculate_resistance_answer + IntToStr(calculate_resistance_temp_coeff)) + ' ppm/K';
end;
  exit(calculate_resistance_answer);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  valid_colors := ['Black', 'Brown', 'Red', 'Orange', 'Yellow', 'Green', 'Blue', 'Violet', 'Grey', 'White', 'Gold', 'Silver'];
  significant_figures_color_values := Map1();
  multiplier_color_values := Map2();
  tolerance_color_values := Map3();
  temperature_coeffecient_color_values := Map4();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
