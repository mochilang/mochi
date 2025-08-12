{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
  mass_central: real;
  mass_object: real;
  r_initial: real;
  r_final: real;
  n: integer;
  x: real;
function pow10(n: integer): real; forward;
function floor(x: real): real; forward;
function format_scientific_3(x: real): string; forward;
function orbital_transfer_work(mass_central: real; mass_object: real; r_initial: real; r_final: real): string; forward;
procedure test_orbital_transfer_work(); forward;
procedure main(); forward;
function pow10(n: integer): real;
var
  pow10_p: real;
  pow10_i: integer;
begin
  pow10_p := 1;
  if n >= 0 then begin
  pow10_i := 0;
  while pow10_i < n do begin
  pow10_p := pow10_p * 10;
  pow10_i := pow10_i + 1;
end;
end else begin
  pow10_i := 0;
  while pow10_i > n do begin
  pow10_p := pow10_p / 10;
  pow10_i := pow10_i - 1;
end;
end;
  exit(pow10_p);
end;
function floor(x: real): real;
var
  floor_i: integer;
  floor_f: real;
begin
  floor_i := Trunc(x);
  floor_f := Double(floor_i);
  if floor_f > x then begin
  exit(Double(floor_i - 1));
end;
  exit(floor_f);
end;
function format_scientific_3(x: real): string;
var
  format_scientific_3_sign: string;
  format_scientific_3_num: real;
  format_scientific_3_exp: integer;
  format_scientific_3_temp: real;
  format_scientific_3_scaled: integer;
  format_scientific_3_int_part: integer;
  format_scientific_3_frac_part: integer;
  format_scientific_3_frac_str: string;
  format_scientific_3_mantissa: string;
  format_scientific_3_exp_sign: string;
  format_scientific_3_exp_abs: integer;
  format_scientific_3_exp_str: string;
begin
  if x = 0 then begin
  exit('0.000e+00');
end;
  format_scientific_3_sign := '';
  format_scientific_3_num := x;
  if format_scientific_3_num < 0 then begin
  format_scientific_3_sign := '-';
  format_scientific_3_num := -format_scientific_3_num;
end;
  format_scientific_3_exp := 0;
  while format_scientific_3_num >= 10 do begin
  format_scientific_3_num := format_scientific_3_num / 10;
  format_scientific_3_exp := format_scientific_3_exp + 1;
end;
  while format_scientific_3_num < 1 do begin
  format_scientific_3_num := format_scientific_3_num * 10;
  format_scientific_3_exp := format_scientific_3_exp - 1;
end;
  format_scientific_3_temp := Floor((format_scientific_3_num * 1000) + 0.5);
  format_scientific_3_scaled := Trunc(format_scientific_3_temp);
  if format_scientific_3_scaled = 10000 then begin
  format_scientific_3_scaled := 1000;
  format_scientific_3_exp := format_scientific_3_exp + 1;
end;
  format_scientific_3_int_part := format_scientific_3_scaled div 1000;
  format_scientific_3_frac_part := format_scientific_3_scaled mod 1000;
  format_scientific_3_frac_str := IntToStr(format_scientific_3_frac_part);
  while Length(format_scientific_3_frac_str) < 3 do begin
  format_scientific_3_frac_str := '0' + format_scientific_3_frac_str;
end;
  format_scientific_3_mantissa := (IntToStr(format_scientific_3_int_part) + '.') + format_scientific_3_frac_str;
  format_scientific_3_exp_sign := '+';
  format_scientific_3_exp_abs := format_scientific_3_exp;
  if format_scientific_3_exp < 0 then begin
  format_scientific_3_exp_sign := '-';
  format_scientific_3_exp_abs := -format_scientific_3_exp;
end;
  format_scientific_3_exp_str := IntToStr(format_scientific_3_exp_abs);
  if format_scientific_3_exp_abs < 10 then begin
  format_scientific_3_exp_str := '0' + format_scientific_3_exp_str;
end;
  exit((((format_scientific_3_sign + format_scientific_3_mantissa) + 'e') + format_scientific_3_exp_sign) + format_scientific_3_exp_str);
end;
function orbital_transfer_work(mass_central: real; mass_object: real; r_initial: real; r_final: real): string;
var
  orbital_transfer_work_G: real;
  orbital_transfer_work_work: real;
begin
  orbital_transfer_work_G := 6.6743 * pow10(-11);
  if (r_initial <= 0) or (r_final <= 0) then begin
  panic('Orbital radii must be greater than zero.');
end;
  orbital_transfer_work_work := (((orbital_transfer_work_G * mass_central) * mass_object) / 2) * ((1 / r_initial) - (1 / r_final));
  exit(format_scientific_3(orbital_transfer_work_work));
end;
procedure test_orbital_transfer_work();
begin
  if orbital_transfer_work(5.972 * pow10(24), 1000, 6.371 * pow10(6), 7 * pow10(6)) <> '2.811e+09' then begin
  panic('case1 failed');
end;
  if orbital_transfer_work(5.972 * pow10(24), 500, 7 * pow10(6), 6.371 * pow10(6)) <> '-1.405e+09' then begin
  panic('case2 failed');
end;
  if orbital_transfer_work(1.989 * pow10(30), 1000, 1.5 * pow10(11), 2.28 * pow10(11)) <> '1.514e+11' then begin
  panic('case3 failed');
end;
end;
procedure main();
begin
  test_orbital_transfer_work();
  writeln(orbital_transfer_work(5.972 * pow10(24), 1000, 6.371 * pow10(6), 7 * pow10(6)));
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
