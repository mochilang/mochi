{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, StrUtils, Math;
type EasterDate = record
  month: int64;
  day: int64;
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
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  years: array of int64;
  i: int64;
  y: int64;
  e: EasterDate;
function makeEasterDate(month: int64; day: int64): EasterDate; forward;
function gauss_easter(gauss_easter_year: int64): EasterDate; forward;
function format_date(format_date_year: int64; format_date_d: EasterDate): string; forward;
function makeEasterDate(month: int64; day: int64): EasterDate;
begin
  Result.month := month;
  Result.day := day;
end;
function gauss_easter(gauss_easter_year: int64): EasterDate;
var
  gauss_easter_metonic_cycle: int64;
  gauss_easter_julian_leap_year: int64;
  gauss_easter_non_leap_year: int64;
  gauss_easter_leap_day_inhibits: int64;
  gauss_easter_lunar_orbit_correction: int64;
  gauss_easter_leap_day_reinstall_number: real;
  gauss_easter_secular_moon_shift: real;
  gauss_easter_century_starting_point: real;
  gauss_easter_days_to_add: real;
  gauss_easter_days_from_phm_to_sunday: real;
  gauss_easter_offset: integer;
  gauss_easter_total: int64;
begin
  gauss_easter_metonic_cycle := gauss_easter_year mod 19;
  gauss_easter_julian_leap_year := gauss_easter_year mod 4;
  gauss_easter_non_leap_year := gauss_easter_year mod 7;
  gauss_easter_leap_day_inhibits := _floordiv(gauss_easter_year, 100);
  gauss_easter_lunar_orbit_correction := _floordiv(13 + (8 * gauss_easter_leap_day_inhibits), 25);
  gauss_easter_leap_day_reinstall_number := Double(gauss_easter_leap_day_inhibits) / 4;
  gauss_easter_secular_moon_shift := (((15 - Double(gauss_easter_lunar_orbit_correction)) + Double(gauss_easter_leap_day_inhibits)) - gauss_easter_leap_day_reinstall_number - Floor(((15 - Double(gauss_easter_lunar_orbit_correction)) + Double(gauss_easter_leap_day_inhibits)) - gauss_easter_leap_day_reinstall_number / 30) * 30);
  gauss_easter_century_starting_point := ((4 + Double(gauss_easter_leap_day_inhibits)) - gauss_easter_leap_day_reinstall_number - Floor((4 + Double(gauss_easter_leap_day_inhibits)) - gauss_easter_leap_day_reinstall_number / 7) * 7);
  gauss_easter_days_to_add := ((19 * Double(gauss_easter_metonic_cycle)) + gauss_easter_secular_moon_shift - Floor((19 * Double(gauss_easter_metonic_cycle)) + gauss_easter_secular_moon_shift / 30) * 30);
  gauss_easter_days_from_phm_to_sunday := ((((2 * Double(gauss_easter_julian_leap_year)) + (4 * Double(gauss_easter_non_leap_year))) + (6 * gauss_easter_days_to_add)) + gauss_easter_century_starting_point - Floor((((2 * Double(gauss_easter_julian_leap_year)) + (4 * Double(gauss_easter_non_leap_year))) + (6 * gauss_easter_days_to_add)) + gauss_easter_century_starting_point / 7) * 7);
  if (gauss_easter_days_to_add = 29) and (gauss_easter_days_from_phm_to_sunday = 6) then begin
  exit(makeEasterDate(4, 19));
end;
  if (gauss_easter_days_to_add = 28) and (gauss_easter_days_from_phm_to_sunday = 6) then begin
  exit(makeEasterDate(4, 18));
end;
  gauss_easter_offset := Trunc(gauss_easter_days_to_add + gauss_easter_days_from_phm_to_sunday);
  gauss_easter_total := 22 + gauss_easter_offset;
  if gauss_easter_total > 31 then begin
  exit(makeEasterDate(4, gauss_easter_total - 31));
end;
  exit(makeEasterDate(3, gauss_easter_total));
end;
function format_date(format_date_year: int64; format_date_d: EasterDate): string;
var
  format_date_month: string;
  format_date_day: string;
begin
  if format_date_d.month < 10 then begin
  format_date_month := '0' + IntToStr(format_date_d.month);
end else begin
  format_date_month := IntToStr(format_date_d.month);
end;
  if format_date_d.day < 10 then begin
  format_date_day := '0' + IntToStr(format_date_d.day);
end else begin
  format_date_day := IntToStr(format_date_d.day);
end;
  exit((((IntToStr(format_date_year) + '-') + format_date_month) + '-') + format_date_day);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  years := [1994, 2000, 2010, 2021, 2023, 2032, 2100];
  i := 0;
  while i < Length(years) do begin
  y := years[i];
  e := gauss_easter(y);
  writeln((('Easter in ' + IntToStr(y)) + ' is ') + format_date(y, e));
  i := i + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
