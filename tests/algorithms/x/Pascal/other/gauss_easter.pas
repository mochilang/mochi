{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, StrUtils;
type EasterDate = record
  month: integer;
  day: integer;
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
  years: array of integer;
  i: integer;
  y: integer;
  e: EasterDate;
  year: integer;
  d: EasterDate;
function makeEasterDate(month: integer; day: integer): EasterDate; forward;
function gauss_easter(year: integer): EasterDate; forward;
function format_date(year: integer; d: EasterDate): string; forward;
function makeEasterDate(month: integer; day: integer): EasterDate;
begin
  Result.month := month;
  Result.day := day;
end;
function gauss_easter(year: integer): EasterDate;
var
  gauss_easter_metonic_cycle: integer;
  gauss_easter_julian_leap_year: integer;
  gauss_easter_non_leap_year: integer;
  gauss_easter_leap_day_inhibits: integer;
  gauss_easter_lunar_orbit_correction: integer;
  gauss_easter_leap_day_reinstall_number: real;
  gauss_easter_secular_moon_shift: real;
  gauss_easter_century_starting_point: real;
  gauss_easter_days_to_add: real;
  gauss_easter_days_from_phm_to_sunday: real;
  gauss_easter_offset: integer;
  gauss_easter_total: integer;
begin
  gauss_easter_metonic_cycle := year mod 19;
  gauss_easter_julian_leap_year := year mod 4;
  gauss_easter_non_leap_year := year mod 7;
  gauss_easter_leap_day_inhibits := year div 100;
  gauss_easter_lunar_orbit_correction := (13 + (8 * gauss_easter_leap_day_inhibits)) div 25;
  gauss_easter_leap_day_reinstall_number := Double(gauss_easter_leap_day_inhibits) / 4;
  gauss_easter_secular_moon_shift := (((15 - Double(gauss_easter_lunar_orbit_correction)) + Double(gauss_easter_leap_day_inhibits)) - gauss_easter_leap_day_reinstall_number) mod 30;
  gauss_easter_century_starting_point := ((4 + Double(gauss_easter_leap_day_inhibits)) - gauss_easter_leap_day_reinstall_number) mod 7;
  gauss_easter_days_to_add := ((19 * Double(gauss_easter_metonic_cycle)) + gauss_easter_secular_moon_shift) mod 30;
  gauss_easter_days_from_phm_to_sunday := ((((2 * Double(gauss_easter_julian_leap_year)) + (4 * Double(gauss_easter_non_leap_year))) + (6 * gauss_easter_days_to_add)) + gauss_easter_century_starting_point) mod 7;
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
function format_date(year: integer; d: EasterDate): string;
var
  format_date_month: string;
  format_date_day: string;
begin
  if d.month < 10 then begin
  format_date_month := '0' + IntToStr(d.month);
end else begin
  format_date_month := IntToStr(d.month);
end;
  if d.day < 10 then begin
  format_date_day := '0' + IntToStr(d.day);
end else begin
  format_date_day := IntToStr(d.day);
end;
  exit((((IntToStr(year) + '-') + format_date_month) + '-') + format_date_day);
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
end.
