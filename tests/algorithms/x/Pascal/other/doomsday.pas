{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math, fgl;
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
  DOOMSDAY_LEAP: array of int64;
  DOOMSDAY_NOT_LEAP: array of int64;
  WEEK_DAY_NAMES: specialize TFPGMap<int64, string>;
function Map1(): specialize TFPGMap<int64, string>; forward;
function get_week_day(get_week_day_year: int64; get_week_day_month: int64; get_week_day_day: int64): string; forward;
function Map1(): specialize TFPGMap<int64, string>;
begin
  Result := specialize TFPGMap<int64, string>.Create();
  Result.AddOrSetData(0, 'Sunday');
  Result.AddOrSetData(1, 'Monday');
  Result.AddOrSetData(2, 'Tuesday');
  Result.AddOrSetData(3, 'Wednesday');
  Result.AddOrSetData(4, 'Thursday');
  Result.AddOrSetData(5, 'Friday');
  Result.AddOrSetData(6, 'Saturday');
end;
function get_week_day(get_week_day_year: int64; get_week_day_month: int64; get_week_day_day: int64): string;
var
  get_week_day_century: int64;
  get_week_day_century_anchor: int64;
  get_week_day_centurian: int64;
  get_week_day_centurian_m: int64;
  get_week_day_dooms_day: int64;
  get_week_day_day_anchor: int64;
  get_week_day_week_day: int64;
begin
  if get_week_day_year < 100 then begin
  panic('year should be in YYYY format');
end;
  if (get_week_day_month < 1) or (get_week_day_month > 12) then begin
  panic('month should be between 1 to 12');
end;
  if (get_week_day_day < 1) or (get_week_day_day > 31) then begin
  panic('day should be between 1 to 31');
end;
  get_week_day_century := _floordiv(get_week_day_year, 100);
  get_week_day_century_anchor := ((5 * (get_week_day_century mod 4)) + 2) mod 7;
  get_week_day_centurian := get_week_day_year mod 100;
  get_week_day_centurian_m := get_week_day_centurian mod 12;
  get_week_day_dooms_day := ((((_floordiv(get_week_day_centurian, 12)) + get_week_day_centurian_m) + (_floordiv(get_week_day_centurian_m, 4))) + get_week_day_century_anchor) mod 7;
  if ((get_week_day_year mod 4) <> 0) or ((get_week_day_centurian = 0) and ((get_week_day_year mod 400) <> 0)) then begin
  get_week_day_day_anchor := DOOMSDAY_NOT_LEAP[get_week_day_month - 1];
end else begin
  get_week_day_day_anchor := DOOMSDAY_LEAP[get_week_day_month - 1];
end;
  get_week_day_week_day := ((get_week_day_dooms_day + get_week_day_day) - get_week_day_day_anchor) mod 7;
  if get_week_day_week_day < 0 then begin
  get_week_day_week_day := get_week_day_week_day + 7;
end;
  exit(WEEK_DAY_NAMES[get_week_day_week_day]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  DOOMSDAY_LEAP := [4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5];
  DOOMSDAY_NOT_LEAP := [3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5];
  WEEK_DAY_NAMES := Map1();
  writeln(get_week_day(2020, 10, 24));
  writeln(get_week_day(2017, 10, 24));
  writeln(get_week_day(2019, 5, 3));
  writeln(get_week_day(1970, 9, 16));
  writeln(get_week_day(1870, 8, 13));
  writeln(get_week_day(2040, 3, 14));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
