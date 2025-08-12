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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  DOOMSDAY_LEAP: array of integer;
  DOOMSDAY_NOT_LEAP: array of integer;
  WEEK_DAY_NAMES: specialize TFPGMap<integer, string>;
  day: integer;
  year: integer;
  month: integer;
function Map1(): specialize TFPGMap<integer, string>; forward;
function get_week_day(year: integer; month: integer; day: integer): string; forward;
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
function get_week_day(year: integer; month: integer; day: integer): string;
var
  get_week_day_century: integer;
  get_week_day_century_anchor: integer;
  get_week_day_centurian: integer;
  get_week_day_centurian_m: integer;
  get_week_day_dooms_day: integer;
  get_week_day_day_anchor: integer;
  get_week_day_week_day: integer;
begin
  if year < 100 then begin
  panic('year should be in YYYY format');
end;
  if (month < 1) or (month > 12) then begin
  panic('month should be between 1 to 12');
end;
  if (day < 1) or (day > 31) then begin
  panic('day should be between 1 to 31');
end;
  get_week_day_century := year div 100;
  get_week_day_century_anchor := ((5 * (get_week_day_century mod 4)) + 2) mod 7;
  get_week_day_centurian := year mod 100;
  get_week_day_centurian_m := get_week_day_centurian mod 12;
  get_week_day_dooms_day := ((((get_week_day_centurian div 12) + get_week_day_centurian_m) + (get_week_day_centurian_m div 4)) + get_week_day_century_anchor) mod 7;
  if ((year mod 4) <> 0) or ((get_week_day_centurian = 0) and ((year mod 400) <> 0)) then begin
  get_week_day_day_anchor := DOOMSDAY_NOT_LEAP[month - 1];
end else begin
  get_week_day_day_anchor := DOOMSDAY_LEAP[month - 1];
end;
  get_week_day_week_day := ((get_week_day_dooms_day + day) - get_week_day_day_anchor) mod 7;
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
end.
