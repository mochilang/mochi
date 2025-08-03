{$mode objfpc}
program Main;
uses SysUtils, Math, fgl;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  TWO_PI: real;
  n: integer;
function Map1(): specialize TFPGMap<string, integer>; forward;
function sinApprox(x: real): real; forward;
function floor(x: real): real; forward;
function absFloat(x: real): real; forward;
function absInt(n: integer): integer; forward;
function parseIntStr(str: string): integer; forward;
function parseDate(s: string): IntArray; forward;
function leap(y: integer): boolean; forward;
function daysInMonth(y: integer; m: integer): integer; forward;
function addDays(y: integer; m: integer; d: integer; n: integer): IntArray; forward;
function pad2(n: integer): string; forward;
function dateString(y: integer; m: integer; d: integer): string; forward;
function day(y: integer; m: integer; d: integer): integer; forward;
procedure biorhythms(birth: string; target: string); forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
  Result.AddOrSetData('0', Variant(0));
  Result.AddOrSetData('1', Variant(1));
  Result.AddOrSetData('2', Variant(2));
  Result.AddOrSetData('3', Variant(3));
  Result.AddOrSetData('4', Variant(4));
  Result.AddOrSetData('5', Variant(5));
  Result.AddOrSetData('6', Variant(6));
  Result.AddOrSetData('7', Variant(7));
  Result.AddOrSetData('8', Variant(8));
  Result.AddOrSetData('9', Variant(9));
end;
function sinApprox(x: real): real;
var
  sinApprox_term: real;
  sinApprox_sum: real;
  sinApprox_n: integer;
  sinApprox_denom: real;
begin
  sinApprox_term := x;
  sinApprox_sum := x;
  sinApprox_n := 1;
  while sinApprox_n <= 8 do begin
  sinApprox_denom := Double((2 * sinApprox_n) * ((2 * sinApprox_n) + 1));
  sinApprox_term := ((-sinApprox_term * x) * x) / sinApprox_denom;
  sinApprox_sum := sinApprox_sum + sinApprox_term;
  sinApprox_n := sinApprox_n + 1;
end;
  exit(sinApprox_sum);
end;
function floor(x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(x);
  if Double(floor_i) > x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function absFloat(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function absInt(n: integer): integer;
begin
  if n < 0 then begin
  exit(-n);
end;
  exit(n);
end;
function parseIntStr(str: string): integer;
var
  parseIntStr_i: integer;
  parseIntStr_neg: boolean;
  parseIntStr_digits: specialize TFPGMap<string, integer>;
begin
  exit(StrToInt(str));
end;
function parseDate(s: string): IntArray;
var
  parseDate_y: integer;
  parseDate_m: integer;
  parseDate_d: integer;
begin
  parseDate_y := parseIntStr(copy(s, 0+1, (4 - (0))));
  parseDate_m := parseIntStr(copy(s, 5+1, (7 - (5))));
  parseDate_d := parseIntStr(copy(s, 8+1, (10 - (8))));
  exit([parseDate_y, parseDate_m, parseDate_d]);
end;
function leap(y: integer): boolean;
begin
  if (y mod 400) = 0 then begin
  exit(true);
end;
  if (y mod 100) = 0 then begin
  exit(false);
end;
  exit((y mod 4) = 0);
end;
function daysInMonth(y: integer; m: integer): integer;
var
  daysInMonth_feb: integer;
  daysInMonth_lengths: array of integer;
begin
  daysInMonth_feb := IfThen(leap(y), 29, 28);
  daysInMonth_lengths := [31, daysInMonth_feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  exit(daysInMonth_lengths[m - 1]);
end;
function addDays(y: integer; m: integer; d: integer; n: integer): IntArray;
var
  addDays_yy: integer;
  addDays_mm: integer;
  addDays_dd: integer;
  addDays_i: integer;
begin
  addDays_yy := y;
  addDays_mm := m;
  addDays_dd := d;
  if n >= 0 then begin
  addDays_i := 0;
  while addDays_i < n do begin
  addDays_dd := addDays_dd + 1;
  if addDays_dd > daysInMonth(addDays_yy, addDays_mm) then begin
  addDays_dd := 1;
  addDays_mm := addDays_mm + 1;
  if addDays_mm > 12 then begin
  addDays_mm := 1;
  addDays_yy := addDays_yy + 1;
end;
end;
  addDays_i := addDays_i + 1;
end;
end else begin
  addDays_i := 0;
  while addDays_i > n do begin
  addDays_dd := addDays_dd - 1;
  if addDays_dd < 1 then begin
  addDays_mm := addDays_mm - 1;
  if addDays_mm < 1 then begin
  addDays_mm := 12;
  addDays_yy := addDays_yy - 1;
end;
  addDays_dd := daysInMonth(addDays_yy, addDays_mm);
end;
  addDays_i := addDays_i - 1;
end;
end;
  exit([addDays_yy, addDays_mm, addDays_dd]);
end;
function pad2(n: integer): string;
begin
  if n < 10 then begin
  exit('0' + IntToStr(n));
end;
  exit(IntToStr(n));
end;
function dateString(y: integer; m: integer; d: integer): string;
begin
  exit((((IntToStr(y) + '-') + pad2(m)) + '-') + pad2(d));
end;
function day(y: integer; m: integer; d: integer): integer;
var
  day_part1: integer;
  day_part2: integer;
  day_part3: integer;
begin
  day_part1 := 367 * y;
  day_part2 := Trunc((7 * Trunc(y + ((m + 9) div 12))) div 4);
  day_part3 := Trunc((275 * m) div 9);
  exit((((day_part1 - day_part2) + day_part3) + d) - 730530);
end;
procedure biorhythms(birth: string; target: string);
var
  biorhythms_bparts: IntArray;
  biorhythms_by: integer;
  biorhythms_bm: integer;
  biorhythms_bd: integer;
  biorhythms_tparts: IntArray;
  biorhythms_ty: integer;
  biorhythms_tm: integer;
  biorhythms_td: integer;
  biorhythms_diff: integer;
  biorhythms_cycles: array of string;
  biorhythms_lengths: array of integer;
  biorhythms_quadrants: array of array of string;
  biorhythms_i: integer;
  biorhythms_length: integer;
  biorhythms_cycle: string;
  biorhythms_position: integer;
  biorhythms_quadrant: integer;
  biorhythms_percent: real;
  biorhythms_description: string;
  biorhythms_daysToAdd: integer;
  biorhythms_res: IntArray;
  biorhythms_ny: integer;
  biorhythms_nm: integer;
  biorhythms_nd: integer;
  biorhythms_transition: string;
  biorhythms_trend: string;
  biorhythms_next: string;
  biorhythms_pct: string;
  biorhythms_posStr: string;
begin
  biorhythms_bparts := parseDate(birth);
  biorhythms_by := biorhythms_bparts[0];
  biorhythms_bm := biorhythms_bparts[1];
  biorhythms_bd := biorhythms_bparts[2];
  biorhythms_tparts := parseDate(target);
  biorhythms_ty := biorhythms_tparts[0];
  biorhythms_tm := biorhythms_tparts[1];
  biorhythms_td := biorhythms_tparts[2];
  biorhythms_diff := absInt(day(biorhythms_ty, biorhythms_tm, biorhythms_td) - day(biorhythms_by, biorhythms_bm, biorhythms_bd));
  writeln((('Born ' + birth) + ', Target ') + target);
  writeln('Day ' + IntToStr(biorhythms_diff));
  biorhythms_cycles := ['Physical day ', 'Emotional day', 'Mental day   '];
  biorhythms_lengths := [23, 28, 33];
  biorhythms_quadrants := [['up and rising', 'peak'], ['up but falling', 'transition'], ['down and falling', 'valley'], ['down but rising', 'transition']];
  biorhythms_i := 0;
  while biorhythms_i < 3 do begin
  biorhythms_length := biorhythms_lengths[biorhythms_i];
  biorhythms_cycle := biorhythms_cycles[biorhythms_i];
  biorhythms_position := biorhythms_diff mod biorhythms_length;
  biorhythms_quadrant := (biorhythms_position * 4) div biorhythms_length;
  biorhythms_percent := sinApprox(((2 * PI) * Double(biorhythms_position)) / Double(biorhythms_length));
  biorhythms_percent := floor(biorhythms_percent * 1000) / 10;
  biorhythms_description := '';
  if biorhythms_percent > 95 then begin
  biorhythms_description := ' peak';
end else begin
  if biorhythms_percent < -95 then begin
  biorhythms_description := ' valley';
end else begin
  if absFloat(biorhythms_percent) < 5 then begin
  biorhythms_description := ' critical transition';
end else begin
  biorhythms_daysToAdd := (((biorhythms_quadrant + 1) * biorhythms_length) div 4) - biorhythms_position;
  biorhythms_res := addDays(biorhythms_ty, biorhythms_tm, biorhythms_td, biorhythms_daysToAdd);
  biorhythms_ny := biorhythms_res[0];
  biorhythms_nm := biorhythms_res[1];
  biorhythms_nd := biorhythms_res[2];
  biorhythms_transition := dateString(biorhythms_ny, biorhythms_nm, biorhythms_nd);
  biorhythms_trend := biorhythms_quadrants[biorhythms_quadrant][0];
  biorhythms_next := biorhythms_quadrants[biorhythms_quadrant][1];
  biorhythms_pct := FloatToStr(biorhythms_percent);
  if not (Pos('.', biorhythms_pct) <> 0) then begin
  biorhythms_pct := biorhythms_pct + '.0';
end;
  biorhythms_description := (((((((' ' + biorhythms_pct) + '% (') + biorhythms_trend) + ', next ') + biorhythms_next) + ' ') + biorhythms_transition) + ')';
end;
end;
end;
  biorhythms_posStr := IntToStr(biorhythms_position);
  if biorhythms_position < 10 then begin
  biorhythms_posStr := ' ' + biorhythms_posStr;
end;
  writeln(((biorhythms_cycle + biorhythms_posStr) + ' : ') + biorhythms_description);
  biorhythms_i := biorhythms_i + 1;
end;
  writeln('');
end;
procedure main();
var
  main_pairs: array of array of string;
  main_idx: integer;
  main_p: array of string;
begin
  main_pairs := [['1943-03-09', '1972-07-11'], ['1809-01-12', '1863-11-19'], ['1809-02-12', '1863-11-19']];
  main_idx := 0;
  while main_idx < Length(main_pairs) do begin
  main_p := main_pairs[main_idx];
  biorhythms(main_p[0], main_p[1]);
  main_idx := main_idx + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  TWO_PI := 6.283185307179586;
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
