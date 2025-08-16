{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  nums: array of real;
  _: boolean;
function check_polygon(nums: RealArray): boolean; forward;
function check_polygon(nums: RealArray): boolean;
var
  check_polygon_i: integer;
  check_polygon_total: real;
  check_polygon_max_side: real;
  check_polygon_v: real;
begin
  if Length(nums) < 2 then begin
  error('Monogons and Digons are not polygons in the Euclidean space');
end;
  check_polygon_i := 0;
  while check_polygon_i < Length(nums) do begin
  if nums[check_polygon_i] <= 0 then begin
  error('All values must be greater than 0');
end;
  check_polygon_i := check_polygon_i + 1;
end;
  check_polygon_total := 0;
  check_polygon_max_side := 0;
  check_polygon_i := 0;
  while check_polygon_i < Length(nums) do begin
  check_polygon_v := nums[check_polygon_i];
  check_polygon_total := check_polygon_total + check_polygon_v;
  if check_polygon_v > check_polygon_max_side then begin
  check_polygon_max_side := check_polygon_v;
end;
  check_polygon_i := check_polygon_i + 1;
end;
  exit(check_polygon_max_side < (check_polygon_total - check_polygon_max_side));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(check_polygon([6, 10, 5]), true)));
  writeln(LowerCase(BoolToStr(check_polygon([3, 7, 13, 2]), true)));
  writeln(LowerCase(BoolToStr(check_polygon([1, 4.3, 5.2, 12.2]), true)));
  nums := [3, 7, 13, 2];
  _ := check_polygon(nums);
  writeln(list_real_to_str(nums));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
