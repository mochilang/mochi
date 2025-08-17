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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  xs: RealArray;
function is_arithmetic_series(xs: RealArray): boolean; forward;
function arithmetic_mean(xs: RealArray): real; forward;
function is_arithmetic_series(xs: RealArray): boolean;
var
  is_arithmetic_series_diff: real;
  is_arithmetic_series_i: integer;
begin
  if Length(xs) = 0 then begin
  panic('Input list must be a non empty list');
end;
  if Length(xs) = 1 then begin
  exit(true);
end;
  is_arithmetic_series_diff := xs[1] - xs[0];
  is_arithmetic_series_i := 0;
  while is_arithmetic_series_i < (Length(xs) - 1) do begin
  if (xs[is_arithmetic_series_i + 1] - xs[is_arithmetic_series_i]) <> is_arithmetic_series_diff then begin
  exit(false);
end;
  is_arithmetic_series_i := is_arithmetic_series_i + 1;
end;
  exit(true);
end;
function arithmetic_mean(xs: RealArray): real;
var
  arithmetic_mean_total: real;
  arithmetic_mean_i: integer;
begin
  if Length(xs) = 0 then begin
  panic('Input list must be a non empty list');
end;
  arithmetic_mean_total := 0;
  arithmetic_mean_i := 0;
  while arithmetic_mean_i < Length(xs) do begin
  arithmetic_mean_total := arithmetic_mean_total + xs[arithmetic_mean_i];
  arithmetic_mean_i := arithmetic_mean_i + 1;
end;
  exit(arithmetic_mean_total / Double(Length(xs)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(is_arithmetic_series([2, 4, 6]), true)));
  writeln(LowerCase(BoolToStr(is_arithmetic_series([3, 6, 12, 24]), true)));
  writeln(FloatToStr(arithmetic_mean([2, 4, 6])));
  writeln(FloatToStr(arithmetic_mean([3, 6, 9, 12])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
