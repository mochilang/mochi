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
  series: RealArray;
function is_harmonic_series(series: RealArray): boolean; forward;
function harmonic_mean(series: RealArray): real; forward;
function is_harmonic_series(series: RealArray): boolean;
var
  is_harmonic_series_rec_series: array of real;
  is_harmonic_series_i: integer;
  is_harmonic_series_val: real;
  is_harmonic_series_common_diff: real;
  is_harmonic_series_idx: integer;
begin
  if Length(series) = 0 then begin
  panic('Input list must be a non empty list');
end;
  if Length(series) = 1 then begin
  if series[0] = 0 then begin
  panic('Input series cannot have 0 as an element');
end;
  exit(true);
end;
  is_harmonic_series_rec_series := [];
  is_harmonic_series_i := 0;
  while is_harmonic_series_i < Length(series) do begin
  is_harmonic_series_val := series[is_harmonic_series_i];
  if is_harmonic_series_val = 0 then begin
  panic('Input series cannot have 0 as an element');
end;
  is_harmonic_series_rec_series := concat(is_harmonic_series_rec_series, [1 / is_harmonic_series_val]);
  is_harmonic_series_i := is_harmonic_series_i + 1;
end;
  is_harmonic_series_common_diff := is_harmonic_series_rec_series[1] - is_harmonic_series_rec_series[0];
  is_harmonic_series_idx := 2;
  while is_harmonic_series_idx < Length(is_harmonic_series_rec_series) do begin
  if (is_harmonic_series_rec_series[is_harmonic_series_idx] - is_harmonic_series_rec_series[is_harmonic_series_idx - 1]) <> is_harmonic_series_common_diff then begin
  exit(false);
end;
  is_harmonic_series_idx := is_harmonic_series_idx + 1;
end;
  exit(true);
end;
function harmonic_mean(series: RealArray): real;
var
  harmonic_mean_total: real;
  harmonic_mean_i: integer;
begin
  if Length(series) = 0 then begin
  panic('Input list must be a non empty list');
end;
  harmonic_mean_total := 0;
  harmonic_mean_i := 0;
  while harmonic_mean_i < Length(series) do begin
  harmonic_mean_total := harmonic_mean_total + (1 / series[harmonic_mean_i]);
  harmonic_mean_i := harmonic_mean_i + 1;
end;
  exit(Double(Length(series)) / harmonic_mean_total);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(Ord(is_harmonic_series([1, 2 / 3, 1 / 2, 2 / 5, 1 / 3])));
  writeln(Ord(is_harmonic_series([1, 2 / 3, 2 / 5, 1 / 3])));
  writeln(harmonic_mean([1, 4, 4]));
  writeln(harmonic_mean([3, 6, 9, 12]));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
