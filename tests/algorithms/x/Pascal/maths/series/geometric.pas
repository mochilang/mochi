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
  base: real;
  exp_: integer;
  n: integer;
  series: RealArray;
  value: real;
function is_geometric_series(series: RealArray): boolean; forward;
function geometric_mean(series: RealArray): real; forward;
function pow_float(base: real; pow_float_exp_: integer): real; forward;
function nth_root(value: real; n: integer): real; forward;
procedure test_geometric(); forward;
procedure main(); forward;
function is_geometric_series(series: RealArray): boolean;
var
  is_geometric_series_ratio: real;
  is_geometric_series_i: integer;
begin
  if Length(series) = 0 then begin
  panic('Input list must be a non empty list');
end;
  if Length(series) = 1 then begin
  exit(true);
end;
  if series[0] = 0 then begin
  exit(false);
end;
  is_geometric_series_ratio := series[1] / series[0];
  is_geometric_series_i := 0;
  while is_geometric_series_i < (Length(series) - 1) do begin
  if series[is_geometric_series_i] = 0 then begin
  exit(false);
end;
  if (series[is_geometric_series_i + 1] / series[is_geometric_series_i]) <> is_geometric_series_ratio then begin
  exit(false);
end;
  is_geometric_series_i := is_geometric_series_i + 1;
end;
  exit(true);
end;
function geometric_mean(series: RealArray): real;
var
  geometric_mean_product: real;
  geometric_mean_i: integer;
  geometric_mean_n: integer;
begin
  if Length(series) = 0 then begin
  panic('Input list must be a non empty list');
end;
  geometric_mean_product := 1;
  geometric_mean_i := 0;
  while geometric_mean_i < Length(series) do begin
  geometric_mean_product := geometric_mean_product * series[geometric_mean_i];
  geometric_mean_i := geometric_mean_i + 1;
end;
  geometric_mean_n := Length(series);
  exit(nth_root(geometric_mean_product, geometric_mean_n));
end;
function pow_float(base: real; pow_float_exp_: integer): real;
var
  pow_float_result_: real;
  pow_float_i: integer;
begin
  pow_float_result_ := 1;
  pow_float_i := 0;
  while pow_float_i < exp_ do begin
  pow_float_result_ := pow_float_result_ * base;
  pow_float_i := pow_float_i + 1;
end;
  exit(pow_float_result_);
end;
function nth_root(value: real; n: integer): real;
var
  nth_root_low: real;
  nth_root_high: real;
  nth_root_mid: real;
  nth_root_i: integer;
  nth_root_mp: real;
begin
  if value = 0 then begin
  exit(0);
end;
  nth_root_low := 0;
  nth_root_high := value;
  if value < 1 then begin
  nth_root_high := 1;
end;
  nth_root_mid := (nth_root_low + nth_root_high) / 2;
  nth_root_i := 0;
  while nth_root_i < 40 do begin
  nth_root_mp := pow_float(nth_root_mid, n);
  if nth_root_mp > value then begin
  nth_root_high := nth_root_mid;
end else begin
  nth_root_low := nth_root_mid;
end;
  nth_root_mid := (nth_root_low + nth_root_high) / 2;
  nth_root_i := nth_root_i + 1;
end;
  exit(nth_root_mid);
end;
procedure test_geometric();
var
  test_geometric_a: array of real;
  test_geometric_b: array of real;
begin
  test_geometric_a := [2, 4, 8];
  if not is_geometric_series(test_geometric_a) then begin
  panic('expected geometric series');
end;
  test_geometric_b := [1, 2, 3];
  if is_geometric_series(test_geometric_b) then begin
  panic('expected non geometric series');
end;
end;
procedure main();
begin
  test_geometric();
  writeln(geometric_mean([2, 4, 8]));
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
