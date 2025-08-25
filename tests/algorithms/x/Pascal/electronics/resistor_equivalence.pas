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
function resistor_parallel(resistor_parallel_resistors: RealArray): real; forward;
function resistor_series(resistor_series_resistors: RealArray): real; forward;
procedure main(); forward;
function resistor_parallel(resistor_parallel_resistors: RealArray): real;
var
  resistor_parallel_sum: real;
  resistor_parallel_i: int64;
  resistor_parallel_r: real;
begin
  resistor_parallel_sum := 0;
  resistor_parallel_i := 0;
  while resistor_parallel_i < Length(resistor_parallel_resistors) do begin
  resistor_parallel_r := resistor_parallel_resistors[resistor_parallel_i];
  if resistor_parallel_r <= 0 then begin
  panic(('Resistor at index ' + IntToStr(resistor_parallel_i)) + ' has a negative or zero value!');
end;
  resistor_parallel_sum := resistor_parallel_sum + (1 / resistor_parallel_r);
  resistor_parallel_i := resistor_parallel_i + 1;
end;
  exit(1 / resistor_parallel_sum);
end;
function resistor_series(resistor_series_resistors: RealArray): real;
var
  resistor_series_sum: real;
  resistor_series_i: int64;
  resistor_series_r: real;
begin
  resistor_series_sum := 0;
  resistor_series_i := 0;
  while resistor_series_i < Length(resistor_series_resistors) do begin
  resistor_series_r := resistor_series_resistors[resistor_series_i];
  if resistor_series_r < 0 then begin
  panic(('Resistor at index ' + IntToStr(resistor_series_i)) + ' has a negative value!');
end;
  resistor_series_sum := resistor_series_sum + resistor_series_r;
  resistor_series_i := resistor_series_i + 1;
end;
  exit(resistor_series_sum);
end;
procedure main();
var
  main_resistors: array of real;
begin
  main_resistors := [3.21389, 2, 3];
  writeln('Parallel: ' + FloatToStr(resistor_parallel(main_resistors)));
  writeln('Series: ' + FloatToStr(resistor_series(main_resistors)));
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
  writeln('');
end.
