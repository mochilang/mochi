{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  capacitors: RealArray;
function capacitor_parallel(capacitors: RealArray): real; forward;
function capacitor_series(capacitors: RealArray): real; forward;
procedure main(); forward;
function capacitor_parallel(capacitors: RealArray): real;
var
  capacitor_parallel_sum_c: real;
  capacitor_parallel_i: integer;
  capacitor_parallel_c: real;
begin
  capacitor_parallel_sum_c := 0;
  capacitor_parallel_i := 0;
  while capacitor_parallel_i < Length(capacitors) do begin
  capacitor_parallel_c := capacitors[capacitor_parallel_i];
  if capacitor_parallel_c < 0 then begin
  panic(('Capacitor at index ' + IntToStr(capacitor_parallel_i)) + ' has a negative value!');
  exit(0);
end;
  capacitor_parallel_sum_c := capacitor_parallel_sum_c + capacitor_parallel_c;
  capacitor_parallel_i := capacitor_parallel_i + 1;
end;
  exit(capacitor_parallel_sum_c);
end;
function capacitor_series(capacitors: RealArray): real;
var
  capacitor_series_first_sum: real;
  capacitor_series_i: integer;
  capacitor_series_c: real;
begin
  capacitor_series_first_sum := 0;
  capacitor_series_i := 0;
  while capacitor_series_i < Length(capacitors) do begin
  capacitor_series_c := capacitors[capacitor_series_i];
  if capacitor_series_c <= 0 then begin
  panic(('Capacitor at index ' + IntToStr(capacitor_series_i)) + ' has a negative or zero value!');
  exit(0);
end;
  capacitor_series_first_sum := capacitor_series_first_sum + (1 / capacitor_series_c);
  capacitor_series_i := capacitor_series_i + 1;
end;
  exit(1 / capacitor_series_first_sum);
end;
procedure main();
var
  main_parallel: real;
  main_series: real;
begin
  main_parallel := capacitor_parallel([5.71389, 12, 3]);
  main_series := capacitor_series([5.71389, 12, 3]);
  writeln(FloatToStr(main_parallel));
  writeln(FloatToStr(main_series));
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
