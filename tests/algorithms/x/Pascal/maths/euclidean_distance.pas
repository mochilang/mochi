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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  x: real;
  v1: RealArray;
  v2: RealArray;
function sqrtApprox(x: real): real; forward;
function euclidean_distance(v1: RealArray; v2: RealArray): real; forward;
function euclidean_distance_no_np(v1: RealArray; v2: RealArray): real; forward;
procedure main(); forward;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function euclidean_distance(v1: RealArray; v2: RealArray): real;
var
  euclidean_distance_sum: real;
  euclidean_distance_i: integer;
  euclidean_distance_diff: real;
begin
  euclidean_distance_sum := 0;
  euclidean_distance_i := 0;
  while euclidean_distance_i < Length(v1) do begin
  euclidean_distance_diff := v1[euclidean_distance_i] - v2[euclidean_distance_i];
  euclidean_distance_sum := euclidean_distance_sum + (euclidean_distance_diff * euclidean_distance_diff);
  euclidean_distance_i := euclidean_distance_i + 1;
end;
  exit(sqrtApprox(euclidean_distance_sum));
end;
function euclidean_distance_no_np(v1: RealArray; v2: RealArray): real;
begin
  exit(euclidean_distance(v1, v2));
end;
procedure main();
begin
  writeln(FloatToStr(euclidean_distance([0, 0], [2, 2])));
  writeln(FloatToStr(euclidean_distance([0, 0, 0], [2, 2, 2])));
  writeln(FloatToStr(euclidean_distance([1, 2, 3, 4], [5, 6, 7, 8])));
  writeln(FloatToStr(euclidean_distance_no_np([1, 2, 3, 4], [5, 6, 7, 8])));
  writeln(FloatToStr(euclidean_distance_no_np([0, 0], [2, 2])));
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
