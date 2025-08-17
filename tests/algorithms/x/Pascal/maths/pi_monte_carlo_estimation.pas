{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Point = record
  x: real;
  y: real;
end;
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
  PI: real;
  seed: integer;
  p: Point;
  simulations: integer;
  x: real;
function makePoint(x: real; y: real): Point; forward;
function next_seed(x: integer): integer; forward;
function rand_unit(): real; forward;
function is_in_unit_circle(p: Point): boolean; forward;
function random_unit_square(): Point; forward;
function estimate_pi(simulations: integer): real; forward;
function abs_float(x: real): real; forward;
procedure main(); forward;
function makePoint(x: real; y: real): Point;
begin
  Result.x := x;
  Result.y := y;
end;
function next_seed(x: integer): integer;
begin
  exit(((x * 1103515245) + 12345) mod 2147483648);
end;
function rand_unit(): real;
begin
  seed := next_seed(seed);
  exit(Double(seed) / 2.147483648e+09);
end;
function is_in_unit_circle(p: Point): boolean;
begin
  exit(((p.x * p.x) + (p.y * p.y)) <= 1);
end;
function random_unit_square(): Point;
begin
  exit(makePoint(rand_unit(), rand_unit()));
end;
function estimate_pi(simulations: integer): real;
var
  estimate_pi_inside: integer;
  estimate_pi_i: integer;
  estimate_pi_p: Point;
begin
  if simulations < 1 then begin
  panic('At least one simulation is necessary to estimate PI.');
end;
  estimate_pi_inside := 0;
  estimate_pi_i := 0;
  while estimate_pi_i < simulations do begin
  estimate_pi_p := random_unit_square();
  if is_in_unit_circle(estimate_pi_p) then begin
  estimate_pi_inside := estimate_pi_inside + 1;
end;
  estimate_pi_i := estimate_pi_i + 1;
end;
  exit((4 * Double(estimate_pi_inside)) / Double(simulations));
end;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
procedure main();
var
  main_n: integer;
  main_my_pi: real;
  main_error: real;
begin
  main_n := 10000;
  main_my_pi := estimate_pi(main_n);
  main_error := abs_float(main_my_pi - PI);
  writeln((('An estimate of PI is ' + FloatToStr(main_my_pi)) + ' with an error of ') + FloatToStr(main_error));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  seed := 1;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
