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
  d_value: real;
  x: real;
  vector: RealArray;
  b_value: real;
  a_value: real;
  c_value: real;
function exp_(x: real): real; forward;
function soboleva_modified_hyperbolic_tangent(vector: RealArray; a_value: real; b_value: real; c_value: real; d_value: real): RealArray; forward;
procedure main(); forward;
function exp_(x: real): real;
var
  exp__term: real;
  exp__sum: real;
  exp__n: integer;
begin
  exp__term := 1;
  exp__sum := 1;
  exp__n := 1;
  while exp__n < 20 do begin
  exp__term := (exp__term * x) / to_float(exp__n);
  exp__sum := exp__sum + exp__term;
  exp__n := exp__n + 1;
end;
  exit(exp__sum);
end;
function soboleva_modified_hyperbolic_tangent(vector: RealArray; a_value: real; b_value: real; c_value: real; d_value: real): RealArray;
var
  soboleva_modified_hyperbolic_tangent_result_: array of real;
  soboleva_modified_hyperbolic_tangent_i: integer;
  soboleva_modified_hyperbolic_tangent_x: real;
  soboleva_modified_hyperbolic_tangent_numerator: real;
  soboleva_modified_hyperbolic_tangent_denominator: real;
begin
  soboleva_modified_hyperbolic_tangent_result_ := [];
  soboleva_modified_hyperbolic_tangent_i := 0;
  while soboleva_modified_hyperbolic_tangent_i < Length(vector) do begin
  soboleva_modified_hyperbolic_tangent_x := vector[soboleva_modified_hyperbolic_tangent_i];
  soboleva_modified_hyperbolic_tangent_numerator := exp(a_value * soboleva_modified_hyperbolic_tangent_x) - exp(-b_value * soboleva_modified_hyperbolic_tangent_x);
  soboleva_modified_hyperbolic_tangent_denominator := exp(c_value * soboleva_modified_hyperbolic_tangent_x) + exp(-d_value * soboleva_modified_hyperbolic_tangent_x);
  soboleva_modified_hyperbolic_tangent_result_ := concat(soboleva_modified_hyperbolic_tangent_result_, [soboleva_modified_hyperbolic_tangent_numerator / soboleva_modified_hyperbolic_tangent_denominator]);
  soboleva_modified_hyperbolic_tangent_i := soboleva_modified_hyperbolic_tangent_i + 1;
end;
  exit(soboleva_modified_hyperbolic_tangent_result_);
end;
procedure main();
var
  main_vector: array of real;
  main_res: RealArray;
begin
  main_vector := [5.4, -2.4, 6.3, -5.23, 3.27, 0.56];
  main_res := soboleva_modified_hyperbolic_tangent(main_vector, 0.2, 0.4, 0.6, 0.8);
  json(main_res);
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
