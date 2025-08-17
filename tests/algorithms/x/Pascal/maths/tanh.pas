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
function expApprox(expApprox_x: real): real; forward;
function tangent_hyperbolic(tangent_hyperbolic_vector: RealArray): RealArray; forward;
procedure main(); forward;
function expApprox(expApprox_x: real): real;
var
  expApprox_neg: boolean;
  expApprox_y: real;
  expApprox_term: real;
  expApprox_sum: real;
  expApprox_n: integer;
begin
  expApprox_neg := false;
  expApprox_y := expApprox_x;
  if expApprox_x < 0 then begin
  expApprox_neg := true;
  expApprox_y := -expApprox_x;
end;
  expApprox_term := 1;
  expApprox_sum := 1;
  expApprox_n := 1;
  while expApprox_n < 30 do begin
  expApprox_term := (expApprox_term * expApprox_y) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  if expApprox_neg then begin
  exit(1 / expApprox_sum);
end;
  exit(expApprox_sum);
end;
function tangent_hyperbolic(tangent_hyperbolic_vector: RealArray): RealArray;
var
  tangent_hyperbolic_result_: array of real;
  tangent_hyperbolic_i: integer;
  tangent_hyperbolic_x: real;
  tangent_hyperbolic_t: real;
begin
  tangent_hyperbolic_result_ := [];
  tangent_hyperbolic_i := 0;
  while tangent_hyperbolic_i < Length(tangent_hyperbolic_vector) do begin
  tangent_hyperbolic_x := tangent_hyperbolic_vector[tangent_hyperbolic_i];
  tangent_hyperbolic_t := (2 / (1 + expApprox(-2 * tangent_hyperbolic_x))) - 1;
  tangent_hyperbolic_result_ := concat(tangent_hyperbolic_result_, [tangent_hyperbolic_t]);
  tangent_hyperbolic_i := tangent_hyperbolic_i + 1;
end;
  exit(tangent_hyperbolic_result_);
end;
procedure main();
var
  main_v1: array of real;
  main_v2: array of real;
begin
  main_v1 := [1, 5, 6, -0.67];
  main_v2 := [8, 10, 2, -0.98, 13];
  writeln(list_real_to_str(tangent_hyperbolic(main_v1)));
  writeln(list_real_to_str(tangent_hyperbolic(main_v2)));
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
