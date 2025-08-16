{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
procedure show_list_real(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  vector: RealArray;
  lambda_: real;
  x: real;
  alpha: real;
function exp_(x: real): real; forward;
function scaled_exponential_linear_unit(vector: RealArray; alpha: real; lambda_: real): RealArray; forward;
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
  exp__term := (exp__term * x) / Double(exp__n);
  exp__sum := exp__sum + exp__term;
  exp__n := exp__n + 1;
end;
  exit(exp__sum);
end;
function scaled_exponential_linear_unit(vector: RealArray; alpha: real; lambda_: real): RealArray;
var
  scaled_exponential_linear_unit_result_: array of real;
  scaled_exponential_linear_unit_i: integer;
  scaled_exponential_linear_unit_x: real;
  scaled_exponential_linear_unit_y: real;
begin
  scaled_exponential_linear_unit_result_ := [];
  scaled_exponential_linear_unit_i := 0;
  while scaled_exponential_linear_unit_i < Length(vector) do begin
  scaled_exponential_linear_unit_x := vector[scaled_exponential_linear_unit_i];
  if scaled_exponential_linear_unit_x > 0 then begin
  scaled_exponential_linear_unit_y := lambda_ * scaled_exponential_linear_unit_x;
end else begin
  scaled_exponential_linear_unit_y := (lambda_ * alpha) * (exp(scaled_exponential_linear_unit_x) - 1);
end;
  scaled_exponential_linear_unit_result_ := concat(scaled_exponential_linear_unit_result_, [scaled_exponential_linear_unit_y]);
  scaled_exponential_linear_unit_i := scaled_exponential_linear_unit_i + 1;
end;
  exit(scaled_exponential_linear_unit_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list_real(scaled_exponential_linear_unit([1.3, 3.7, 2.4], 1.6732, 1.0507));
  show_list_real(scaled_exponential_linear_unit([1.3, 4.7, 8.2], 1.6732, 1.0507));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
