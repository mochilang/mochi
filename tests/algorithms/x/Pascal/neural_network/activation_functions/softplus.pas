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
function ln_(ln__x: real): real; forward;
function exp_(exp__x: real): real; forward;
function softplus(softplus_vector: RealArray): RealArray; forward;
procedure main(); forward;
function ln_(ln__x: real): real;
var
  ln__y: real;
  ln__y2: real;
  ln__term: real;
  ln__sum: real;
  ln__k: integer;
  ln__denom: real;
begin
  if ln__x <= 0 then begin
  panic('ln domain error');
end;
  ln__y := (ln__x - 1) / (ln__x + 1);
  ln__y2 := ln__y * ln__y;
  ln__term := ln__y;
  ln__sum := 0;
  ln__k := 0;
  while ln__k < 10 do begin
  ln__denom := Double((2 * ln__k) + 1);
  ln__sum := ln__sum + (ln__term / ln__denom);
  ln__term := ln__term * ln__y2;
  ln__k := ln__k + 1;
end;
  exit(2 * ln__sum);
end;
function exp_(exp__x: real): real;
var
  exp__term: real;
  exp__sum: real;
  exp__n: integer;
begin
  exp__term := 1;
  exp__sum := 1;
  exp__n := 1;
  while exp__n < 20 do begin
  exp__term := (exp__term * exp__x) / Double(exp__n);
  exp__sum := exp__sum + exp__term;
  exp__n := exp__n + 1;
end;
  exit(exp__sum);
end;
function softplus(softplus_vector: RealArray): RealArray;
var
  softplus_result_: array of real;
  softplus_i: integer;
  softplus_x: real;
  softplus_value: real;
begin
  softplus_result_ := [];
  softplus_i := 0;
  while softplus_i < Length(softplus_vector) do begin
  softplus_x := softplus_vector[softplus_i];
  softplus_value := ln(1 + exp(softplus_x));
  softplus_result_ := concat(softplus_result_, [softplus_value]);
  softplus_i := softplus_i + 1;
end;
  exit(softplus_result_);
end;
procedure main();
var
  main_v1: array of real;
  main_v2: array of real;
  main_r1: RealArray;
  main_r2: RealArray;
begin
  main_v1 := [2.3, 0.6, -2, -3.8];
  main_v2 := [-9.2, -0.3, 0.45, -4.56];
  main_r1 := softplus(main_v1);
  main_r2 := softplus(main_v2);
  show_list_real(main_r1);
  show_list_real(main_r2);
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
