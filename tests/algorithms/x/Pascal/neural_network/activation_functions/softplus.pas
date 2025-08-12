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
  vector: RealArray;
  x: real;
function ln(x: real): real; forward;
function exp(x: real): real; forward;
function softplus(vector: RealArray): RealArray; forward;
procedure main(); forward;
function ln(x: real): real;
var
  ln_y: real;
  ln_y2: real;
  ln_term: real;
  ln_sum: real;
  ln_k: integer;
  ln_denom: real;
begin
  if x <= 0 then begin
  panic('ln domain error');
end;
  ln_y := (x - 1) / (x + 1);
  ln_y2 := ln_y * ln_y;
  ln_term := ln_y;
  ln_sum := 0;
  ln_k := 0;
  while ln_k < 10 do begin
  ln_denom := Double((2 * ln_k) + 1);
  ln_sum := ln_sum + (ln_term / ln_denom);
  ln_term := ln_term * ln_y2;
  ln_k := ln_k + 1;
end;
  exit(2 * ln_sum);
end;
function exp(x: real): real;
var
  exp_term: real;
  exp_sum: real;
  exp_n: integer;
begin
  exp_term := 1;
  exp_sum := 1;
  exp_n := 1;
  while exp_n < 20 do begin
  exp_term := (exp_term * x) / Double(exp_n);
  exp_sum := exp_sum + exp_term;
  exp_n := exp_n + 1;
end;
  exit(exp_sum);
end;
function softplus(vector: RealArray): RealArray;
var
  softplus_result_: array of real;
  softplus_i: integer;
  softplus_x: real;
  softplus_value: real;
begin
  softplus_result_ := [];
  softplus_i := 0;
  while softplus_i < Length(vector) do begin
  softplus_x := vector[softplus_i];
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
  writeln(list_real_to_str(main_r1));
  writeln(list_real_to_str(main_r2));
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
