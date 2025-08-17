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
  a: real;
  b: real;
  vec: RealArray;
  x: real;
function exp_approx(x: real): real; forward;
function softmax(vec: RealArray): RealArray; forward;
function abs_val(x: real): real; forward;
function approx_equal(a: real; b: real): boolean; forward;
procedure test_softmax(); forward;
procedure main(); forward;
function exp_approx(x: real): real;
var
  exp_approx_term: real;
  exp_approx_sum: real;
  exp_approx_i: integer;
begin
  exp_approx_term := 1;
  exp_approx_sum := 1;
  exp_approx_i := 1;
  while exp_approx_i < 20 do begin
  exp_approx_term := (exp_approx_term * x) / Double(exp_approx_i);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_i := exp_approx_i + 1;
end;
  exit(exp_approx_sum);
end;
function softmax(vec: RealArray): RealArray;
var
  softmax_exps: array of real;
  softmax_i: integer;
  softmax_total: real;
  softmax_result_: array of real;
begin
  softmax_exps := [];
  softmax_i := 0;
  while softmax_i < Length(vec) do begin
  softmax_exps := concat(softmax_exps, [exp_approx(vec[softmax_i])]);
  softmax_i := softmax_i + 1;
end;
  softmax_total := 0;
  softmax_i := 0;
  while softmax_i < Length(softmax_exps) do begin
  softmax_total := softmax_total + softmax_exps[softmax_i];
  softmax_i := softmax_i + 1;
end;
  softmax_result_ := [];
  softmax_i := 0;
  while softmax_i < Length(softmax_exps) do begin
  softmax_result_ := concat(softmax_result_, [softmax_exps[softmax_i] / softmax_total]);
  softmax_i := softmax_i + 1;
end;
  exit(softmax_result_);
end;
function abs_val(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function approx_equal(a: real; b: real): boolean;
begin
  exit(abs_val(a - b) < 0.0001);
end;
procedure test_softmax();
var
  test_softmax_s1: RealArray;
  test_softmax_sum1: real;
  test_softmax_i: integer;
  test_softmax_s2: RealArray;
  test_softmax_s3: RealArray;
begin
  test_softmax_s1 := softmax([1, 2, 3, 4]);
  test_softmax_sum1 := 0;
  test_softmax_i := 0;
  while test_softmax_i < Length(test_softmax_s1) do begin
  test_softmax_sum1 := test_softmax_sum1 + test_softmax_s1[test_softmax_i];
  test_softmax_i := test_softmax_i + 1;
end;
  if not approx_equal(test_softmax_sum1, 1) then begin
  panic('sum test failed');
end;
  test_softmax_s2 := softmax([5, 5]);
  if not (approx_equal(test_softmax_s2[0], 0.5) and approx_equal(test_softmax_s2[1], 0.5)) then begin
  panic('equal elements test failed');
end;
  test_softmax_s3 := softmax([0]);
  if not approx_equal(test_softmax_s3[0], 1) then begin
  panic('zero vector test failed');
end;
end;
procedure main();
begin
  test_softmax();
  writeln(list_real_to_str(softmax([1, 2, 3, 4])));
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
