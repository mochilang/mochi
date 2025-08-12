{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Dual = record
  real: real;
  duals: array of real;
end;
type FuncType1 = function(arg0: Dual): Dual is nested;
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
  n: integer;
  position: real;
  func: FuncType1;
  ds: RealArray;
  real_: real;
  order: integer;
  x: Dual;
  rank: integer;
  b: real;
  a: Dual;
function makeDual(real: real; duals: RealArray): Dual; forward;
function make_dual(real_: real; rank: integer): Dual; forward;
function dual_from_list(real_: real; ds: RealArray): Dual; forward;
function dual_add(a: Dual; b: Dual): Dual; forward;
function dual_add_real(a: Dual; b: real): Dual; forward;
function dual_mul(a: Dual; b: Dual): Dual; forward;
function dual_mul_real(a: Dual; b: real): Dual; forward;
function dual_pow(x: Dual; n: integer): Dual; forward;
function factorial(n: integer): real; forward;
function differentiate(func: FuncType1; position: real; order: integer): real; forward;
procedure test_differentiate(); forward;
procedure main(); forward;
function makeDual(real: real; duals: RealArray): Dual;
begin
  Result.real := real;
  Result.duals := duals;
end;
function make_dual(real_: real; rank: integer): Dual;
var
  make_dual_ds: array of real;
  make_dual_i: integer;
begin
  make_dual_ds := [];
  make_dual_i := 0;
  while make_dual_i < rank do begin
  make_dual_ds := concat(make_dual_ds, [1]);
  make_dual_i := make_dual_i + 1;
end;
  exit(makeDual(real_, make_dual_ds));
end;
function dual_from_list(real_: real; ds: RealArray): Dual;
begin
  exit(makeDual(real_, ds));
end;
function dual_add(a: Dual; b: Dual): Dual;
var
  dual_add_s_dual: array of real;
  dual_add_i: integer;
  dual_add_o_dual: array of real;
  dual_add_j: integer;
  dual_add_diff: integer;
  dual_add_k: integer;
  dual_add_diff2: integer;
  dual_add_k2: integer;
  dual_add_new_duals: array of real;
  dual_add_idx: integer;
begin
  dual_add_s_dual := [];
  dual_add_i := 0;
  while dual_add_i < Length(a.duals) do begin
  dual_add_s_dual := concat(dual_add_s_dual, [a.duals[dual_add_i]]);
  dual_add_i := dual_add_i + 1;
end;
  dual_add_o_dual := [];
  dual_add_j := 0;
  while dual_add_j < Length(b.duals) do begin
  dual_add_o_dual := concat(dual_add_o_dual, [b.duals[dual_add_j]]);
  dual_add_j := dual_add_j + 1;
end;
  if Length(dual_add_s_dual) > Length(dual_add_o_dual) then begin
  dual_add_diff := Length(dual_add_s_dual) - Length(dual_add_o_dual);
  dual_add_k := 0;
  while dual_add_k < dual_add_diff do begin
  dual_add_o_dual := concat(dual_add_o_dual, [1]);
  dual_add_k := dual_add_k + 1;
end;
end else begin
  if Length(dual_add_s_dual) < Length(dual_add_o_dual) then begin
  dual_add_diff2 := Length(dual_add_o_dual) - Length(dual_add_s_dual);
  dual_add_k2 := 0;
  while dual_add_k2 < dual_add_diff2 do begin
  dual_add_s_dual := concat(dual_add_s_dual, [1]);
  dual_add_k2 := dual_add_k2 + 1;
end;
end;
end;
  dual_add_new_duals := [];
  dual_add_idx := 0;
  while dual_add_idx < Length(dual_add_s_dual) do begin
  dual_add_new_duals := concat(dual_add_new_duals, [dual_add_s_dual[dual_add_idx] + dual_add_o_dual[dual_add_idx]]);
  dual_add_idx := dual_add_idx + 1;
end;
  exit(makeDual(a.real + b.real, dual_add_new_duals));
end;
function dual_add_real(a: Dual; b: real): Dual;
var
  dual_add_real_ds: array of real;
  dual_add_real_i: integer;
begin
  dual_add_real_ds := [];
  dual_add_real_i := 0;
  while dual_add_real_i < Length(a.duals) do begin
  dual_add_real_ds := concat(dual_add_real_ds, [a.duals[dual_add_real_i]]);
  dual_add_real_i := dual_add_real_i + 1;
end;
  exit(makeDual(a.real + b, dual_add_real_ds));
end;
function dual_mul(a: Dual; b: Dual): Dual;
var
  dual_mul_new_len: integer;
  dual_mul_new_duals: array of real;
  dual_mul_idx: integer;
  dual_mul_i: integer;
  dual_mul_j: integer;
  dual_mul_pos: integer;
  dual_mul_val: real;
  dual_mul_k: integer;
  dual_mul_l: integer;
begin
  dual_mul_new_len := (Length(a.duals) + Length(b.duals)) + 1;
  dual_mul_new_duals := [];
  dual_mul_idx := 0;
  while dual_mul_idx < dual_mul_new_len do begin
  dual_mul_new_duals := concat(dual_mul_new_duals, [0]);
  dual_mul_idx := dual_mul_idx + 1;
end;
  dual_mul_i := 0;
  while dual_mul_i < Length(a.duals) do begin
  dual_mul_j := 0;
  while dual_mul_j < Length(b.duals) do begin
  dual_mul_pos := (dual_mul_i + dual_mul_j) + 1;
  dual_mul_val := dual_mul_new_duals[dual_mul_pos] + (a.duals[dual_mul_i] * b.duals[dual_mul_j]);
  dual_mul_new_duals[dual_mul_pos] := dual_mul_val;
  dual_mul_j := dual_mul_j + 1;
end;
  dual_mul_i := dual_mul_i + 1;
end;
  dual_mul_k := 0;
  while dual_mul_k < Length(a.duals) do begin
  dual_mul_val := dual_mul_new_duals[dual_mul_k] + (a.duals[dual_mul_k] * b.real);
  dual_mul_new_duals[dual_mul_k] := dual_mul_val;
  dual_mul_k := dual_mul_k + 1;
end;
  dual_mul_l := 0;
  while dual_mul_l < Length(b.duals) do begin
  dual_mul_val := dual_mul_new_duals[dual_mul_l] + (b.duals[dual_mul_l] * a.real);
  dual_mul_new_duals[dual_mul_l] := dual_mul_val;
  dual_mul_l := dual_mul_l + 1;
end;
  exit(makeDual(a.real * b.real, dual_mul_new_duals));
end;
function dual_mul_real(a: Dual; b: real): Dual;
var
  dual_mul_real_ds: array of real;
  dual_mul_real_i: integer;
begin
  dual_mul_real_ds := [];
  dual_mul_real_i := 0;
  while dual_mul_real_i < Length(a.duals) do begin
  dual_mul_real_ds := concat(dual_mul_real_ds, [a.duals[dual_mul_real_i] * b]);
  dual_mul_real_i := dual_mul_real_i + 1;
end;
  exit(makeDual(a.real * b, dual_mul_real_ds));
end;
function dual_pow(x: Dual; n: integer): Dual;
var
  dual_pow_res: Dual;
  dual_pow_i: integer;
begin
  if n < 0 then begin
  panic('power must be a positive integer');
end;
  if n = 0 then begin
  exit(makeDual(1, []));
end;
  dual_pow_res := x;
  dual_pow_i := 1;
  while dual_pow_i < n do begin
  dual_pow_res := dual_mul(dual_pow_res, x);
  dual_pow_i := dual_pow_i + 1;
end;
  exit(dual_pow_res);
end;
function factorial(n: integer): real;
var
  factorial_res: real;
  factorial_i: integer;
begin
  factorial_res := 1;
  factorial_i := 2;
  while factorial_i <= n do begin
  factorial_res := factorial_res * Double(factorial_i);
  factorial_i := factorial_i + 1;
end;
  exit(factorial_res);
end;
function differentiate(func: FuncType1; position: real; order: integer): real;
var
  differentiate_d: Dual;
  differentiate_result_: Dual;
begin
  differentiate_d := make_dual(position, 1);
  differentiate_result_ := func(differentiate_d);
  if order = 0 then begin
  exit(differentiate_result_.real);
end;
  exit(differentiate_result_.duals[order - 1] * factorial(order));
end;
procedure test_differentiate();
var
  test_differentiate_y: Dual;
  function f1(x: Dual): Dual;
begin
  exit(dual_pow(x, 2));
end;
  function f2(x: Dual): Dual;
begin
  exit(dual_mul(dual_pow(x, 2), dual_pow(x, 4)));
end;
  function f3(test_differentiate_y: Dual): Dual;
begin
  exit(dual_mul_real(dual_pow(dual_add_real(test_differentiate_y, 3), 6), 0.5));
end;
  function f4(test_differentiate_y: Dual): Dual;
begin
  exit(dual_pow(test_differentiate_y, 2));
end;
begin
  if differentiate(@f1, 2, 2) <> 2 then begin
  panic('f1 failed');
end;
  if differentiate(@f2, 9, 2) <> 196830 then begin
  panic('f2 failed');
end;
  if differentiate(@f3, 3.5, 4) <> 7605 then begin
  panic('f3 failed');
end;
  if differentiate(@f4, 4, 3) <> 0 then begin
  panic('f4 failed');
end;
end;
procedure main();
var
  main_res: real;
  main_y: Dual;
  function f(main_y: Dual): Dual;
begin
  exit(dual_mul(dual_pow(main_y, 2), dual_pow(main_y, 4)));
end;
begin
  test_differentiate();
  main_res := differentiate(@f, 9, 2);
  writeln(main_res);
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
