{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
  base: integer;
  exponent: integer;
  modulus: integer;
function binary_exp_recursive(base: real; exponent: integer): real; forward;
function binary_exp_iterative(base: real; exponent: integer): real; forward;
function binary_exp_mod_recursive(base: integer; exponent: integer; modulus: integer): integer; forward;
function binary_exp_mod_iterative(base: integer; exponent: integer; modulus: integer): integer; forward;
function binary_exp_recursive(base: real; exponent: integer): real;
var
  binary_exp_recursive_half: real;
begin
  if exponent < 0 then begin
  panic('exponent must be non-negative');
end;
  if exponent = 0 then begin
  exit(1);
end;
  if (exponent mod 2) = 1 then begin
  exit(binary_exp_recursive(base, exponent - 1) * base);
end;
  binary_exp_recursive_half := binary_exp_recursive(base, exponent div 2);
  exit(binary_exp_recursive_half * binary_exp_recursive_half);
end;
function binary_exp_iterative(base: real; exponent: integer): real;
var
  binary_exp_iterative_result_: real;
  binary_exp_iterative_b: real;
  binary_exp_iterative_e: integer;
begin
  if exponent < 0 then begin
  panic('exponent must be non-negative');
end;
  binary_exp_iterative_result_ := 1;
  binary_exp_iterative_b := base;
  binary_exp_iterative_e := exponent;
  while binary_exp_iterative_e > 0 do begin
  if (binary_exp_iterative_e mod 2) = 1 then begin
  binary_exp_iterative_result_ := binary_exp_iterative_result_ * binary_exp_iterative_b;
end;
  binary_exp_iterative_b := binary_exp_iterative_b * binary_exp_iterative_b;
  binary_exp_iterative_e := binary_exp_iterative_e div 2;
end;
  exit(binary_exp_iterative_result_);
end;
function binary_exp_mod_recursive(base: integer; exponent: integer; modulus: integer): integer;
var
  binary_exp_mod_recursive_r: integer;
begin
  if exponent < 0 then begin
  panic('exponent must be non-negative');
end;
  if modulus <= 0 then begin
  panic('modulus must be positive');
end;
  if exponent = 0 then begin
  exit(1 mod modulus);
end;
  if (exponent mod 2) = 1 then begin
  exit((binary_exp_mod_recursive(base, exponent - 1, modulus) * (base mod modulus)) mod modulus);
end;
  binary_exp_mod_recursive_r := binary_exp_mod_recursive(base, exponent div 2, modulus);
  exit((binary_exp_mod_recursive_r * binary_exp_mod_recursive_r) mod modulus);
end;
function binary_exp_mod_iterative(base: integer; exponent: integer; modulus: integer): integer;
var
  binary_exp_mod_iterative_result_: integer;
  binary_exp_mod_iterative_b: integer;
  binary_exp_mod_iterative_e: integer;
begin
  if exponent < 0 then begin
  panic('exponent must be non-negative');
end;
  if modulus <= 0 then begin
  panic('modulus must be positive');
end;
  binary_exp_mod_iterative_result_ := 1 mod modulus;
  binary_exp_mod_iterative_b := base mod modulus;
  binary_exp_mod_iterative_e := exponent;
  while binary_exp_mod_iterative_e > 0 do begin
  if (binary_exp_mod_iterative_e mod 2) = 1 then begin
  binary_exp_mod_iterative_result_ := (binary_exp_mod_iterative_result_ * binary_exp_mod_iterative_b) mod modulus;
end;
  binary_exp_mod_iterative_b := (binary_exp_mod_iterative_b * binary_exp_mod_iterative_b) mod modulus;
  binary_exp_mod_iterative_e := binary_exp_mod_iterative_e div 2;
end;
  exit(binary_exp_mod_iterative_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(binary_exp_recursive(3, 5));
  writeln(binary_exp_iterative(1.5, 4));
  writeln(binary_exp_mod_recursive(3, 4, 5));
  writeln(binary_exp_mod_iterative(11, 13, 7));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
