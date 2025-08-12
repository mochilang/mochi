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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  p: integer;
  a: integer;
  b: integer;
  left: integer;
  right_fast: integer;
  right_naive: integer;
  mod_: integer;
  n: integer;
function binary_exponentiation(a: integer; n: integer; mod_: integer): integer; forward;
function naive_exponent_mod(a: integer; n: integer; mod_: integer): integer; forward;
procedure print_bool(b: boolean); forward;
function binary_exponentiation(a: integer; n: integer; mod_: integer): integer;
var
  binary_exponentiation_b: integer;
begin
  if n = 0 then begin
  exit(1);
end;
  if (n mod 2) = 1 then begin
  exit((binary_exponentiation(a, n - 1, mod_) * a) mod mod_);
end;
  binary_exponentiation_b := binary_exponentiation(a, n div 2, mod_);
  exit((binary_exponentiation_b * binary_exponentiation_b) mod mod_);
end;
function naive_exponent_mod(a: integer; n: integer; mod_: integer): integer;
var
  naive_exponent_mod_result_: integer;
  naive_exponent_mod_i: integer;
begin
  naive_exponent_mod_result_ := 1;
  naive_exponent_mod_i := 0;
  while naive_exponent_mod_i < n do begin
  naive_exponent_mod_result_ := (naive_exponent_mod_result_ * a) mod mod_;
  naive_exponent_mod_i := naive_exponent_mod_i + 1;
end;
  exit(naive_exponent_mod_result_);
end;
procedure print_bool(b: boolean);
begin
  if b then begin
  writeln(Ord(true));
end else begin
  writeln(Ord(false));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  p := 701;
  a := 1000000000;
  b := 10;
  left := (a div b) mod p;
  right_fast := (a * binary_exponentiation(b, p - 2, p)) mod p;
  print_bool(left = right_fast);
  right_naive := (a * naive_exponent_mod(b, p - 2, p)) mod p;
  print_bool(left = right_naive);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
