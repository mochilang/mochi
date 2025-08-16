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
  a: integer;
  b: integer;
  modulus: integer;
function binary_multiply(a: integer; b: integer): integer; forward;
function binary_mod_multiply(a: integer; b: integer; modulus: integer): integer; forward;
procedure main(); forward;
function binary_multiply(a: integer; b: integer): integer;
var
  binary_multiply_x: integer;
  binary_multiply_y: integer;
  binary_multiply_res: integer;
begin
  binary_multiply_x := a;
  binary_multiply_y := b;
  binary_multiply_res := 0;
  while binary_multiply_y > 0 do begin
  if (binary_multiply_y mod 2) = 1 then begin
  binary_multiply_res := binary_multiply_res + binary_multiply_x;
end;
  binary_multiply_x := binary_multiply_x + binary_multiply_x;
  binary_multiply_y := Trunc(binary_multiply_y div 2);
end;
  exit(binary_multiply_res);
end;
function binary_mod_multiply(a: integer; b: integer; modulus: integer): integer;
var
  binary_mod_multiply_x: integer;
  binary_mod_multiply_y: integer;
  binary_mod_multiply_res: integer;
begin
  binary_mod_multiply_x := a;
  binary_mod_multiply_y := b;
  binary_mod_multiply_res := 0;
  while binary_mod_multiply_y > 0 do begin
  if (binary_mod_multiply_y mod 2) = 1 then begin
  binary_mod_multiply_res := ((binary_mod_multiply_res mod modulus) + (binary_mod_multiply_x mod modulus)) mod modulus;
end;
  binary_mod_multiply_x := binary_mod_multiply_x + binary_mod_multiply_x;
  binary_mod_multiply_y := Trunc(binary_mod_multiply_y div 2);
end;
  exit(binary_mod_multiply_res mod modulus);
end;
procedure main();
begin
  writeln(IntToStr(binary_multiply(2, 3)));
  writeln(IntToStr(binary_multiply(5, 0)));
  writeln(IntToStr(binary_mod_multiply(2, 3, 5)));
  writeln(IntToStr(binary_mod_multiply(10, 5, 13)));
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
