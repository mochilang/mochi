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
  num: integer;
function multiplicative_persistence(num: integer): integer; forward;
function additive_persistence(num: integer): integer; forward;
procedure test_persistence(); forward;
procedure main(); forward;
function multiplicative_persistence(num: integer): integer;
var
  multiplicative_persistence_steps: integer;
  multiplicative_persistence_n: integer;
  multiplicative_persistence_product: integer;
  multiplicative_persistence_temp: integer;
  multiplicative_persistence_digit: integer;
begin
  if num < 0 then begin
  panic('multiplicative_persistence() does not accept negative values');
end;
  multiplicative_persistence_steps := 0;
  multiplicative_persistence_n := num;
  while multiplicative_persistence_n >= 10 do begin
  multiplicative_persistence_product := 1;
  multiplicative_persistence_temp := multiplicative_persistence_n;
  while multiplicative_persistence_temp > 0 do begin
  multiplicative_persistence_digit := multiplicative_persistence_temp mod 10;
  multiplicative_persistence_product := multiplicative_persistence_product * multiplicative_persistence_digit;
  multiplicative_persistence_temp := multiplicative_persistence_temp div 10;
end;
  multiplicative_persistence_n := multiplicative_persistence_product;
  multiplicative_persistence_steps := multiplicative_persistence_steps + 1;
end;
  exit(multiplicative_persistence_steps);
end;
function additive_persistence(num: integer): integer;
var
  additive_persistence_steps: integer;
  additive_persistence_n: integer;
  additive_persistence_total: integer;
  additive_persistence_temp: integer;
  additive_persistence_digit: integer;
begin
  if num < 0 then begin
  panic('additive_persistence() does not accept negative values');
end;
  additive_persistence_steps := 0;
  additive_persistence_n := num;
  while additive_persistence_n >= 10 do begin
  additive_persistence_total := 0;
  additive_persistence_temp := additive_persistence_n;
  while additive_persistence_temp > 0 do begin
  additive_persistence_digit := additive_persistence_temp mod 10;
  additive_persistence_total := additive_persistence_total + additive_persistence_digit;
  additive_persistence_temp := additive_persistence_temp div 10;
end;
  additive_persistence_n := additive_persistence_total;
  additive_persistence_steps := additive_persistence_steps + 1;
end;
  exit(additive_persistence_steps);
end;
procedure test_persistence();
begin
  if multiplicative_persistence(217) <> 2 then begin
  panic('multiplicative_persistence failed');
end;
  if additive_persistence(199) <> 3 then begin
  panic('additive_persistence failed');
end;
end;
procedure main();
begin
  test_persistence();
  writeln(IntToStr(multiplicative_persistence(217)));
  writeln(IntToStr(additive_persistence(199)));
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
