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
  PI: real;
  a: real;
  b: real;
  degree: real;
  x: real;
function radians(degree: real): real; forward;
function abs_float(x: real): real; forward;
function almost_equal(a: real; b: real): boolean; forward;
procedure test_radians(); forward;
procedure main(); forward;
function radians(degree: real): real;
begin
  exit(degree / (180 / PI));
end;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function almost_equal(a: real; b: real): boolean;
begin
  exit(abs_float(a - b) <= 1e-08);
end;
procedure test_radians();
begin
  if not almost_equal(radians(180), PI) then begin
  panic('radians 180 failed');
end;
  if not almost_equal(radians(92), 1.6057029118347832) then begin
  panic('radians 92 failed');
end;
  if not almost_equal(radians(274), 4.782202150464463) then begin
  panic('radians 274 failed');
end;
  if not almost_equal(radians(109.82), 1.9167205845401725) then begin
  panic('radians 109.82 failed');
end;
end;
procedure main();
begin
  test_radians();
  writeln(FloatToStr(radians(180)));
  writeln(FloatToStr(radians(92)));
  writeln(FloatToStr(radians(274)));
  writeln(FloatToStr(radians(109.82)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
