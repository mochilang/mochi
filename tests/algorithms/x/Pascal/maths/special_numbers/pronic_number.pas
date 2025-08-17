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
  n: integer;
function int_sqrt(n: integer): integer; forward;
function is_pronic(n: integer): boolean; forward;
procedure test_is_pronic(); forward;
procedure main(); forward;
function int_sqrt(n: integer): integer;
var
  int_sqrt_r: integer;
begin
  int_sqrt_r := 0;
  while ((int_sqrt_r + 1) * (int_sqrt_r + 1)) <= n do begin
  int_sqrt_r := int_sqrt_r + 1;
end;
  exit(int_sqrt_r);
end;
function is_pronic(n: integer): boolean;
var
  is_pronic_root: integer;
begin
  if n < 0 then begin
  exit(false);
end;
  if (n mod 2) <> 0 then begin
  exit(false);
end;
  is_pronic_root := int_sqrt(n);
  exit(n = (is_pronic_root * (is_pronic_root + 1)));
end;
procedure test_is_pronic();
begin
  if is_pronic(-1) then begin
  panic('-1 should not be pronic');
end;
  if not is_pronic(0) then begin
  panic('0 should be pronic');
end;
  if not is_pronic(2) then begin
  panic('2 should be pronic');
end;
  if is_pronic(5) then begin
  panic('5 should not be pronic');
end;
  if not is_pronic(6) then begin
  panic('6 should be pronic');
end;
  if is_pronic(8) then begin
  panic('8 should not be pronic');
end;
  if not is_pronic(30) then begin
  panic('30 should be pronic');
end;
  if is_pronic(32) then begin
  panic('32 should not be pronic');
end;
  if not is_pronic(2147441940) then begin
  panic('2147441940 should be pronic');
end;
end;
procedure main();
begin
  test_is_pronic();
  writeln(Ord(is_pronic(56)));
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
