{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
  a: integer;
  b: integer;
function gcd(a: integer; b: integer): integer; forward;
function lcm_slow(a: integer; b: integer): integer; forward;
function lcm_fast(a: integer; b: integer): integer; forward;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_temp: integer;
begin
  gcd_x := IfThen(a >= 0, a, -a);
  gcd_y := IfThen(b >= 0, b, -b);
  while gcd_y <> 0 do begin
  gcd_temp := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_temp;
end;
  exit(gcd_x);
end;
function lcm_slow(a: integer; b: integer): integer;
var
  lcm_slow_max: integer;
  lcm_slow_multiple: integer;
begin
  if a >= b then begin
  lcm_slow_max := a;
end else begin
  lcm_slow_max := b;
end;
  lcm_slow_multiple := lcm_slow_max;
  while ((lcm_slow_multiple mod a) <> 0) or ((lcm_slow_multiple mod b) <> 0) do begin
  lcm_slow_multiple := lcm_slow_multiple + lcm_slow_max;
end;
  exit(lcm_slow_multiple);
end;
function lcm_fast(a: integer; b: integer): integer;
begin
  exit((a div gcd(a, b)) * b);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(lcm_slow(5, 2)));
  writeln(IntToStr(lcm_slow(12, 76)));
  writeln(IntToStr(lcm_fast(5, 2)));
  writeln(IntToStr(lcm_fast(12, 76)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
