{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  sinApprox_term: real;
  sinApprox_sum: real;
  sinApprox_n: integer;
  sinApprox_denom: real;
  dt: real;
  s: real;
  t1: real;
  k1: real;
  i: integer;
  t2: real;
  k2: real;
  i2: integer;
function sinApprox(x: real): real; forward;
function sinApprox(x: real): real;
begin
  sinApprox_term := x;
  sinApprox_sum := x;
  sinApprox_n := 1;
  while sinApprox_n <= 12 do begin
  sinApprox_denom := Double((2 * sinApprox_n) * ((2 * sinApprox_n) + 1));
  sinApprox_term := ((-sinApprox_term * x) * x) / sinApprox_denom;
  sinApprox_sum := sinApprox_sum + sinApprox_term;
  sinApprox_n := sinApprox_n + 1;
end;
  exit(sinApprox_sum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  dt := 0.01;
  s := 0;
  t1 := 0;
  k1 := sinApprox(0);
  i := 1;
  while i <= 200 do begin
  t2 := Double(i) * dt;
  k2 := sinApprox(t2 * PI);
  s := s + (((k1 + k2) * 0.5) * (t2 - t1));
  t1 := t2;
  k1 := k2;
  i := i + 1;
end;
  i2 := 1;
  while i2 <= 50 do begin
  t2 := 2 + (Double(i2) * dt);
  k2 := 0;
  s := s + (((k1 + k2) * 0.5) * (t2 - t1));
  t1 := t2;
  k1 := k2;
  i2 := i2 + 1;
end;
  writeln(s);
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
