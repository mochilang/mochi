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
  L: real;
  G: real;
  dt: real;
  phi0: real;
  omega: real;
  t: real;
  step: integer;
  phi: real;
  pos: integer;
function sinApprox(x: real): real; forward;
function cosApprox(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function sinApprox(x: real): real;
var
  sinApprox_term: real;
  sinApprox_sum: real;
  sinApprox_n: integer;
  sinApprox_denom: real;
begin
  sinApprox_term := x;
  sinApprox_sum := x;
  sinApprox_n := 1;
  while sinApprox_n <= 10 do begin
  sinApprox_denom := Double((2 * sinApprox_n) * ((2 * sinApprox_n) + 1));
  sinApprox_term := ((-sinApprox_term * x) * x) / sinApprox_denom;
  sinApprox_sum := sinApprox_sum + sinApprox_term;
  sinApprox_n := sinApprox_n + 1;
end;
  exit(sinApprox_sum);
end;
function cosApprox(x: real): real;
var
  cosApprox_term: real;
  cosApprox_sum: real;
  cosApprox_n: integer;
  cosApprox_denom: real;
begin
  cosApprox_term := 1;
  cosApprox_sum := 1;
  cosApprox_n := 1;
  while cosApprox_n <= 10 do begin
  cosApprox_denom := Double(((2 * cosApprox_n) - 1) * (2 * cosApprox_n));
  cosApprox_term := ((-cosApprox_term * x) * x) / cosApprox_denom;
  cosApprox_sum := cosApprox_sum + cosApprox_term;
  cosApprox_n := cosApprox_n + 1;
end;
  exit(cosApprox_sum);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 10 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  L := 10;
  G := 9.81;
  dt := 0.2;
  phi0 := PI / 4;
  omega := sqrtApprox(G / L);
  t := 0;
  for step := 0 to (10 - 1) do begin
  phi := phi0 * cosApprox(omega * t);
  pos := Trunc((10 * sinApprox(phi)) + 0.5);
  writeln(IntToStr(pos));
  t := t + dt;
end;
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
