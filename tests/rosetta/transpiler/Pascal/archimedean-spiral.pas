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
  degreesIncr: real;
  turns: real;
  stop: real;
  width: real;
  centre: real;
  a: real;
  b: real;
  theta: real;
  count: integer;
  r: real;
  x: real;
  y: real;
function sinApprox(x: real): real; forward;
function cosApprox(x: real): real; forward;
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
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  degreesIncr := (0.1 * PI) / 180;
  turns := 2;
  stop := ((360 * turns) * 10) * degreesIncr;
  width := 600;
  centre := width / 2;
  a := 1;
  b := 20;
  theta := 0;
  count := 0;
  while theta < stop do begin
  r := a + (b * theta);
  x := r * cosApprox(theta);
  y := r * sinApprox(theta);
  if (count mod 100) = 0 then begin
  writeln((FloatToStr(centre + x) + ',') + FloatToStr(centre - y));
end;
  theta := theta + degreesIncr;
  count := count + 1;
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
