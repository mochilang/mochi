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
function d2d(d: real): real; forward;
function g2g(g: real): real; forward;
function m2m(m: real): real; forward;
function r2r(r: real): real; forward;
function d2g(d: real): real; forward;
function d2m(d: real): real; forward;
function d2r(d: real): real; forward;
function g2d(g: real): real; forward;
function g2m(g: real): real; forward;
function g2r(g: real): real; forward;
function m2d(m: real): real; forward;
function m2g(m: real): real; forward;
function m2r(m: real): real; forward;
function r2d(r: real): real; forward;
function r2g(r: real): real; forward;
function r2m(r: real): real; forward;
procedure main(); forward;
function d2d(d: real): real;
begin
  exit(d mod 360);
end;
function g2g(g: real): real;
begin
  exit(g mod 400);
end;
function m2m(m: real): real;
begin
  exit(m mod 6400);
end;
function r2r(r: real): real;
begin
  exit(r mod (2 * 3.141592653589793));
end;
function d2g(d: real): real;
begin
  exit((d2d(d) * 400) / 360);
end;
function d2m(d: real): real;
begin
  exit((d2d(d) * 6400) / 360);
end;
function d2r(d: real): real;
begin
  exit((d2d(d) * 3.141592653589793) / 180);
end;
function g2d(g: real): real;
begin
  exit((g2g(g) * 360) / 400);
end;
function g2m(g: real): real;
begin
  exit((g2g(g) * 6400) / 400);
end;
function g2r(g: real): real;
begin
  exit((g2g(g) * 3.141592653589793) / 200);
end;
function m2d(m: real): real;
begin
  exit((m2m(m) * 360) / 6400);
end;
function m2g(m: real): real;
begin
  exit((m2m(m) * 400) / 6400);
end;
function m2r(m: real): real;
begin
  exit((m2m(m) * 3.141592653589793) / 3200);
end;
function r2d(r: real): real;
begin
  exit((r2r(r) * 180) / 3.141592653589793);
end;
function r2g(r: real): real;
begin
  exit((r2r(r) * 200) / 3.141592653589793);
end;
function r2m(r: real): real;
begin
  exit((r2r(r) * 3200) / 3.141592653589793);
end;
procedure main();
var
  main_angles: array of real;
  main_a: integer;
begin
  main_angles := [-2, -1, 0, 1, 2, 6.2831853, 16, 57.2957795, 359, 399, 6399, 1e+06];
  writeln('degrees normalized_degs gradians mils radians');
  for main_a in main_angles do begin
  writeln((((((((IntToStr(main_a) + ' ') + FloatToStr(d2d(main_a))) + ' ') + FloatToStr(d2g(main_a))) + ' ') + FloatToStr(d2m(main_a))) + ' ') + FloatToStr(d2r(main_a)));
end;
  writeln('' + #10 + 'gradians normalized_grds degrees mils radians');
  for main_a in main_angles do begin
  writeln((((((((IntToStr(main_a) + ' ') + FloatToStr(g2g(main_a))) + ' ') + FloatToStr(g2d(main_a))) + ' ') + FloatToStr(g2m(main_a))) + ' ') + FloatToStr(g2r(main_a)));
end;
  writeln('' + #10 + 'mils normalized_mils degrees gradians radians');
  for main_a in main_angles do begin
  writeln((((((((IntToStr(main_a) + ' ') + FloatToStr(m2m(main_a))) + ' ') + FloatToStr(m2d(main_a))) + ' ') + FloatToStr(m2g(main_a))) + ' ') + FloatToStr(m2r(main_a)));
end;
  writeln('' + #10 + 'radians normalized_rads degrees gradians mils');
  for main_a in main_angles do begin
  writeln((((((((IntToStr(main_a) + ' ') + FloatToStr(r2r(main_a))) + ' ') + FloatToStr(r2d(main_a))) + ' ') + FloatToStr(r2g(main_a))) + ' ') + FloatToStr(r2m(main_a)));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
