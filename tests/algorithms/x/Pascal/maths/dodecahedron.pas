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
  b: real;
  a: real;
  eps: real;
  edge: integer;
  num: real;
  x: real;
function sqrtApprox(x: real): real; forward;
function abs_val(num: real): real; forward;
function approx_equal(a: real; b: real; eps: real): boolean; forward;
function dodecahedron_surface_area(edge: integer): real; forward;
function dodecahedron_volume(edge: integer): real; forward;
procedure test_dodecahedron(); forward;
procedure main(); forward;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function abs_val(num: real): real;
begin
  if num < 0 then begin
  exit(-num);
end;
  exit(num);
end;
function approx_equal(a: real; b: real; eps: real): boolean;
begin
  exit(abs_val(a - b) < eps);
end;
function dodecahedron_surface_area(edge: integer): real;
var
  dodecahedron_surface_area_term: real;
  dodecahedron_surface_area_e: real;
begin
  if edge <= 0 then begin
  panic('Length must be a positive.');
end;
  dodecahedron_surface_area_term := sqrtApprox(25 + (10 * sqrtApprox(5)));
  dodecahedron_surface_area_e := Double(edge);
  exit(((3 * dodecahedron_surface_area_term) * dodecahedron_surface_area_e) * dodecahedron_surface_area_e);
end;
function dodecahedron_volume(edge: integer): real;
var
  dodecahedron_volume_term: real;
  dodecahedron_volume_e: real;
begin
  if edge <= 0 then begin
  panic('Length must be a positive.');
end;
  dodecahedron_volume_term := (15 + (7 * sqrtApprox(5))) / 4;
  dodecahedron_volume_e := Double(edge);
  exit(((dodecahedron_volume_term * dodecahedron_volume_e) * dodecahedron_volume_e) * dodecahedron_volume_e);
end;
procedure test_dodecahedron();
begin
  if not approx_equal(dodecahedron_surface_area(5), 516.1432201766901, 0.0001) then begin
  panic('surface area 5 failed');
end;
  if not approx_equal(dodecahedron_surface_area(10), 2064.5728807067603, 0.0001) then begin
  panic('surface area 10 failed');
end;
  if not approx_equal(dodecahedron_volume(5), 957.8898700780791, 0.0001) then begin
  panic('volume 5 failed');
end;
  if not approx_equal(dodecahedron_volume(10), 7663.118960624633, 0.0001) then begin
  panic('volume 10 failed');
end;
end;
procedure main();
begin
  test_dodecahedron();
  writeln(dodecahedron_surface_area(5));
  writeln(dodecahedron_volume(5));
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
