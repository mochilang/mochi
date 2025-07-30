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
  testCases: array of array of real;
  tc: integer;
function angleDiff(b1: real; b2: real): real; forward;
function angleDiff(b1: real; b2: real): real;
var
  angleDiff_diff: real;
begin
  angleDiff_diff := b2 - b1;
  exit(((((angleDiff_diff mod 360) + 360) + 180) mod 360) - 180);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  testCases := [[20, 45], [0 - 45, 45], [0 - 85, 90], [0 - 95, 90], [0 - 45, 125], [0 - 45, 145], [29.4803, 0 - 88.6381], [0 - 78.3251, 0 - 159.036], [0 - 70099.74233810938, 29840.67437876723], [0 - 165313.6666297357, 33693.9894517456], [1174.8380510598456, 0 - 154146.66490124757], [60175.77306795546, 42213.07192354373]];
  for tc in testCases do begin
  writeln(angleDiff(tc[0], tc[1]));
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
