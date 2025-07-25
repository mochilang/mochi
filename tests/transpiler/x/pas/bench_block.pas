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
    _now := Integer(GetTickCount64());
  end;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  n: integer;
  s: integer;
  i: integer;
begin
  init_now();
  bench_start_0 := _now();
  n := 1000;
  s := 0;
  for i := 1 to (n - 1) do begin
  s := s + i;
end;
  bench_dur_0 := (_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln('  "memory_bytes": 0,');
  writeln(('  "name": "' + 'simple') + '"');
  writeln('}');
end.
