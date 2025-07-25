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
  door: integer;
  incrementer: integer;
  current: integer;
  line: string;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _now();
  door := 1;
  incrementer := 0;
  for current := 1 to (101 - 1) do begin
  line := ('Door ' + IntToStr(current)) + ' ';
  if current = door then begin
  line := line + 'Open';
  incrementer := incrementer + 1;
  door := (door + (2 * incrementer)) + 1;
end else begin
  line := line + 'Closed';
end;
  writeln(line);
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
