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
  doors: array of boolean;
  i: integer;
  pass: integer;
  idx: integer;
  row: integer;
  line: string;
  col: integer;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _now();
  doors := [];
  for i := 0 to (100 - 1) do begin
  doors := concat(doors, [false]);
end;
  for pass := 1 to (101 - 1) do begin
  idx := pass - 1;
  while idx < 100 do begin
  doors[idx] := not doors[idx];
  idx := idx + pass;
end;
end;
  for row := 0 to (10 - 1) do begin
  line := '';
  for col := 0 to (10 - 1) do begin
  idx := (row * 10) + col;
  if doors[idx] then begin
  line := line + '1';
end else begin
  line := line + '0';
end;
  if col < 9 then begin
  line := line + ' ';
end;
end;
  writeln(line);
end;
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
