{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
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
  grid: array of StrArray;
  row: StrArray;
  line: string;
  ch: string;
procedure flood(x: integer; y: integer; repl: string); forward;
procedure flood(x: integer; y: integer; repl: string);
var
  flood_target: string;
  procedure ff(flood_px: integer; flood_py: integer);
begin
  if (((flood_px < 0) or (flood_py < 0)) or (flood_py >= Length(grid))) or (flood_px >= Length(grid[0])) then begin
  exit();
end;
  if grid[flood_py][flood_px] <> flood_target then begin
  exit();
end;
  grid[flood_py][flood_px] := repl;
  ff(flood_px - 1, flood_py);
  ff(flood_px + 1, flood_py);
  ff(flood_px, flood_py - 1);
  ff(flood_px, flood_py + 1);
end;
begin
  flood_target := grid[y][x];
  if flood_target = repl then begin
  exit();
end;
  ff(x, y);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  grid := [['.', '.', '.', '.', '.'], ['.', '#', '#', '#', '.'], ['.', '#', '.', '#', '.'], ['.', '#', '#', '#', '.'], ['.', '.', '.', '.', '.']];
  flood(2, 2, 'o');
  for row in grid do begin
  line := '';
  for ch in row do begin
  line := line + ch;
end;
  writeln(line);
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
