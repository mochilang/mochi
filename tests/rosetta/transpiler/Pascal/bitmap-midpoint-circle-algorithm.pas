{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type StrArrayArray = array of StrArray;
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
  size: integer;
  g: StrArrayArray;
  x: integer;
  y: integer;
  row: StrArray;
function initGrid(size: integer): StrArrayArray; forward;
procedure set_(g: StrArrayArray; x: integer; y: integer); forward;
function circle(r: integer): StrArrayArray; forward;
function trimRight(row: StrArray): string; forward;
function initGrid(size: integer): StrArrayArray;
var
  initGrid_g: array of StrArray;
  initGrid_y: integer;
  initGrid_row: array of string;
  initGrid_x: integer;
begin
  initGrid_g := [];
  initGrid_y := 0;
  while initGrid_y < size do begin
  initGrid_row := [];
  initGrid_x := 0;
  while initGrid_x < size do begin
  initGrid_row := concat(initGrid_row, [' ']);
  initGrid_x := initGrid_x + 1;
end;
  initGrid_g := concat(initGrid_g, [initGrid_row]);
  initGrid_y := initGrid_y + 1;
end;
  exit(initGrid_g);
end;
procedure set_(g: StrArrayArray; x: integer; y: integer);
begin
  if (((x >= 0) and (x < Length(g[0]))) and (y >= 0)) and (y < Length(g)) then begin
  g[y][x] := '#';
end;
end;
function circle(r: integer): StrArrayArray;
var
  circle_err: integer;
begin
  size := (r * 2) + 1;
  g := initGrid(size);
  x := r;
  y := 0;
  circle_err := 1 - r;
  while y <= x do begin
  set_(g, r + x, r + y);
  set_(g, r + y, r + x);
  set_(g, r - x, r + y);
  set_(g, r - y, r + x);
  set_(g, r - x, r - y);
  set_(g, r - y, r - x);
  set_(g, r + x, r - y);
  set_(g, r + y, r - x);
  y := y + 1;
  if circle_err < 0 then begin
  circle_err := (circle_err + (2 * y)) + 1;
end else begin
  x := x - 1;
  circle_err := (circle_err + (2 * (y - x))) + 1;
end;
end;
  exit(g);
end;
function trimRight(row: StrArray): string;
var
  trimRight_end: integer;
  trimRight_s: string;
  trimRight_i: integer;
begin
  trimRight_end := Length(row);
  while (trimRight_end > 0) and (row[trimRight_end - 1] = ' ') do begin
  trimRight_end := trimRight_end - 1;
end;
  trimRight_s := '';
  trimRight_i := 0;
  while trimRight_i < trimRight_end do begin
  trimRight_s := trimRight_s + row[trimRight_i];
  trimRight_i := trimRight_i + 1;
end;
  exit(trimRight_s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  g := circle(10);
  for row in g do begin
  writeln(trimRight(row));
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
