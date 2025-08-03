{$mode objfpc}
program Main;
uses SysUtils;
type Point = record
  x: integer;
  y: integer;
end;
type PointArray = array of Point;
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
function makePoint(x: integer; y: integer): Point; forward;
function absi(x: integer): integer; forward;
function bresenham(x0: integer; y0: integer; x1: integer; y1: integer): PointArray; forward;
procedure main(); forward;
function makePoint(x: integer; y: integer): Point;
begin
  Result.x := x;
  Result.y := y;
end;
function absi(x: integer): integer;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function bresenham(x0: integer; y0: integer; x1: integer; y1: integer): PointArray;
var
  bresenham_dx: integer;
  bresenham_dy: integer;
  bresenham_sx: integer;
  bresenham_sy: integer;
  bresenham_err: integer;
  bresenham_pts: array of Point;
  bresenham_e2: integer;
begin
  bresenham_dx := absi(x1 - x0);
  bresenham_dy := absi(y1 - y0);
  bresenham_sx := -1;
  if x0 < x1 then begin
  bresenham_sx := 1;
end;
  bresenham_sy := -1;
  if y0 < y1 then begin
  bresenham_sy := 1;
end;
  bresenham_err := bresenham_dx - bresenham_dy;
  bresenham_pts := [];
  while true do begin
  bresenham_pts := concat(bresenham_pts, [makePoint(x0, y0)]);
  if (x0 = x1) and (y0 = y1) then begin
  break;
end;
  bresenham_e2 := 2 * bresenham_err;
  if bresenham_e2 > -bresenham_dy then begin
  bresenham_err := bresenham_err - bresenham_dy;
  x0 := x0 + bresenham_sx;
end;
  if bresenham_e2 < bresenham_dx then begin
  bresenham_err := bresenham_err + bresenham_dx;
  y0 := y0 + bresenham_sy;
end;
end;
  exit(bresenham_pts);
end;
procedure main();
var
  main_pts: PointArray;
  main_i: integer;
  main_p: Point;
begin
  main_pts := bresenham(0, 0, 6, 4);
  main_i := 0;
  while main_i < Length(main_pts) do begin
  main_p := main_pts[main_i];
  writeln(((('(' + IntToStr(main_p.x)) + ',') + IntToStr(main_p.y)) + ')');
  main_i := main_i + 1;
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
