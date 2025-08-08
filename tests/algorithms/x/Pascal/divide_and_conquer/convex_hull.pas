{$mode objfpc}
program Main;
uses SysUtils;
type Point = record
  x: integer;
  y: integer;
end;
type PointArray = array of Point;
type IntArray = array of integer;
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
  b: Point;
  o: Point;
  a: Point;
  ps: PointArray;
function makePoint(x: integer; y: integer): Point; forward;
function cross(o: Point; a: Point; b: Point): integer; forward;
function sortPoints(ps: PointArray): PointArray; forward;
function convex_hull(ps: PointArray): PointArray; forward;
function makePoint(x: integer; y: integer): Point;
begin
  Result.x := x;
  Result.y := y;
end;
function cross(o: Point; a: Point; b: Point): integer;
begin
  exit(((a.x - o.x) * (b.y - o.y)) - ((a.y - o.y) * (b.x - o.x)));
end;
function sortPoints(ps: PointArray): PointArray;
var
  sortPoints_arr: array of Point;
  sortPoints_n: integer;
  sortPoints_i: integer;
  sortPoints_j: integer;
  sortPoints_p: Point;
  sortPoints_q: Point;
begin
  sortPoints_arr := ps;
  sortPoints_n := Length(sortPoints_arr);
  sortPoints_i := 0;
  while sortPoints_i < sortPoints_n do begin
  sortPoints_j := 0;
  while sortPoints_j < (sortPoints_n - 1) do begin
  sortPoints_p := sortPoints_arr[sortPoints_j];
  sortPoints_q := sortPoints_arr[sortPoints_j + 1];
  if (sortPoints_p.x > sortPoints_q.x) or ((sortPoints_p.x = sortPoints_q.x) and (sortPoints_p.y > sortPoints_q.y)) then begin
  sortPoints_arr[sortPoints_j] := sortPoints_q;
  sortPoints_arr[sortPoints_j + 1] := sortPoints_p;
end;
  sortPoints_j := sortPoints_j + 1;
end;
  sortPoints_i := sortPoints_i + 1;
end;
  exit(sortPoints_arr);
end;
function convex_hull(ps: PointArray): PointArray;
var
  convex_hull_lower: array of Point;
  convex_hull_p: Point;
  convex_hull_upper: array of Point;
  convex_hull_i: integer;
  convex_hull_hull: array of integer;
  convex_hull_j: integer;
begin
  ps := sortPoints(ps);
  convex_hull_lower := [];
  for convex_hull_p in ps do begin
  while (Length(convex_hull_lower) >= 2) and (cross(convex_hull_lower[Length(convex_hull_lower) - 2], convex_hull_lower[Length(convex_hull_lower) - 1], convex_hull_p) <= 0) do begin
  convex_hull_lower := copy(convex_hull_lower, 0, Length(convex_hull_lower) - 1);
end;
  convex_hull_lower := concat(convex_hull_lower, [convex_hull_p]);
end;
  convex_hull_upper := [];
  convex_hull_i := Length(ps) - 1;
  while convex_hull_i >= 0 do begin
  convex_hull_p := ps[convex_hull_i];
  while (Length(convex_hull_upper) >= 2) and (cross(convex_hull_upper[Length(convex_hull_upper) - 2], convex_hull_upper[Length(convex_hull_upper) - 1], convex_hull_p) <= 0) do begin
  convex_hull_upper := copy(convex_hull_upper, 0, Length(convex_hull_upper) - 1);
end;
  convex_hull_upper := concat(convex_hull_upper, [convex_hull_p]);
  convex_hull_i := convex_hull_i - 1;
end;
  convex_hull_hull := copy(convex_hull_lower, 0, Length(convex_hull_lower) - 1);
  convex_hull_j := 0;
  while convex_hull_j < (Length(convex_hull_upper) - 1) do begin
  convex_hull_hull := concat(convex_hull_hull, IntArray([convex_hull_upper[convex_hull_j]]));
  convex_hull_j := convex_hull_j + 1;
end;
  exit(convex_hull_hull);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
