{$mode objfpc}
program Main;
uses SysUtils, fgl;
type PointArray = array of Point;
type QuadCurveArray = array of QuadCurve;
type QuadSplineArray = array of QuadSpline;
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
type Point = record
  x: real;
  y: real;
end;
type QuadSpline = record
  c0: real;
  c1: real;
  c2: real;
end;
type QuadCurve = record
  x: QuadSpline;
  y: QuadSpline;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function Map6(findIntersects_p1: QuadCurve; findIntersects_q1: QuadCurve): specialize TFPGMap<string, Variant>; forward;
function Map5(findIntersects_p1: QuadCurve; findIntersects_q0: QuadCurve): specialize TFPGMap<string, Variant>; forward;
function Map4(findIntersects_p0: QuadCurve; findIntersects_q1: QuadCurve): specialize TFPGMap<string, Variant>; forward;
function Map3(findIntersects_p0: QuadCurve; findIntersects_q0: QuadCurve): specialize TFPGMap<string, Variant>; forward;
function Map2(p: QuadCurve; q: QuadCurve): specialize TFPGMap<string, Variant>; forward;
function Map1(testIntersect_accept: boolean; testIntersect_exclude: boolean; testIntersect_inter: Point): specialize TFPGMap<string, Variant>; forward;
function makeQuadCurve(x: QuadSpline; y: QuadSpline): QuadCurve; forward;
function makeQuadSpline(c0: real; c1: real; c2: real): QuadSpline; forward;
function makePoint(x: real; y: real): Point; forward;
function absf(x: real): real; forward;
function maxf(a: real; b: real): real; forward;
function minf(a: real; b: real): real; forward;
function max3(a: real; b: real; c: real): real; forward;
function min3(a: real; b: real; c: real): real; forward;
function subdivideQuadSpline(q: QuadSpline; t: real): QuadSplineArray; forward;
function subdivideQuadCurve(q: QuadCurve; t: real): QuadCurveArray; forward;
function rectsOverlap(xa0: real; ya0: real; xa1: real; ya1: real; xb0: real; yb0: real; xb1: real; yb1: real): boolean; forward;
function testIntersect(p: QuadCurve; q: QuadCurve; tol: real): specialize TFPGMap<string, Variant>; forward;
function seemsToBeDuplicate(pts: PointArray; xy: Point; spacing: real): boolean; forward;
function findIntersects(p: QuadCurve; q: QuadCurve; tol: real; spacing: real): PointArray; forward;
procedure main(); forward;
function Map6(findIntersects_p1: QuadCurve; findIntersects_q1: QuadCurve): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('p', Variant(findIntersects_p1));
  Result.AddOrSetData('q', Variant(findIntersects_q1));
end;
function Map5(findIntersects_p1: QuadCurve; findIntersects_q0: QuadCurve): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('p', Variant(findIntersects_p1));
  Result.AddOrSetData('q', Variant(findIntersects_q0));
end;
function Map4(findIntersects_p0: QuadCurve; findIntersects_q1: QuadCurve): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('p', Variant(findIntersects_p0));
  Result.AddOrSetData('q', Variant(findIntersects_q1));
end;
function Map3(findIntersects_p0: QuadCurve; findIntersects_q0: QuadCurve): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('p', Variant(findIntersects_p0));
  Result.AddOrSetData('q', Variant(findIntersects_q0));
end;
function Map2(p: QuadCurve; q: QuadCurve): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('p', Variant(p));
  Result.AddOrSetData('q', Variant(q));
end;
function Map1(testIntersect_accept: boolean; testIntersect_exclude: boolean; testIntersect_inter: Point): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('exclude', Variant(testIntersect_exclude));
  Result.AddOrSetData('accept', Variant(testIntersect_accept));
  Result.AddOrSetData('intersect', Variant(testIntersect_inter));
end;
function makeQuadCurve(x: QuadSpline; y: QuadSpline): QuadCurve;
begin
  Result.x := x;
  Result.y := y;
end;
function makeQuadSpline(c0: real; c1: real; c2: real): QuadSpline;
begin
  Result.c0 := c0;
  Result.c1 := c1;
  Result.c2 := c2;
end;
function makePoint(x: real; y: real): Point;
begin
  Result.x := x;
  Result.y := y;
end;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function maxf(a: real; b: real): real;
begin
  if a > b then begin
  exit(a);
end;
  exit(b);
end;
function minf(a: real; b: real): real;
begin
  if a < b then begin
  exit(a);
end;
  exit(b);
end;
function max3(a: real; b: real; c: real): real;
var
  max3_m: real;
begin
  max3_m := a;
  if b > max3_m then begin
  max3_m := b;
end;
  if c > max3_m then begin
  max3_m := c;
end;
  exit(max3_m);
end;
function min3(a: real; b: real; c: real): real;
var
  min3_m: real;
begin
  min3_m := a;
  if b < min3_m then begin
  min3_m := b;
end;
  if c < min3_m then begin
  min3_m := c;
end;
  exit(min3_m);
end;
function subdivideQuadSpline(q: QuadSpline; t: real): QuadSplineArray;
var
  subdivideQuadSpline_s: real;
  subdivideQuadSpline_u: QuadSpline;
  subdivideQuadSpline_v: QuadSpline;
begin
  subdivideQuadSpline_s := 1 - t;
  subdivideQuadSpline_u := makeQuadSpline(q.c0, 0, 0);
  subdivideQuadSpline_v := makeQuadSpline(0, 0, q.c2);
  subdivideQuadSpline_u.c1 := (subdivideQuadSpline_s * q.c0) + (t * q.c1);
  subdivideQuadSpline_v.c1 := (subdivideQuadSpline_s * q.c1) + (t * q.c2);
  subdivideQuadSpline_u.c2 := (subdivideQuadSpline_s * subdivideQuadSpline_u.c1) + (t * subdivideQuadSpline_v.c1);
  subdivideQuadSpline_v.c0 := subdivideQuadSpline_u.c2;
  exit([subdivideQuadSpline_u, subdivideQuadSpline_v]);
end;
function subdivideQuadCurve(q: QuadCurve; t: real): QuadCurveArray;
var
  subdivideQuadCurve_xs: QuadSplineArray;
  subdivideQuadCurve_ys: QuadSplineArray;
  subdivideQuadCurve_u: QuadCurve;
  subdivideQuadCurve_v: QuadCurve;
begin
  subdivideQuadCurve_xs := subdivideQuadSpline(q.x, t);
  subdivideQuadCurve_ys := subdivideQuadSpline(q.y, t);
  subdivideQuadCurve_u := makeQuadCurve(subdivideQuadCurve_xs[0], subdivideQuadCurve_ys[0]);
  subdivideQuadCurve_v := makeQuadCurve(subdivideQuadCurve_xs[1], subdivideQuadCurve_ys[1]);
  exit([subdivideQuadCurve_u, subdivideQuadCurve_v]);
end;
function rectsOverlap(xa0: real; ya0: real; xa1: real; ya1: real; xb0: real; yb0: real; xb1: real; yb1: real): boolean;
begin
  exit((((xb0 <= xa1) and (xa0 <= xb1)) and (yb0 <= ya1)) and (ya0 <= yb1));
end;
function testIntersect(p: QuadCurve; q: QuadCurve; tol: real): specialize TFPGMap<string, Variant>;
var
  testIntersect_pxmin: real;
  testIntersect_pymin: real;
  testIntersect_pxmax: real;
  testIntersect_pymax: real;
  testIntersect_qxmin: real;
  testIntersect_qymin: real;
  testIntersect_qxmax: real;
  testIntersect_qymax: real;
  testIntersect_exclude: boolean;
  testIntersect_accept: boolean;
  testIntersect_inter: Point;
  testIntersect_xmin: real;
  testIntersect_xmax: real;
  testIntersect_ymin: real;
  testIntersect_ymax: real;
begin
  testIntersect_pxmin := min3(p.x.c0, p.x.c1, p.x.c2);
  testIntersect_pymin := min3(p.y.c0, p.y.c1, p.y.c2);
  testIntersect_pxmax := max3(p.x.c0, p.x.c1, p.x.c2);
  testIntersect_pymax := max3(p.y.c0, p.y.c1, p.y.c2);
  testIntersect_qxmin := min3(q.x.c0, q.x.c1, q.x.c2);
  testIntersect_qymin := min3(q.y.c0, q.y.c1, q.y.c2);
  testIntersect_qxmax := max3(q.x.c0, q.x.c1, q.x.c2);
  testIntersect_qymax := max3(q.y.c0, q.y.c1, q.y.c2);
  testIntersect_exclude := true;
  testIntersect_accept := false;
  testIntersect_inter := makePoint(0, 0);
  if rectsOverlap(testIntersect_pxmin, testIntersect_pymin, testIntersect_pxmax, testIntersect_pymax, testIntersect_qxmin, testIntersect_qymin, testIntersect_qxmax, testIntersect_qymax) then begin
  testIntersect_exclude := false;
  testIntersect_xmin := maxf(testIntersect_pxmin, testIntersect_qxmin);
  testIntersect_xmax := minf(testIntersect_pxmax, testIntersect_qxmax);
  if (testIntersect_xmax - testIntersect_xmin) <= tol then begin
  testIntersect_ymin := maxf(testIntersect_pymin, testIntersect_qymin);
  testIntersect_ymax := minf(testIntersect_pymax, testIntersect_qymax);
  if (testIntersect_ymax - testIntersect_ymin) <= tol then begin
  testIntersect_accept := true;
  testIntersect_inter.x := 0.5 * (testIntersect_xmin + testIntersect_xmax);
  testIntersect_inter.y := 0.5 * (testIntersect_ymin + testIntersect_ymax);
end;
end;
end;
  exit(Map1(testIntersect_accept, testIntersect_exclude, testIntersect_inter));
end;
function seemsToBeDuplicate(pts: PointArray; xy: Point; spacing: real): boolean;
var
  seemsToBeDuplicate_i: integer;
  seemsToBeDuplicate_pt: Point;
begin
  seemsToBeDuplicate_i := 0;
  while seemsToBeDuplicate_i < Length(pts) do begin
  seemsToBeDuplicate_pt := pts[seemsToBeDuplicate_i];
  if (absf(seemsToBeDuplicate_pt.x - xy.x) < spacing) and (absf(seemsToBeDuplicate_pt.y - xy.y) < spacing) then begin
  exit(true);
end;
  seemsToBeDuplicate_i := seemsToBeDuplicate_i + 1;
end;
  exit(false);
end;
function findIntersects(p: QuadCurve; q: QuadCurve; tol: real; spacing: real): PointArray;
var
  findIntersects_inters: array of Point;
  findIntersects_workload: array of specialize TFPGMap<string, QuadCurve>;
  findIntersects_idx: integer;
  findIntersects_work: specialize TFPGMap<string, QuadCurve>;
  findIntersects_res: specialize TFPGMap<string, Variant>;
  findIntersects_excl: integer;
  findIntersects_excl_idx: integer;
  findIntersects_acc: integer;
  findIntersects_acc_idx: integer;
  findIntersects_inter: integer;
  findIntersects_inter_idx: integer;
  findIntersects_ps: QuadCurveArray;
  findIntersects_qs: QuadCurveArray;
  findIntersects_p0: QuadCurve;
  findIntersects_p1: QuadCurve;
  findIntersects_q0: QuadCurve;
  findIntersects_q1: QuadCurve;
begin
  findIntersects_inters := [];
  findIntersects_workload := [Map2(p, q)];
  while Length(findIntersects_workload) > 0 do begin
  findIntersects_idx := Length(findIntersects_workload) - 1;
  findIntersects_work := findIntersects_workload[findIntersects_idx];
  findIntersects_workload := copy(findIntersects_workload, 0, findIntersects_idx);
  findIntersects_res := testIntersect(findIntersects_work['p'], findIntersects_work['q'], tol);
  findIntersects_excl_idx := findIntersects_res.IndexOf('exclude');
  if findIntersects_excl_idx <> -1 then begin
  findIntersects_excl := findIntersects_res.Data[findIntersects_excl_idx];
end else begin
  findIntersects_excl := 0;
end;
  findIntersects_acc_idx := findIntersects_res.IndexOf('accept');
  if findIntersects_acc_idx <> -1 then begin
  findIntersects_acc := findIntersects_res.Data[findIntersects_acc_idx];
end else begin
  findIntersects_acc := 0;
end;
  findIntersects_inter_idx := findIntersects_res.IndexOf('intersect');
  if findIntersects_inter_idx <> -1 then begin
  findIntersects_inter := findIntersects_res.Data[findIntersects_inter_idx];
end else begin
  findIntersects_inter := 0;
end;
  if findIntersects_acc then begin
  if not seemsToBeDuplicate(findIntersects_inters, findIntersects_inter, spacing) then begin
  findIntersects_inters := concat(findIntersects_inters, [findIntersects_inter]);
end;
end else begin
  if not findIntersects_excl then begin
  findIntersects_ps := subdivideQuadCurve(findIntersects_work['p'], 0.5);
  findIntersects_qs := subdivideQuadCurve(findIntersects_work['q'], 0.5);
  findIntersects_p0 := findIntersects_ps[0];
  findIntersects_p1 := findIntersects_ps[1];
  findIntersects_q0 := findIntersects_qs[0];
  findIntersects_q1 := findIntersects_qs[1];
  findIntersects_workload := concat(findIntersects_workload, [Map3(findIntersects_p0, findIntersects_q0)]);
  findIntersects_workload := concat(findIntersects_workload, [Map4(findIntersects_p0, findIntersects_q1)]);
  findIntersects_workload := concat(findIntersects_workload, [Map5(findIntersects_p1, findIntersects_q0)]);
  findIntersects_workload := concat(findIntersects_workload, [Map6(findIntersects_p1, findIntersects_q1)]);
end;
end;
end;
  exit(findIntersects_inters);
end;
procedure main();
var
  main_p: QuadCurve;
  main_q: QuadCurve;
  main_tol: real;
  main_spacing: real;
  main_inters: PointArray;
  main_i: integer;
  main_pt: Point;
begin
  main_p := makeQuadCurve((c0: -1; c1: 0; c2: 1), (c0: 0; c1: 10; c2: 0));
  main_q := makeQuadCurve((c0: 2; c1: -8; c2: 2), (c0: 1; c1: 2; c2: 3));
  main_tol := 1e-07;
  main_spacing := main_tol * 10;
  main_inters := findIntersects(main_p, main_q, main_tol, main_spacing);
  main_i := 0;
  while main_i < Length(main_inters) do begin
  main_pt := main_inters[main_i];
  writeln(((('(' + FloatToStr(main_pt.x)) + ', ') + FloatToStr(main_pt.y)) + ')');
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
