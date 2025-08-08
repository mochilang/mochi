{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
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
  points: array of RealArray;
  x: real;
  p1: RealArray;
  arr: RealArrayArray;
  px: RealArrayArray;
  count: integer;
  min_dis: real;
  p2: RealArray;
  py: RealArrayArray;
  column: integer;
function abs(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function euclidean_distance_sqr(p1: RealArray; p2: RealArray): real; forward;
function column_based_sort(arr: RealArrayArray; column: integer): RealArrayArray; forward;
function dis_between_closest_pair(points: RealArrayArray; count: integer; min_dis: real): real; forward;
function dis_between_closest_in_strip(points: RealArrayArray; count: integer; min_dis: real): real; forward;
function closest_pair_of_points_sqr(px: RealArrayArray; py: RealArrayArray; count: integer): real; forward;
function closest_pair_of_points(points: RealArrayArray; count: integer): real; forward;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(0 - x);
end;
  exit(x);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function euclidean_distance_sqr(p1: RealArray; p2: RealArray): real;
var
  euclidean_distance_sqr_dx: real;
  euclidean_distance_sqr_dy: real;
begin
  euclidean_distance_sqr_dx := p1[0] - p2[0];
  euclidean_distance_sqr_dy := p1[1] - p2[1];
  exit((euclidean_distance_sqr_dx * euclidean_distance_sqr_dx) + (euclidean_distance_sqr_dy * euclidean_distance_sqr_dy));
end;
function column_based_sort(arr: RealArrayArray; column: integer): RealArrayArray;
var
  column_based_sort_points: array of RealArray;
  column_based_sort_i: integer;
  column_based_sort_j: integer;
  column_based_sort_tmp: array of real;
begin
  column_based_sort_points := arr;
  column_based_sort_i := 0;
  while column_based_sort_i < Length(column_based_sort_points) do begin
  column_based_sort_j := 0;
  while column_based_sort_j < (Length(column_based_sort_points) - 1) do begin
  if column_based_sort_points[column_based_sort_j][column] > column_based_sort_points[column_based_sort_j + 1][column] then begin
  column_based_sort_tmp := column_based_sort_points[column_based_sort_j];
  column_based_sort_points[column_based_sort_j] := column_based_sort_points[column_based_sort_j + 1];
  column_based_sort_points[column_based_sort_j + 1] := column_based_sort_tmp;
end;
  column_based_sort_j := column_based_sort_j + 1;
end;
  column_based_sort_i := column_based_sort_i + 1;
end;
  exit(column_based_sort_points);
end;
function dis_between_closest_pair(points: RealArrayArray; count: integer; min_dis: real): real;
var
  dis_between_closest_pair_i: integer;
  dis_between_closest_pair_j: integer;
  dis_between_closest_pair_current: real;
begin
  dis_between_closest_pair_i := 0;
  while dis_between_closest_pair_i < (count - 1) do begin
  dis_between_closest_pair_j := dis_between_closest_pair_i + 1;
  while dis_between_closest_pair_j < count do begin
  dis_between_closest_pair_current := euclidean_distance_sqr(points[dis_between_closest_pair_i], points[dis_between_closest_pair_j]);
  if dis_between_closest_pair_current < min_dis then begin
  min_dis := dis_between_closest_pair_current;
end;
  dis_between_closest_pair_j := dis_between_closest_pair_j + 1;
end;
  dis_between_closest_pair_i := dis_between_closest_pair_i + 1;
end;
  exit(min_dis);
end;
function dis_between_closest_in_strip(points: RealArrayArray; count: integer; min_dis: real): real;
var
  dis_between_closest_in_strip_i_start: integer;
  dis_between_closest_in_strip_i: integer;
  dis_between_closest_in_strip_j_start: integer;
  dis_between_closest_in_strip_j: integer;
  dis_between_closest_in_strip_current: real;
begin
  dis_between_closest_in_strip_i_start := 0;
  if 6 < (count - 1) then begin
  dis_between_closest_in_strip_i_start := 6;
end else begin
  dis_between_closest_in_strip_i_start := count - 1;
end;
  dis_between_closest_in_strip_i := dis_between_closest_in_strip_i_start;
  while dis_between_closest_in_strip_i < count do begin
  dis_between_closest_in_strip_j_start := 0;
  if (dis_between_closest_in_strip_i - 6) > 0 then begin
  dis_between_closest_in_strip_j_start := dis_between_closest_in_strip_i - 6;
end;
  dis_between_closest_in_strip_j := dis_between_closest_in_strip_j_start;
  while dis_between_closest_in_strip_j < dis_between_closest_in_strip_i do begin
  dis_between_closest_in_strip_current := euclidean_distance_sqr(points[dis_between_closest_in_strip_i], points[dis_between_closest_in_strip_j]);
  if dis_between_closest_in_strip_current < min_dis then begin
  min_dis := dis_between_closest_in_strip_current;
end;
  dis_between_closest_in_strip_j := dis_between_closest_in_strip_j + 1;
end;
  dis_between_closest_in_strip_i := dis_between_closest_in_strip_i + 1;
end;
  exit(min_dis);
end;
function closest_pair_of_points_sqr(px: RealArrayArray; py: RealArrayArray; count: integer): real;
var
  closest_pair_of_points_sqr_mid: integer;
  closest_pair_of_points_sqr_left: real;
  closest_pair_of_points_sqr_right: real;
  closest_pair_of_points_sqr_best: real;
  closest_pair_of_points_sqr_strip: array of RealArray;
  closest_pair_of_points_sqr_i: integer;
  closest_pair_of_points_sqr_strip_best: real;
begin
  if count <= 3 then begin
  exit(dis_between_closest_pair(px, count, 1e+18));
end;
  closest_pair_of_points_sqr_mid := count div 2;
  closest_pair_of_points_sqr_left := closest_pair_of_points_sqr(px, copy(py, 0, (closest_pair_of_points_sqr_mid - (0))), closest_pair_of_points_sqr_mid);
  closest_pair_of_points_sqr_right := closest_pair_of_points_sqr(py, copy(py, closest_pair_of_points_sqr_mid, (count - (closest_pair_of_points_sqr_mid))), count - closest_pair_of_points_sqr_mid);
  closest_pair_of_points_sqr_best := closest_pair_of_points_sqr_left;
  if closest_pair_of_points_sqr_right < closest_pair_of_points_sqr_best then begin
  closest_pair_of_points_sqr_best := closest_pair_of_points_sqr_right;
end;
  closest_pair_of_points_sqr_strip := [];
  closest_pair_of_points_sqr_i := 0;
  while closest_pair_of_points_sqr_i < Length(px) do begin
  if abs(px[closest_pair_of_points_sqr_i][0] - px[closest_pair_of_points_sqr_mid][0]) < closest_pair_of_points_sqr_best then begin
  closest_pair_of_points_sqr_strip := concat(closest_pair_of_points_sqr_strip, [px[closest_pair_of_points_sqr_i]]);
end;
  closest_pair_of_points_sqr_i := closest_pair_of_points_sqr_i + 1;
end;
  closest_pair_of_points_sqr_strip_best := dis_between_closest_in_strip(closest_pair_of_points_sqr_strip, Length(closest_pair_of_points_sqr_strip), closest_pair_of_points_sqr_best);
  if closest_pair_of_points_sqr_strip_best < closest_pair_of_points_sqr_best then begin
  closest_pair_of_points_sqr_best := closest_pair_of_points_sqr_strip_best;
end;
  exit(closest_pair_of_points_sqr_best);
end;
function closest_pair_of_points(points: RealArrayArray; count: integer): real;
var
  closest_pair_of_points_points_sorted_on_x: RealArrayArray;
  closest_pair_of_points_points_sorted_on_y: RealArrayArray;
  closest_pair_of_points_dist_sqr: real;
begin
  closest_pair_of_points_points_sorted_on_x := column_based_sort(points, 0);
  closest_pair_of_points_points_sorted_on_y := column_based_sort(points, 1);
  closest_pair_of_points_dist_sqr := closest_pair_of_points_sqr(closest_pair_of_points_points_sorted_on_x, closest_pair_of_points_points_sorted_on_y, count);
  exit(sqrtApprox(closest_pair_of_points_dist_sqr));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  points := [[2, 3], [12, 30], [40, 50], [5, 1], [12, 10], [3, 4]];
  writeln('Distance: ' + FloatToStr(closest_pair_of_points(points, Length(points))));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
