{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Angle = record
  degrees: real;
end;
type Ellipse = record
  major: real;
  minor: real;
end;
type Circle = record
  radius: real;
end;
type Side = record
  length_: real;
  angle: Angle;
  next: int64;
end;
type Polygon = record
  sides: array of Side;
end;
type Rectangle = record
  short_side: Side;
  long_side: Side;
  poly: Polygon;
end;
type Square = record
  side: Side;
  rect: Rectangle;
end;
type SideArray = array of Side;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
function makeSquare(side_46_var: Side; rect: Rectangle): Square; forward;
function makeRectangle(short_side: Side; long_side: Side; poly: Polygon): Rectangle; forward;
function makePolygon(sides: SideArray): Polygon; forward;
function makeCircle(radius: real): Circle; forward;
function makeEllipse(major: real; minor: real): Ellipse; forward;
function makeSide(length_: real; angle_46_var: Angle; next: int64): Side; forward;
function makeAngle(degrees: real): Angle; forward;
function make_angle(make_angle_deg: real): Angle; forward;
function make_side(make_side_length_: real; make_side_angle_var: Angle): Side; forward;
function ellipse_area(ellipse_area_e: Ellipse): real; forward;
function ellipse_perimeter(ellipse_perimeter_e: Ellipse): real; forward;
function circle_area(circle_area_c: Circle): real; forward;
function circle_perimeter(circle_perimeter_c: Circle): real; forward;
function circle_diameter(circle_diameter_c: Circle): real; forward;
function circle_max_parts(circle_max_parts_num_cuts: real): real; forward;
function make_polygon(): Polygon; forward;
procedure polygon_add_side(polygon_add_side_p: Polygon; polygon_add_side_s: Side); forward;
function polygon_get_side(polygon_get_side_p: Polygon; polygon_get_side_index: int64): Side; forward;
procedure polygon_set_side(polygon_set_side_p: Polygon; polygon_set_side_index: int64; polygon_set_side_s: Side); forward;
function make_rectangle(make_rectangle_short_len: real; make_rectangle_long_len: real): Rectangle; forward;
function rectangle_perimeter(rectangle_perimeter_r: Rectangle): real; forward;
function rectangle_area(rectangle_area_r: Rectangle): real; forward;
function make_square(make_square_side_len: real): Square; forward;
function square_perimeter(square_perimeter_s: Square): real; forward;
function square_area(square_area_s: Square): real; forward;
procedure main(); forward;
function makeSquare(side_46_var: Side; rect: Rectangle): Square;
begin
  Result.side := side_46_var;
  Result.rect := rect;
end;
function makeRectangle(short_side: Side; long_side: Side; poly: Polygon): Rectangle;
begin
  Result.short_side := short_side;
  Result.long_side := long_side;
  Result.poly := poly;
end;
function makePolygon(sides: SideArray): Polygon;
begin
  Result.sides := sides;
end;
function makeCircle(radius: real): Circle;
begin
  Result.radius := radius;
end;
function makeEllipse(major: real; minor: real): Ellipse;
begin
  Result.major := major;
  Result.minor := minor;
end;
function makeSide(length_: real; angle_46_var: Angle; next: int64): Side;
begin
  Result.length_ := length_;
  Result.angle := angle_46_var;
  Result.next := next;
end;
function makeAngle(degrees: real): Angle;
begin
  Result.degrees := degrees;
end;
function make_angle(make_angle_deg: real): Angle;
begin
  if (make_angle_deg < 0) or (make_angle_deg > 360) then begin
  panic('degrees must be between 0 and 360');
end;
  exit(makeAngle(make_angle_deg));
end;
function make_side(make_side_length_: real; make_side_angle_var: Angle): Side;
begin
  if make_side_length_ <= 0 then begin
  panic('length must be positive');
end;
  exit(makeSide(make_side_length_, make_side_angle_var, -1));
end;
function ellipse_area(ellipse_area_e: Ellipse): real;
begin
  exit((PI * ellipse_area_e.major) * ellipse_area_e.minor);
end;
function ellipse_perimeter(ellipse_perimeter_e: Ellipse): real;
begin
  exit(PI * (ellipse_perimeter_e.major + ellipse_perimeter_e.minor));
end;
function circle_area(circle_area_c: Circle): real;
var
  circle_area_e: Ellipse;
  circle_area_area: real;
begin
  circle_area_e := makeEllipse(circle_area_c.radius, circle_area_c.radius);
  circle_area_area := ellipse_area(circle_area_e);
  exit(circle_area_area);
end;
function circle_perimeter(circle_perimeter_c: Circle): real;
var
  circle_perimeter_e: Ellipse;
  circle_perimeter_per: real;
begin
  circle_perimeter_e := makeEllipse(circle_perimeter_c.radius, circle_perimeter_c.radius);
  circle_perimeter_per := ellipse_perimeter(circle_perimeter_e);
  exit(circle_perimeter_per);
end;
function circle_diameter(circle_diameter_c: Circle): real;
begin
  exit(circle_diameter_c.radius * 2);
end;
function circle_max_parts(circle_max_parts_num_cuts: real): real;
begin
  if circle_max_parts_num_cuts < 0 then begin
  panic('num_cuts must be positive');
end;
  exit(((circle_max_parts_num_cuts + 2) + (circle_max_parts_num_cuts * circle_max_parts_num_cuts)) * 0.5);
end;
function make_polygon(): Polygon;
var
  make_polygon_s: array of Side;
begin
  make_polygon_s := [];
  exit(makePolygon(make_polygon_s));
end;
procedure polygon_add_side(polygon_add_side_p: Polygon; polygon_add_side_s: Side);
begin
  polygon_add_side_p.sides := concat(polygon_add_side_p.sides, [polygon_add_side_s]);
end;
function polygon_get_side(polygon_get_side_p: Polygon; polygon_get_side_index: int64): Side;
begin
  exit(polygon_get_side_p.sides[polygon_get_side_index]);
end;
procedure polygon_set_side(polygon_set_side_p: Polygon; polygon_set_side_index: int64; polygon_set_side_s: Side);
var
  polygon_set_side_tmp: array of Side;
begin
  polygon_set_side_tmp := polygon_set_side_p.sides;
  polygon_set_side_tmp[polygon_set_side_index] := polygon_set_side_s;
  polygon_set_side_p.sides := polygon_set_side_tmp;
end;
function make_rectangle(make_rectangle_short_len: real; make_rectangle_long_len: real): Rectangle;
var
  make_rectangle_short: Side;
  make_rectangle_long: Side;
  make_rectangle_p: Polygon;
begin
  if (make_rectangle_short_len <= 0) or (make_rectangle_long_len <= 0) then begin
  panic('length must be positive');
end;
  make_rectangle_short := make_side(make_rectangle_short_len, make_angle(90));
  make_rectangle_long := make_side(make_rectangle_long_len, make_angle(90));
  make_rectangle_p := make_polygon();
  polygon_add_side(make_rectangle_p, make_rectangle_short);
  polygon_add_side(make_rectangle_p, make_rectangle_long);
  exit(makeRectangle(make_rectangle_short, make_rectangle_long, make_rectangle_p));
end;
function rectangle_perimeter(rectangle_perimeter_r: Rectangle): real;
begin
  exit((rectangle_perimeter_r.short_side.length_ + rectangle_perimeter_r.long_side.length_) * 2);
end;
function rectangle_area(rectangle_area_r: Rectangle): real;
begin
  exit(rectangle_area_r.short_side.length_ * rectangle_area_r.long_side.length_);
end;
function make_square(make_square_side_len: real): Square;
var
  make_square_rect: Rectangle;
begin
  make_square_rect := make_rectangle(make_square_side_len, make_square_side_len);
  exit(makeSquare(make_square_rect.short_side, make_square_rect));
end;
function square_perimeter(square_perimeter_s: Square): real;
var
  square_perimeter_p: real;
begin
  square_perimeter_p := rectangle_perimeter(square_perimeter_s.rect);
  exit(square_perimeter_p);
end;
function square_area(square_area_s: Square): real;
var
  square_area_a: real;
begin
  square_area_a := rectangle_area(square_area_s.rect);
  exit(square_area_a);
end;
procedure main();
var
  main_a: Angle;
  main_s: Side;
  main_e: Ellipse;
  main_c: Circle;
  main_r: Rectangle;
  main_q: Square;
begin
  main_a := make_angle(90);
  writeln(main_a.degrees);
  main_s := make_side(5, main_a);
  writeln(main_s.length_);
  main_e := makeEllipse(5, 10);
  writeln(ellipse_area(main_e));
  writeln(ellipse_perimeter(main_e));
  main_c := makeCircle(5);
  writeln(circle_area(main_c));
  writeln(circle_perimeter(main_c));
  writeln(circle_diameter(main_c));
  writeln(circle_max_parts(7));
  main_r := make_rectangle(5, 10);
  writeln(rectangle_perimeter(main_r));
  writeln(rectangle_area(main_r));
  main_q := make_square(5);
  writeln(square_perimeter(main_q));
  writeln(square_area(main_q));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
