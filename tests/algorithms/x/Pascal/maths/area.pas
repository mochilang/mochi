{$mode objfpc}{$modeswitch nestedprocvars}
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
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  TWO_PI: real;
  TRI_THREE_SIDES: real;
  side2: real;
  side_length: real;
  side1: real;
  side3: real;
  radius_x: real;
  radius1: real;
  m: real;
  x: real;
  torus_radius: real;
  diagonal2: real;
  height: real;
  base: real;
  radius_y: real;
  sides: integer;
  base2: real;
  tube_radius: real;
  width: real;
  base1: real;
  diagonal1: real;
  radius2: real;
  length_: real;
  breadth: real;
  radius: real;
function _mod(x: real; m: real): real; forward;
function sin_approx(x: real): real; forward;
function cos_approx(x: real): real; forward;
function tan_approx(x: real): real; forward;
function sqrt_approx(x: real): real; forward;
function surface_area_cube(side_length: real): real; forward;
function surface_area_cuboid(surface_area_cuboid_length_: real; breadth: real; height: real): real; forward;
function surface_area_sphere(radius: real): real; forward;
function surface_area_hemisphere(radius: real): real; forward;
function surface_area_cone(radius: real; height: real): real; forward;
function surface_area_conical_frustum(radius1: real; radius2: real; height: real): real; forward;
function surface_area_cylinder(radius: real; height: real): real; forward;
function surface_area_torus(torus_radius: real; tube_radius: real): real; forward;
function area_rectangle(surface_area_cuboid_length_: real; width: real): real; forward;
function area_square(side_length: real): real; forward;
function area_triangle(base: real; height: real): real; forward;
function area_triangle_three_sides(side1: real; side2: real; side3: real): real; forward;
function area_parallelogram(base: real; height: real): real; forward;
function area_trapezium(base1: real; base2: real; height: real): real; forward;
function area_circle(radius: real): real; forward;
function area_ellipse(radius_x: real; radius_y: real): real; forward;
function area_rhombus(diagonal1: real; diagonal2: real): real; forward;
function area_reg_polygon(sides: integer; surface_area_cuboid_length_: real): real; forward;
function _mod(x: real; m: real): real;
begin
  exit(x - (Double(Trunc(x / m)) * m));
end;
function sin_approx(x: real): real;
var
  sin_approx_y: real;
  sin_approx_y2: real;
  sin_approx_y3: real;
  sin_approx_y5: real;
  sin_approx_y7: real;
begin
  sin_approx_y := _mod(x + PI, TWO_PI) - PI;
  sin_approx_y2 := sin_approx_y * sin_approx_y;
  sin_approx_y3 := sin_approx_y2 * sin_approx_y;
  sin_approx_y5 := sin_approx_y3 * sin_approx_y2;
  sin_approx_y7 := sin_approx_y5 * sin_approx_y2;
  exit(((sin_approx_y - (sin_approx_y3 / 6)) + (sin_approx_y5 / 120)) - (sin_approx_y7 / 5040));
end;
function cos_approx(x: real): real;
var
  cos_approx_y: real;
  cos_approx_y2: real;
  cos_approx_y4: real;
  cos_approx_y6: real;
begin
  cos_approx_y := _mod(x + PI, TWO_PI) - PI;
  cos_approx_y2 := cos_approx_y * cos_approx_y;
  cos_approx_y4 := cos_approx_y2 * cos_approx_y2;
  cos_approx_y6 := cos_approx_y4 * cos_approx_y2;
  exit(((1 - (cos_approx_y2 / 2)) + (cos_approx_y4 / 24)) - (cos_approx_y6 / 720));
end;
function tan_approx(x: real): real;
begin
  exit(sin_approx(x) / cos_approx(x));
end;
function sqrt_approx(x: real): real;
var
  sqrt_approx_guess: real;
  sqrt_approx_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrt_approx_guess := x / 2;
  sqrt_approx_i := 0;
  while sqrt_approx_i < 20 do begin
  sqrt_approx_guess := (sqrt_approx_guess + (x / sqrt_approx_guess)) / 2;
  sqrt_approx_i := sqrt_approx_i + 1;
end;
  exit(sqrt_approx_guess);
end;
function surface_area_cube(side_length: real): real;
begin
  if side_length < 0 then begin
  writeln('ValueError: surface_area_cube() only accepts non-negative values');
  exit(0);
end;
  exit((6 * side_length) * side_length);
end;
function surface_area_cuboid(surface_area_cuboid_length_: real; breadth: real; height: real): real;
begin
  if ((length_ < 0) or (breadth < 0)) or (height < 0) then begin
  writeln('ValueError: surface_area_cuboid() only accepts non-negative values');
  exit(0);
end;
  exit(2 * (((length_ * breadth) + (breadth * height)) + (length_ * height)));
end;
function surface_area_sphere(radius: real): real;
begin
  if radius < 0 then begin
  writeln('ValueError: surface_area_sphere() only accepts non-negative values');
  exit(0);
end;
  exit(((4 * PI) * radius) * radius);
end;
function surface_area_hemisphere(radius: real): real;
begin
  if radius < 0 then begin
  writeln('ValueError: surface_area_hemisphere() only accepts non-negative values');
  exit(0);
end;
  exit(((3 * PI) * radius) * radius);
end;
function surface_area_cone(radius: real; height: real): real;
var
  surface_area_cone_slant: real;
begin
  if (radius < 0) or (height < 0) then begin
  writeln('ValueError: surface_area_cone() only accepts non-negative values');
  exit(0);
end;
  surface_area_cone_slant := sqrt_approx((height * height) + (radius * radius));
  exit((PI * radius) * (radius + surface_area_cone_slant));
end;
function surface_area_conical_frustum(radius1: real; radius2: real; height: real): real;
var
  surface_area_conical_frustum_slant: real;
begin
  if ((radius1 < 0) or (radius2 < 0)) or (height < 0) then begin
  writeln('ValueError: surface_area_conical_frustum() only accepts non-negative values');
  exit(0);
end;
  surface_area_conical_frustum_slant := sqrt_approx((height * height) + ((radius1 - radius2) * (radius1 - radius2)));
  exit(PI * (((surface_area_conical_frustum_slant * (radius1 + radius2)) + (radius1 * radius1)) + (radius2 * radius2)));
end;
function surface_area_cylinder(radius: real; height: real): real;
begin
  if (radius < 0) or (height < 0) then begin
  writeln('ValueError: surface_area_cylinder() only accepts non-negative values');
  exit(0);
end;
  exit(((2 * PI) * radius) * (height + radius));
end;
function surface_area_torus(torus_radius: real; tube_radius: real): real;
begin
  if (torus_radius < 0) or (tube_radius < 0) then begin
  writeln('ValueError: surface_area_torus() only accepts non-negative values');
  exit(0);
end;
  if torus_radius < tube_radius then begin
  writeln('ValueError: surface_area_torus() does not support spindle or self intersecting tori');
  exit(0);
end;
  exit((((4 * PI) * PI) * torus_radius) * tube_radius);
end;
function area_rectangle(surface_area_cuboid_length_: real; width: real): real;
begin
  if (length_ < 0) or (width < 0) then begin
  writeln('ValueError: area_rectangle() only accepts non-negative values');
  exit(0);
end;
  exit(length_ * width);
end;
function area_square(side_length: real): real;
begin
  if side_length < 0 then begin
  writeln('ValueError: area_square() only accepts non-negative values');
  exit(0);
end;
  exit(side_length * side_length);
end;
function area_triangle(base: real; height: real): real;
begin
  if (base < 0) or (height < 0) then begin
  writeln('ValueError: area_triangle() only accepts non-negative values');
  exit(0);
end;
  exit((base * height) / 2);
end;
function area_triangle_three_sides(side1: real; side2: real; side3: real): real;
var
  area_triangle_three_sides_s: real;
  area_triangle_three_sides_prod: real;
  area_triangle_three_sides_res: real;
begin
  if ((side1 < 0) or (side2 < 0)) or (side3 < 0) then begin
  writeln('ValueError: area_triangle_three_sides() only accepts non-negative values');
  exit(0);
end;
  if (((side1 + side2) < side3) or ((side1 + side3) < side2)) or ((side2 + side3) < side1) then begin
  writeln('ValueError: Given three sides do not form a triangle');
  exit(0);
end;
  area_triangle_three_sides_s := ((side1 + side2) + side3) / 2;
  area_triangle_three_sides_prod := ((area_triangle_three_sides_s * (area_triangle_three_sides_s - side1)) * (area_triangle_three_sides_s - side2)) * (area_triangle_three_sides_s - side3);
  area_triangle_three_sides_res := sqrt_approx(area_triangle_three_sides_prod);
  exit(area_triangle_three_sides_res);
end;
function area_parallelogram(base: real; height: real): real;
begin
  if (base < 0) or (height < 0) then begin
  writeln('ValueError: area_parallelogram() only accepts non-negative values');
  exit(0);
end;
  exit(base * height);
end;
function area_trapezium(base1: real; base2: real; height: real): real;
begin
  if ((base1 < 0) or (base2 < 0)) or (height < 0) then begin
  writeln('ValueError: area_trapezium() only accepts non-negative values');
  exit(0);
end;
  exit((0.5 * (base1 + base2)) * height);
end;
function area_circle(radius: real): real;
begin
  if radius < 0 then begin
  writeln('ValueError: area_circle() only accepts non-negative values');
  exit(0);
end;
  exit((PI * radius) * radius);
end;
function area_ellipse(radius_x: real; radius_y: real): real;
begin
  if (radius_x < 0) or (radius_y < 0) then begin
  writeln('ValueError: area_ellipse() only accepts non-negative values');
  exit(0);
end;
  exit((PI * radius_x) * radius_y);
end;
function area_rhombus(diagonal1: real; diagonal2: real): real;
begin
  if (diagonal1 < 0) or (diagonal2 < 0) then begin
  writeln('ValueError: area_rhombus() only accepts non-negative values');
  exit(0);
end;
  exit((0.5 * diagonal1) * diagonal2);
end;
function area_reg_polygon(sides: integer; surface_area_cuboid_length_: real): real;
var
  area_reg_polygon_n: real;
begin
  if sides < 3 then begin
  writeln('ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides');
  exit(0);
end;
  if length_ < 0 then begin
  writeln('ValueError: area_reg_polygon() only accepts non-negative values as length of a side');
  exit(0);
end;
  area_reg_polygon_n := Double(sides);
  exit(((area_reg_polygon_n * length_) * length_) / (4 * tan_approx(PI / area_reg_polygon_n)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  TWO_PI := 6.283185307179586;
  writeln('[DEMO] Areas of various geometric shapes:');
  writeln('Rectangle: ' + FloatToStr(area_rectangle(10, 20)));
  writeln('Square: ' + FloatToStr(area_square(10)));
  writeln('Triangle: ' + FloatToStr(area_triangle(10, 10)));
  TRI_THREE_SIDES := area_triangle_three_sides(5, 12, 13);
  writeln('Triangle Three Sides: ' + FloatToStr(TRI_THREE_SIDES));
  writeln('Parallelogram: ' + FloatToStr(area_parallelogram(10, 20)));
  writeln('Rhombus: ' + FloatToStr(area_rhombus(10, 20)));
  writeln('Trapezium: ' + FloatToStr(area_trapezium(10, 20, 30)));
  writeln('Circle: ' + FloatToStr(area_circle(20)));
  writeln('Ellipse: ' + FloatToStr(area_ellipse(10, 20)));
  writeln('');
  writeln('Surface Areas of various geometric shapes:');
  writeln('Cube: ' + FloatToStr(surface_area_cube(20)));
  writeln('Cuboid: ' + FloatToStr(surface_area_cuboid(10, 20, 30)));
  writeln('Sphere: ' + FloatToStr(surface_area_sphere(20)));
  writeln('Hemisphere: ' + FloatToStr(surface_area_hemisphere(20)));
  writeln('Cone: ' + FloatToStr(surface_area_cone(10, 20)));
  writeln('Conical Frustum: ' + FloatToStr(surface_area_conical_frustum(10, 20, 30)));
  writeln('Cylinder: ' + FloatToStr(surface_area_cylinder(10, 20)));
  writeln('Torus: ' + FloatToStr(surface_area_torus(20, 10)));
  writeln('Equilateral Triangle: ' + FloatToStr(area_reg_polygon(3, 10)));
  writeln('Square: ' + FloatToStr(area_reg_polygon(4, 10)));
  writeln('Regular Pentagon: ' + FloatToStr(area_reg_polygon(5, 10)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
