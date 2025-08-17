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
  SQRT5: real;
function minf(minf_a: real; minf_b: real): real; forward;
function maxf(maxf_a: real; maxf_b: real): real; forward;
function vol_cube(vol_cube_side_length: real): real; forward;
function vol_spherical_cap(vol_spherical_cap_height: real; vol_spherical_cap_radius: real): real; forward;
function vol_sphere(vol_sphere_radius: real): real; forward;
function vol_spheres_intersect(vol_spheres_intersect_radius_1: real; vol_spheres_intersect_radius_2: real; vol_spheres_intersect_centers_distance: real): real; forward;
function vol_spheres_union(vol_spheres_union_radius_1: real; vol_spheres_union_radius_2: real; vol_spheres_union_centers_distance: real): real; forward;
function vol_cuboid(vol_cuboid_width: real; vol_cuboid_height: real; vol_cuboid_length_: real): real; forward;
function vol_cone(vol_cone_area_of_base: real; vol_cone_height: real): real; forward;
function vol_right_circ_cone(vol_right_circ_cone_radius: real; vol_right_circ_cone_height: real): real; forward;
function vol_prism(vol_prism_area_of_base: real; vol_prism_height: real): real; forward;
function vol_pyramid(vol_pyramid_area_of_base: real; vol_pyramid_height: real): real; forward;
function vol_hemisphere(vol_hemisphere_radius: real): real; forward;
function vol_circular_cylinder(vol_circular_cylinder_radius: real; vol_circular_cylinder_height: real): real; forward;
function vol_hollow_circular_cylinder(vol_hollow_circular_cylinder_inner_radius: real; vol_hollow_circular_cylinder_outer_radius: real; vol_hollow_circular_cylinder_height: real): real; forward;
function vol_conical_frustum(vol_conical_frustum_height: real; vol_conical_frustum_radius_1: real; vol_conical_frustum_radius_2: real): real; forward;
function vol_torus(vol_torus_torus_radius: real; vol_torus_tube_radius: real): real; forward;
function vol_icosahedron(vol_icosahedron_tri_side: real): real; forward;
procedure main(); forward;
function minf(minf_a: real; minf_b: real): real;
begin
  if minf_a < minf_b then begin
  exit(minf_a);
end;
  exit(minf_b);
end;
function maxf(maxf_a: real; maxf_b: real): real;
begin
  if maxf_a > maxf_b then begin
  exit(maxf_a);
end;
  exit(maxf_b);
end;
function vol_cube(vol_cube_side_length: real): real;
begin
  if vol_cube_side_length < 0 then begin
  panic('vol_cube() only accepts non-negative values');
end;
  exit((vol_cube_side_length * vol_cube_side_length) * vol_cube_side_length);
end;
function vol_spherical_cap(vol_spherical_cap_height: real; vol_spherical_cap_radius: real): real;
begin
  if (vol_spherical_cap_height < 0) or (vol_spherical_cap_radius < 0) then begin
  panic('vol_spherical_cap() only accepts non-negative values');
end;
  exit(((((1 / 3) * PI) * vol_spherical_cap_height) * vol_spherical_cap_height) * ((3 * vol_spherical_cap_radius) - vol_spherical_cap_height));
end;
function vol_sphere(vol_sphere_radius: real): real;
begin
  if vol_sphere_radius < 0 then begin
  panic('vol_sphere() only accepts non-negative values');
end;
  exit(((((4 / 3) * PI) * vol_sphere_radius) * vol_sphere_radius) * vol_sphere_radius);
end;
function vol_spheres_intersect(vol_spheres_intersect_radius_1: real; vol_spheres_intersect_radius_2: real; vol_spheres_intersect_centers_distance: real): real;
var
  vol_spheres_intersect_h1: real;
  vol_spheres_intersect_h2: real;
begin
  if ((vol_spheres_intersect_radius_1 < 0) or (vol_spheres_intersect_radius_2 < 0)) or (vol_spheres_intersect_centers_distance < 0) then begin
  panic('vol_spheres_intersect() only accepts non-negative values');
end;
  if vol_spheres_intersect_centers_distance = 0 then begin
  exit(vol_sphere(minf(vol_spheres_intersect_radius_1, vol_spheres_intersect_radius_2)));
end;
  vol_spheres_intersect_h1 := (((vol_spheres_intersect_radius_1 - vol_spheres_intersect_radius_2) + vol_spheres_intersect_centers_distance) * ((vol_spheres_intersect_radius_1 + vol_spheres_intersect_radius_2) - vol_spheres_intersect_centers_distance)) / (2 * vol_spheres_intersect_centers_distance);
  vol_spheres_intersect_h2 := (((vol_spheres_intersect_radius_2 - vol_spheres_intersect_radius_1) + vol_spheres_intersect_centers_distance) * ((vol_spheres_intersect_radius_2 + vol_spheres_intersect_radius_1) - vol_spheres_intersect_centers_distance)) / (2 * vol_spheres_intersect_centers_distance);
  exit(vol_spherical_cap(vol_spheres_intersect_h1, vol_spheres_intersect_radius_2) + vol_spherical_cap(vol_spheres_intersect_h2, vol_spheres_intersect_radius_1));
end;
function vol_spheres_union(vol_spheres_union_radius_1: real; vol_spheres_union_radius_2: real; vol_spheres_union_centers_distance: real): real;
begin
  if ((vol_spheres_union_radius_1 <= 0) or (vol_spheres_union_radius_2 <= 0)) or (vol_spheres_union_centers_distance < 0) then begin
  panic('vol_spheres_union() only accepts non-negative values, non-zero radius');
end;
  if vol_spheres_union_centers_distance = 0 then begin
  exit(vol_sphere(maxf(vol_spheres_union_radius_1, vol_spheres_union_radius_2)));
end;
  exit((vol_sphere(vol_spheres_union_radius_1) + vol_sphere(vol_spheres_union_radius_2)) - vol_spheres_intersect(vol_spheres_union_radius_1, vol_spheres_union_radius_2, vol_spheres_union_centers_distance));
end;
function vol_cuboid(vol_cuboid_width: real; vol_cuboid_height: real; vol_cuboid_length_: real): real;
begin
  if ((vol_cuboid_width < 0) or (vol_cuboid_height < 0)) or (vol_cuboid_length_ < 0) then begin
  panic('vol_cuboid() only accepts non-negative values');
end;
  exit((vol_cuboid_width * vol_cuboid_height) * vol_cuboid_length_);
end;
function vol_cone(vol_cone_area_of_base: real; vol_cone_height: real): real;
begin
  if (vol_cone_height < 0) or (vol_cone_area_of_base < 0) then begin
  panic('vol_cone() only accepts non-negative values');
end;
  exit((vol_cone_area_of_base * vol_cone_height) / 3);
end;
function vol_right_circ_cone(vol_right_circ_cone_radius: real; vol_right_circ_cone_height: real): real;
begin
  if (vol_right_circ_cone_height < 0) or (vol_right_circ_cone_radius < 0) then begin
  panic('vol_right_circ_cone() only accepts non-negative values');
end;
  exit((((PI * vol_right_circ_cone_radius) * vol_right_circ_cone_radius) * vol_right_circ_cone_height) / 3);
end;
function vol_prism(vol_prism_area_of_base: real; vol_prism_height: real): real;
begin
  if (vol_prism_height < 0) or (vol_prism_area_of_base < 0) then begin
  panic('vol_prism() only accepts non-negative values');
end;
  exit(vol_prism_area_of_base * vol_prism_height);
end;
function vol_pyramid(vol_pyramid_area_of_base: real; vol_pyramid_height: real): real;
begin
  if (vol_pyramid_height < 0) or (vol_pyramid_area_of_base < 0) then begin
  panic('vol_pyramid() only accepts non-negative values');
end;
  exit((vol_pyramid_area_of_base * vol_pyramid_height) / 3);
end;
function vol_hemisphere(vol_hemisphere_radius: real): real;
begin
  if vol_hemisphere_radius < 0 then begin
  panic('vol_hemisphere() only accepts non-negative values');
end;
  exit(((((vol_hemisphere_radius * vol_hemisphere_radius) * vol_hemisphere_radius) * PI) * 2) / 3);
end;
function vol_circular_cylinder(vol_circular_cylinder_radius: real; vol_circular_cylinder_height: real): real;
begin
  if (vol_circular_cylinder_height < 0) or (vol_circular_cylinder_radius < 0) then begin
  panic('vol_circular_cylinder() only accepts non-negative values');
end;
  exit(((vol_circular_cylinder_radius * vol_circular_cylinder_radius) * vol_circular_cylinder_height) * PI);
end;
function vol_hollow_circular_cylinder(vol_hollow_circular_cylinder_inner_radius: real; vol_hollow_circular_cylinder_outer_radius: real; vol_hollow_circular_cylinder_height: real): real;
begin
  if ((vol_hollow_circular_cylinder_inner_radius < 0) or (vol_hollow_circular_cylinder_outer_radius < 0)) or (vol_hollow_circular_cylinder_height < 0) then begin
  panic('vol_hollow_circular_cylinder() only accepts non-negative values');
end;
  if vol_hollow_circular_cylinder_outer_radius <= vol_hollow_circular_cylinder_inner_radius then begin
  panic('outer_radius must be greater than inner_radius');
end;
  exit((PI * ((vol_hollow_circular_cylinder_outer_radius * vol_hollow_circular_cylinder_outer_radius) - (vol_hollow_circular_cylinder_inner_radius * vol_hollow_circular_cylinder_inner_radius))) * vol_hollow_circular_cylinder_height);
end;
function vol_conical_frustum(vol_conical_frustum_height: real; vol_conical_frustum_radius_1: real; vol_conical_frustum_radius_2: real): real;
begin
  if ((vol_conical_frustum_radius_1 < 0) or (vol_conical_frustum_radius_2 < 0)) or (vol_conical_frustum_height < 0) then begin
  panic('vol_conical_frustum() only accepts non-negative values');
end;
  exit((((1 / 3) * PI) * vol_conical_frustum_height) * (((vol_conical_frustum_radius_1 * vol_conical_frustum_radius_1) + (vol_conical_frustum_radius_2 * vol_conical_frustum_radius_2)) + (vol_conical_frustum_radius_1 * vol_conical_frustum_radius_2)));
end;
function vol_torus(vol_torus_torus_radius: real; vol_torus_tube_radius: real): real;
begin
  if (vol_torus_torus_radius < 0) or (vol_torus_tube_radius < 0) then begin
  panic('vol_torus() only accepts non-negative values');
end;
  exit(((((2 * PI) * PI) * vol_torus_torus_radius) * vol_torus_tube_radius) * vol_torus_tube_radius);
end;
function vol_icosahedron(vol_icosahedron_tri_side: real): real;
begin
  if vol_icosahedron_tri_side < 0 then begin
  panic('vol_icosahedron() only accepts non-negative values');
end;
  exit(((((vol_icosahedron_tri_side * vol_icosahedron_tri_side) * vol_icosahedron_tri_side) * (3 + SQRT5)) * 5) / 12);
end;
procedure main();
begin
  writeln('Volumes:');
  writeln('Cube: ' + FloatToStr(vol_cube(2)));
  writeln('Cuboid: ' + FloatToStr(vol_cuboid(2, 2, 2)));
  writeln('Cone: ' + FloatToStr(vol_cone(2, 2)));
  writeln('Right Circular Cone: ' + FloatToStr(vol_right_circ_cone(2, 2)));
  writeln('Prism: ' + FloatToStr(vol_prism(2, 2)));
  writeln('Pyramid: ' + FloatToStr(vol_pyramid(2, 2)));
  writeln('Sphere: ' + FloatToStr(vol_sphere(2)));
  writeln('Hemisphere: ' + FloatToStr(vol_hemisphere(2)));
  writeln('Circular Cylinder: ' + FloatToStr(vol_circular_cylinder(2, 2)));
  writeln('Torus: ' + FloatToStr(vol_torus(2, 2)));
  writeln('Conical Frustum: ' + FloatToStr(vol_conical_frustum(2, 2, 4)));
  writeln('Spherical cap: ' + FloatToStr(vol_spherical_cap(1, 2)));
  writeln('Spheres intersection: ' + FloatToStr(vol_spheres_intersect(2, 2, 1)));
  writeln('Spheres union: ' + FloatToStr(vol_spheres_union(2, 2, 1)));
  writeln('Hollow Circular Cylinder: ' + FloatToStr(vol_hollow_circular_cylinder(1, 2, 3)));
  writeln('Icosahedron: ' + FloatToStr(vol_icosahedron(2.5)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  SQRT5 := 2.23606797749979;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
