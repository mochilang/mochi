<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  $SQRT5 = 2.23606797749979;
  function minf($a, $b) {
  global $PI, $SQRT5;
  if ($a < $b) {
  return $a;
}
  return $b;
};
  function maxf($a, $b) {
  global $PI, $SQRT5;
  if ($a > $b) {
  return $a;
}
  return $b;
};
  function vol_cube($side_length) {
  global $PI, $SQRT5;
  if ($side_length < 0.0) {
  _panic('vol_cube() only accepts non-negative values');
}
  return $side_length * $side_length * $side_length;
};
  function vol_spherical_cap($height, $radius) {
  global $PI, $SQRT5;
  if ($height < 0.0 || $radius < 0.0) {
  _panic('vol_spherical_cap() only accepts non-negative values');
}
  return (1.0 / 3.0) * $PI * $height * $height * (3.0 * $radius - $height);
};
  function vol_sphere($radius) {
  global $PI, $SQRT5;
  if ($radius < 0.0) {
  _panic('vol_sphere() only accepts non-negative values');
}
  return (4.0 / 3.0) * $PI * $radius * $radius * $radius;
};
  function vol_spheres_intersect($radius_1, $radius_2, $centers_distance) {
  global $PI, $SQRT5;
  if ($radius_1 < 0.0 || $radius_2 < 0.0 || $centers_distance < 0.0) {
  _panic('vol_spheres_intersect() only accepts non-negative values');
}
  if ($centers_distance == 0.0) {
  return vol_sphere(minf($radius_1, $radius_2));
}
  $h1 = ($radius_1 - $radius_2 + $centers_distance) * ($radius_1 + $radius_2 - $centers_distance) / (2.0 * $centers_distance);
  $h2 = ($radius_2 - $radius_1 + $centers_distance) * ($radius_2 + $radius_1 - $centers_distance) / (2.0 * $centers_distance);
  return vol_spherical_cap($h1, $radius_2) + vol_spherical_cap($h2, $radius_1);
};
  function vol_spheres_union($radius_1, $radius_2, $centers_distance) {
  global $PI, $SQRT5;
  if ($radius_1 <= 0.0 || $radius_2 <= 0.0 || $centers_distance < 0.0) {
  _panic('vol_spheres_union() only accepts non-negative values, non-zero radius');
}
  if ($centers_distance == 0.0) {
  return vol_sphere(maxf($radius_1, $radius_2));
}
  return vol_sphere($radius_1) + vol_sphere($radius_2) - vol_spheres_intersect($radius_1, $radius_2, $centers_distance);
};
  function vol_cuboid($width, $height, $length) {
  global $PI, $SQRT5;
  if ($width < 0.0 || $height < 0.0 || $length < 0.0) {
  _panic('vol_cuboid() only accepts non-negative values');
}
  return $width * $height * $length;
};
  function vol_cone($area_of_base, $height) {
  global $PI, $SQRT5;
  if ($height < 0.0 || $area_of_base < 0.0) {
  _panic('vol_cone() only accepts non-negative values');
}
  return $area_of_base * $height / 3.0;
};
  function vol_right_circ_cone($radius, $height) {
  global $PI, $SQRT5;
  if ($height < 0.0 || $radius < 0.0) {
  _panic('vol_right_circ_cone() only accepts non-negative values');
}
  return $PI * $radius * $radius * $height / 3.0;
};
  function vol_prism($area_of_base, $height) {
  global $PI, $SQRT5;
  if ($height < 0.0 || $area_of_base < 0.0) {
  _panic('vol_prism() only accepts non-negative values');
}
  return $area_of_base * $height;
};
  function vol_pyramid($area_of_base, $height) {
  global $PI, $SQRT5;
  if ($height < 0.0 || $area_of_base < 0.0) {
  _panic('vol_pyramid() only accepts non-negative values');
}
  return $area_of_base * $height / 3.0;
};
  function vol_hemisphere($radius) {
  global $PI, $SQRT5;
  if ($radius < 0.0) {
  _panic('vol_hemisphere() only accepts non-negative values');
}
  return $radius * $radius * $radius * $PI * 2.0 / 3.0;
};
  function vol_circular_cylinder($radius, $height) {
  global $PI, $SQRT5;
  if ($height < 0.0 || $radius < 0.0) {
  _panic('vol_circular_cylinder() only accepts non-negative values');
}
  return $radius * $radius * $height * $PI;
};
  function vol_hollow_circular_cylinder($inner_radius, $outer_radius, $height) {
  global $PI, $SQRT5;
  if ($inner_radius < 0.0 || $outer_radius < 0.0 || $height < 0.0) {
  _panic('vol_hollow_circular_cylinder() only accepts non-negative values');
}
  if ($outer_radius <= $inner_radius) {
  _panic('outer_radius must be greater than inner_radius');
}
  return $PI * ($outer_radius * $outer_radius - $inner_radius * $inner_radius) * $height;
};
  function vol_conical_frustum($height, $radius_1, $radius_2) {
  global $PI, $SQRT5;
  if ($radius_1 < 0.0 || $radius_2 < 0.0 || $height < 0.0) {
  _panic('vol_conical_frustum() only accepts non-negative values');
}
  return (1.0 / 3.0) * $PI * $height * ($radius_1 * $radius_1 + $radius_2 * $radius_2 + $radius_1 * $radius_2);
};
  function vol_torus($torus_radius, $tube_radius) {
  global $PI, $SQRT5;
  if ($torus_radius < 0.0 || $tube_radius < 0.0) {
  _panic('vol_torus() only accepts non-negative values');
}
  return 2.0 * $PI * $PI * $torus_radius * $tube_radius * $tube_radius;
};
  function vol_icosahedron($tri_side) {
  global $PI, $SQRT5;
  if ($tri_side < 0.0) {
  _panic('vol_icosahedron() only accepts non-negative values');
}
  return $tri_side * $tri_side * $tri_side * (3.0 + $SQRT5) * 5.0 / 12.0;
};
  function main() {
  global $PI, $SQRT5;
  echo rtrim('Volumes:'), PHP_EOL;
  echo rtrim('Cube: ' . _str(vol_cube(2.0))), PHP_EOL;
  echo rtrim('Cuboid: ' . _str(vol_cuboid(2.0, 2.0, 2.0))), PHP_EOL;
  echo rtrim('Cone: ' . _str(vol_cone(2.0, 2.0))), PHP_EOL;
  echo rtrim('Right Circular Cone: ' . _str(vol_right_circ_cone(2.0, 2.0))), PHP_EOL;
  echo rtrim('Prism: ' . _str(vol_prism(2.0, 2.0))), PHP_EOL;
  echo rtrim('Pyramid: ' . _str(vol_pyramid(2.0, 2.0))), PHP_EOL;
  echo rtrim('Sphere: ' . _str(vol_sphere(2.0))), PHP_EOL;
  echo rtrim('Hemisphere: ' . _str(vol_hemisphere(2.0))), PHP_EOL;
  echo rtrim('Circular Cylinder: ' . _str(vol_circular_cylinder(2.0, 2.0))), PHP_EOL;
  echo rtrim('Torus: ' . _str(vol_torus(2.0, 2.0))), PHP_EOL;
  echo rtrim('Conical Frustum: ' . _str(vol_conical_frustum(2.0, 2.0, 4.0))), PHP_EOL;
  echo rtrim('Spherical cap: ' . _str(vol_spherical_cap(1.0, 2.0))), PHP_EOL;
  echo rtrim('Spheres intersection: ' . _str(vol_spheres_intersect(2.0, 2.0, 1.0))), PHP_EOL;
  echo rtrim('Spheres union: ' . _str(vol_spheres_union(2.0, 2.0, 1.0))), PHP_EOL;
  echo rtrim('Hollow Circular Cylinder: ' . _str(vol_hollow_circular_cylinder(1.0, 2.0, 3.0))), PHP_EOL;
  echo rtrim('Icosahedron: ' . _str(vol_icosahedron(2.5))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
