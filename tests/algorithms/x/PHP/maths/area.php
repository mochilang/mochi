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
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  $TWO_PI = 6.283185307179586;
  function _mod($x, $m) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  return $x - (floatval(intval($x / $m))) * $m;
};
  function sin_approx($x) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y3 = $y2 * $y;
  $y5 = $y3 * $y2;
  $y7 = $y5 * $y2;
  return $y - $y3 / 6.0 + $y5 / 120.0 - $y7 / 5040.0;
};
  function cos_approx($x) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y4 = $y2 * $y2;
  $y6 = $y4 * $y2;
  return 1.0 - $y2 / 2.0 + $y4 / 24.0 - $y6 / 720.0;
};
  function tan_approx($x) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  return sin_approx($x) / cos_approx($x);
};
  function sqrt_approx($x) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function surface_area_cube($side_length) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($side_length < 0.0) {
  echo rtrim('ValueError: surface_area_cube() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return 6.0 * $side_length * $side_length;
};
  function surface_area_cuboid($length, $breadth, $height) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($length < 0.0 || $breadth < 0.0 || $height < 0.0) {
  echo rtrim('ValueError: surface_area_cuboid() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return 2.0 * (($length * $breadth) + ($breadth * $height) + ($length * $height));
};
  function surface_area_sphere($radius) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($radius < 0.0) {
  echo rtrim('ValueError: surface_area_sphere() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return 4.0 * $PI * $radius * $radius;
};
  function surface_area_hemisphere($radius) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($radius < 0.0) {
  echo rtrim('ValueError: surface_area_hemisphere() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return 3.0 * $PI * $radius * $radius;
};
  function surface_area_cone($radius, $height) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($radius < 0.0 || $height < 0.0) {
  echo rtrim('ValueError: surface_area_cone() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  $slant = sqrt_approx($height * $height + $radius * $radius);
  return $PI * $radius * ($radius + $slant);
};
  function surface_area_conical_frustum($radius1, $radius2, $height) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($radius1 < 0.0 || $radius2 < 0.0 || $height < 0.0) {
  echo rtrim('ValueError: surface_area_conical_frustum() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  $slant = sqrt_approx($height * $height + ($radius1 - $radius2) * ($radius1 - $radius2));
  return $PI * ($slant * ($radius1 + $radius2) + $radius1 * $radius1 + $radius2 * $radius2);
};
  function surface_area_cylinder($radius, $height) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($radius < 0.0 || $height < 0.0) {
  echo rtrim('ValueError: surface_area_cylinder() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return 2.0 * $PI * $radius * ($height + $radius);
};
  function surface_area_torus($torus_radius, $tube_radius) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($torus_radius < 0.0 || $tube_radius < 0.0) {
  echo rtrim('ValueError: surface_area_torus() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  if ($torus_radius < $tube_radius) {
  echo rtrim('ValueError: surface_area_torus() does not support spindle or self intersecting tori'), PHP_EOL;
  return 0.0;
}
  return 4.0 * $PI * $PI * $torus_radius * $tube_radius;
};
  function area_rectangle($length, $width) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($length < 0.0 || $width < 0.0) {
  echo rtrim('ValueError: area_rectangle() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return $length * $width;
};
  function area_square($side_length) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($side_length < 0.0) {
  echo rtrim('ValueError: area_square() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return $side_length * $side_length;
};
  function area_triangle($base, $height) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($base < 0.0 || $height < 0.0) {
  echo rtrim('ValueError: area_triangle() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return ($base * $height) / 2.0;
};
  function area_triangle_three_sides($side1, $side2, $side3) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($side1 < 0.0 || $side2 < 0.0 || $side3 < 0.0) {
  echo rtrim('ValueError: area_triangle_three_sides() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  if ($side1 + $side2 < $side3 || $side1 + $side3 < $side2 || $side2 + $side3 < $side1) {
  echo rtrim('ValueError: Given three sides do not form a triangle'), PHP_EOL;
  return 0.0;
}
  $s = ($side1 + $side2 + $side3) / 2.0;
  $prod = $s * ($s - $side1) * ($s - $side2) * ($s - $side3);
  $res = sqrt_approx($prod);
  return $res;
};
  function area_parallelogram($base, $height) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($base < 0.0 || $height < 0.0) {
  echo rtrim('ValueError: area_parallelogram() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return $base * $height;
};
  function area_trapezium($base1, $base2, $height) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($base1 < 0.0 || $base2 < 0.0 || $height < 0.0) {
  echo rtrim('ValueError: area_trapezium() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return 0.5 * ($base1 + $base2) * $height;
};
  function area_circle($radius) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($radius < 0.0) {
  echo rtrim('ValueError: area_circle() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return $PI * $radius * $radius;
};
  function area_ellipse($radius_x, $radius_y) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($radius_x < 0.0 || $radius_y < 0.0) {
  echo rtrim('ValueError: area_ellipse() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return $PI * $radius_x * $radius_y;
};
  function area_rhombus($diagonal1, $diagonal2) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($diagonal1 < 0.0 || $diagonal2 < 0.0) {
  echo rtrim('ValueError: area_rhombus() only accepts non-negative values'), PHP_EOL;
  return 0.0;
}
  return 0.5 * $diagonal1 * $diagonal2;
};
  function area_reg_polygon($sides, $length) {
  global $PI, $TWO_PI, $TRI_THREE_SIDES;
  if ($sides < 3) {
  echo rtrim('ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides'), PHP_EOL;
  return 0.0;
}
  if ($length < 0.0) {
  echo rtrim('ValueError: area_reg_polygon() only accepts non-negative values as length of a side'), PHP_EOL;
  return 0.0;
}
  $n = floatval($sides);
  return ($n * $length * $length) / (4.0 * tan_approx($PI / $n));
};
  echo rtrim('[DEMO] Areas of various geometric shapes:'), PHP_EOL;
  echo rtrim('Rectangle: ' . _str(area_rectangle(10.0, 20.0))), PHP_EOL;
  echo rtrim('Square: ' . _str(area_square(10.0))), PHP_EOL;
  echo rtrim('Triangle: ' . _str(area_triangle(10.0, 10.0))), PHP_EOL;
  $TRI_THREE_SIDES = area_triangle_three_sides(5.0, 12.0, 13.0);
  echo rtrim('Triangle Three Sides: ' . _str($TRI_THREE_SIDES)), PHP_EOL;
  echo rtrim('Parallelogram: ' . _str(area_parallelogram(10.0, 20.0))), PHP_EOL;
  echo rtrim('Rhombus: ' . _str(area_rhombus(10.0, 20.0))), PHP_EOL;
  echo rtrim('Trapezium: ' . _str(area_trapezium(10.0, 20.0, 30.0))), PHP_EOL;
  echo rtrim('Circle: ' . _str(area_circle(20.0))), PHP_EOL;
  echo rtrim('Ellipse: ' . _str(area_ellipse(10.0, 20.0))), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  echo rtrim('Surface Areas of various geometric shapes:'), PHP_EOL;
  echo rtrim('Cube: ' . _str(surface_area_cube(20.0))), PHP_EOL;
  echo rtrim('Cuboid: ' . _str(surface_area_cuboid(10.0, 20.0, 30.0))), PHP_EOL;
  echo rtrim('Sphere: ' . _str(surface_area_sphere(20.0))), PHP_EOL;
  echo rtrim('Hemisphere: ' . _str(surface_area_hemisphere(20.0))), PHP_EOL;
  echo rtrim('Cone: ' . _str(surface_area_cone(10.0, 20.0))), PHP_EOL;
  echo rtrim('Conical Frustum: ' . _str(surface_area_conical_frustum(10.0, 20.0, 30.0))), PHP_EOL;
  echo rtrim('Cylinder: ' . _str(surface_area_cylinder(10.0, 20.0))), PHP_EOL;
  echo rtrim('Torus: ' . _str(surface_area_torus(20.0, 10.0))), PHP_EOL;
  echo rtrim('Equilateral Triangle: ' . _str(area_reg_polygon(3, 10.0))), PHP_EOL;
  echo rtrim('Square: ' . _str(area_reg_polygon(4, 10.0))), PHP_EOL;
  echo rtrim('Regular Pentagon: ' . _str(area_reg_polygon(5, 10.0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
