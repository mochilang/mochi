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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  function _mod($x, $m) {
  global $PI, $width, $height, $depth, $angle, $length, $frac;
  return $x - (floatval(intval(($x / $m)))) * $m;
};
  function _sin($x) {
  global $PI, $width, $height, $depth, $angle, $length, $frac;
  $y = _mod($x + $PI, 2.0 * $PI) - $PI;
  $y2 = $y * $y;
  $y3 = $y2 * $y;
  $y5 = $y3 * $y2;
  $y7 = $y5 * $y2;
  return $y - $y3 / 6.0 + $y5 / 120.0 - $y7 / 5040.0;
};
  function _cos($x) {
  global $PI, $width, $height, $depth, $angle, $length, $frac;
  $y = _mod($x + $PI, 2.0 * $PI) - $PI;
  $y2 = $y * $y;
  $y4 = $y2 * $y2;
  $y6 = $y4 * $y2;
  return 1.0 - $y2 / 2.0 + $y4 / 24.0 - $y6 / 720.0;
};
  $width = 80;
  $height = 40;
  $depth = 6;
  $angle = 12.0;
  $length = 12.0;
  $frac = 0.8;
  function clearGrid() {
  global $PI, $width, $height, $depth, $angle, $length, $frac;
  $g = [];
  $y = 0;
  while ($y < $height) {
  $row = [];
  $x = 0;
  while ($x < $width) {
  $row = array_merge($row, [' ']);
  $x = $x + 1;
};
  $g = array_merge($g, [$row]);
  $y = $y + 1;
};
  return $g;
};
  function drawPoint(&$g, $x, $y) {
  global $PI, $width, $height, $depth, $angle, $length, $frac;
  if ($x >= 0 && $x < $width && $y >= 0 && $y < $height) {
  $row = $g[$y];
  $row[$x] = '#';
  $g[$y] = $row;
}
};
  function bresenham($x0, $y0, $x1, $y1, &$g) {
  global $PI, $width, $height, $depth, $angle, $length, $frac;
  $dx = $x1 - $x0;
  if ($dx < 0) {
  $dx = -$dx;
}
  $dy = $y1 - $y0;
  if ($dy < 0) {
  $dy = -$dy;
}
  $sx = -1;
  if ($x0 < $x1) {
  $sx = 1;
}
  $sy = -1;
  if ($y0 < $y1) {
  $sy = 1;
}
  $err = $dx - $dy;
  while (true) {
  drawPoint($g, $x0, $y0);
  if ($x0 == $x1 && $y0 == $y1) {
  break;
}
  $e2 = 2 * $err;
  if ($e2 > (-$dy)) {
  $err = $err - $dy;
  $x0 = $x0 + $sx;
}
  if ($e2 < $dx) {
  $err = $err + $dx;
  $y0 = $y0 + $sy;
}
};
};
  function ftree(&$g, $x, $y, $dist, $dir, $d) {
  global $PI, $width, $height, $depth, $angle, $length, $frac;
  $rad = $dir * $PI / 180.0;
  $x2 = $x + $dist * _sin($rad);
  $y2 = $y - $dist * _cos($rad);
  bresenham(intval($x), intval($y), intval($x2), intval($y2), $g);
  if ($d > 0) {
  ftree($g, $x2, $y2, $dist * $frac, $dir - $angle, $d - 1);
  ftree($g, $x2, $y2, $dist * $frac, $dir + $angle, $d - 1);
}
};
  function render($g) {
  global $PI, $width, $height, $depth, $angle, $length, $frac;
  $out = '';
  $y = 0;
  while ($y < $height) {
  $line = '';
  $x = 0;
  while ($x < $width) {
  $line = $line . $g[$y][$x];
  $x = $x + 1;
};
  $out = $out . $line;
  if ($y < $height - 1) {
  $out = $out . '
';
}
  $y = $y + 1;
};
  return $out;
};
  function main() {
  global $PI, $width, $height, $depth, $angle, $length, $frac;
  $grid = clearGrid();
  ftree($grid, floatval((_intdiv($width, 2))), floatval(($height - 1)), $length, 0.0, $depth);
  echo rtrim(render($grid)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
