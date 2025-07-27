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
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  $TWO_PI = 6.283185307179586;
  function _mod($x, $m) {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
  return $x - (floatval(intval(($x / $m)))) * $m;
};
  function _sin($x) {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y3 = $y2 * $y;
  $y5 = $y3 * $y2;
  $y7 = $y5 * $y2;
  return $y - $y3 / 6.0 + $y5 / 120.0 - $y7 / 5040.0;
};
  function _cos($x) {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y4 = $y2 * $y2;
  $y6 = $y4 * $y2;
  return 1.0 - $y2 / 2.0 + $y4 / 24.0 - $y6 / 720.0;
};
  $nodes = [['x' => -1.0, 'y' => -1.0, 'z' => -1.0], ['x' => -1.0, 'y' => -1.0, 'z' => 1.0], ['x' => -1.0, 'y' => 1.0, 'z' => -1.0], ['x' => -1.0, 'y' => 1.0, 'z' => 1.0], ['x' => 1.0, 'y' => -1.0, 'z' => -1.0], ['x' => 1.0, 'y' => -1.0, 'z' => 1.0], ['x' => 1.0, 'y' => 1.0, 'z' => -1.0], ['x' => 1.0, 'y' => 1.0, 'z' => 1.0]];
  $edges = [[0, 1], [1, 3], [3, 2], [2, 0], [4, 5], [5, 7], [7, 6], [6, 4], [0, 4], [1, 5], [2, 6], [3, 7]];
  function rotate($p, $ax, $ay) {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
  $sinx = _sin($ax);
  $cosx = _cos($ax);
  $siny = _sin($ay);
  $cosy = _cos($ay);
  $x1 = $p['x'];
  $y1 = $p['y'] * $cosx - $p['z'] * $sinx;
  $z1 = $p['y'] * $sinx + $p['z'] * $cosx;
  $x2 = $x1 * $cosy + $z1 * $siny;
  $z2 = -$x1 * $siny + $z1 * $cosy;
  return ['x' => $x2, 'y' => $y1, 'z' => $z2];
};
  $width = 40;
  $height = 20;
  $distance = 3.0;
  $scale = 8.0;
  function project($p) {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
  $factor = $scale / ($p['z'] + $distance);
  $x = intval(($p['x'] * $factor)) + _intdiv($width, 2);
  $y = intval((-$p['y'] * $factor)) + _intdiv($height, 2);
  return ['x' => $x, 'y' => $y];
};
  function clearGrid() {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
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
  function drawPoint(&$g, $x, $y, $ch) {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
  if ($x >= 0 && $x < $width && $y >= 0 && $y < $height) {
  $row = $g[$y];
  $row[$x] = $ch;
  $g[$y] = $row;
}
};
  function bresenham($x0, $y0, $x1, $y1, &$g, $ch) {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
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
  drawPoint($g, $x0, $y0, $ch);
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
  function render($g) {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
  $out = '';
  $y = 0;
  while ($y < $height) {
  $line = '';
  $x = 0;
  while ($x < $width) {
  $line = $line . $g[$y][$x];
  $x = $x + 1;
};
  $out = $out . $line . '
';
  $y = $y + 1;
};
  return $out;
};
  function main() {
  global $PI, $TWO_PI, $nodes, $edges, $width, $height, $distance, $scale;
  $f = 0;
  while ($f < 10) {
  $grid = clearGrid();
  $rot = [];
  $i = 0;
  $ay = ($PI / 4.0) + (floatval($f)) * $PI / 10.0;
  while ($i < count($nodes)) {
  $p = rotate($nodes[$i], $PI / 4.0, $ay);
  $pp = project($p);
  $rot = array_merge($rot, [$pp]);
  $i = $i + 1;
};
  $e = 0;
  while ($e < count($edges)) {
  $a = $edges[$e][0];
  $b = $edges[$e][1];
  $p1 = $rot[$a];
  $p2 = $rot[$b];
  bresenham($p1['x'], $p1['y'], $p2['x'], $p2['y'], $grid, '#');
  $e = $e + 1;
};
  echo rtrim(render($grid)), PHP_EOL;
  $f = $f + 1;
};
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
