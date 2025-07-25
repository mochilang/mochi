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
$__start_mem = memory_get_usage();
$__start = _now();
  $xMin = -2.182;
  $xMax = 2.6558;
  $yMin = 0.0;
  $yMax = 9.9983;
  $width = 60;
  $nIter = 10000;
  $dx = $xMax - $xMin;
  $dy = $yMax - $yMin;
  $height = intval(($width * $dy / $dx));
  $grid = [];
  $row = 0;
  while ($row < $height) {
  $line = [];
  $col = 0;
  while ($col < $width) {
  $line = array_merge($line, [' ']);
  $col = $col + 1;
};
  $grid = array_merge($grid, [$line]);
  $row = $row + 1;
}
  $seed = 1;
  function randInt($s, $n) {
  global $xMin, $xMax, $yMin, $yMax, $width, $nIter, $dx, $dy, $height, $grid, $row, $line, $col, $seed, $x, $y, $ix, $iy, $i, $res, $r, $nx, $ny;
  $next = ($s * 1664525 + 1013904223) % 2147483647;
  return [$next, $next % $n];
};
  $x = 0.0;
  $y = 0.0;
  $ix = intval(((floatval($width)) * ($x - $xMin) / $dx));
  $iy = intval(((floatval($height)) * ($yMax - $y) / $dy));
  if ($ix >= 0 && $ix < $width && $iy >= 0 && $iy < $height) {
  $grid[$iy][$ix] = '*';
}
  $i = 0;
  while ($i < $nIter) {
  $res = randInt($seed, 100);
  $seed = $res[0];
  $r = $res[1];
  if ($r < 85) {
  $nx = 0.85 * $x + 0.04 * $y;
  $ny = -0.04 * $x + 0.85 * $y + 1.6;
  $x = $nx;
  $y = $ny;
} else {
  if ($r < 92) {
  $nx = 0.2 * $x - 0.26 * $y;
  $ny = 0.23 * $x + 0.22 * $y + 1.6;
  $x = $nx;
  $y = $ny;
} else {
  if ($r < 99) {
  $nx = -0.15 * $x + 0.28 * $y;
  $ny = 0.26 * $x + 0.24 * $y + 0.44;
  $x = $nx;
  $y = $ny;
} else {
  $x = 0.0;
  $y = 0.16 * $y;
};
};
}
  $ix = intval(((floatval($width)) * ($x - $xMin) / $dx));
  $iy = intval(((floatval($height)) * ($yMax - $y) / $dy));
  if ($ix >= 0 && $ix < $width && $iy >= 0 && $iy < $height) {
  $grid[$iy][$ix] = '*';
}
  $i = $i + 1;
}
  $row = 0;
  while ($row < $height) {
  $line = '';
  $col = 0;
  while ($col < $width) {
  $line = $line . $grid[$row][$col];
  $col = $col + 1;
};
  echo rtrim($line), PHP_EOL;
  $row = $row + 1;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
