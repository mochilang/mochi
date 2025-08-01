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
  $b3Seg = 30;
  function pixelFromRgb($rgb) {
  global $b3Seg;
  $r = intval(((_intdiv($rgb, 65536)) % 256));
  $g = intval(((_intdiv($rgb, 256)) % 256));
  $b = intval(($rgb % 256));
  return ['r' => $r, 'g' => $g, 'b' => $b];
};
  function newBitmap($cols, $rows) {
  global $b3Seg, $b;
  $d = [];
  $y = 0;
  while ($y < $rows) {
  $row = [];
  $x = 0;
  while ($x < $cols) {
  $row = array_merge($row, [['r' => 0, 'g' => 0, 'b' => 0]]);
  $x = $x + 1;
};
  $d = array_merge($d, [$row]);
  $y = $y + 1;
};
  return ['cols' => $cols, 'rows' => $rows, 'data' => $d];
};
  function setPx(&$b, $x, $y, $p) {
  global $b3Seg;
  $cols = intval($b['cols']);
  $rows = intval($b['rows']);
  if ($x >= 0 && $x < $cols && $y >= 0 && $y < $rows) {
  $b['data'][$y][$x] = $p;
}
};
  function fill(&$b, $p) {
  global $b3Seg;
  $cols = intval($b['cols']);
  $rows = intval($b['rows']);
  $y = 0;
  while ($y < $rows) {
  $x = 0;
  while ($x < $cols) {
  $b['data'][$y][$x] = $p;
  $x = $x + 1;
};
  $y = $y + 1;
};
};
  function fillRgb(&$b, $rgb) {
  global $b3Seg;
  fill($b, pixelFromRgb($rgb));
};
  function line(&$b, $x0, $y0, $x1, $y1, $p) {
  global $b3Seg;
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
  setPx($b, $x0, $y0, $p);
  if ($x0 == $x1 && $y0 == $y1) {
  break;
}
  $e2 = 2 * $err;
  if ($e2 > (0 - $dy)) {
  $err = $err - $dy;
  $x0 = $x0 + $sx;
}
  if ($e2 < $dx) {
  $err = $err + $dx;
  $y0 = $y0 + $sy;
}
};
};
  function bezier3(&$b, $x1, $y1, $x2, $y2, $x3, $y3, $x4, $y4, $p) {
  global $b3Seg;
  $px = [];
  $py = [];
  $i = 0;
  while ($i <= $b3Seg) {
  $px = array_merge($px, [0]);
  $py = array_merge($py, [0]);
  $i = $i + 1;
};
  $fx1 = floatval($x1);
  $fy1 = floatval($y1);
  $fx2 = floatval($x2);
  $fy2 = floatval($y2);
  $fx3 = floatval($x3);
  $fy3 = floatval($y3);
  $fx4 = floatval($x4);
  $fy4 = floatval($y4);
  $i = 0;
  while ($i <= $b3Seg) {
  $d = (floatval($i)) / (floatval($b3Seg));
  $a = 1.0 - $d;
  $bcoef = $a * $a;
  $ccoef = $d * $d;
  $a2 = $a * $bcoef;
  $b2 = 3.0 * $bcoef * $d;
  $c2 = 3.0 * $a * $ccoef;
  $d2 = $ccoef * $d;
  $px[$i] = intval(($a2 * $fx1 + $b2 * $fx2 + $c2 * $fx3 + $d2 * $fx4));
  $py[$i] = intval(($a2 * $fy1 + $b2 * $fy2 + $c2 * $fy3 + $d2 * $fy4));
  $i = $i + 1;
};
  $x0 = $px[0];
  $y0 = $py[0];
  $i = 1;
  while ($i <= $b3Seg) {
  $x = $px[$i];
  $y = $py[$i];
  line($b, $x0, $y0, $x, $y, $p);
  $x0 = $x;
  $y0 = $y;
  $i = $i + 1;
};
};
  $b = newBitmap(400, 300);
  fillRgb($b, 16773055);
  bezier3($b, 20, 200, 700, 50, -300, 50, 380, 150, pixelFromRgb(4165615));
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
