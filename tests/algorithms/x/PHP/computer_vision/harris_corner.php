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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  function zeros($h, $w) {
  global $img, $corners;
  $m = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $row = _append($row, 0.0);
  $x = $x + 1;
};
  $m = _append($m, $row);
  $y = $y + 1;
};
  return $m;
};
  function gradient($img) {
  global $corners;
  $h = count($img);
  $w = count($img[0]);
  $dx = zeros($h, $w);
  $dy = zeros($h, $w);
  $y = 1;
  while ($y < $h - 1) {
  $x = 1;
  while ($x < $w - 1) {
  $dx[$y][$x] = (floatval($img[$y][$x + 1])) - (floatval($img[$y][$x - 1]));
  $dy[$y][$x] = (floatval($img[$y + 1][$x])) - (floatval($img[$y - 1][$x]));
  $x = $x + 1;
};
  $y = $y + 1;
};
  return [$dx, $dy];
};
  function harris($img, $k, $window, $thresh) {
  $h = count($img);
  $w = count($img[0]);
  $grads = gradient($img);
  $dx = $grads[0];
  $dy = $grads[1];
  $ixx = zeros($h, $w);
  $iyy = zeros($h, $w);
  $ixy = zeros($h, $w);
  $y = 0;
  while ($y < $h) {
  $x = 0;
  while ($x < $w) {
  $gx = $dx[$y][$x];
  $gy = $dy[$y][$x];
  $ixx[$y][$x] = $gx * $gx;
  $iyy[$y][$x] = $gy * $gy;
  $ixy[$y][$x] = $gx * $gy;
  $x = $x + 1;
};
  $y = $y + 1;
};
  $offset = _intdiv($window, 2);
  $corners = [];
  $y = $offset;
  while ($y < $h - $offset) {
  $x = $offset;
  while ($x < $w - $offset) {
  $wxx = 0.0;
  $wyy = 0.0;
  $wxy = 0.0;
  $yy = $y - $offset;
  while ($yy <= $y + $offset) {
  $xx = $x - $offset;
  while ($xx <= $x + $offset) {
  $wxx = $wxx + $ixx[$yy][$xx];
  $wyy = $wyy + $iyy[$yy][$xx];
  $wxy = $wxy + $ixy[$yy][$xx];
  $xx = $xx + 1;
};
  $yy = $yy + 1;
};
  $det = $wxx * $wyy - ($wxy * $wxy);
  $trace = $wxx + $wyy;
  $r = $det - $k * ($trace * $trace);
  if ($r > $thresh) {
  $corners = _append($corners, [$x, $y]);
}
  $x = $x + 1;
};
  $y = $y + 1;
};
  return $corners;
};
  $img = [[1, 1, 1, 1, 1], [1, 255, 255, 255, 1], [1, 255, 0, 255, 1], [1, 255, 255, 255, 1], [1, 1, 1, 1, 1]];
  $corners = harris($img, 0.04, 3, 10000000000.0);
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($corners, 1344))))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
