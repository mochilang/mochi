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
  function initGrid($size) {
  $g = [];
  $y = 0;
  while ($y < $size) {
  $row = [];
  $x = 0;
  while ($x < $size) {
  $row = array_merge($row, [' ']);
  $x = $x + 1;
};
  $g = array_merge($g, [$row]);
  $y = $y + 1;
};
  return $g;
};
  function set(&$g, $x, $y) {
  if ($x >= 0 && $x < count($g[0]) && $y >= 0 && $y < count($g)) {
  $g[$y][$x] = '#';
}
};
  function circle($r) {
  $size = $r * 2 + 1;
  $g = initGrid($size);
  $x = $r;
  $y = 0;
  $err = 1 - $r;
  while ($y <= $x) {
  set($g, $r + $x, $r + $y);
  set($g, $r + $y, $r + $x);
  set($g, $r - $x, $r + $y);
  set($g, $r - $y, $r + $x);
  set($g, $r - $x, $r - $y);
  set($g, $r - $y, $r - $x);
  set($g, $r + $x, $r - $y);
  set($g, $r + $y, $r - $x);
  $y = $y + 1;
  if ($err < 0) {
  $err = $err + 2 * $y + 1;
} else {
  $x = $x - 1;
  $err = $err + 2 * ($y - $x) + 1;
}
};
  return $g;
};
  function trimRight($row) {
  global $g;
  $end = count($row);
  while ($end > 0 && $row[$end - 1] == ' ') {
  $end = $end - 1;
};
  $s = '';
  $i = 0;
  while ($i < $end) {
  $s = $s . $row[$i];
  $i = $i + 1;
};
  return $s;
};
  $g = circle(10);
  foreach ($g as $row) {
  echo rtrim(trimRight($row)), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
