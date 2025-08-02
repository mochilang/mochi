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
  $width = 60;
  $height = intval((floatval($width) * 0.86602540378));
  $iterations = 5000;
  $grid = [];
  $y = 0;
  while ($y < $height) {
  $line = [];
  $x = 0;
  while ($x < $width) {
  $line = array_merge($line, [' ']);
  $x = $x + 1;
};
  $grid = array_merge($grid, [$line]);
  $y = $y + 1;
}
  function randInt($s, $n) {
  global $width, $height, $iterations, $grid, $y, $line, $x, $seed, $vertices, $px, $py, $i, $r, $idx, $v;
  $next = ($s * 1664525 + 1013904223) % 2147483647;
  return [$next, $next % $n];
};
  $seed = 1;
  $vertices = [[0, $height - 1], [$width - 1, $height - 1], [intval((_intdiv($width, 2))), 0]];
  $px = intval((_intdiv($width, 2)));
  $py = intval((_intdiv($height, 2)));
  $i = 0;
  while ($i < $iterations) {
  $r = randInt($seed, 3);
  $seed = $r[0];
  $idx = intval($r[1]);
  $v = $vertices[$idx];
  $px = intval((($px + $v[0]) / 2));
  $py = intval((($py + $v[1]) / 2));
  if ($px >= 0 && $px < $width && $py >= 0 && $py < $height) {
  $grid[$py][$px] = '*';
}
  $i = $i + 1;
}
  $y = 0;
  while ($y < $height) {
  $line = '';
  $x = 0;
  while ($x < $width) {
  $line = $line . $grid[$y][$x];
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
