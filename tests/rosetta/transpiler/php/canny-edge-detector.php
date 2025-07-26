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
  function conv2d($img, $k) {
  global $PI;
  $h = count($img);
  $w = count($img[0]);
  $n = count($k);
  $half = _intdiv($n, 2);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $sum = 0.0;
  $j = 0;
  while ($j < $n) {
  $i = 0;
  while ($i < $n) {
  $yy = $y + $j - $half;
  if ($yy < 0) {
  $yy = 0;
}
  if ($yy >= $h) {
  $yy = $h - 1;
}
  $xx = $x + $i - $half;
  if ($xx < 0) {
  $xx = 0;
}
  if ($xx >= $w) {
  $xx = $w - 1;
}
  $sum = $sum + $img[$yy][$xx] * $k[$j][$i];
  $i = $i + 1;
};
  $j = $j + 1;
};
  $row = array_merge($row, [$sum]);
  $x = $x + 1;
};
  $out = array_merge($out, [$row]);
  $y = $y + 1;
};
  return $out;
};
  function gradient($img) {
  global $PI;
  $hx = [[-1.0, 0.0, 1.0], [-2.0, 0.0, 2.0], [-1.0, 0.0, 1.0]];
  $hy = [[1.0, 2.0, 1.0], [0.0, 0.0, 0.0], [-1.0, -2.0, -1.0]];
  $gx = conv2d($img, $hx);
  $gy = conv2d($img, $hy);
  $h = count($img);
  $w = count($img[0]);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $g = $gx[$y][$x] * $gx[$y][$x] + $gy[$y][$x] * $gy[$y][$x];
  $row = array_merge($row, [$g]);
  $x = $x + 1;
};
  $out = array_merge($out, [$row]);
  $y = $y + 1;
};
  return $out;
};
  function threshold($g, $t) {
  global $PI;
  $h = count($g);
  $w = count($g[0]);
  $out = [];
  $y = 0;
  while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  if ($g[$y][$x] >= $t) {
  $row = array_merge($row, [1]);
} else {
  $row = array_merge($row, [0]);
}
  $x = $x + 1;
};
  $out = array_merge($out, [$row]);
  $y = $y + 1;
};
  return $out;
};
  function printMatrix($m) {
  global $PI;
  $y = 0;
  while ($y < count($m)) {
  $line = '';
  $x = 0;
  while ($x < count($m[0])) {
  $line = $line . _str($m[$y][$x]);
  if ($x < count($m[0]) - 1) {
  $line = $line . ' ';
}
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
};
};
  function main() {
  global $PI;
  $img = [[0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0]];
  $g = gradient($img);
  $edges = threshold($g, 1020.0 * 1020.0);
  printMatrix($edges);
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
