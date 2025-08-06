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
  function sqrtApprox($x) {
  global $PI;
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function atanApprox($x) {
  global $PI;
  if ($x > 1.0) {
  return $PI / 2.0 - $x / ($x * $x + 0.28);
}
  if ($x < (-1.0)) {
  return -$PI / 2.0 - $x / ($x * $x + 0.28);
}
  return $x / (1.0 + 0.28 * $x * $x);
};
  function atan2Approx($y, $x) {
  global $PI;
  if ($x > 0.0) {
  $r = atanApprox($y / $x);
  return $r;
}
  if ($x < 0.0) {
  if ($y >= 0.0) {
  return atanApprox($y / $x) + $PI;
};
  return atanApprox($y / $x) - $PI;
}
  if ($y > 0.0) {
  return $PI / 2.0;
}
  if ($y < 0.0) {
  return -$PI / 2.0;
}
  return 0.0;
};
  function deg($rad) {
  global $PI;
  return $rad * 180.0 / $PI;
};
  function mochi_floor($x) {
  global $PI;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function pow10($n) {
  global $PI;
  $p = 1.0;
  $i = 0;
  while ($i < $n) {
  $p = $p * 10.0;
  $i = $i + 1;
};
  return $p;
};
  function mochi_round($x, $n) {
  global $PI;
  $m = pow10($n);
  return mochi_floor($x * $m + 0.5) / $m;
};
  function rectangular_to_polar($real, $img) {
  global $PI;
  $mod = mochi_round(sqrtApprox($real * $real + $img * $img), 2);
  $ang = mochi_round(deg(atan2Approx($img, $real)), 2);
  return [$mod, $ang];
};
  function show($real, $img) {
  global $PI;
  $r = rectangular_to_polar($real, $img);
  echo rtrim(_str($r)), PHP_EOL;
};
  show(5.0, -5.0);
  show(-1.0, 1.0);
  show(-1.0, -1.0);
  show(0.0000000001, 0.0000000001);
  show(-0.0000000001, 0.0000000001);
  show(9.75, 5.93);
  show(10000.0, 99999.0);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
