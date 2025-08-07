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
  $N_STEPS = 1000;
  function mochi_floor($x) {
  global $N_STEPS;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function pow10($n) {
  global $N_STEPS;
  $p = 1.0;
  $i = 0;
  while ($i < $n) {
  $p = $p * 10.0;
  $i = $i + 1;
};
  return $p;
};
  function mochi_round($x, $n) {
  global $N_STEPS;
  $m = pow10($n);
  return mochi_floor($x * $m + 0.5) / $m;
};
  function simpson_integration($f, $a, $b, $precision) {
  global $N_STEPS;
  if ($precision <= 0) {
  $panic('precision should be positive');
}
  $h = ($b - $a) / (floatval($N_STEPS));
  $result = $f($a) + $f($b);
  $i = 1;
  while ($i < $N_STEPS) {
  $x = $a + $h * (floatval($i));
  if ($i % 2 == 1) {
  $result = $result + 4.0 * $f($x);
} else {
  $result = $result + 2.0 * $f($x);
}
  $i = $i + 1;
};
  $result = $result * ($h / 3.0);
  $r = mochi_round($result, $precision);
  return $r;
};
  function square($x) {
  global $N_STEPS;
  return $x * $x;
};
  echo rtrim(_str(simpson_integration('square', 1.0, 2.0, 3))), PHP_EOL;
  echo rtrim(_str(simpson_integration('square', 3.45, 3.2, 1))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
