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
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function sortFloat($xs) {
  global $x1, $x2, $x3;
  $arr = $xs;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $t = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $t;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function ceilf($x) {
  global $x1, $x2, $x3;
  $i = intval($x);
  if ($x > (floatval($i))) {
  return $i + 1;
}
  return $i;
};
  function fivenum($a) {
  global $x1, $x2, $x3;
  $arr = sortFloat($a);
  $n = count($arr);
  $half = ($n + 3) - (($n + 3) % 2);
  $n4 = floatval((_intdiv($half, 2))) / 2.0;
  $nf = floatval($n);
  $d = [1.0, $n4, ($nf + 1.0) / 2.0, $nf + 1.0 - $n4, $nf];
  $result = [];
  $idx = 0;
  while ($idx < count($d)) {
  $de = $d[$idx];
  $fl = intval($de - 1.0);
  $cl = ceilf($de - 1.0);
  $result = array_merge($result, [0.5 * ($arr[$fl] + $arr[$cl])]);
  $idx = $idx + 1;
};
  return $result;
};
  $x1 = [36.0, 40.0, 7.0, 39.0, 41.0, 15.0];
  $x2 = [15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0];
  $x3 = [0.14082834, 0.0974879, 1.73131507, 0.87636009, -1.95059594, 0.73438555, -0.03035726, 1.4667597, -0.74621349, -0.72588772, 0.6390516, 0.61501527, -0.9898378, -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578];
  echo rtrim(_str(fivenum($x1))), PHP_EOL;
  echo rtrim(_str(fivenum($x2))), PHP_EOL;
  echo rtrim(_str(fivenum($x3))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
