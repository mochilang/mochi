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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function leaky_rectified_linear_unit($vector, $alpha) {
  global $vector1, $result1, $vector2, $result2;
  $result = [];
  $i = 0;
  while ($i < count($vector)) {
  $x = $vector[$i];
  if ($x > 0.0) {
  $result = _append($result, $x);
} else {
  $result = _append($result, $alpha * $x);
}
  $i = $i + 1;
};
  return $result;
};
  $vector1 = [2.3, 0.6, -2.0, -3.8];
  $result1 = leaky_rectified_linear_unit($vector1, 0.3);
  echo rtrim(_str($result1)), PHP_EOL;
  $vector2 = [-9.2, -0.3, 0.45, -4.56];
  $result2 = leaky_rectified_linear_unit($vector2, 0.067);
  echo rtrim(_str($result2)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
