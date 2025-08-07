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
  function binomial_coefficient($total_elements, $elements_to_choose) {
  if ($elements_to_choose == 0 || $elements_to_choose == $total_elements) {
  return 1;
}
  $k = $elements_to_choose;
  if ($k > $total_elements - $k) {
  $k = $total_elements - $k;
}
  $coefficient = 1;
  $i = 0;
  while ($i < $k) {
  $coefficient = $coefficient * ($total_elements - $i);
  $coefficient = _intdiv($coefficient, ($i + 1));
  $i = $i + 1;
};
  return $coefficient;
};
  function bell_numbers($max_set_length) {
  if ($max_set_length < 0) {
  $panic('max_set_length must be non-negative');
}
  $bell = [];
  $i = 0;
  while ($i <= $max_set_length) {
  $bell = _append($bell, 0);
  $i = $i + 1;
};
  $bell[0] = 1;
  $i = 1;
  while ($i <= $max_set_length) {
  $j = 0;
  while ($j < $i) {
  $bell[$i] = $bell[$i] + binomial_coefficient($i - 1, $j) * $bell[$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $bell;
};
  function main() {
  echo rtrim(_str(bell_numbers(5))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
