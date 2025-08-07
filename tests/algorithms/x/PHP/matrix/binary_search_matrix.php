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
  function binary_search($arr, $lower_bound, $upper_bound, $value) {
  $r = _intdiv(($lower_bound + $upper_bound), 2);
  if ($arr[$r] == $value) {
  return $r;
}
  if ($lower_bound >= $upper_bound) {
  return -1;
}
  if ($arr[$r] < $value) {
  return binary_search($arr, $r + 1, $upper_bound, $value);
}
  return binary_search($arr, $lower_bound, $r - 1, $value);
};
  function mat_bin_search($value, $matrix) {
  $index = 0;
  if ($matrix[$index][0] == $value) {
  return [$index, 0];
}
  while ($index < count($matrix) && $matrix[$index][0] < $value) {
  $r = binary_search($matrix[$index], 0, count($matrix[$index]) - 1, $value);
  if ($r != (-1)) {
  return [$index, $r];
}
  $index = $index + 1;
};
  return [-1, -1];
};
  function main() {
  $row = [1, 4, 7, 11, 15];
  echo rtrim(_str(binary_search($row, 0, count($row) - 1, 1))), PHP_EOL;
  echo rtrim(_str(binary_search($row, 0, count($row) - 1, 23))), PHP_EOL;
  $matrix = [[1, 4, 7, 11, 15], [2, 5, 8, 12, 19], [3, 6, 9, 16, 22], [10, 13, 14, 17, 24], [18, 21, 23, 26, 30]];
  echo rtrim(_str(mat_bin_search(1, $matrix))), PHP_EOL;
  echo rtrim(_str(mat_bin_search(34, $matrix))), PHP_EOL;
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
