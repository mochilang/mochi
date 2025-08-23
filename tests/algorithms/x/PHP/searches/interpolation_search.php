<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
  function interpolation_search($arr, $item) {
  $left = 0;
  $right = count($arr) - 1;
  while ($left <= $right) {
  if ($arr[$left] == $arr[$right]) {
  if ($arr[$left] == $item) {
  return $left;
};
  return -1;
}
  $point = $left + (($item - $arr[$left]) * ($right - $left)) / ($arr[$right] - $arr[$left]);
  if ($point < 0 || $point >= count($arr)) {
  return -1;
}
  $current = $arr[$point];
  if ($current == $item) {
  return $point;
}
  if ($point < $left) {
  $right = $left;
  $left = $point;
} else {
  if ($point > $right) {
  $left = $right;
  $right = $point;
} else {
  if ($item < $current) {
  $right = $point - 1;
} else {
  $left = $point + 1;
};
};
}
};
  return -1;
};
  function interpolation_search_recursive($arr, $item, $left, $right) {
  if ($left > $right) {
  return -1;
}
  if ($arr[$left] == $arr[$right]) {
  if ($arr[$left] == $item) {
  return $left;
};
  return -1;
}
  $point = $left + (($item - $arr[$left]) * ($right - $left)) / ($arr[$right] - $arr[$left]);
  if ($point < 0 || $point >= count($arr)) {
  return -1;
}
  if ($arr[$point] == $item) {
  return $point;
}
  if ($point < $left) {
  return interpolation_search_recursive($arr, $item, $point, $left);
}
  if ($point > $right) {
  return interpolation_search_recursive($arr, $item, $right, $left);
}
  if ($arr[$point] > $item) {
  return interpolation_search_recursive($arr, $item, $left, $point - 1);
}
  return interpolation_search_recursive($arr, $item, $point + 1, $right);
};
  function interpolation_search_by_recursion($arr, $item) {
  return interpolation_search_recursive($arr, $item, 0, count($arr) - 1);
};
  echo rtrim(_str(interpolation_search([1, 2, 3, 4, 5], 2))), PHP_EOL;
  echo rtrim(_str(interpolation_search([1, 2, 3, 4, 5], 6))), PHP_EOL;
  echo rtrim(_str(interpolation_search_by_recursion([0, 5, 7, 10, 15], 5))), PHP_EOL;
  echo rtrim(_str(interpolation_search_by_recursion([0, 5, 7, 10, 15], 100))), PHP_EOL;
  echo rtrim(_str(interpolation_search_by_recursion([5, 5, 5, 5, 5], 3))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
