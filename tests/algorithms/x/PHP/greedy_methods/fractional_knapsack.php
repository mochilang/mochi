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
  function sort_by_ratio_desc($arr) {
  global $result, $vl, $wt;
  $i = 1;
  while ($i < count($arr)) {
  $key = $arr[$i];
  $j = $i - 1;
  while ($j >= 0) {
  $current = $arr[$j];
  if ($current['value'] / $current['weight'] < $key['value'] / $key['weight']) {
  $arr[$j + 1] = $current;
  $j = $j - 1;
} else {
  break;
}
};
  $arr[$j + 1] = $key;
  $i = $i + 1;
};
  return $arr;
};
  function sum_first($arr, $k) {
  global $result, $vl, $wt;
  $s = 0.0;
  $i = 0;
  while ($i < $k && $i < count($arr)) {
  $s = $s + $arr[$i];
  $i = $i + 1;
};
  return $s;
};
  function frac_knapsack($vl, $wt, $w, $n) {
  global $result;
  $items = [];
  $i = 0;
  while ($i < count($vl) && $i < count($wt)) {
  $items = _append($items, ['value' => $vl[$i], 'weight' => $wt[$i]]);
  $i = $i + 1;
};
  $items = sort_by_ratio_desc($items);
  $values = [];
  $weights = [];
  $i = 0;
  while ($i < count($items)) {
  $itm = $items[$i];
  $values = _append($values, $itm['value']);
  $weights = _append($weights, $itm['weight']);
  $i = $i + 1;
};
  $acc = [];
  $total = 0.0;
  $i = 0;
  while ($i < count($weights)) {
  $total = $total + $weights[$i];
  $acc = _append($acc, $total);
  $i = $i + 1;
};
  $k = 0;
  while ($k < count($acc) && $w >= $acc[$k]) {
  $k = $k + 1;
};
  if ($k == 0) {
  return 0.0;
}
  if ($k >= count($values)) {
  return sum_first($values, count($values));
}
  if ($k != $n) {
  return sum_first($values, $k) + ($w - $acc[$k - 1]) * $values[$k] / $weights[$k];
}
  return sum_first($values, $k);
};
  $vl = [60.0, 100.0, 120.0];
  $wt = [10.0, 20.0, 30.0];
  $result = frac_knapsack($vl, $wt, 50.0, 3);
  echo rtrim(_str($result)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
