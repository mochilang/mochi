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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function set_at_int($xs, $idx, $value) {
  $i = 0;
  $res = [];
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function sort_int($xs) {
  $res = $xs;
  $i = 1;
  while ($i < count($res)) {
  $key = $res[$i];
  $j = $i - 1;
  while ($j >= 0 && $res[$j] > $key) {
  $res = set_at_int($res, $j + 1, $res[$j]);
  $j = $j - 1;
};
  $res = set_at_int($res, $j + 1, $key);
  $i = $i + 1;
};
  return $res;
};
  function median_of_five($arr) {
  $sorted = sort_int($arr);
  return $sorted[count($sorted) / 2];
};
  function median_of_medians($arr) {
  if (count($arr) <= 5) {
  return median_of_five($arr);
}
  $medians = [];
  $i = 0;
  while ($i < count($arr)) {
  if ($i + 5 <= count($arr)) {
  $medians = _append($medians, median_of_five(array_slice($arr, $i, $i + 5 - $i)));
} else {
  $medians = _append($medians, median_of_five(array_slice($arr, $i, count($arr) - $i)));
}
  $i = $i + 5;
};
  return median_of_medians($medians);
};
  function quick_select($arr, $target) {
  if ($target > count($arr)) {
  return -1;
}
  $x = median_of_medians($arr);
  $left = [];
  $right = [];
  $check = false;
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] < $x) {
  $left = _append($left, $arr[$i]);
} else {
  if ($arr[$i] > $x) {
  $right = _append($right, $arr[$i]);
} else {
  if ($arr[$i] == $x) {
  if (!$check) {
  $check = true;
} else {
  $right = _append($right, $arr[$i]);
};
} else {
  $right = _append($right, $arr[$i]);
};
};
}
  $i = $i + 1;
};
  $rank_x = count($left) + 1;
  $answer = 0;
  if ($rank_x == $target) {
  $answer = $x;
} else {
  if ($rank_x > $target) {
  $answer = quick_select($left, $target);
} else {
  $answer = quick_select($right, $target - $rank_x);
};
}
  return $answer;
};
  function main() {
  echo rtrim(_str(median_of_five([5, 4, 3, 2]))), PHP_EOL;
  echo rtrim(_str(quick_select([2, 4, 5, 7, 899, 54, 32], 5))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
