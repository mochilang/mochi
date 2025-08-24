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
  function tail($xs) {
  $res = [];
  $i = 1;
  while ($i < count($xs)) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
};
  function rotate_left($xs) {
  if (count($xs) == 0) {
  return $xs;
}
  $res = [];
  $i = 1;
  while ($i < count($xs)) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  $res = _append($res, $xs[0]);
  return $res;
};
  function permute_recursive($nums) {
  if (count($nums) == 0) {
  $base = [];
  return _append($base, []);
}
  $result = [];
  $current = $nums;
  $count = 0;
  while ($count < count($nums)) {
  $n = $current[0];
  $rest = tail($current);
  $perms = permute_recursive($rest);
  $j = 0;
  while ($j < count($perms)) {
  $perm = _append($perms[$j], $n);
  $result = _append($result, $perm);
  $j = $j + 1;
};
  $current = rotate_left($current);
  $count = $count + 1;
};
  return $result;
};
  function swap($xs, $i, $j) {
  $res = [];
  $k = 0;
  while ($k < count($xs)) {
  if ($k == $i) {
  $res = _append($res, $xs[$j]);
} else {
  if ($k == $j) {
  $res = _append($res, $xs[$i]);
} else {
  $res = _append($res, $xs[$k]);
};
}
  $k = $k + 1;
};
  return $res;
};
  function permute_backtrack_helper($nums, $start, $output) {
  if ($start == count($nums) - 1) {
  return _append($output, $nums);
}
  $i = $start;
  $res = $output;
  while ($i < count($nums)) {
  $swapped = swap($nums, $start, $i);
  $res = permute_backtrack_helper($swapped, $start + 1, $res);
  $i = $i + 1;
};
  return $res;
};
  function permute_backtrack($nums) {
  $output = [];
  return permute_backtrack_helper($nums, 0, $output);
};
  echo rtrim(_str(permute_recursive([1, 2, 3]))), PHP_EOL;
  echo rtrim(_str(permute_backtrack([1, 2, 3]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
