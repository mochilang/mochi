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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function int_to_float($x) {
  return _imul($x, 1.0);
};
  function floor_int($x) {
  $i = 0;
  while (int_to_float(_iadd($i, 1)) <= $x) {
  $i = _iadd($i, 1);
};
  return $i;
};
  function set_at_float($xs, $idx, $value) {
  $i = 0;
  $res = [];
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
};
  function set_at_list_float($xs, $idx, $value) {
  $i = 0;
  $res = [];
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
};
  function sort_float($xs) {
  $res = $xs;
  $i = 1;
  while ($i < count($res)) {
  $key = $res[$i];
  $j = _isub($i, 1);
  while ($j >= 0 && $res[$j] > $key) {
  $res = set_at_float($res, _iadd($j, 1), $res[$j]);
  $j = _isub($j, 1);
};
  $res = set_at_float($res, _iadd($j, 1), $key);
  $i = _iadd($i, 1);
};
  return $res;
};
  function bucket_sort_with_count($xs, $bucket_count) {
  if (count($xs) == 0 || $bucket_count <= 0) {
  return [];
}
  $min_value = $xs[0];
  $max_value = $xs[0];
  $i = 1;
  while ($i < count($xs)) {
  if ($xs[$i] < $min_value) {
  $min_value = $xs[$i];
}
  if ($xs[$i] > $max_value) {
  $max_value = $xs[$i];
}
  $i = _iadd($i, 1);
};
  if ($max_value == $min_value) {
  return $xs;
}
  $bucket_size = ($max_value - $min_value) / int_to_float($bucket_count);
  $buckets = [];
  $i = 0;
  while ($i < $bucket_count) {
  $buckets = _append($buckets, []);
  $i = _iadd($i, 1);
};
  $i = 0;
  while ($i < count($xs)) {
  $val = $xs[$i];
  $idx = floor_int(($val - $min_value) / $bucket_size);
  if ($idx < 0) {
  $idx = 0;
}
  if ($idx >= $bucket_count) {
  $idx = _isub($bucket_count, 1);
}
  $bucket = $buckets[$idx];
  $bucket = _append($bucket, $val);
  $buckets = set_at_list_float($buckets, $idx, $bucket);
  $i = _iadd($i, 1);
};
  $result = [];
  $i = 0;
  while ($i < count($buckets)) {
  $sorted_bucket = sort_float($buckets[$i]);
  $j = 0;
  while ($j < count($sorted_bucket)) {
  $result = _append($result, $sorted_bucket[$j]);
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $result;
};
  function bucket_sort($xs) {
  return bucket_sort_with_count($xs, 10);
};
  echo rtrim(_str(bucket_sort([-1.0, 2.0, -5.0, 0.0]))), PHP_EOL;
  echo rtrim(_str(bucket_sort([9.0, 8.0, 7.0, 6.0, -12.0]))), PHP_EOL;
  echo rtrim(_str(bucket_sort([0.4, 1.2, 0.1, 0.2, -0.9]))), PHP_EOL;
  echo rtrim(_str(bucket_sort([]))), PHP_EOL;
  echo rtrim(_str(bucket_sort([-10000000000.0, 10000000000.0]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
