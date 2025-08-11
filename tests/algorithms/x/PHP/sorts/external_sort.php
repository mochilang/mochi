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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
  function subarray($xs, $start, $end) {
  $result = [];
  $k = $start;
  while ($k < $end) {
  $result = _append($result, $xs[$k]);
  $k = _iadd($k, 1);
};
  return $result;
};
  function merge($left_half, $right_half) {
  $result = [];
  $i = 0;
  $j = 0;
  while ($i < count($left_half) && $j < count($right_half)) {
  if ($left_half[$i] < $right_half[$j]) {
  $result = _append($result, $left_half[$i]);
  $i = _iadd($i, 1);
} else {
  $result = _append($result, $right_half[$j]);
  $j = _iadd($j, 1);
}
};
  while ($i < count($left_half)) {
  $result = _append($result, $left_half[$i]);
  $i = _iadd($i, 1);
};
  while ($j < count($right_half)) {
  $result = _append($result, $right_half[$j]);
  $j = _iadd($j, 1);
};
  return $result;
};
  function merge_sort($array) {
  if (count($array) <= 1) {
  return $array;
}
  $middle = _idiv(count($array), 2);
  $left_half = subarray($array, 0, $middle);
  $right_half = subarray($array, $middle, count($array));
  $sorted_left = merge_sort($left_half);
  $sorted_right = merge_sort($right_half);
  return merge($sorted_left, $sorted_right);
};
  function split_into_blocks($data, $block_size) {
  $blocks = [];
  $i = 0;
  while ($i < count($data)) {
  $end = (_iadd($i, $block_size) < count($data) ? _iadd($i, $block_size) : count($data));
  $block = subarray($data, $i, $end);
  $sorted_block = merge_sort($block);
  $blocks = _append($blocks, $sorted_block);
  $i = $end;
};
  return $blocks;
};
  function merge_blocks($blocks) {
  $num_blocks = count($blocks);
  $indices = [];
  $i = 0;
  while ($i < $num_blocks) {
  $indices = _append($indices, 0);
  $i = _iadd($i, 1);
};
  $result = [];
  $done = false;
  while (!$done) {
  $done = true;
  $min_val = 0;
  $min_block = _isub(0, 1);
  $j = 0;
  while ($j < $num_blocks) {
  $idx = $indices[$j];
  if ($idx < count($blocks[$j])) {
  $val = $blocks[$j][$idx];
  if ($min_block == (_isub(0, 1)) || $val < $min_val) {
  $min_val = $val;
  $min_block = $j;
};
  $done = false;
}
  $j = _iadd($j, 1);
};
  if (!$done) {
  $result = _append($result, $min_val);
  $indices[$min_block] = _iadd($indices[$min_block], 1);
}
};
  return $result;
};
  function external_sort($data, $block_size) {
  $blocks = split_into_blocks($data, $block_size);
  return merge_blocks($blocks);
};
  function main() {
  $data = [7, 1, 5, 3, 9, 2, 6, 4, 8, 0];
  $sorted_data = external_sort($data, 3);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($sorted_data, 1344)))))), PHP_EOL;
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
