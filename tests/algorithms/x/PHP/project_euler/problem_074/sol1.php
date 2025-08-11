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
  $DIGIT_FACTORIALS = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880];
  $cache_sum_digit_factorials = [145 => 145];
  $chain_length_cache = [145 => 0, 169 => 3, 36301 => 3, 1454 => 3, 871 => 2, 45361 => 2, 872 => 2];
  function sum_digit_factorials($n) {
  global $DIGIT_FACTORIALS, $cache_sum_digit_factorials, $chain_length_cache;
  if (array_key_exists($n, $cache_sum_digit_factorials)) {
  return $cache_sum_digit_factorials[$n];
}
  $m = $n;
  $ret = 0;
  if ($m == 0) {
  $ret = $DIGIT_FACTORIALS[0];
}
  while ($m > 0) {
  $digit = _imod($m, 10);
  $ret = _iadd($ret, $DIGIT_FACTORIALS[$digit]);
  $m = _intdiv($m, 10);
};
  $cache_sum_digit_factorials[$n] = $ret;
  return $ret;
};
  function chain_length($n) {
  global $DIGIT_FACTORIALS, $cache_sum_digit_factorials, $chain_length_cache;
  if (array_key_exists($n, $chain_length_cache)) {
  return $chain_length_cache[$n];
}
  $chain = [];
  $seen = [];
  $current = $n;
  while (true) {
  if (array_key_exists($current, $chain_length_cache)) {
  $known = $chain_length_cache[$current];
  $total = $known;
  $i = _isub(count($chain), 1);
  while ($i >= 0) {
  $total = _iadd($total, 1);
  $chain_length_cache[$chain[$i]] = $total;
  $i = _isub($i, 1);
};
  return $chain_length_cache[$n];
}
  if (array_key_exists($current, $seen)) {
  $loop_start = $seen[$current];
  $loop_len = _isub(count($chain), $loop_start);
  $i = _isub(count($chain), 1);
  $ahead = 0;
  while ($i >= 0) {
  if ($i >= $loop_start) {
  $chain_length_cache[$chain[$i]] = $loop_len;
} else {
  $chain_length_cache[$chain[$i]] = _iadd($loop_len, (_iadd($ahead, 1)));
}
  $ahead = _iadd($ahead, 1);
  $i = _isub($i, 1);
};
  return $chain_length_cache[$n];
}
  $seen[$current] = count($chain);
  $chain = _append($chain, $current);
  $current = sum_digit_factorials($current);
};
};
  function solution($num_terms, $max_start) {
  global $DIGIT_FACTORIALS, $cache_sum_digit_factorials, $chain_length_cache;
  $count = 0;
  $i = 1;
  while ($i < $max_start) {
  if (chain_length($i) == $num_terms) {
  $count = _iadd($count, 1);
}
  $i = _iadd($i, 1);
};
  return $count;
};
  echo rtrim('solution() = ' . _str(solution(60, 1000))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
