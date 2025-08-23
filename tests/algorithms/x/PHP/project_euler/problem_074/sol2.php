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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv(intval($a), intval($b));
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $DIGIT_FACTORIAL = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880];
  function digit_factorial_sum($number) {
  global $DIGIT_FACTORIAL;
  if ($number < 0) {
  _panic('Parameter number must be greater than or equal to 0');
}
  if ($number == 0) {
  return $DIGIT_FACTORIAL[0];
}
  $n = $number;
  $total = 0;
  while ($n > 0) {
  $digit = $n % 10;
  $total = $total + $DIGIT_FACTORIAL[$digit];
  $n = _intdiv($n, 10);
};
  return $total;
};
  function chain_len($n, $limit) {
  global $DIGIT_FACTORIAL;
  $seen = [];
  $length = 0;
  $cur = $n;
  while ((array_key_exists($cur, $seen)) == false && $length <= $limit) {
  $seen[$cur] = true;
  $length = $length + 1;
  $cur = digit_factorial_sum($cur);
};
  return $length;
};
  function solution($chain_length, $number_limit) {
  global $DIGIT_FACTORIAL;
  if ($chain_length <= 0 || $number_limit <= 0) {
  _panic('Parameters chain_length and number_limit must be greater than 0');
}
  $count = 0;
  $start = 1;
  while ($start < $number_limit) {
  if (chain_len($start, $chain_length) == $chain_length) {
  $count = $count + 1;
}
  $start = $start + 1;
};
  return $count;
};
  echo rtrim(_str(solution(60, 1000000))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
