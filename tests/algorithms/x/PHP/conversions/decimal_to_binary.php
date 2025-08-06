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
  function decimal_to_binary_iterative($num) {
  if ($num == 0) {
  return '0b0';
}
  $negative = false;
  $n = $num;
  if ($n < 0) {
  $negative = true;
  $n = -$n;
}
  $result = '';
  while ($n > 0) {
  $result = _str($n % 2) . $result;
  $n = _intdiv($n, 2);
};
  if ($negative) {
  return '-0b' . $result;
}
  return '0b' . $result;
};
  function decimal_to_binary_recursive_helper($n) {
  if ($n == 0) {
  return '0';
}
  if ($n == 1) {
  return '1';
}
  $div = _intdiv($n, 2);
  $mod = $n % 2;
  return decimal_to_binary_recursive_helper($div) . _str($mod);
};
  function decimal_to_binary_recursive($num) {
  if ($num == 0) {
  return '0b0';
}
  if ($num < 0) {
  return '-0b' . decimal_to_binary_recursive_helper(-$num);
}
  return '0b' . decimal_to_binary_recursive_helper($num);
};
  echo rtrim(decimal_to_binary_iterative(0)), PHP_EOL;
  echo rtrim(decimal_to_binary_iterative(2)), PHP_EOL;
  echo rtrim(decimal_to_binary_iterative(7)), PHP_EOL;
  echo rtrim(decimal_to_binary_iterative(35)), PHP_EOL;
  echo rtrim(decimal_to_binary_iterative(-2)), PHP_EOL;
  echo rtrim(decimal_to_binary_recursive(0)), PHP_EOL;
  echo rtrim(decimal_to_binary_recursive(40)), PHP_EOL;
  echo rtrim(decimal_to_binary_recursive(-40)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
