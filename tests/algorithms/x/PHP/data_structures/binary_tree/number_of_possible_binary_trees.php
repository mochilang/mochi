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
$__start_mem = memory_get_usage();
$__start = _now();
  function binomial_coefficient($n, $k) {
  global $bst, $bt, $input_str, $node_count;
  $result = 1;
  $kk = $k;
  if ($k > $n - $k) {
  $kk = $n - $k;
}
  for ($i = 0; $i < $kk; $i++) {
  $result = $result * ($n - $i);
  $result = _intdiv($result, ($i + 1));
};
  return $result;
};
  function catalan_number($node_count) {
  global $bst, $bt, $input_str;
  return binomial_coefficient(2 * $node_count, $node_count) / ($node_count + 1);
};
  function factorial($n) {
  global $bst, $bt, $input_str, $node_count;
  if ($n < 0) {
  echo rtrim('factorial() not defined for negative values'), PHP_EOL;
  return 0;
}
  $result = 1;
  for ($i = 1; $i < ($n + 1); $i++) {
  $result = $result * $i;
};
  return $result;
};
  function binary_tree_count($node_count) {
  global $bst, $bt, $input_str;
  return catalan_number($node_count) * factorial($node_count);
};
  echo rtrim('Enter the number of nodes:'), PHP_EOL;
  $input_str = trim(fgets(STDIN));
  $node_count = intval($input_str);
  if ($node_count <= 0) {
  echo rtrim('We need some nodes to work with.'), PHP_EOL;
} else {
  $bst = catalan_number($node_count);
  $bt = binary_tree_count($node_count);
  echo rtrim('Given') . " " . rtrim(json_encode($node_count, 1344)) . " " . rtrim('nodes, there are') . " " . rtrim(json_encode($bt, 1344)) . " " . rtrim('binary trees and') . " " . rtrim(json_encode($bst, 1344)) . " " . rtrim('binary search trees.'), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
