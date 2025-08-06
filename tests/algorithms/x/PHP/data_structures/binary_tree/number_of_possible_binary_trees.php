<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function binomial_coefficient($n, $k) {
  global $input_str, $node_count, $bst, $bt;
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
}
function catalan_number($node_count) {
  global $input_str, $bst, $bt;
  return binomial_coefficient(2 * $node_count, $node_count) / ($node_count + 1);
}
function factorial($n) {
  global $input_str, $node_count, $bst, $bt;
  if ($n < 0) {
  echo rtrim('factorial() not defined for negative values'), PHP_EOL;
  return 0;
}
  $result = 1;
  for ($i = 1; $i < ($n + 1); $i++) {
  $result = $result * $i;
};
  return $result;
}
function binary_tree_count($node_count) {
  global $input_str, $bst, $bt;
  return catalan_number($node_count) * factorial($node_count);
}
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
