<?php
ini_set('memory_limit', '-1');
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
function knapsack($weights, $values, $number_of_items, $max_weight, $index) {
  if ($index == $number_of_items) {
  return 0;
}
  $ans1 = knapsack($weights, $values, $number_of_items, $max_weight, $index + 1);
  $ans2 = 0;
  if ($weights[$index] <= $max_weight) {
  $ans2 = $values[$index] + knapsack($weights, $values, $number_of_items, $max_weight - $weights[$index], $index + 1);
}
  if ($ans1 > $ans2) {
  return $ans1;
}
  return $ans2;
}
function main() {
  $w1 = [1, 2, 4, 5];
  $v1 = [5, 4, 8, 6];
  $r1 = knapsack($w1, $v1, 4, 5, 0);
  echo rtrim(_str($r1)), PHP_EOL;
  $w2 = [3, 4, 5];
  $v2 = [10, 9, 8];
  $r2 = knapsack($w2, $v2, 3, 25, 0);
  echo rtrim(_str($r2)), PHP_EOL;
}
main();
