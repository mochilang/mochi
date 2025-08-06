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
function knapsack($capacity, $weights, $values, $counter) {
  if ($counter == 0 || $capacity == 0) {
  return 0;
}
  if ($weights[$counter - 1] > $capacity) {
  return knapsack($capacity, $weights, $values, $counter - 1);
} else {
  $left_capacity = $capacity - $weights[$counter - 1];
  $new_value_included = $values[$counter - 1] + knapsack($left_capacity, $weights, $values, $counter - 1);
  $without_new_value = knapsack($capacity, $weights, $values, $counter - 1);
  if ($new_value_included > $without_new_value) {
  return $new_value_included;
} else {
  return $without_new_value;
};
}
}
function main() {
  $weights = [10, 20, 30];
  $values = [60, 100, 120];
  $cap = 50;
  $count = count($values);
  $result = knapsack($cap, $weights, $values, $count);
  echo rtrim(_str($result)), PHP_EOL;
}
main();
