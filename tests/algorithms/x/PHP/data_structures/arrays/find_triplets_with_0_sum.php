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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function sort_triplet($a, $b, $c) {
  $x = $a;
  $y = $b;
  $z = $c;
  if ($x > $y) {
  $t = $x;
  $x = $y;
  $y = $t;
}
  if ($y > $z) {
  $t = $y;
  $y = $z;
  $z = $t;
}
  if ($x > $y) {
  $t = $x;
  $x = $y;
  $y = $t;
}
  return [$x, $y, $z];
}
function contains_triplet($arr, $target) {
  for ($i = 0; $i < count($arr); $i++) {
  $item = $arr[$i];
  $same = true;
  for ($j = 0; $j < count($target); $j++) {
  if ($item[$j] != $target[$j]) {
  $same = false;
  break;
}
};
  if ($same) {
  return true;
}
};
  return false;
}
function contains_int($arr, $value) {
  for ($i = 0; $i < count($arr); $i++) {
  if ($arr[$i] == $value) {
  return true;
}
};
  return false;
}
function find_triplets_with_0_sum($nums) {
  $n = count($nums);
  $result = [];
  for ($i = 0; $i < $n; $i++) {
  for ($j = ($i + 1); $j < $n; $j++) {
  for ($k = ($j + 1); $k < $n; $k++) {
  $a = $nums[$i];
  $b = $nums[$j];
  $c = $nums[$k];
  if ($a + $b + $c == 0) {
  $trip = sort_triplet($a, $b, $c);
  if (!contains_triplet($result, $trip)) {
  $result = _append($result, $trip);
};
}
};
};
};
  return $result;
}
function find_triplets_with_0_sum_hashing($arr) {
  $target_sum = 0;
  $output = [];
  for ($i = 0; $i < count($arr); $i++) {
  $seen = [];
  $current_sum = $target_sum - $arr[$i];
  for ($j = ($i + 1); $j < count($arr); $j++) {
  $other = $arr[$j];
  $required = $current_sum - $other;
  if (contains_int($seen, $required)) {
  $trip = sort_triplet($arr[$i], $other, $required);
  if (!contains_triplet($output, $trip)) {
  $output = _append($output, $trip);
};
}
  $seen = _append($seen, $other);
};
};
  return $output;
}
echo rtrim(_str(find_triplets_with_0_sum([-1, 0, 1, 2, -1, -4]))), PHP_EOL;
echo rtrim(_str(find_triplets_with_0_sum([]))), PHP_EOL;
echo rtrim(_str(find_triplets_with_0_sum([0, 0, 0]))), PHP_EOL;
echo rtrim(_str(find_triplets_with_0_sum([1, 2, 3, 0, -1, -2, -3]))), PHP_EOL;
echo rtrim(_str(find_triplets_with_0_sum_hashing([-1, 0, 1, 2, -1, -4]))), PHP_EOL;
echo rtrim(_str(find_triplets_with_0_sum_hashing([]))), PHP_EOL;
echo rtrim(_str(find_triplets_with_0_sum_hashing([0, 0, 0]))), PHP_EOL;
echo rtrim(_str(find_triplets_with_0_sum_hashing([1, 2, 3, 0, -1, -2, -3]))), PHP_EOL;
