<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function ceil_index($v, $left, $right, $key) {
  $l = $left;
  $r = $right;
  while ($r - $l > 1) {
  $middle = _intdiv(($l + $r), 2);
  if ($v[$middle] >= $key) {
  $r = $middle;
} else {
  $l = $middle;
}
};
  return $r;
}
function longest_increasing_subsequence_length($v) {
  if (count($v) == 0) {
  return 0;
}
  $tail = [];
  $i = 0;
  while ($i < count($v)) {
  $tail = _append($tail, 0);
  $i = $i + 1;
};
  $length = 1;
  $tail[0] = $v[0];
  $j = 1;
  while ($j < count($v)) {
  if ($v[$j] < $tail[0]) {
  $tail[0] = $v[$j];
} else {
  if ($v[$j] > $tail[$length - 1]) {
  $tail[$length] = $v[$j];
  $length = $length + 1;
} else {
  $idx = ceil_index($tail, -1, $length - 1, $v[$j]);
  $tail[$idx] = $v[$j];
};
}
  $j = $j + 1;
};
  return $length;
}
function main() {
  $example1 = [2, 5, 3, 7, 11, 8, 10, 13, 6];
  $example2 = [];
  $example3 = [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15];
  $example4 = [5, 4, 3, 2, 1];
  echo rtrim(json_encode(longest_increasing_subsequence_length($example1), 1344)), PHP_EOL;
  echo rtrim(json_encode(longest_increasing_subsequence_length($example2), 1344)), PHP_EOL;
  echo rtrim(json_encode(longest_increasing_subsequence_length($example3), 1344)), PHP_EOL;
  echo rtrim(json_encode(longest_increasing_subsequence_length($example4), 1344)), PHP_EOL;
}
main();
