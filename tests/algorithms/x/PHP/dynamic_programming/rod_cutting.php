<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function enforce_args($n, $prices) {
  if ($n < 0) {
  $panic('n must be non-negative');
}
  if ($n > count($prices)) {
  $panic('price list is shorter than n');
}
}
function bottom_up_cut_rod($n, $prices) {
  enforce_args($n, $prices);
  $max_rev = null;
  $i = 0;
  while ($i <= $n) {
  if ($i == 0) {
  $max_rev = _append($max_rev, 0);
} else {
  $max_rev = _append($max_rev, -2147483648);
}
  $i = $i + 1;
};
  $length = 1;
  while ($length <= $n) {
  $best = $max_rev[$length];
  $j = 1;
  while ($j <= $length) {
  $candidate = $prices[$j - 1] + $max_rev[$length - $j];
  if ($candidate > $best) {
  $best = $candidate;
}
  $j = $j + 1;
};
  $max_rev[$length] = $best;
  $length = $length + 1;
};
  return $max_rev[$n];
}
$prices = [1, 5, 8, 9, 10, 17, 17, 20, 24, 30];
echo rtrim(json_encode(bottom_up_cut_rod(4, $prices), 1344)), PHP_EOL;
echo rtrim(json_encode(bottom_up_cut_rod(10, $prices), 1344)), PHP_EOL;
