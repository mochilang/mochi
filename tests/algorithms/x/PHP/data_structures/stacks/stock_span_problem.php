<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function calculation_span($price) {
  global $spans;
  $n = count($price);
  $st = [];
  $span = [];
  $st = _append($st, 0);
  $span = _append($span, 1);
  for ($i = 1; $i < $n; $i++) {
  while (count($st) > 0 && $price[$st[count($st) - 1]] <= $price[$i]) {
  $st = array_slice($st, 0, count($st) - 1 - 0);
};
  $s = (count($st) <= 0 ? $i + 1 : $i - $st[count($st) - 1]);
  $span = _append($span, $s);
  $st = _append($st, $i);
};
  return $span;
}
function print_array($arr) {
  global $price, $spans;
  for ($i = 0; $i < count($arr); $i++) {
  echo rtrim(json_encode($arr[$i], 1344)), PHP_EOL;
};
}
$price = [10, 4, 5, 90, 120, 80];
$spans = calculation_span($price);
print_array($spans);
