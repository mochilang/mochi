<?php
ini_set('memory_limit', '-1');
function product_sum($arr, $depth) {
  global $example;
  $total = 0;
  $i = 0;
  while ($i < count($arr)) {
  $el = $arr[$i];
  if (count($el) > 0) {
  $total = $total + product_sum($el, $depth + 1);
} else {
  $total = $total + intval($el);
}
  $i = $i + 1;
};
  return $total * $depth;
}
function product_sum_array($array) {
  global $example;
  $res = product_sum($array, 1);
  return $res;
}
$example = [5, 2, [-7, 1], 3, [6, [-13, 8], 4]];
echo rtrim(json_encode(product_sum_array($example), 1344)), PHP_EOL;
