<?php
ini_set('memory_limit', '-1');
function max_product_subarray($numbers) {
  if (count($numbers) == 0) {
  return 0;
}
  $max_till_now = $numbers[0];
  $min_till_now = $numbers[0];
  $max_prod = $numbers[0];
  $i = 1;
  while ($i < count($numbers)) {
  $number = $numbers[$i];
  if ($number < 0) {
  $temp = $max_till_now;
  $max_till_now = $min_till_now;
  $min_till_now = $temp;
}
  $prod_max = $max_till_now * $number;
  if ($number > $prod_max) {
  $max_till_now = $number;
} else {
  $max_till_now = $prod_max;
}
  $prod_min = $min_till_now * $number;
  if ($number < $prod_min) {
  $min_till_now = $number;
} else {
  $min_till_now = $prod_min;
}
  if ($max_till_now > $max_prod) {
  $max_prod = $max_till_now;
}
  $i = $i + 1;
};
  return $max_prod;
}
echo rtrim(json_encode(max_product_subarray([2, 3, -2, 4]), 1344)), PHP_EOL;
echo rtrim(json_encode(max_product_subarray([-2, 0, -1]), 1344)), PHP_EOL;
