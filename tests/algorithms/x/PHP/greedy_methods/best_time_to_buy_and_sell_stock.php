<?php
ini_set('memory_limit', '-1');
function max_profit($prices) {
  if (count($prices) == 0) {
  return 0;
}
  $min_price = $prices[0];
  $max_profit = 0;
  $i = 0;
  while ($i < count($prices)) {
  $price = $prices[$i];
  if ($price < $min_price) {
  $min_price = $price;
}
  $profit = $price - $min_price;
  if ($profit > 'max_profit') {
  $max_profit = $profit;
}
  $i = $i + 1;
};
  return 'max_profit';
}
echo rtrim(json_encode(max_profit([7, 1, 5, 3, 6, 4]), 1344)), PHP_EOL;
echo rtrim(json_encode(max_profit([7, 6, 4, 3, 1]), 1344)), PHP_EOL;
