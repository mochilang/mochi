<?php
function sum_rec($n, $acc) {
  if ($n == 0) {
  return $acc;
}
  return sum_rec($n - 1, $acc + $n);
}
echo (is_float(sum_rec(10, 0)) ? json_encode(sum_rec(10, 0), 1344) : sum_rec(10, 0)), PHP_EOL;
