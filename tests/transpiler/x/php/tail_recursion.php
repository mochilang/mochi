<?php
function sum_rec($n, $acc) {
  if ($n == 0) {
  return $acc;
}
  return sum_rec($n - 1, $acc + $n);
}
echo rtrim((is_float(sum_rec(10, 0)) ? sprintf("%.15f", sum_rec(10, 0)) : sum_rec(10, 0))), PHP_EOL;
?>
