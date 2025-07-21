<?php
function sum3($a, $b, $c) {
  return $a + $b + $c;
}
echo (is_float(sum3(1, 2, 3)) ? json_encode(sum3(1, 2, 3), 1344) : sum3(1, 2, 3)), PHP_EOL;
