<?php
function sum3($a, $b, $c) {
  return $a + $b + $c;
}
echo rtrim((is_float(sum3(1, 2, 3)) ? sprintf("%.15f", sum3(1, 2, 3)) : sum3(1, 2, 3))), PHP_EOL;
?>
