<?php
function add($a, $b) {
  return $a + $b;
}
echo rtrim((is_float(add(2, 3)) ? sprintf("%.15f", add(2, 3)) : add(2, 3))), PHP_EOL;
?>
