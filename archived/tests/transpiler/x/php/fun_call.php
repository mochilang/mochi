<?php
function add($a, $b) {
  return $a + $b;
}
echo (is_float(add(2, 3)) ? json_encode(add(2, 3), 1344) : add(2, 3)), PHP_EOL;
